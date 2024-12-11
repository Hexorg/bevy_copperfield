use core::f32;

use glam::{Vec2, Vec3};
use bevy_utils::{hashbrown::HashMap, HashSet};
use itertools::Itertools;
use slotmap::SecondaryMap;
use sprs::{CsMatView, CsVec, CsVecView, TriMat};

use crate::mesh::{
    attributes::{AttributeValues, TraversalQueries},
    FaceId, HalfEdgeId, HalfEdgeMesh, StackVec, VertexId,
};

use super::Chart;

/// Solves Ax = b matrix in a least-squares way. A does not need to be diagonal, or square.
/// Based on https://web.stanford.edu/group/SOL/software/cgls/
fn conjugate_gradient_least_squares(
    a: CsMatView<f32>,
    b: CsVecView<'_, f32>,
    guess: Option<CsVecView<'_, f32>>,
    tolerance: f32,
) -> Option<CsVec<f32>> {
    let max_iteration = a.cols().max(a.rows());
    let empty = CsVec::empty(a.cols());
    let mut x = if let Some(x0) = guess {
        x0.to_owned()
    } else {
        empty
    };
    let a_transpose = a.transpose_view();
    let mut r = &b - &(&a * &x).view();
    let mut s = &a_transpose * &r.view();

    let mut p = s.clone();
    let norm_s0 = s.l2_norm();
    let mut gamma = norm_s0.powi(2);
    let mut norm_x = x.l2_norm();
    let mut x_max = norm_x;

    for _ in 0..max_iteration {
        let q = &a * &p;
        let delta = q.squared_l2_norm();
        if delta < 0.0 {
            return None;
        }
        let alpha = gamma / delta;

        x = x + p.map(|&v| v * alpha);
        r = &r - &q.map(|&v| v * alpha);

        s = &a_transpose * &r.view();
        let norm_s = s.l2_norm();
        let gamma1 = gamma;
        gamma = norm_s.powi(2);
        let beta = gamma / gamma1;
        p = s + p.map(|&v| beta * v);

        norm_x = x.l2_norm();
        x_max = x_max.max(norm_x);

        let is_met_tolerance = (norm_s <= norm_s0 * tolerance) || (norm_x * tolerance >= 1.0);
        if is_met_tolerance {
            break;
        }
    }

    Some(x)
}

/// Section 2 of the [Least Squares Conformal Maps](https://dl.acm.org/doi/pdf/10.1145/566654.566590) paper.
/// Given a set of charts, project each chart to a 2D plane, preserving face angles.
fn lscm(mesh: &HalfEdgeMesh, chart: &HashSet<FaceId>) -> Vec<(HalfEdgeId, Vec2)> {
    let pinned_uv = [Vec2 { x: 0.5, y: 0.5 }, Vec2 { x: 0.5, y: 1.0 }];
    let pinned_vertices = mesh
        .goto(*chart.iter().next().unwrap())
        .iter_loop()
        .take(2)
        .map(|t| t.vertex())
        .collect::<StackVec<_>>();
    let pinned_vertex_count = pinned_vertices.len();
    let free_vertex_count = chart
        .iter()
        .flat_map(|&f| mesh.goto(f).iter_loop().map(|e| e.vertex()))
        .unique()
        .count()
        - pinned_vertex_count;

    let mut coefficients = HashMap::new();
    let mut triangle_count = 0; // amount of triangles
    for face in chart {
        // faces can be any polygons
        let face = mesh.goto(*face);
        for (v1, v2, v3) in face.triangulate().into_iter().tuples() {
            triangle_count += 1;
            let p = [
                mesh.goto(v1).position(),
                mesh.goto(v2).position(),
                mesh.goto(v3).position(),
            ];
            let l = [
                (p[1] - p[0]).length(),
                (p[2] - p[1]).length(),
                (p[0] - p[2]).length(),
            ];
            let angle = ((l[0] * l[0] + l[2] * l[2] - l[1] * l[1]) / (2.0 * l[0] * l[2])).acos();
            let (s, c) = angle.sin_cos();
            let triangle = [
                Vec3::ZERO,
                Vec3 {
                    x: l[0],
                    y: 0.0,
                    z: 0.0,
                },
                Vec3 {
                    x: l[2] * c,
                    y: l[2] * s,
                    z: 0.0,
                },
            ];
            let mut n = (triangle[1] - triangle[0]).cross(triangle[2] - triangle[0]);
            let area = n.length();
            n /= area;
            let s = triangle
                .into_iter()
                .circular_tuple_windows()
                .map(|(l, r)| n.cross(r - l) / area)
                .collect::<StackVec<_>>();
            coefficients.insert((v1, v2), s[1]);
            coefficients.insert((v2, v3), s[2]);
            coefficients.insert((v3, v1), s[0]);
        }
    }

    let mut a_mat = TriMat::new((2 * triangle_count, 2 * free_vertex_count));
    let mut b_mat = TriMat::new((2 * triangle_count, 2 * pinned_vertex_count));
    let mut b_indices = Vec::new();
    let mut b_data = Vec::new();

    let mut vertex_mapping: SecondaryMap<VertexId, usize> = SecondaryMap::new();
    b_indices.push(0);
    b_data.push(pinned_uv[0].x);
    b_indices.push(pinned_vertex_count);
    b_data.push(pinned_uv[0].y);
    b_indices.push(1);
    b_data.push(pinned_uv[1].x);
    b_indices.push(pinned_vertex_count + 1);
    b_data.push(pinned_uv[1].y);

    let mut triangle_idx = 0;
    for face in chart {
        let face = mesh.goto(*face);
        for (v1, v2, v3) in face.triangulate().into_iter().tuples() {
            for pair in [(v1, v2), (v2, v3), (v3, v1)] {
                let m_ij = coefficients[&pair];
                if pinned_vertices.contains(&pair.0) {
                    let vertex_idx = if pinned_vertices[0] == pair.0 { 0 } else { 1 };
                    b_mat.add_triplet(triangle_idx, vertex_idx, m_ij.x);
                    b_mat.add_triplet(
                        triangle_count + triangle_idx,
                        pinned_vertex_count + vertex_idx,
                        m_ij.x,
                    );
                    b_mat.add_triplet(triangle_idx, pinned_vertex_count + vertex_idx, -m_ij.y);
                    b_mat.add_triplet(triangle_count + triangle_idx, vertex_idx, m_ij.y);
                } else {
                    let vertex_mapping_size = vertex_mapping.len();
                    let vertex_idx = if let Some(vid) = vertex_mapping.get(pair.0) {
                        *vid
                    } else {
                        vertex_mapping.insert(pair.0, vertex_mapping_size);
                        vertex_mapping_size
                    };
                    a_mat.add_triplet(triangle_idx, vertex_idx, m_ij.x);
                    a_mat.add_triplet(triangle_idx, free_vertex_count + vertex_idx, -m_ij.y);
                    a_mat.add_triplet(
                        triangle_count + triangle_idx,
                        free_vertex_count + vertex_idx,
                        m_ij.x,
                    );
                    a_mat.add_triplet(triangle_count + triangle_idx, vertex_idx, m_ij.y);
                }
            }
            triangle_idx += 1;
        }
    }
    let b = CsVec::new_from_unsorted(pinned_vertex_count * 2, b_indices, b_data).unwrap();
    let a_mat = a_mat.to_csc::<usize>();
    let b_mat = b_mat.to_csc::<usize>();
    let r = -(&b_mat * &b);
    let x = conjugate_gradient_least_squares(a_mat.view(), r.view(), None, 1.0 / 256.0).unwrap();

    chart
        .iter()
        .flat_map(|&f| {
            mesh.goto(f).iter_loop().map(|e| {
                let uv = if let Some(&i) = vertex_mapping.get(e.vertex()) {
                    Vec2 {
                        x: *x.get(i).unwrap_or(&0.0),
                        y: *x.get(i + free_vertex_count).unwrap_or(&0.0),
                    }
                } else if e.vertex() == pinned_vertices[0] {
                    pinned_uv[0]
                } else {
                    pinned_uv[1]
                };
                (e.halfedge(), uv)
            })
        })
        .collect()
}

pub(crate) fn project(mesh: &mut HalfEdgeMesh, charts: Vec<Chart>) {
    let charts_per_row = (charts.len() as f32).sqrt().ceil() as usize;
    let mut uvmaps = Vec::new();
    for mut chart in charts.into_iter() {
        let mut edge_map = lscm(mesh, &chart.faces);
        while let Some(new_chart) = check_self_intersection(mesh, &chart.faces, &edge_map) {
            chart.faces = chart
                .faces
                .difference(&new_chart)
                .copied()
                .collect::<HashSet<_>>();
            edge_map = lscm(mesh, &chart.faces);
            uvmaps.push(lscm(mesh, &new_chart));
        }
        uvmaps.push(edge_map);
    }
    for (idx, mut edge_map) in uvmaps.into_iter().enumerate() {
        // Now we can transform edge_map in any way - it contains the whole chart
        let scale = charts_per_row as f32;
        let shift = Vec2 {
            x: (idx % charts_per_row) as f32,
            y: (idx / charts_per_row) as f32,
        };
        let (top_left, bottom_right) = edge_map
            .iter()
            .fold((Vec2::ONE * 1e6, Vec2::ONE * -1e6), |(tl, br), (_, uv)| {
                (tl.min(*uv), br.max(*uv))
            });
        for (_, uv) in &mut edge_map {
            // TODO: Better chart packing
            *uv = (shift + ((*uv - top_left) / (bottom_right - top_left))) / scale;
        }
        if let Some(uvmap) = mesh.attribute_mut(&crate::mesh::attributes::AttributeKind::UVs) {
            uvmap.as_edge_vec2_mut().extend(edge_map);
        } else {
            mesh.add_attribute(
                crate::mesh::attributes::AttributeKind::UVs,
                AttributeValues::EdgeVec2(SecondaryMap::from_iter(edge_map)),
            );
        }
    }
}

fn check_self_intersection(
    mesh: &HalfEdgeMesh,
    chart: &HashSet<FaceId>,
    map: &[(HalfEdgeId, Vec2)],
) -> Option<HashSet<FaceId>> {
    let map: HashMap<HalfEdgeId, Vec2> = HashMap::from_iter(map.iter().copied());
    let chart_boundary_edge = |e: HalfEdgeId| {
        mesh.goto(e)
            .twin()
            .face()
            .map(|f| !chart.contains(&f))
            .unwrap_or(true)
    };
    if let Some((&start_edge, &start_uv)) = map.iter().find(|(e, _)| chart_boundary_edge(**e)) {
        let mut trace = vec![(start_edge, start_uv)];
        let mut current_edge = start_edge;
        while let Some(next_boundary) = mesh
            .goto(current_edge)
            .next()
            .iter_outgoing()
            .find(|t| chart_boundary_edge(t.halfedge()))
        {
            let line_from = trace.last().unwrap().1;
            let next_edge = next_boundary.halfedge();
            let line_to = map[&next_edge];
            if Some(&(next_edge, line_to)) == trace.first() {
                break;
            }
            if let Some((idx, _collision)) =
                trace
                    .iter()
                    .tuple_windows()
                    .enumerate()
                    .find(|(_, ((_, f), (l, t)))| {
                        let t = *t;
                        let f = *f;
                        let c = (line_to - line_from).perp_dot(t - f).recip();
                        let u = c * (t - f).perp_dot(line_from - f);
                        let v = c * (line_to - line_from).perp_dot(line_from - f);
                        *l != current_edge && (0.0..=1.0).contains(&u) && (0.0..=1.0).contains(&v)
                    })
            {
                let mut new_chart = HashSet::new();
                fn insert(
                    mesh: &HalfEdgeMesh,
                    chart: &mut HashSet<FaceId>,
                    trace: &[(HalfEdgeId, Vec2)],
                    edge: HalfEdgeId,
                ) {
                    if let Some(face) = mesh.goto(edge).face() {
                        if chart.insert(face) {
                            for edge in mesh.goto(face).iter_loop() {
                                if !trace.iter().any(|(e, _)| *e == edge.halfedge())
                                    && edge.face().map(|f| !chart.contains(&f)).unwrap_or(false)
                                {
                                    insert(mesh, chart, trace, edge.halfedge());
                                }
                            }
                        }
                    };
                }
                for i in idx..trace.len() {
                    insert(
                        mesh,
                        &mut new_chart,
                        &trace[idx.saturating_sub(1)..],
                        trace[i].0,
                    );
                }
                return Some(new_chart);
            } else {
                trace.push((next_edge, line_to));
                current_edge = next_edge;
            }
        }
    }
    None
}
