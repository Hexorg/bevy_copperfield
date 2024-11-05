use std::collections::BinaryHeap;

use bevy::{prelude::{Vec2, Vec3}, utils::hashbrown::HashSet};
use itertools::Itertools;
use slotmap::SecondaryMap;

use crate::mesh::{attributes::{AttributeKind, AttributeValues, TraversalQueries}, FaceId, HalfEdgeId, HalfEdgeMesh};


// mod fit;
// TODO: Depending on complexity, several UV-Mapping approaches exist. Explore what's usefull.
// 1. Cube/Cylindrical Mapping - Project each face of a mesh to a face of a cube/cylinder. 
pub(crate) mod primitive_mapping;
//      Since Cube/Cylinder UV mapping is known - projected faces can also be uv-mapped.
// 2. Conformal Mapping - Generate UV-Mapping by preserving angle locality (e.g. angles of faces in 3D mesh are preverved in UV-map) 
pub(crate) mod least_squares_conformal_maps;
// 3. Radial Basis Functions (RBF)
// 4. Multi-dimensional Scaling (MDS)
// 5. Boundary representation (B-Rep) and Parametrization


/// Basis class to compute tangent space basis, ortogonalizations and to transform vectors from one space to another.
pub struct Basis{
    pub tangent: Vec3,
    pub bitangent: Vec3,
    pub normal: Vec3
}

impl Basis {
    /// Project 3D vertex position onto the basis vectors
    pub fn project_vertex(&self, pos:Vec3) -> Vec2 {
        Vec2{x:self.tangent.dot(pos), y:self.bitangent.dot(pos)}
    }

    pub fn from_normal(normal:Vec3) -> Self {
        let tangent = Self::compute_tangent(normal);
        let bitangent = Self::compute_bitangent(normal, tangent);
        Self { tangent, bitangent, normal}
    }

    pub fn from_eigen_vectors(eigen_vectors:[Vec3;3]) -> Self {
        Basis{tangent:eigen_vectors[0].normalize(), bitangent:eigen_vectors[1].normalize(), normal:eigen_vectors[2].normalize()}
    }

    fn compute_tangent(normal:Vec3) -> Vec3 {
        let mut tangent = Vec3::ZERO;
        // Choose minimum axis
        if normal.x.abs() < normal.y.abs() && normal.x.abs() < normal.z.abs() {
            tangent.x = 1.0;
        } else if normal.y.abs() < normal.z.abs() {
            tangent.y = 1.0;
        } else {
            tangent.z = 1.0;
        }
        // Ortogonalize
        tangent -= normal * normal.dot(tangent);
        tangent.normalize()
    }

    #[inline]
    fn compute_bitangent(normal:Vec3, tangent:Vec3) -> Vec3 {
        normal.cross(tangent)
    }
}



/// Set of model parts homeomorphic to discs as per 
/// [Least Squares Conformal Maps](https://dl.acm.org/doi/pdf/10.1145/566654.566590) 
/// paper by Bruno Levy
pub struct Chart {
    pub faces: HashSet<FaceId>, 
    /// maximum distance from one of the faces to a feature
    max_distance:i32 
    // XAtlas has Material(u32) here too
}

impl Default for Chart {
    fn default() -> Self {
        Self::new()
    }
}

impl Chart {
    pub fn new() -> Self {
        Self { faces: HashSet::new(), max_distance:0}
    }
}

pub enum ProjectionMethod{
    LSCM,
    Sphere{center:Vec3, radius:Vec3},
    Cube{center:Vec3, scale:Vec3}
}

/// Based on [Xatlas](https://github.com/jpcy/xatlas/tree/master)'s implementation and
/// https://dl.acm.org/doi/10.1145/566654.566590 paper
pub fn create_charts(mesh:&mut HalfEdgeMesh) -> Vec<Chart>{
    const MIN_FEATURE_LENGTH:usize = 15;
    let count = mesh.face_keys().len();
    let edges = mesh.face_keys().map(|f| mesh[f].halfedge).sorted_by(|&l, &r| {
        mesh.goto(r).sharpness().partial_cmp(&mesh.goto(l).sharpness()).unwrap_or(std::cmp::Ordering::Less)
    }).take(5 * count / 100).collect::<Vec<_>>(); // take 5% of the sharpest faces

    let mut neighborhoods =  HashSet::new();
    let mut feature_faces:Vec<FaceId> = Vec::new();
    let mut uv_seams:SecondaryMap<HalfEdgeId, bool> = SecondaryMap::new();
    for edge in edges {
        let feature = expand_feature_curve(mesh,&mut neighborhoods, edge);
        if feature.len() >= MIN_FEATURE_LENGTH {
            for &edge in &feature {
                uv_seams.insert(edge, true);
                let edge = mesh.goto(edge);
                uv_seams.insert(*edge.twin(), true);
                if let Some(face) = edge.face() {
                    feature_faces.push(face);
                }
                if let Some(twin_face) = edge.twin().face() {
                    feature_faces.push(twin_face);
                }
                for edge in edge.iter_outgoing() {
                    neighborhoods.insert(*edge);
                    neighborhoods.insert(*edge.twin());
                }
            }
        }
    }

    let (maximas, distances) = find_distance_to_features_with_dikstra(mesh, &feature_faces);
    let (charts, boundaries) = expand_charts(mesh, &maximas, &distances);
    let mut uv_seams:SecondaryMap<HalfEdgeId, bool> = SecondaryMap::new();
    for &edge in &boundaries {
        let twin = * mesh.goto(edge).twin();
        uv_seams.insert(edge, true);
        uv_seams.insert(twin, true);
    }
    mesh.add_attribute(AttributeKind::UVSeams, AttributeValues::EdgeBool(uv_seams));
    let charted_faces = charts.iter().fold(0, |acc, i| acc + i.faces.len());
    assert_eq!(mesh.face_count(), charted_faces);
    charts
}

pub fn expand_feature_curve(mesh:&mut HalfEdgeMesh, neighborhood_edges:&mut HashSet<HalfEdgeId>, start:HalfEdgeId) -> Vec<HalfEdgeId> {
    const MAX_STRING_LENGTH:usize = 5;
    const THRESHOLD:f32 = 0.1;
    fn dfs(mesh:&HalfEdgeMesh, start_pos:Vec3, pos:HalfEdgeId, string:Vec<HalfEdgeId>, neighborhood_edges: &HashSet<HalfEdgeId>) -> (f32, Vec<HalfEdgeId>) {
        let v_pos = mesh.goto(pos).position();
        let (mut best_path_sharpness, mut best_path) = (0.0, Vec::new());
        for next in mesh.goto(pos).next().iter_outgoing().sorted_by(|l, r| r.sharpness().partial_cmp(&l.sharpness()).unwrap_or(std::cmp::Ordering::Less)) {
            let next_pos = next.position();
            let next = *next;
            if (start_pos - v_pos).length() < (start_pos - next_pos).length() && // no halfedge goes backwards relative to h'
                string.len() <= MAX_STRING_LENGTH && // length of string is <= max_string_length
                !neighborhood_edges.contains(&next) // no halfedge is tagged as feature neighbor
            {
                let mut string = string.clone();
                string.push(next);
                let (path_sharpness, path) = dfs(mesh, start_pos, next, string, neighborhood_edges);
                if path_sharpness > best_path_sharpness {
                    best_path_sharpness = path_sharpness;
                    best_path = path;
                }
            }
        }
        if best_path.len() > string.len() {
            (best_path_sharpness, best_path)
        } else {
            let path_sharpness = string.iter().map(|e| mesh.goto(*e).sharpness()).sum();
            (path_sharpness, string)
        }
    }

    let mut detected_feature:Vec<HalfEdgeId> = Vec::new();
    let twin = *mesh.goto(start).twin();
    for edge in [start, twin] {
        let mut current = edge;
        let pos = mesh.goto(edge).position();
        loop {
            let (sharpness, string) = dfs(mesh, pos, current, Vec::new(), neighborhood_edges);
            if sharpness > MAX_STRING_LENGTH as f32 * THRESHOLD {
                current = string[0];
                detected_feature.push(current);
            } else {
                break;
            }
            
        }
    }
    detected_feature
}

fn find_distance_to_features_with_dikstra(mesh:&HalfEdgeMesh, features:&[FaceId]) -> (Vec<FaceId>, SecondaryMap<FaceId, usize>) {
    #[derive(PartialOrd, Eq)]
    struct FaceWithDistance{
        face:FaceId,
        distance:usize
    }
    impl PartialEq for FaceWithDistance {
        fn eq(&self, other: &Self) -> bool {
            self.face == other.face
        }
    }
    impl Ord for FaceWithDistance {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            other.distance.cmp(&self.distance)
        }
    }
    let mut min_heap:BinaryHeap<FaceWithDistance> = BinaryHeap::new();
    let mut distances:SecondaryMap<FaceId, usize> = SecondaryMap::new();
    let mut local_maxima: HashSet<FaceId> = HashSet::new();
    for &face in features {
        distances.insert(face, 0);
        min_heap.push(FaceWithDistance{face, distance:0});
    }
    while let Some(FaceWithDistance{face, distance}) = min_heap.pop() {
        let mut local_found = true;
        for neighbor_face in mesh.goto(face).iter_loop().flat_map(|e| e.adjacent_faces()) {
            let alternative_distance = distance + 1;
            if let Some(face) = neighbor_face.face() {
                let mut known_distance = *distances.get(face).unwrap_or(&usize::MAX);
                if alternative_distance < known_distance {
                    distances.insert(face, alternative_distance);
                    min_heap.push(FaceWithDistance{face, distance:alternative_distance});
                    known_distance = alternative_distance;
                }
                if known_distance >= distance {
                    local_found = false;
                }
            }
        }
        if local_found {
            local_maxima.insert(face);
        }
    }
    let mut other_local_maxima: Vec<FaceId> = Vec::new();
    for face in mesh.face_keys() {
        let face_distance = distances[face];
        if face_distance != 0 && !mesh.goto(face).iter_loop().any(|e| face_distance <= e.twin().face().map(|f| distances[f]).unwrap_or(0)) {
            other_local_maxima.push(face);
            // assert_eq!(local_maxima.contains(&face), true);
        }
    }
    // assert_eq!(local_maxima.len(), other_local_maxima.len());1111
    (other_local_maxima, distances)
}

fn expand_charts(mesh:&HalfEdgeMesh, seeds:&[FaceId], distances:&SecondaryMap<FaceId, usize>) -> (Vec<Chart>, HashSet<HalfEdgeId>) {
    #[derive(Eq)]
    struct EdgeWithCost{
        edge:HalfEdgeId,
        cost:usize
    }
    impl Ord for EdgeWithCost {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.cost.cmp(&other.cost)
        }
    }
    impl PartialOrd for EdgeWithCost {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }
    impl PartialEq for EdgeWithCost {
        fn eq(&self, other: &Self) -> bool {
            self.edge == other.edge
        }
    }

    let mut chart_boundaries:HashSet<HalfEdgeId> = HashSet::from_iter(mesh.edge_keys());
    let mut heap = BinaryHeap::new();
    let mut charts = Vec::new();
    let mut global_max_distance = 0;
    for &face in seeds {
        let mut chart = Chart::new();
        chart.faces.insert(face);
        chart.max_distance = distances[face] as i32;
        global_max_distance = global_max_distance.max(chart.max_distance);
        charts.push(chart);
        for edge in mesh.goto(face).iter_loop() {
            heap.push(EdgeWithCost{edge:*edge, cost:0});
        }
    }

    let threshold:i32 = global_max_distance / 4;
    while let Some(e) = heap.pop() {
        let edge = mesh.goto(e.edge);
        let face = edge.face().unwrap();
        let twin = edge.twin();
        let face_opposite = twin.face().unwrap();
        let (mut face_chart_idx, _) = charts.iter().enumerate().find(|c| c.1.faces.contains(&face)).unwrap();
        let face_opposite_chart_idx = charts.iter().enumerate().find(|c| c.1.faces.contains(&face_opposite));
        
        if let Some((face_opposite_chart_idx, _)) = face_opposite_chart_idx { 
            if face_chart_idx != face_opposite_chart_idx &&
            charts[face_chart_idx].max_distance - (distances[face] as i32) < threshold &&
            charts[face_opposite_chart_idx].max_distance - (distances[face] as i32) < threshold
            {
                chart_boundaries.remove(&(*edge));
                chart_boundaries.remove(&(*twin));
                // merge charts 
                let mut opposite_chart = charts.remove(face_opposite_chart_idx);
                if face_opposite_chart_idx < face_chart_idx {
                    face_chart_idx -= 1;
                }
                charts[face_chart_idx].faces.extend(opposite_chart.faces.drain());
            }
            if face_chart_idx == face_opposite_chart_idx {
                chart_boundaries.remove(&(*edge));
                chart_boundaries.remove(&(*twin));
            }
        } else {
            chart_boundaries.remove(&(*edge));
            charts[face_chart_idx].faces.insert(face_opposite); 
            for edge in twin.iter_loop() {
                // Find if other faces are in our chart. Those that are should be removed from chart_boundaries,
                // those that aren't should be added to the heap.
                if !edge.twin().face().map(|f| charts[face_chart_idx].faces.contains(&f)).unwrap_or(true) {
                    heap.push(EdgeWithCost { edge: *edge, cost: distances[face_opposite] });
                } else {
                    chart_boundaries.remove(&(*edge));
                    chart_boundaries.remove(&(*edge.twin()));
                }
            }
        }
    }
    (charts, chart_boundaries)
}