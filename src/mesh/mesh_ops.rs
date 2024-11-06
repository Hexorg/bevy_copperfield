use bevy::{prelude::{Transform, Vec3}, utils::hashbrown::HashSet};
use itertools::Itertools;
use slotmap::SecondaryMap;

use crate::mesh::attributes::{TraversalQueries, AttributeKind};

use super::{attributes::SelectionQueries, edge_ops, FaceId, HalfEdgeId, HalfEdgeMesh, StackVec, VertexId};

pub fn transform(mesh:&mut HalfEdgeMesh, transform:Transform) {
    let positions = mesh.attributes.get_mut(&AttributeKind::Positions).unwrap().as_vertices_vec3_mut();
    for (_, position) in positions.iter_mut() {
        *position = transform.transform_point(*position);
    }
}

/// Catmull-Clark subdivision. 
/// Based on [Wikipedia Article](https://en.wikipedia.org/wiki/Catmull%E2%80%93Clark_subdivision_surface)
pub fn subdivide(mesh:&mut HalfEdgeMesh) {
    let mut face_points:SecondaryMap<FaceId, Vec3> = SecondaryMap::new();
    let mut edge_points:SecondaryMap<HalfEdgeId, Vec3> = SecondaryMap::new();
    let original_vertices = mesh.vertex_keys().collect::<HashSet<_>>();
    // let positions = mesh.attributes.get_mut(&AttributeKind::Positions).unwrap().as_vertices_vec3();
    // Populate face_points
    for face in mesh.faces.keys() {
        let fp = mesh.select(face).calculate_centroid();
        // positions.insert(vertex, );
        face_points.insert(face, fp);
        // mesh.attributes.get_mut(&AttributeKind::Positions).unwrap().as_vertices_vec3_mut().insert(vertex, sum/count);
    }

    let mut split_vertices:HashSet<VertexId> = HashSet::new();


    // Populate edge_points
    for edge in mesh.halfedges.keys().collect::<Vec<_>>() {
        if !edge_points.contains_key(edge) {
            let e = mesh.goto(edge);
            let a = e.position();
            let (m, n) = e.face().map(|f| (face_points[f], 1.0)).unwrap_or((Vec3::ZERO, 0.0));
            let twin = e.twin();
            let f = twin.position();
            let (e, n2) = twin.face().map(|f| (face_points[f], 1.0)).unwrap_or((Vec3::ZERO, 0.0));
            let twin = *twin;
            let vertex = edge_ops::split(mesh, edge, 0.5);
            split_vertices.insert(vertex);
            // let vertex = mesh.new_vertex();
            edge_points.insert(edge, (a+f+m+e)/(n+n2+2.0));
            edge_points.insert(twin, (a+f+m+e)/(n+n2+2.0));
            mesh.attributes.get_mut(&AttributeKind::Positions).unwrap().as_vertices_vec3_mut().insert(vertex, (a+f+m+e)/4.0);
        }
    }

    for &vertex in &original_vertices {
        let p = mesh.goto(vertex).position();
        let (sum, nf) = mesh.goto(vertex).adjacent_faces().fold((Vec3::ZERO, 0.0_f32), |acc, i| (acc.0 + face_points[i.face().unwrap()], acc.1+1.0));
        let f = sum / nf;
        let (sum, ne) = mesh.goto(vertex).iter_outgoing().fold((Vec3::ZERO, 0.0_f32), |acc, i| (acc.0 + *edge_points.get(*i).unwrap_or_else(|| edge_points.get(*i.next()).unwrap()), acc.1+1.0));
        let r = sum / ne;
        let n = nf.max(ne);
        let new_position = (f+2.0*r+(n-3.0)*p)/n;
        mesh.attributes.get_mut(&AttributeKind::Positions).unwrap().as_vertices_vec3_mut().insert(vertex, new_position);
    }

    for (face, face_point) in face_points {
        // println!("** subdivide(): Face {face:?}");
        let split_halfedge_start = if split_vertices.contains(&mesh.goto(face).vertex()) {
            mesh.goto(face).halfedge()
        } else {
            mesh.goto(face).next().halfedge()
        };
        let face_vertex = mesh.new_vertex();
        mesh.attributes.get_mut(&AttributeKind::Positions).unwrap().as_vertices_vec3_mut().insert(face_vertex, face_point);
        for edge in mesh.goto(face).iter_loop().map(|t| *t).collect::<StackVec<_>>() {
            mesh[edge].face = None;
        }
        mesh.faces.remove(face);
        let tuples = mesh.goto(split_halfedge_start).iter_loop().map(|t| t.vertex()).tuples::<(VertexId, VertexId)>().collect::<StackVec<_>>();
        for (&(split, original), &(split_next, _)) in tuples.iter().circular_tuple_windows() {
            let new_face = [split, original, split_next, face_vertex];
            // println!("\tsubdivided into new face: {new_face:?}");
            mesh.new_face(&new_face);
            // break;
        }
        // if face == FaceId::from_ffi(1) { break }
    }

    // todo!("Figure out how to split mesh")
}

/// Invert winding of all faces in a mesh. Since normal direction is calculated based 
/// of face winding, this effectively inverses normals.
pub fn invert_normals(mesh:&mut HalfEdgeMesh) {
    for face in mesh.faces.keys() {
        let edges = mesh.goto(face).iter_loop().map(|t| *t).collect::<StackVec<_>>();
        let mut last_twin = mesh.halfedges[*edges.first().unwrap()].twin;
        for (&from, &to) in edges.iter().circular_tuple_windows() {
            mesh.halfedges[to].next = from;
            let tmp = mesh.halfedges[to].twin;
            mesh.halfedges[to].twin = last_twin;
            mesh.halfedges[last_twin].twin = to;
            last_twin = tmp;
        }
    }
}