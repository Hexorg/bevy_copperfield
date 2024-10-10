use bevy::prelude::Vec3;
use slotmap::SecondaryMap;

use super::{FaceId, HalfEdgeId, HalfEdgeMesh};

/// Catmull-Clark subdivision. 
/// Based on [Wikipedia Article](https://en.wikipedia.org/wiki/Catmull%E2%80%93Clark_subdivision_surface)
fn subdivide(mesh:&mut HalfEdgeMesh) {
    let mut face_points:SecondaryMap<FaceId, Vec3> = SecondaryMap::new();
    let mut edge_points:SecondaryMap<HalfEdgeId, Vec3> = SecondaryMap::new();

    // Populate face_points
    for face in mesh.faces.keys() {
        let (sum, count) = mesh.goto(face).iter_loop().fold((Vec3::ZERO,0.0_f32), |acc, v| (acc.0 + v.get_vertex_position().unwrap(), acc.1 + 1.0));
        face_points.insert(face, sum / count);
    }

    // Populate edge_points
    for edge in mesh.halfedges.keys() {
        if !edge_points.contains_key(edge) {
            let e = mesh.goto(edge);
            let a = e.get_vertex_position().unwrap();
            let m = e.get_face().unwrap().map(|f| face_points[f]).unwrap_or_default();
            let twin = e.twin();
            let f = twin.get_vertex_position().unwrap();
            let e = twin.get_face().unwrap().map(|f| face_points[f]).unwrap_or_default();
            let twin = twin.get_halfedge().unwrap();
            edge_points.insert(edge, (a+f+m+e)/4.0);
            edge_points.insert(twin, (a+f+m+e)/4.0);
        }
    }

    for vertex in mesh.vertices.keys() {
        let p = mesh.goto(vertex).get_vertex_position().unwrap();
        let (sum, n) = mesh.goto(vertex).adjacent_faces().fold((Vec3::ZERO, 0.0_f32), |acc, i| (acc.0 + face_points[i.get_face().ok().flatten().unwrap()], acc.1+1.0));
        let f = sum / n;
        let sum = mesh.goto(vertex).iter_outgoing().fold(Vec3::ZERO, |acc, i| acc + 0.5*(i.get_vertex_position().unwrap() + i.twin().get_vertex_position().unwrap()));
        let r = sum / n;
        let new_position = (f+2.0*r+(n-3.0)*p)/n;
    }

    todo!("Reconstruct new mesh. EZ")
}