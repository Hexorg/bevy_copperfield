use bevy::{prelude::Vec3, utils::hashbrown::HashSet};
use itertools::Itertools;
use slotmap::SecondaryMap;

use crate::mesh::attributes::{AttributeQueries, AttributeKind};

use super::{edge_ops, FaceId, HalfEdgeId, HalfEdgeMesh};

/// Catmull-Clark subdivision. 
/// Based on [Wikipedia Article](https://en.wikipedia.org/wiki/Catmull%E2%80%93Clark_subdivision_surface)
pub fn subdivide(mesh:&mut HalfEdgeMesh) {
    let mut face_points:SecondaryMap<FaceId, Vec3> = SecondaryMap::new();
    let mut edge_points:SecondaryMap<HalfEdgeId, Vec3> = SecondaryMap::new();
    let original_vertices = mesh.vertex_keys().collect::<HashSet<_>>();
    // let positions = mesh.attributes.get_mut(&AttributeKind::Positions).unwrap().as_vertices_vec3();
    // Populate face_points
    for face in mesh.faces.keys().collect::<Vec<_>>() {
        let (sum, count) = mesh.goto(face).iter_loop().fold((Vec3::ZERO,0.0_f32), |acc, v| (acc.0 + v.position(), acc.1 + 1.0));
        // positions.insert(vertex, );
        face_points.insert(face, sum/count);
        // mesh.attributes.get_mut(&AttributeKind::Positions).unwrap().as_vertices_vec3_mut().insert(vertex, sum/count);
    }

    // Populate edge_points
    for edge in mesh.halfedges.keys().collect::<Vec<_>>() {
        if !edge_points.contains_key(edge) {
            let e = mesh.goto(edge);
            let a = e.position();
            let m = e.face().map(|f| face_points[f]).unwrap_or_default();
            let twin = e.twin();
            let f = twin.position();
            let e = twin.face().map(|f| face_points[f]).unwrap_or_default();
            let twin = *twin;
            // let vertex = edge_ops::split(mesh, edge, 0.5);
            // let vertex = mesh.new_vertex();
            edge_points.insert(edge, (a+f+m+e)/4.0);
            edge_points.insert(twin, (a+f+m+e)/4.0);
            // mesh.attributes.get_mut(&AttributeKind::Positions).unwrap().as_vertices_vec3_mut().insert(vertex, (a+f+m+e)/4.0);
        }
    }

    for &vertex in &original_vertices {
        let p = mesh.goto(vertex).position();
        let (sum, nf) = mesh.goto(vertex).adjacent_faces().fold((Vec3::ZERO, 0.0_f32), |acc, i| (acc.0 + face_points[i.face().unwrap()], acc.1+1.0));
        let f = sum / nf;
        let (sum, ne) = mesh.goto(vertex).iter_outgoing().fold((Vec3::ZERO, 0.0_f32), |acc, i| (acc.0 + *edge_points.get(*i).unwrap_or_else(|| edge_points.get(*i.next()).unwrap()), acc.1+1.0));
        let r = sum / ne;
        // assert_eq!(nf, ne);
        // println!("nf: {nf:?}, ne:{ne:?}");
        let new_position = (f+2.0*r+(nf.max(3.0)-3.0)*p)/nf;
        mesh.attributes.get_mut(&AttributeKind::Positions).unwrap().as_vertices_vec3_mut().insert(vertex, new_position);
    }

    // todo!("Figure out how to split mesh")
}