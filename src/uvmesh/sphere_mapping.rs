use core::f32;

use bevy::prelude::{Vec2, Vec3};
use slotmap::SecondaryMap;

use crate::mesh::{attributes::{AttributeStore, TraversalQueries}, HalfEdgeId, HalfEdgeMesh};


pub(crate) fn project(mesh:&mut HalfEdgeMesh, sphere_center:Vec3) {
    let mut uvmap = SecondaryMap::<HalfEdgeId, Vec2>::new();
    for edge in mesh.edge_keys() {
        let d = (mesh.goto(edge).position() - sphere_center).normalize();
        let uv = Vec2{
            x: 1.0 - (0.5 + f32::atan2(d.z, d.x) / (2.0*f32::consts::PI)),
            y: 1.0 - (0.5 + f32::asin(d.y) / f32::consts::PI)
        };
        uvmap.insert(edge, uv);
    }
    mesh.add_attribute(crate::mesh::attributes::AttributeKind::UVs, uvmap);
}