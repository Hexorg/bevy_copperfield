use core::f32;

use bevy::prelude::{Transform, TransformPoint, Vec2, Vec3};
use slotmap::SecondaryMap;

use crate::mesh::{
    attributes::{SelectionQueries, TraversalQueries},
    HalfEdgeId, HalfEdgeMesh,
};

pub(crate) fn sphere(mesh: &mut HalfEdgeMesh, sphere_center: Vec3, sphere_radius: Vec3) {
    let mut uvmap = SecondaryMap::<HalfEdgeId, Vec2>::new();
    for edge in mesh.edge_keys() {
        let d = (mesh.goto(edge).position() - sphere_center) / sphere_radius;
        let uv = Vec2 {
            x: 1.0 - (0.5 + f32::atan2(d.z, d.x) / (2.0 * f32::consts::PI)),
            y: 1.0 - (0.5 + f32::asin(d.y) / f32::consts::PI),
        };
        uvmap.insert(edge, uv);
    }
    mesh.add_attribute(crate::mesh::attributes::AttributeKind::UVs, uvmap);
}

pub(crate) fn cube(mesh: &mut HalfEdgeMesh, transform: Transform) {
    const CUBE_NORMALS: [Vec3; 6] = [
        Vec3 {
            x: 0.0,
            y: 1.0,
            z: 0.0,
        },
        Vec3 {
            x: -1.0,
            y: 0.0,
            z: 0.0,
        },
        Vec3 {
            x: 0.0,
            y: -1.0,
            z: 0.0,
        },
        Vec3 {
            x: 1.0,
            y: 0.0,
            z: 0.0,
        },
        Vec3 {
            x: 0.0,
            y: 0.0,
            z: 1.0,
        },
        Vec3 {
            x: 0.0,
            y: 0.0,
            z: -1.0,
        },
    ];
    const CUBE_UV_BASIS: [(Vec3, Vec3); 6] = [
        (
            Vec3 {
                x: 0.0,
                y: 0.0,
                z: 1.0,
            },
            Vec3 {
                x: -1.0,
                y: 0.0,
                z: 0.0,
            },
        ),
        (
            Vec3 {
                x: 0.0,
                y: 0.0,
                z: 1.0,
            },
            Vec3 {
                x: 0.0,
                y: -1.0,
                z: 0.0,
            },
        ),
        (
            Vec3 {
                x: 0.0,
                y: 0.0,
                z: 1.0,
            },
            Vec3 {
                x: 1.0,
                y: 0.0,
                z: 0.0,
            },
        ),
        (
            Vec3 {
                x: 0.0,
                y: 0.0,
                z: -1.0,
            },
            Vec3 {
                x: 0.0,
                y: 1.0,
                z: 0.0,
            },
        ),
        (
            Vec3 {
                x: 1.0,
                y: 0.0,
                z: 0.0,
            },
            Vec3 {
                x: 0.0,
                y: 1.0,
                z: 0.0,
            },
        ),
        (
            Vec3 {
                x: 1.0,
                y: 0.0,
                z: 0.0,
            },
            Vec3 {
                x: 0.0,
                y: 1.0,
                z: 0.0,
            },
        ),
    ];
    const UV_TRANSLATE: [Vec2; 6] = [
        Vec2 { x: 0.25, y: 0.00 },
        Vec2 { x: 0.25, y: 0.25 },
        Vec2 { x: 0.25, y: 0.50 },
        Vec2 { x: 0.25, y: 0.75 },
        Vec2 { x: 0.00, y: 0.75 },
        Vec2 { x: 0.50, y: 0.75 },
    ];
    let transform = transform.compute_affine().inverse();
    let mut uvmap = SecondaryMap::<HalfEdgeId, Vec2>::new();
    for face in mesh.face_keys() {
        let face = mesh.goto(face);
        let normal = face.calculate_normal().unwrap();
        let (idx, _) = CUBE_NORMALS
            .iter()
            .enumerate()
            .max_by_key(|(_, n)| {
                let n = n.dot(normal);
                if n < 0.0 {
                    0
                } else {
                    (1000.0 * n) as i32
                }
            })
            .unwrap();
        let (basis_u, basis_v) = CUBE_UV_BASIS[idx];
        let translate = UV_TRANSLATE[idx];
        for edge in face.iter_loop() {
            let p = transform.transform_point(edge.position());
            let uv = if true {
                0.25 * Vec2 {
                    x: p.dot(basis_u),
                    y: p.dot(basis_v),
                } + translate
                    + Vec2 { x: 0.125, y: 0.125 }
            } else {
                Vec2::ZERO
            };

            uvmap.insert(*edge, uv);
        }
    }
    mesh.add_attribute(crate::mesh::attributes::AttributeKind::UVs, uvmap);
}
