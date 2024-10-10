use bevy::{math::VectorSpace, prelude::{Cuboid, Vec2, Vec3}};
use crate::mesh::{attributes::{AttributeKind, AttributeStore}, HalfEdgeId, HalfEdgeMesh, VertexId};

/// A trait used for primitives toconstruct a [`HalfEdgeMesh`]
pub trait HalfEdgeMeshBuilder {
    /// Generate am editable [`HalfEdgeMesh`]
    fn procgen(&self) -> HalfEdgeMesh;
}

impl HalfEdgeMeshBuilder for Cuboid {
    fn procgen(&self) -> HalfEdgeMesh {
        let Vec3{x, y, z} = self.half_size;
        let quads = [
            [0, 1, 2, 3], //     7-..__ 
            [4, 5, 6, 7], //    4|..__  6
            [7, 6, 1, 0], //    ||     5|        y
            [0, 3, 4, 7], //    ||     ||        |
            [1, 6, 5, 2], //    |0 -..__1       /-> x
            [2, 5, 4, 3], //    3 -..__2       z
        ];
        let positions = [
            Vec3::new(-x, -y, -z), // 0
            Vec3::new(x, -y, -z),  // 1
            Vec3::new(x, -y, z),   // 2
            Vec3::new(-x, -y, z),  // 3
            Vec3::new(-x, y, z),   // 4
            Vec3::new(x, y, z),    // 5
            Vec3::new(x, y, -z),   // 6
            Vec3::new(-x, y, -z)   // 7
        ];
        let mut mesh = HalfEdgeMesh::new();
        let mut index_to_vertex_map:Vec<Option<VertexId>> = (0..positions.len()).map(|_| None).collect();
        let mut position_attribute:AttributeStore<VertexId, Vec3> = AttributeStore::new();
        let mut uv_attribute:AttributeStore<HalfEdgeId, Vec2> = AttributeStore::new();
        for quad in quads {
            let new_face = quad.map(|idx| match index_to_vertex_map[idx] {
                Some(v) => v,
                None => {let v = mesh.new_vertex(); index_to_vertex_map[idx] = Some(v); v}
            });
            let face_id = mesh.new_face(&new_face);
            quad.iter().zip(new_face).for_each(
                |(&idx, vertex)| _ = position_attribute.insert(vertex, positions[idx])
            );
            mesh.goto(face_id).iter_loop().for_each(|edge| _ = uv_attribute.insert(edge.get_halfedge().unwrap(), Vec2::ZERO));
        }
        mesh.add_attribute(AttributeKind::Positions, position_attribute);
        mesh.add_attribute(AttributeKind::UVs, uv_attribute);
        mesh
    }
}
