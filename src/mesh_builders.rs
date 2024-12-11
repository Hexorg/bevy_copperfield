use bevy_math::prelude::{Cuboid, Ellipse, RegularPolygon};
use bevy_render::mesh::{CircleMeshBuilder, EllipseMeshBuilder, Meshable};
use glam::{Vec2, Vec3};

use crate::mesh::{
    attributes::{AttributeKind, AttributeStore},
    HalfEdgeId, HalfEdgeMesh, VertexId,
};


/// A trait used for primitives toconstruct a [`HalfEdgeMesh`]
pub trait HalfEdgeMeshBuilder {
    /// Generate am editable [`HalfEdgeMesh`]
    fn procgen(&self) -> HalfEdgeMesh;
}

impl HalfEdgeMeshBuilder for EllipseMeshBuilder {
    fn procgen(&self) -> HalfEdgeMesh {
        let mut mesh = HalfEdgeMesh::new();
        let mut position_attribute: AttributeStore<VertexId, Vec3> = AttributeStore::new();
        let mut polygon = Vec::with_capacity(self.resolution as usize);
        // Add pi/2 so that there is a vertex at the top (sin is 1.0 and cos is 0.0)
        let start_angle = std::f32::consts::FRAC_PI_2;
        let step = std::f32::consts::TAU / self.resolution as f32;

        for i in 0..self.resolution {
            // Compute vertex position at angle theta
            let theta = start_angle + i as f32 * step;
            let (sin, cos) = theta.sin_cos();
            let x = cos * self.ellipse.half_size.x;
            let y = sin * self.ellipse.half_size.y;
            let v = mesh.new_vertex();
            polygon.push(v);
            position_attribute.insert(v, Vec3 { x, y, z: 0.0 });
        }
        mesh.add_attribute(AttributeKind::Positions, position_attribute);
        mesh.new_face(&polygon);
        mesh
    }
}

impl HalfEdgeMeshBuilder for RegularPolygon {
    fn procgen(&self) -> HalfEdgeMesh {
        Ellipse::new(self.circumcircle.radius, self.circumcircle.radius)
            .mesh()
            .resolution(self.sides)
            .procgen()
    }
}

impl HalfEdgeMeshBuilder for CircleMeshBuilder {
    fn procgen(&self) -> HalfEdgeMesh {
        RegularPolygon::new(self.circle.radius, self.resolution).procgen()
    }
}

impl HalfEdgeMeshBuilder for Cuboid {
    fn procgen(&self) -> HalfEdgeMesh {
        let Vec3 { x, y, z } = self.half_size;
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
            Vec3::new(-x, y, -z),  // 7
        ];
        let mut mesh = HalfEdgeMesh::new();
        let mut index_to_vertex_map: Vec<Option<VertexId>> =
            (0..positions.len()).map(|_| None).collect();
        let mut position_attribute: AttributeStore<VertexId, Vec3> = AttributeStore::new();
        let mut uv_attribute: AttributeStore<HalfEdgeId, Vec2> = AttributeStore::new();
        for quad in quads {
            let new_face = quad.map(|idx| match index_to_vertex_map[idx] {
                Some(v) => v,
                None => {
                    let v = mesh.new_vertex();
                    index_to_vertex_map[idx] = Some(v);
                    v
                }
            });
            let face_id = mesh.new_face(&new_face);
            quad.iter()
                .zip(new_face)
                .for_each(|(&idx, vertex)| _ = position_attribute.insert(vertex, positions[idx]));
            mesh.goto(face_id)
                .iter_loop()
                .for_each(|edge| _ = uv_attribute.insert(edge.halfedge(), Vec2::ZERO));
        }
        mesh.add_attribute(AttributeKind::Positions, position_attribute);
        mesh.add_attribute(AttributeKind::UVs, uv_attribute);
        mesh
    }
}

#[cfg(test)]
mod test {
    use bevy_math::prelude::Cuboid;
    use smallvec::SmallVec;

    use crate::{mesh::VertexId, mesh_builders::HalfEdgeMeshBuilder};

    #[test]
    fn from_cuboid() {
        let mesh = Cuboid::new(1.0, 1.0, 1.0).procgen();
        assert_eq!(mesh.face_count(), 6);
        assert_eq!(mesh.vertex_count(), 8);
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.count_face_edges(), mesh.halfedge_count());
        assert_eq!(mesh.count_face_edges(), 24);
        let vertices: SmallVec<[_; 8]> = mesh.vertex_keys().collect();
        let next_vertices: SmallVec<[_; 8]> = vertices
            .iter()
            .map(|v| (*v, mesh.goto(*v).next().vertex()))
            .collect();
        assert_eq!(
            next_vertices,
            SmallVec::from_buf([
                (VertexId::from_ffi(1), VertexId::from_ffi(8)),
                (VertexId::from_ffi(2), VertexId::from_ffi(7)),
                (VertexId::from_ffi(3), VertexId::from_ffi(6)),
                (VertexId::from_ffi(4), VertexId::from_ffi(5)),
                (VertexId::from_ffi(5), VertexId::from_ffi(4)),
                (VertexId::from_ffi(6), VertexId::from_ffi(3)),
                (VertexId::from_ffi(7), VertexId::from_ffi(2)),
                (VertexId::from_ffi(8), VertexId::from_ffi(1)),
            ])
        );
        let twin_vertices: SmallVec<[_; 8]> = vertices
            .iter()
            .map(|v| (*v, mesh.goto(*v).twin().vertex()))
            .collect();
        assert_eq!(
            twin_vertices,
            SmallVec::from_buf([
                (VertexId::from_ffi(1), VertexId::from_ffi(8)),
                (VertexId::from_ffi(2), VertexId::from_ffi(7)),
                (VertexId::from_ffi(3), VertexId::from_ffi(6)),
                (VertexId::from_ffi(4), VertexId::from_ffi(5)),
                (VertexId::from_ffi(5), VertexId::from_ffi(4)),
                (VertexId::from_ffi(6), VertexId::from_ffi(3)),
                (VertexId::from_ffi(7), VertexId::from_ffi(2)),
                (VertexId::from_ffi(8), VertexId::from_ffi(1)),
            ])
        );
        assert_eq!(twin_vertices, next_vertices);
    }
}
