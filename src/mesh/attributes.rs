use std::collections::BTreeMap;

use bevy::prelude::{Vec2, Vec3};
use slotmap::SecondaryMap;

use super::{traversal::Traversal, HalfEdgeId, VertexId};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AttributeKind {
    Positions,
    Normals,
    UVs,
    Indices
}


pub type AttributeStore<K, T> = SecondaryMap<K, T>;
pub enum AttributeValues {
    VertexU32(AttributeStore<VertexId, u32>),
    VertexVec3(AttributeStore<VertexId, Vec3>),
    EdgeVec2(AttributeStore<HalfEdgeId, Vec2>),
    EdgeVec3(AttributeStore<HalfEdgeId, Vec3>)
}

impl AttributeValues {
    pub fn as_vertices_u32(&self) -> &AttributeStore<VertexId, u32> {
        match self {
            Self::VertexU32(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_vertices_vec3(&self) -> &AttributeStore<VertexId, Vec3> {
        match self {
            Self::VertexVec3(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_vertices_vec3_mut(&mut self) -> &mut AttributeStore<VertexId, Vec3> {
        match self {
            Self::VertexVec3(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_edge_vec2(&self) -> &AttributeStore<HalfEdgeId, Vec2> {
        match self {
            Self::EdgeVec2(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }
    pub fn as_edge_vec3(&self) -> &AttributeStore<HalfEdgeId, Vec3> {
        match self {
            Self::EdgeVec3(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }
}

pub type Attributes = BTreeMap<AttributeKind, AttributeValues>;


impl From<AttributeStore<VertexId, u32>> for AttributeValues {
    fn from(value: AttributeStore<VertexId, u32>) -> Self {
        Self::VertexU32(value)
    }
}

impl From<AttributeStore<VertexId, Vec3>> for AttributeValues {
    fn from(value: AttributeStore<VertexId, Vec3>) -> Self {
        Self::VertexVec3(value)
    }
}

impl From<AttributeStore<HalfEdgeId, Vec2>> for AttributeValues {
    fn from(value: AttributeStore<HalfEdgeId, Vec2>) -> Self {
        Self::EdgeVec2(value)
    }
}

impl From<AttributeStore<HalfEdgeId, Vec3>> for AttributeValues {
    fn from(value: AttributeStore<HalfEdgeId, Vec3>) -> Self {
        Self::EdgeVec3(value)
    }
}

pub trait AttributeQueries{
    /// Get position of the associated vertex
    fn position(&self) -> Vec3;
    /// Get the average of all vertices of this face
    fn face_point(&self) -> Vec3;
}

impl<'m> AttributeQueries for Traversal<'m> {
    fn position(&self) -> Vec3 {
        let vertex = self.vertex();
        let values = self.mesh.attribute(&super::attributes::AttributeKind::Positions).expect("Vertices don't have position attribute.");
        values.as_vertices_vec3().get(vertex).copied().unwrap()
    }
    fn face_point(&self) -> Vec3 {
        // We keep count since we don't know ahead of time the amount of face edges
        let (sum, count) = self.iter_loop().fold((Vec3::ZERO, 0.0), |acc, i| (acc.0 + i.position(), acc.1 + 1.0));
        sum / count
    }
}