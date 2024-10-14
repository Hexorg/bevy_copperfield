use std::collections::BTreeMap;

use bevy::prelude::{Vec2, Vec3};
use itertools::Itertools;
use slotmap::SecondaryMap;

use super::{traversal::Traversal, HalfEdgeId, StackVec, VertexId};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AttributeKind {
    Positions,
    Normals,
    UVs,
    Indices,
    Creases
}


pub type AttributeStore<K, T> = SecondaryMap<K, T>;
pub enum AttributeValues {
    VertexU32(AttributeStore<VertexId, u32>),
    VertexVec3(AttributeStore<VertexId, Vec3>),
    VertexBool(AttributeStore<VertexId, bool>),
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

    pub fn as_vertices_bool(&self) -> &AttributeStore<VertexId, bool> {
        match self {
            Self::VertexBool(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_vertices_bool_mut(&mut self) -> &mut AttributeStore<VertexId, bool> {
        match self {
            Self::VertexBool(v) => v,
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
    fn calculate_face_point(&self) -> Vec3;
    /// Get normal of current face
    fn calculate_face_normal(&self) -> Vec3;
    /// Get average of adjacent face's normals
    fn calculate_smooth_normal(&self) -> Vec3;
    /// Uses AttributeKind::Creases to tell if the vertex is supposed to use smooth normals or sharp
    fn is_smooth_normals(&self) -> bool;
}

impl<'m> AttributeQueries for Traversal<'m> {
    fn position(&self) -> Vec3 {
        let vertex = self.vertex();
        let values = self.mesh.attribute(&super::attributes::AttributeKind::Positions).expect("Vertices don't have position attribute.");
        values.as_vertices_vec3().get(vertex).copied().unwrap()
    }
    fn calculate_face_point(&self) -> Vec3 {
        // We keep count since we don't know ahead of time the amount of face edges
        let (sum, count) = self.iter_loop().fold((Vec3::ZERO, 0.0), |acc, i| (acc.0 + i.position(), acc.1 + 1.0));
        sum / count
    }
    fn calculate_face_normal(&self) -> Vec3 {
        for (v1, v2, v3) in self
            .iter_loop()
            .map(|f| f.position()).collect::<StackVec<_>>().iter().circular_tuple_windows() {
            let left = *v1 - *v2;
            let right = *v2 - *v3;
            let normal = left.cross(right).normalize();
            if !normal.is_nan() {
                return normal
            }
        }
        Vec3::ZERO
    }
    fn calculate_smooth_normal(&self) -> Vec3 {
        let (sum, count) = self.adjacent_faces().map(|t| t.calculate_face_normal()).fold((Vec3::ZERO, 0.0), |acc, i| (acc.0 + i, acc.1 + 1.0));
        sum / count
    }

    fn is_smooth_normals(&self) -> bool {
        if let Some(store) = self.mesh.attribute(&super::attributes::AttributeKind::Creases) {
            store.as_vertices_bool().get(self.vertex()).copied().unwrap_or(self.mesh.is_smooth)
        } else {
            self.mesh.is_smooth
        }
    }
}