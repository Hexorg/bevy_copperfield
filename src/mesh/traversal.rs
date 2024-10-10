
use bevy::prelude::{Deref, DerefMut, Vec3};


use super::{FaceId, HalfEdgeId, HalfEdgeMesh, MeshPosition, StackVec, VertexId};

const TRAVERSAL_LOOP_LIMIT:usize = 32; // We really don't expect more than TRAVERSAL_LOOP_LIMIT-gons or more than TRAVERSAL_LOOP_LIMIT edges coming out of a vertex

pub type TraversalResult<T> = Result<T, TraversalError>;
pub type TraversalVec<T> = TraversalResult<StackVec<T>>;


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TraversalErrorKind{
    /// Output in [`Traversal::get_edge_vertices`] if current position is not an edge
    NoAnEdge, 
    /// Output in [`Traversal::halfedge_to`]
    NotFound, 
    /// Output in [`Traversal::next`] if current position is face
    UndefinedOperation,
    /// Output whenever a face is crated that doesn't match existing winding.
    WrongWinding,
    UnassignedHalfedge,
    UnassignedVertex,
    UnassignedFace,
    BoundaryEdge,
    NoPositionAttribute
}

#[derive(Copy, Clone, Debug)]
pub struct TraversalError{
    pub position: MeshPosition,
    pub kind: TraversalErrorKind,
}

impl TraversalError {
    pub fn new(pos: impl Into<MeshPosition>, kind:TraversalErrorKind) -> Self {
        Self{position:pos.into(), kind}
    }
}

#[derive(Copy, Clone, Deref, DerefMut)]
/// Collection of convenience methods to traverse the mesh
pub struct Traversal<'m> {
    #[deref]
    position: MeshPosition,
    error:Option<TraversalErrorKind>,
    mesh:&'m HalfEdgeMesh,
}

#[derive(Copy, Clone, Debug)]
enum EdgeIteratorKind{
    Incoming,
    Outgoing,
    Loop,
}

#[derive(Copy, Clone)]
pub struct EdgeIterator<'m> {
    traversal: Traversal<'m>,
    kind:EdgeIteratorKind,
    start: HalfEdgeId, // iteration can only happen over half-edges. To make sure correct comparison of positions, we force this type to be HalfEdgeId, instead of MeshPosition.
    count:usize,
}

impl<'m> EdgeIterator<'m> {
    pub fn contains(mut self, pos:impl Into<MeshPosition>+Copy) -> bool {
        use MeshPosition::*;
        self.any( |edge|
            match (edge.position, pos.into()) {
                (HalfEdge(e), Vertex(v)) => self.traversal.mesh[e].vertex == v || self.traversal.mesh[v].halfedge == e,
                (HalfEdge(e), HalfEdge(e2)) => e == e2,
                (HalfEdge(e), Face(f)) => self.traversal.mesh[e].face == Some(f),
                _ => false
            }
        )
    }
}


impl<'m> Iterator for EdgeIterator<'m> {
    type Item = Traversal<'m>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.traversal.error.is_some() {
            None
        } else {
            self.count += 1;
            if self.count > TRAVERSAL_LOOP_LIMIT {
                panic!("Iterated {TRAVERSAL_LOOP_LIMIT} times.");
            } 
            let item = match self.kind {
                EdgeIteratorKind::Loop | EdgeIteratorKind::Outgoing => Some(self.traversal),
                EdgeIteratorKind::Incoming => Some(self.traversal.twin())
            };
            // println!("{:?} iterator yielding {item:?}", self.kind);
            self.traversal = match self.kind {
                EdgeIteratorKind::Loop => self.traversal.next(),
                EdgeIteratorKind::Incoming | EdgeIteratorKind::Outgoing => self.traversal.twin().next(),
            };
            if self.traversal.position == self.start.into() {
                self.traversal.error = Some(TraversalErrorKind::NotFound)
            }
            item
        }
    }
}

impl<'m> PartialEq for Traversal<'m> {
    fn eq(&self, other: &Self) -> bool {
        self.position == other.position
    }
}

impl<'m> Eq for Traversal<'m> { }


impl<'m> From<Traversal<'m>> for MeshPosition {
    fn from(value: Traversal<'m>) -> Self {
        value.position
    }
}

impl<'m> From<Traversal<'m>> for HalfEdgeId {
    fn from(value: Traversal<'m>) -> Self {
        value.get_halfedge().unwrap()
    }
}

impl<'m> Traversal<'m> {
    pub fn new(mesh:&'m HalfEdgeMesh, pos: impl Into<MeshPosition>) -> Self {
        let position = pos.into();
        let error = if position.is_valid(mesh) {
            None
        } else {
            panic!("Created Traversal with invalid position");
        };
        Self{mesh, position, error}
    }

    #[inline]
    fn clone_with_error(&self, error:TraversalErrorKind) -> Self {
        let mut result = *self;
        result.error = Some(error);
        result
    }

    #[inline]
    fn clone_with_position(&self, position: impl Into<MeshPosition>) -> Self {
        let mut result = *self;
        result.position = position.into();
        if !result.position.is_valid(self.mesh) {
            panic!("Provided invalid positoin!");
        }
        result
    }


    /// Get current vertex and scan over outgoing edges. If any of the edges land on `vertex` - return that edge.
    pub fn halfedge_to(&self, vertex:VertexId) -> Self {
        match self.iter_outgoing().find(|e| e.twin().get_vertex().ok().map(|v| v == vertex).unwrap_or(false)) {
            Some(e) => e,
            None => self.clone_with_error(TraversalErrorKind::NotFound)
        }
    }

    /// Iterate over a loop of edges (likely forming a face)
    /// E.g.
    /// ```text
    ///   . ---- .
    ///  /        \
    /// .          .
    ///  \.______./
    /// ```
    /// 
    pub fn iter_loop(&self) -> EdgeIterator<'m> {
        EdgeIterator{traversal:self.halfedge(), kind:EdgeIteratorKind::Loop, start:self.get_halfedge().unwrap(), count:0}
    }

    /// Iterate over all outgoing edges (Also known as edge fan)
    /// This results in iterating over all the edges that come out of the same vertex
    /// E.g.
    /// ```text
    ///    \ | /
    ///     \|/
    ///  --- . ---
    ///     /|\
    ///    / | \
    /// ```
    pub fn iter_outgoing(&self) -> EdgeIterator<'m> {
        EdgeIterator{traversal:self.halfedge(), kind:EdgeIteratorKind::Outgoing, start:self.get_halfedge().unwrap(), count:0}
    }

    /// Iterate over all incoming edges (twins of outgoing edges)
    pub fn iter_incoming(&self) -> EdgeIterator<'m> {
        EdgeIterator{traversal:self.halfedge(), kind:EdgeIteratorKind::Incoming, start:self.get_halfedge().unwrap(), count:0}
    }

    pub fn adjacent_faces(&self) -> impl Iterator<Item = Self> {
        self.iter_outgoing().filter(|p| !p.is_boundary())
    }

    #[inline]
    /// Returns the associate half-edge with whatever current position is
    pub fn get_halfedge(&self) -> Result<HalfEdgeId, TraversalError> {
        if let Some(kind) = self.error { return Err(TraversalError::new(self.position, kind)) };
        match self.position {
            MeshPosition::Vertex(vertex_id) => Ok(self.mesh[vertex_id].halfedge),
            MeshPosition::HalfEdge(half_edge_id) => Ok(half_edge_id),
            MeshPosition::Face(face_id) => Ok(self.mesh[face_id].halfedge),
        }
    }

    #[inline]
    /// Get position of the associated vertex
    pub fn get_vertex_position(&self) -> Result<Vec3, TraversalError> {
        if let Some(kind) = self.error { return Err(TraversalError::new(self.position, kind)) };
        let vertex = self.get_vertex()?;
        let values = self.mesh.attribute(&super::attributes::AttributeKind::Positions).ok_or(TraversalError::new(self.position, TraversalErrorKind::NoPositionAttribute))?;
        values.as_vertices_vec3().get(vertex).copied().ok_or(TraversalError::new(self.position, TraversalErrorKind::UnassignedVertex))
    }

    /// Get length of the associated edge
    pub fn get_length(&self) -> Result<f32, TraversalError> {
        if let Some(kind) = self.error { return Err(TraversalError::new(self.position, kind)) };
        let start = self.get_vertex_position()?;
        let end = self.next().get_vertex_position()?;
        Ok((end - start).length())
    }

    /// Switch selected element to the associated half-edge.
    pub fn halfedge(&self) -> Self {
        if self.is_error() {
            *self
        } else {
            let halfedge = match self.position {
                MeshPosition::Vertex(vertex_id) => self.mesh[vertex_id].halfedge,
                MeshPosition::HalfEdge(e) => e,
                MeshPosition::Face(face_id) => self.mesh[face_id].halfedge,
            };
            if self.mesh.halfedges.contains_key(halfedge) {
                self.clone_with_position(halfedge)
            } else {
                self.clone_with_error(TraversalErrorKind::UnassignedHalfedge)
            }
        }
    }

     /// Switch selected element to the associated half-edge.
     pub fn vertex(&self) -> Self {
        if self.is_error() {
            *self
        } else {
            let vertex = match self.position {
                MeshPosition::Vertex(vertex_id) => vertex_id,
                MeshPosition::HalfEdge(e) => self.mesh[e].vertex,
                MeshPosition::Face(face_id) => {
                    let edge = self.mesh.faces[face_id].halfedge;
                    if self.mesh.halfedges.contains_key(edge) {
                        self.mesh[edge].vertex
                    } else {
                        return self.clone_with_error(TraversalErrorKind::UnassignedHalfedge);
                    }
                },
            };
            if self.mesh.vertices.contains_key(vertex) {
                self.clone_with_position(vertex)
            } else {
                self.clone_with_error(TraversalErrorKind::UnassignedVertex)
            }
        }
    }

    #[inline]
    /// Returns the associated vertex for vertex and half-edge positions. 
    /// Returns vertex of the associated half-edge for face positions.
    pub fn get_vertex(&self) -> Result<VertexId, TraversalError> {
        let r = self.vertex();
        if let Some(kind) = r.error { return Err(TraversalError::new(self.position, kind)) }
        match r.position {
            MeshPosition::Vertex(vertex_id) => Ok(vertex_id),
            _ => panic!("Unreachable.")
        }
    }

    pub fn face(&self) -> Self {
        if self.is_error() {
            *self
        } else {
            let halfedge = match self.position {
                MeshPosition::Vertex(vertex_id) => self.mesh[vertex_id].halfedge,
                MeshPosition::HalfEdge(half_edge_id) => half_edge_id,
                MeshPosition::Face(_) => return *self,
            };
            if self.mesh.halfedges.contains_key(halfedge)  {
                if let Some(face_id) = self.mesh[halfedge].face {
                    if self.mesh.faces.contains_key(face_id) {
                        self.clone_with_position(face_id)
                    } else {
                        self.clone_with_error(TraversalErrorKind::UnassignedFace)
                    }
                } else {
                    self.clone_with_error(TraversalErrorKind::BoundaryEdge)
                }
            } else {
                self.clone_with_error(TraversalErrorKind::UnassignedHalfedge)
            }
        }
    }

    #[inline]
    /// Returns the associated face with a given position.
    /// If option is none, then the associated position is a boundary position (mesh ends there) 
    pub fn get_face(&self) -> Result<Option<FaceId>, TraversalError> {
        let r = self.face();
        if let Some(kind) = r.error { 
            if kind == TraversalErrorKind::BoundaryEdge {
                Ok(None)
            } else {
                 Err(TraversalError::new(self.position, kind)) 
            }
        } else {
            match r.position {
                MeshPosition::Face(face_id) => Ok(Some(face_id)),
                _ => panic!("Unreachable.")
            }
        }
    }

    #[inline]
    /// Syntactic sugar to see if we are on a face or an edge of the mesh
    pub fn is_boundary(&self) -> bool {
        self.get_face().ok().flatten().is_none()
    }

    #[inline]
    /// Returns a tuple of [`VertexId`] - first is from vertex for this edge, second is destination vertex
    /// Returns error for Vertex and Face positions as those don't have clear "next" vertex associated with them.
    pub fn get_edge_vertices(&self) -> Result<(VertexId, VertexId), TraversalError> {
        if let Some(kind) = self.error { return Err(TraversalError::new(self.position, kind)) };
        match self.position {
            MeshPosition::Vertex(_) => Err(TraversalError::new(self.position, TraversalErrorKind::NoAnEdge)),
            MeshPosition::HalfEdge(_) => Ok((self.get_vertex()?, self.next().get_vertex()?)),
            MeshPosition::Face(_) => Err(TraversalError::new(self.position, TraversalErrorKind::NoAnEdge)),
        }
    }

    pub fn is_error(&self) -> bool {
        self.error.is_some()
    }

    pub fn to_error(self) -> Option<TraversalError> {
        self.error.map(|e| TraversalError::new(self.position, e))
    }

    pub fn is_vertex(&self) -> bool {
        match self.position {
            MeshPosition::Vertex(_) => true,
            _ => false,
        }
    }

    pub fn get_position(&self) -> MeshPosition {
        self.position
    }
 
    /// Traverse to the next half-edge or to the next vertex in a Face
    pub fn next(&self) -> Self {
        let edge = self.halfedge();
        if edge.is_error() {
            edge
        } else {
            match edge.position {
                MeshPosition::HalfEdge(e) => {
                    let next = self.mesh.halfedges[e].next;
                    if self.mesh.halfedges.contains_key(next) {
                        edge.clone_with_position(next)
                    } else {
                        edge.clone_with_error(TraversalErrorKind::UnassignedHalfedge)
                    }
                }
                _ => panic!("Unreachable.")
            } 
        }
    }

    /// Get the previous vertex or edge in a face. 
    pub fn previous(&self) -> Self {
        if self.is_error() {
            *self
        } else {
            self.iter_incoming()
                .find(|t| t.next().position == self.position)
                .unwrap_or_else(|| self.clone_with_error(TraversalErrorKind::NotFound))
        }
    }

    pub fn twin(&self) -> Self {
        let edge = self.halfedge();
        if edge.is_error() {
            edge
        } else {
            match edge.position {
                MeshPosition::HalfEdge(e) => {
                    let twin = self.mesh[e].twin;
                    if self.mesh.halfedges.contains_key(twin) {
                        self.clone_with_position(twin)
                    } else {
                        self.clone_with_error(TraversalErrorKind::UnassignedHalfedge)
                    }
                },
                _ => panic!("Unreachable.")
            }
        }
    }

}

impl<'m> std::fmt::Debug for Traversal<'m> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self.position))
    }
}

#[cfg(test)]
mod tests {
    use bevy::prelude::Cuboid;
    use slotmap::KeyData;
    use smallvec::SmallVec;

    use crate::{mesh::{FaceId, HalfEdgeMesh, VertexId}, mesh_builders::HalfEdgeMeshBuilder};

    #[test]
    fn test_iter_outgoing() {
        let mesh = Cuboid::new(1.0, 1.0, 1.0).procgen();
        let vertices:SmallVec<[_;8]> = mesh.vertices.keys().collect();
        let fan:SmallVec<[_;6]> = mesh.goto(vertices[0]).iter_outgoing().map(|v| (v.get_vertex().unwrap(), v.next().get_vertex().unwrap())).collect();
        assert_eq!(fan, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(1)), VertexId(KeyData::from_ffi(8))),
            (VertexId(KeyData::from_ffi(1)), VertexId(KeyData::from_ffi(4))),
            (VertexId(KeyData::from_ffi(1)), VertexId(KeyData::from_ffi(2))),
            ]));
        let fan:SmallVec<[_;6]> = mesh.goto(vertices[1]).iter_outgoing().map(|v| (v.get_vertex().unwrap(), v.next().get_vertex().unwrap())).collect();
        assert_eq!(fan, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(2)), VertexId(KeyData::from_ffi(7))),
            (VertexId(KeyData::from_ffi(2)), VertexId(KeyData::from_ffi(1))),
            (VertexId(KeyData::from_ffi(2)), VertexId(KeyData::from_ffi(3))),
            ]));
        let fan:SmallVec<[_;6]> = mesh.goto(vertices[2]).iter_outgoing().map(|v| (v.get_vertex().unwrap(), v.next().get_vertex().unwrap())).collect();
        assert_eq!(fan, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(3)), VertexId(KeyData::from_ffi(6))),
            (VertexId(KeyData::from_ffi(3)), VertexId(KeyData::from_ffi(2))),
            (VertexId(KeyData::from_ffi(3)), VertexId(KeyData::from_ffi(4))),
            ]));
        let fan:SmallVec<[_;6]> = mesh.goto(vertices[3]).iter_outgoing().map(|v| (v.get_vertex().unwrap(), v.next().get_vertex().unwrap())).collect();
        assert_eq!(fan, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(4)), VertexId(KeyData::from_ffi(5))),
            (VertexId(KeyData::from_ffi(4)), VertexId(KeyData::from_ffi(3))),
            (VertexId(KeyData::from_ffi(4)), VertexId(KeyData::from_ffi(1))),
            ]));
        let fan:SmallVec<[_;6]> = mesh.goto(vertices[4]).iter_outgoing().map(|v| (v.get_vertex().unwrap(), v.next().get_vertex().unwrap())).collect();
        assert_eq!(fan, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(5)), VertexId(KeyData::from_ffi(4))),
            (VertexId(KeyData::from_ffi(5)), VertexId(KeyData::from_ffi(8))),
            (VertexId(KeyData::from_ffi(5)), VertexId(KeyData::from_ffi(6))),
            ]));
        let fan:SmallVec<[_;6]> = mesh.goto(vertices[5]).iter_outgoing().map(|v| (v.get_vertex().unwrap(), v.next().get_vertex().unwrap())).collect();
        assert_eq!(fan, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(6)), VertexId(KeyData::from_ffi(3))),
            (VertexId(KeyData::from_ffi(6)), VertexId(KeyData::from_ffi(5))),
            (VertexId(KeyData::from_ffi(6)), VertexId(KeyData::from_ffi(7))),
            ]));
        let fan:SmallVec<[_;6]> = mesh.goto(vertices[6]).iter_outgoing().map(|v| (v.get_vertex().unwrap(), v.next().get_vertex().unwrap())).collect();
        assert_eq!(fan, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(7)), VertexId(KeyData::from_ffi(2))),
            (VertexId(KeyData::from_ffi(7)), VertexId(KeyData::from_ffi(6))),
            (VertexId(KeyData::from_ffi(7)), VertexId(KeyData::from_ffi(8))),
            ]));
        let fan:SmallVec<[_;6]> = mesh.goto(vertices[7]).iter_outgoing().map(|v| (v.get_vertex().unwrap(), v.next().get_vertex().unwrap())).collect();
        assert_eq!(fan, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(8)), VertexId(KeyData::from_ffi(1))),
            (VertexId(KeyData::from_ffi(8)), VertexId(KeyData::from_ffi(7))),
            (VertexId(KeyData::from_ffi(8)), VertexId(KeyData::from_ffi(5))),
            ]));
    }

    #[test]
    fn test_iter_loop() {
        let mesh = Cuboid::new(1.0, 1.0, 1.0).procgen();
        let vertices:SmallVec<[_;8]> = mesh.vertices.keys().collect();
        let r#loop:SmallVec<[_;6]> = mesh.goto(vertices[0]).iter_loop().map(|v| (v.get_vertex().unwrap(), v.next().get_vertex().unwrap())).collect();
        assert_eq!(r#loop, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(1)), VertexId(KeyData::from_ffi(8))),
            (VertexId(KeyData::from_ffi(8)), VertexId(KeyData::from_ffi(7))),
            (VertexId(KeyData::from_ffi(7)), VertexId(KeyData::from_ffi(2))),
            (VertexId(KeyData::from_ffi(2)), VertexId(KeyData::from_ffi(1))),
            ]));
        let r#loop:SmallVec<[_;6]> = mesh.goto(VertexId(KeyData::from_ffi(5))).halfedge().twin().iter_loop().map(|v| (v.get_vertex().unwrap(), v.next().get_vertex().unwrap())).collect();
        assert_eq!(r#loop, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(4)), VertexId(KeyData::from_ffi(5))),
            (VertexId(KeyData::from_ffi(5)), VertexId(KeyData::from_ffi(8))),
            (VertexId(KeyData::from_ffi(8)), VertexId(KeyData::from_ffi(1))),
            (VertexId(KeyData::from_ffi(1)), VertexId(KeyData::from_ffi(4))),
            ]));
        let r#loop:SmallVec<[_;6]> = mesh.goto(VertexId(KeyData::from_ffi(4))).halfedge().twin().iter_loop().map(|v| (v.get_vertex().unwrap(), v.next().get_vertex().unwrap())).collect();
        assert_eq!(r#loop, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(5)), VertexId(KeyData::from_ffi(4))),
            (VertexId(KeyData::from_ffi(4)), VertexId(KeyData::from_ffi(3))),
            (VertexId(KeyData::from_ffi(3)), VertexId(KeyData::from_ffi(6))),
            (VertexId(KeyData::from_ffi(6)), VertexId(KeyData::from_ffi(5))),
            ]));
    }

    #[test]
    fn test_adjacent_faces() {
        let mesh = Cuboid::new(1.0, 1.0, 1.0).procgen();
        let faces:SmallVec<[_;3]> = mesh.goto(VertexId(KeyData::from_ffi(1))).adjacent_faces().map(|t| t.get_face().unwrap().unwrap()).collect();
        assert_eq!(faces, SmallVec::from_buf([
            FaceId(KeyData::from_ffi(3)),
            FaceId(KeyData::from_ffi(4)),
            FaceId(KeyData::from_ffi(1)),
            ]));
        let faces:SmallVec<[_;3]> = mesh.goto(VertexId(KeyData::from_ffi(7))).adjacent_faces().map(|t| t.get_face().unwrap().unwrap()).collect();
        assert_eq!(faces, SmallVec::from_buf([
            FaceId(KeyData::from_ffi(3)),
            FaceId(KeyData::from_ffi(5)),
            FaceId(KeyData::from_ffi(2)),
            ]));
    }

    #[test]
    fn test_iter_loop_contains() {
        let mesh = Cuboid::new(1.0, 1.0, 1.0).procgen();
        assert!(mesh.goto(VertexId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(1))));
        assert!(mesh.goto(VertexId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(8))));
        assert!(mesh.goto(VertexId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(7))));
        assert!(mesh.goto(VertexId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(2))));
        assert!(!mesh.goto(VertexId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(3))));
        assert!(!mesh.goto(VertexId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(4))));
        assert!(!mesh.goto(VertexId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(5))));
        assert!(!mesh.goto(VertexId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(6))));
        assert!(mesh.goto(FaceId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(1))));
        assert!(mesh.goto(FaceId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(2))));
        assert!(mesh.goto(FaceId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(3))));
        assert!(mesh.goto(FaceId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(4))));
        assert!(!mesh.goto(FaceId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(5))));
        assert!(!mesh.goto(FaceId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(6))));
        assert!(!mesh.goto(FaceId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(7))));
        assert!(!mesh.goto(FaceId(KeyData::from_ffi(1))).iter_loop().contains(VertexId(KeyData::from_ffi(8))));
    }

    #[test]
    fn test_adjacent_faces_contains() {
        let mesh = Cuboid::new(1.0, 1.0, 1.0).procgen();
        assert!(mesh.goto(VertexId(KeyData::from_ffi(1))).adjacent_faces().any(|f| f.iter_loop().contains(VertexId(KeyData::from_ffi(3)))));
    }

    #[test]
    fn test_halfedge_to() {
        let mut mesh = HalfEdgeMesh::new();
        let v1 = mesh.new_vertex();
        let v2 = mesh.new_vertex();
        let edge = mesh.new_edge(v1.into(), v2.into());
        let search_result = mesh.goto(v1).halfedge_to(v2).get_halfedge().unwrap();
        assert_eq!(edge.0, search_result)
    }

}