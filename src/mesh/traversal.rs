
use bevy::{prelude::{default, Deref, DerefMut}, render::render_graph::Edge};


use super::{selection::Selection, FaceId, HalfEdgeId, HalfEdgeMesh, MeshPosition, StackVec, VertexId};

const TRAVERSAL_LOOP_LIMIT:usize = 32; // We really don't expect more than TRAVERSAL_LOOP_LIMIT-gons or more than TRAVERSAL_LOOP_LIMIT edges coming out of a vertex


#[derive(Clone, Copy, Deref, DerefMut)]
/// Collection of convenience methods to traverse the mesh.
/// Has paniking and non-paniking methods for checking mesh state.
pub struct Traversal<'m> {
    #[deref]
    position: HalfEdgeId,
    pub(crate) mesh:&'m HalfEdgeMesh,
}

#[derive(Copy, Clone, Debug)]
enum EdgeIteratorKind{
    Incoming,
    Outgoing,
    Loop,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct VertexFlow{
    pub incoming:HalfEdgeId,
    pub vertex:VertexId,
    pub outgoing:HalfEdgeId,
}

impl VertexFlow {
    pub fn new(vertex:VertexId) -> Self {
        VertexFlow { incoming: default(), vertex, outgoing: default() }
    }
}

impl std::fmt::Debug for VertexFlow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = f.debug_struct("");
        if self.incoming != default() {
            output.field("incoming", &self.incoming);
        }
        output.field("vertex", &self.vertex);
        if self.outgoing != default() {
            output.field("outgoing", &self.outgoing);
        }
        output.finish()
    }
}

#[derive(Clone, Copy)]
pub struct EdgeIterator<'m> {
    // We want iterator to stop doing anything if one of the pointers is broken
    // However that means we need to be able to check if we are in a broken state and 
    // emit None. However, Traversal can't be in a broken state, so we wrap it in the 
    // Option to signify end of iteration
    traversal:Option<Traversal<'m>>,
    kind:EdgeIteratorKind,
    start: HalfEdgeId, // iteration can only happen over half-edges, any iteration lands on halfedges too, so we use it for comparison to know when to stop iteration
    count:usize,
}

impl<'m> EdgeIterator<'m> {
    pub fn contains(mut self, pos:impl Into<MeshPosition>+Copy) -> bool {
        use MeshPosition::*;
        self.any( |edge|
            match pos.into() {
                Vertex(v) => edge.vertex() == v || edge.mesh[v].halfedge == *edge,
                HalfEdge(e2) => *edge == e2,
                Face(f) => edge.face() == Some(f),
            }
        )
    }
}


impl<'m> Iterator for EdgeIterator<'m> {
    type Item = Traversal<'m>;

    fn next(&mut self) -> Option<Self::Item> {
        self.count += 1;
        if self.count > TRAVERSAL_LOOP_LIMIT {
            panic!("Traversal::Iter_{:?} Iterated {TRAVERSAL_LOOP_LIMIT} times starting from {:?}. Assuming vertices can't have that many edges, therefore the mesh is broken.", self.kind, self.start);
        } 
        let item = match self.kind {
            EdgeIteratorKind::Loop | EdgeIteratorKind::Outgoing => self.traversal,
            EdgeIteratorKind::Incoming => self.traversal.map(|t| t.twin()),
        };
        self.traversal = match self.kind {
            EdgeIteratorKind::Loop => self.traversal.and_then(|t| t.try_next()),
            EdgeIteratorKind::Outgoing | EdgeIteratorKind::Incoming => self.traversal.and_then(|t| t.try_twin()).and_then(|t| t.try_next()),
        }.and_then(|t| if t.position == self.start { None } else { Some(t) });
        item
    }
}

impl<'m> PartialEq for Traversal<'m> {
    fn eq(&self, other: &Self) -> bool {
        self.position == other.position
    }
}

impl<'m> Eq for Traversal<'m> { }

impl<'m> From<Traversal<'m>> for HalfEdgeId {
    fn from(value: Traversal<'m>) -> Self {
        value.position
    }
}

impl<'m> Traversal<'m> {
    pub fn new(mesh:&'m HalfEdgeMesh, position: HalfEdgeId) -> Self {
        if !mesh.halfedges.contains_key(position) {
            panic!("Created Traversal with invalid position");
        };
        Self{mesh, position}
    }

    /// Convert current position into vertex selection
    pub fn select_vertex(self) -> Selection<'m> {
        let selection = MeshPosition::Vertex(self.vertex());
        Selection::new(self.mesh, selection.into())
    }

    /// Convert current position into halfedge selection
    pub fn select_edge(self) -> Selection<'m> {
        let selection = MeshPosition::HalfEdge(self.position);
        Selection::new(self.mesh, selection.into())
    }

    /// Convert current position into face selection. Panics if current position is boundary
    pub fn select_face(self) -> Selection<'m> {
        let selection = MeshPosition::Face(self.face().unwrap());
        Selection::new(self.mesh, selection.into())
    }

    /// Get current vertex and scan over outgoing edges. If any of the edges land on `vertex` - return that edge.
    /// Panics if halfedge is not found
    pub fn halfedge_to(self, vertex:VertexId) -> Self {
        match self.find_halfedge_to(vertex) {
            Some(e) => e,
            None => panic!("{:?} has no edge to {vertex:?}", self.position)
        }
    }

    /// Get current vertex and scan over outgoing edges. If any of the edges land on `vertex` - return that edge.
    pub fn find_halfedge_to(self, vertex:VertexId) -> Option<Self> {
        self.iter_outgoing().find(|e| e.twin().vertex() == vertex)
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
    pub fn iter_loop(self) -> EdgeIterator<'m> {
        EdgeIterator{traversal:Some(self), kind:EdgeIteratorKind::Loop, start:self.position, count:0}
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
    pub fn iter_outgoing(self) -> EdgeIterator<'m> {
        EdgeIterator{traversal:Some(self), kind:EdgeIteratorKind::Outgoing, start:self.position, count:0}
    }

    /// Iterate over all incoming edges (twins of outgoing edges)
    /// It's similar to outgoing, except all edges are twins E.g.
    /// ```text
    ///    \ | /
    ///     \|/
    ///  --- . ---
    ///     /|\
    ///    / | \
    /// ```
    pub fn iter_incoming(self) -> EdgeIterator<'m> {
        EdgeIterator{traversal:Some(self), kind:EdgeIteratorKind::Incoming, start:self.position, count:0}

    }

    /// Iterate over all halfedges that have a face associated with them
    pub fn adjacent_faces(self) -> impl Iterator<Item = Traversal<'m>> {
        self.iter_outgoing().filter(|p| p.face().is_some())
    }

    #[inline]
    /// Returns the associate half-edge with whatever current position is
    /// [`Traversal`] also derefereces to this so you can say
    /// ```
    /// use bevy::prelude::default;
    /// use bevy_copperfield::mesh::{HalfEdgeMesh, HalfEdge};
    /// let mut mesh = HalfEdgeMesh::new();
    /// // Fill mesh here
    /// let v = [mesh.new_vertex(), mesh.new_vertex()];
    /// mesh.new_edge(HalfEdge{vertex:v[0],..default()}, HalfEdge{vertex:v[1],..default()});
    /// // Get position
    /// let some_position = mesh.edge_keys().next().unwrap();
    /// let traversal = mesh.goto(some_position);
    /// assert_eq!(traversal.halfedge(), *traversal);
    /// ```
    pub fn halfedge(&self) -> HalfEdgeId {
        self.position
    }

    #[inline]
    /// Returns the associate half-edge with whatever current position is
    pub fn vertex(&self) -> VertexId {
        self.mesh[self.position].vertex
    }

    #[inline]
    /// Returns the associated face with a given position.
    /// If option is none, then the associated position is a boundary position (mesh ends there) 
    pub fn face(&self) -> Option<FaceId> {
        self.mesh[self.position].face
    }

    /// Scans the nearby halfedges and returns incoming and outgoing boundary edges to this vertex
    /// Or `HalfEdgeId::default()` if those edges aren't found
    /// HalfEdgeMesh assumes `HalfEdgeId::default()` is equivalent to a NULL pointer
    pub fn get_flow(self, face:Option<FaceId>) -> StackVec<VertexFlow> {
        let mut result:StackVec<_> = self.iter_incoming().filter_map(|incoming| if incoming.face() == face {
            let outgoing = incoming.try_next().map(|t| if t.face() == face { *t } else { 
                self.mesh.print_mesh();
                panic!("Outgoing edge {t:?} has different face {:?} from incoming edge {incoming:?} face {:?}. Mesh is malformed", t.face(), incoming.face());
             }).unwrap_or_else(|| if face.is_none() { *incoming.twin() } else { default()});
            let vertex = incoming.twin().vertex();
                Some(VertexFlow { incoming: *incoming, vertex, outgoing })
            } else {
                None
            }
        ).collect();
        if result.is_empty() {
            result = self.iter_outgoing().filter_map(|outgoing| if outgoing.face() == face {
                Some(VertexFlow{incoming:default(), vertex:outgoing.vertex(), outgoing:*outgoing})
            } else {
                None
            }).collect();
        }
        // If there's neither incoming nor outgoing edges, we output just the vertex

        result
    }

    /// Traverse to the next half-edge or to the next vertex in a Face
    pub fn next(self) -> Self {
        match self.try_next() {
            Some(t) => t,
            None => {
                #[cfg(test)]
                self.mesh.print_mesh();
                panic!("{:?}.next is invalid", self.position)
            }
        }
    }

    /// Traverse to the next half-edge or to its twin if the next pointer is invalid (happens during first face construction)
    pub fn next_or_twin(mut self) -> Self {
        let next = self.mesh[self.position].next;
        self.position = if next != default() { next } else { self.mesh[self.position].twin };
        self
    }

    /// Get the previous vertex or edge in a face. 
    pub fn previous(self) -> Self {
        match self.iter_incoming().find(|t| t.next().position == self.position) {
            Some(t) => t,
            None => {
                #[cfg(test)]
                self.mesh.print_mesh();
                panic!("Can't find previous of {:?}", self.position);
            }
        }
    }

    /// Get the previous vertex or edge in a face or to its twin if the next pointer is invalid (happens during first face construction) 
    pub fn previous_or_twin(self) -> Self {
        self.iter_incoming()
            .find(|t| t.try_next().map(|n| n.position == self.position).unwrap_or(false))
            .unwrap_or(self.twin())
    }

    pub fn twin(self) -> Self {
        match self.try_twin() {
            Some(t) => t,
            None => {
                #[cfg(test)]
                self.mesh.print_mesh();
                panic!("{:?}.twin is invalid", self.position)
            }
        }
    }

    pub fn try_next(mut self) -> Option<Self> {
        let next= self.mesh[self.position].next;
        if next != default() {
            self.position = next;
            Some(self)
        } else {
            None
        }
    }

    pub fn try_twin(mut self) -> Option<Self> {
        let twin= self.mesh[self.position].twin;
        if twin != default() {
            self.position = twin;
            Some(self)
        } else {
            None
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
    use bevy::prelude::default;
    use slotmap::KeyData;
    

    use crate::mesh::{traversal::{Traversal, VertexFlow}, HalfEdge, HalfEdgeId, HalfEdgeMesh, VertexId};

    fn straight_line(count:usize, is_face:bool) -> HalfEdgeMesh {
        let mut mesh = HalfEdgeMesh::new();
        let mut last_vertex = mesh.vertices.insert(default());
        let mut last_edge = default();
        let mut last_twin = default();
        let face = if is_face { Some(mesh.faces.insert(default())) } else { None };
        for _ in 0..count {
            let next_vertex = mesh.vertices.insert(default());
            let edge = mesh.halfedges.insert(HalfEdge { twin: default(), next: default(), vertex:last_vertex, face});
            let twin = mesh.halfedges.insert(HalfEdge { twin: edge, next: last_twin, vertex:next_vertex, face:None});
            mesh[edge].twin = twin;
            mesh[last_vertex].halfedge = edge;
            mesh[next_vertex].halfedge = twin;
            if let Some(last_edge) = mesh.halfedges.get_mut(last_edge) {
                last_edge.next = edge;
            }
            last_edge = edge;
            last_twin = twin;
            last_vertex = next_vertex;
        }
        mesh
    }

    #[test]
    fn test_tries() {
        let mesh = straight_line(1, false);
        let t = mesh.goto(mesh.vertex_keys().next().unwrap());
        
        assert_eq!(t.try_next(), None);
        assert_eq!(t.try_twin(), Some(Traversal{mesh:&mesh, position:HalfEdgeId(KeyData::from_ffi(2))}));
        assert_eq!(t.iter_loop().count(), 1);
        assert_eq!(t.iter_incoming().count(), 1);
        assert_eq!(t.iter_outgoing().count(), 1);
    }

    #[test]
    fn test_flow_with_face() {
        let mesh = straight_line(2, true);
        let t = mesh.goto(VertexId(KeyData::from_ffi(2))).get_flow(None);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0], VertexFlow{
            incoming: HalfEdgeId(KeyData::from_ffi(4)),
            vertex:VertexId(KeyData::from_ffi(2)),
            outgoing:HalfEdgeId(KeyData::from_ffi(2))
        });
        let t = mesh.goto(VertexId(KeyData::from_ffi(3))).get_flow(None);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0], VertexFlow{
            incoming: default(),
            vertex: VertexId(KeyData::from_ffi(3)),
            outgoing:HalfEdgeId(KeyData::from_ffi(4))
        });
        let t = mesh.goto(VertexId(KeyData::from_ffi(1))).get_flow(None);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0], VertexFlow{
            incoming: HalfEdgeId(KeyData::from_ffi(2)),
            vertex: VertexId(KeyData::from_ffi(1)),
            outgoing:HalfEdgeId(KeyData::from_ffi(1))
        });
    }

    #[test]
    fn test_flow_without_face() {
        let mesh = straight_line(1, false);
        let t = mesh.goto(VertexId(KeyData::from_ffi(1))).get_flow(None);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0], VertexFlow{
            incoming:HalfEdgeId(KeyData::from_ffi(2)), 
            vertex:VertexId(KeyData::from_ffi(1)), 
            outgoing:HalfEdgeId(KeyData::from_ffi(1))});

        let t = mesh.goto(VertexId(KeyData::from_ffi(2))).get_flow(None);
        assert_eq!(t.len(), 1);
        assert_eq!(t[0], VertexFlow{
            incoming:HalfEdgeId(KeyData::from_ffi(1)), 
            vertex:VertexId(KeyData::from_ffi(2)), 
            outgoing:HalfEdgeId(KeyData::from_ffi(2))});
    }

    // #[test]
    // fn test_halfedge_to() {
    //     let mut mesh = HalfEdgeMesh::new();
    //     let v1 = mesh.new_vertex();
    //     let v2 = mesh.new_vertex();
    //     let edge = mesh.new_edge(v1, v2, default());
    //     let search_result = *mesh.goto(v1).halfedge_to(v2);
    //     assert_eq!(edge.0, search_result)
    // }

}