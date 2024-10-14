use super::{traversal::Traversal, FaceId, HalfEdgeId, MeshPosition, VertexId};


#[derive(Clone, PartialEq, Eq)]
/// A mononotous selection on a mesh, several edges, vertices, or faces, but not a mix. 
pub enum MeshSelection{
    Vertices(Vec<VertexId>),
    HalfEdges(Vec<HalfEdgeId>),
    Faces(Vec<FaceId>)
}

#[derive(Clone)]
/// This structure keeps track of selecting multiple mesh items at a time to enable some more-complex operations
/// to act differently based on if they are applied to one item or multiple.
pub struct Selection<'m> {
    traversal:Traversal<'m>,
    selection:MeshSelection,
}

impl From<MeshPosition> for MeshSelection {
    fn from(value: MeshPosition) -> Self {
        match value {
            MeshPosition::Vertex(vertex_id) => Self::Vertices(vec![vertex_id]),
            MeshPosition::HalfEdge(half_edge_id) => Self::HalfEdges(vec![half_edge_id]),
            MeshPosition::Face(face_id) => Self::Faces(vec![face_id]),
        }
    }
}


impl<'m> Selection<'m> {
    pub(crate) fn new(traversal:Traversal<'m>, position:MeshPosition) -> Self {
        Self{traversal, selection:position.into()}
    }
    pub fn build(self) -> MeshSelection {
        self.selection
    }

    pub fn next(&mut self) {
        self.traversal = self.traversal.next();
        let halfedge = *self.traversal;
        self.append(halfedge);
    }

    fn append(&mut self, halfedge:HalfEdgeId) {
        match &mut self.selection {
            MeshSelection::Vertices(vec) => vec.push(self.traversal.mesh[halfedge].vertex),
            MeshSelection::HalfEdges(vec) => vec.push(halfedge),
            MeshSelection::Faces(vec) => vec.push(self.traversal.mesh[halfedge].face.unwrap()),
        }
    }
}
