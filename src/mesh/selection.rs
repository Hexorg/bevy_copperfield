use bevy::utils::{hashbrown::{hash_map::Keys, hash_set::Iter, HashSet}};
use slotmap::Key;

use super::{traversal::Traversal, FaceId, HalfEdgeId, HalfEdgeMesh, MeshPosition, VertexId};


#[derive(Clone, PartialEq, Eq)]
/// A mononotous selection on a mesh, several edges, vertices, or faces, but not a mix. 
pub enum MeshSelection{
    Vertices(HashSet<VertexId>),
    HalfEdges(HashSet<HalfEdgeId>),
    Faces(HashSet<FaceId>)
}

impl MeshSelection {
    pub fn len(&self) -> usize {
        match self {
            MeshSelection::Vertices(vec) => vec.len(),
            MeshSelection::HalfEdges(vec) => vec.len(),
            MeshSelection::Faces(vec) => vec.len(),
        }
    }
}

#[derive(Clone)]
/// This structure keeps track of selecting multiple mesh items at a time to enable some more-complex operations
/// to act differently based on if they are applied to one item or multiple.
pub struct Selection<'m> {
    mesh: &'m HalfEdgeMesh,
    selection:MeshSelection,
}

impl From<MeshPosition> for MeshSelection {
    fn from(value: MeshPosition) -> Self {
        match value {
            MeshPosition::Vertex(vertex_id) => Self::Vertices(HashSet::from([vertex_id])),
            MeshPosition::HalfEdge(half_edge_id) => Self::HalfEdges(HashSet::from([half_edge_id])),
            MeshPosition::Face(face_id) => Self::Faces(HashSet::from([face_id])),
        }
    }
}

enum SelectionIteratorKeys<'m>{
    Vertices(Iter<'m, VertexId>),
    Edges(Iter<'m, HalfEdgeId>),
    Faces(Iter<'m, FaceId>)
}

pub struct SelectionIterator<'m>{
    mesh: &'m HalfEdgeMesh,
    selection:SelectionIteratorKeys<'m>,
    // selection_index:usize
}


impl<'m> Selection<'m> {
    pub(crate) fn new(mesh:&'m HalfEdgeMesh, position:MeshPosition) -> Self {
        Self{mesh, selection:position.into()}
    }
    pub fn build(self) -> MeshSelection {
        self.selection
    }

    pub fn append(&mut self, halfedge:HalfEdgeId) -> bool {
        match &mut self.selection {
            MeshSelection::Vertices(vec) => vec.insert(self.mesh[halfedge].vertex),
            MeshSelection::HalfEdges(vec) => vec.insert(halfedge),
            MeshSelection::Faces(vec) => vec.insert(self.mesh[halfedge].face.unwrap()),
        }
    }

    pub fn iter(&'m self) -> SelectionIterator<'m> {
        let selection = match &self.selection {
            MeshSelection::Vertices(hash_set) => SelectionIteratorKeys::Vertices(hash_set.iter()),
            MeshSelection::HalfEdges(hash_set) => SelectionIteratorKeys::Edges(hash_set.iter()),
            MeshSelection::Faces(hash_set) => SelectionIteratorKeys::Faces(hash_set.iter()),
        };
        SelectionIterator { mesh: self.mesh, selection}
    }

    pub fn to_vertex_selection(self) -> Self {
        let selection = match &self.selection {
            MeshSelection::Vertices(_) => return self,
            MeshSelection::HalfEdges(vec) => MeshSelection::Vertices(vec.iter().map(|e| self.mesh.goto(*e).vertex()).collect::<HashSet<_>>()),
            MeshSelection::Faces(vec) => MeshSelection::Vertices(vec.iter().map(|f| self.mesh.goto(*f).iter_loop().map(|t| t.vertex())).flatten().collect::<HashSet<_>>()),
        };
        Selection { mesh: self.mesh, selection }
    }

    pub fn to_edge_selection(self) -> Self {
        let selection = match &self.selection {
            MeshSelection::Vertices(vec) => MeshSelection::HalfEdges(vec.iter().map(|&v| self.mesh[v].halfedge).collect::<HashSet<_>>()),
            MeshSelection::HalfEdges(_) => return self,
            MeshSelection::Faces(vec) => MeshSelection::HalfEdges(vec.iter().map(|&f| self.mesh.goto(f).iter_loop().map(|t| *t)).flatten().collect::<HashSet<_>>()),
        };
        Selection { mesh: self.mesh, selection }
    }

    /// Selects adjacent faces of vertices and exact faces of halfedges
    pub fn to_face_selection(self) -> Self {
        let selection = match &self.selection {
            MeshSelection::Vertices(vec) => MeshSelection::Faces(vec.iter().map(|&v| self.mesh.goto(v).adjacent_faces().map(|t| t.face().unwrap())).flatten().collect::<HashSet<_>>()),
            MeshSelection::HalfEdges(vec) => MeshSelection::Faces(vec.iter().map(|&e| self.mesh[e].face.unwrap()).collect::<HashSet<_>>()),
            MeshSelection::Faces(_) => return self,
        };
        Selection { mesh: self.mesh, selection }
    }
}

impl<'m> Iterator for SelectionIterator<'m> {
    type Item = Traversal<'m>;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.selection {
            SelectionIteratorKeys::Vertices(iter) => iter.next().and_then(|&v| Some(self.mesh.goto(v))),
            SelectionIteratorKeys::Edges(iter) => iter.next().and_then(|&v| Some(self.mesh.goto(v))),
            SelectionIteratorKeys::Faces(iter) => iter.next().and_then(|&v| Some(self.mesh.goto(v))),
        }
    }
}