

use attributes::{AttributeKind, AttributeStore, AttributeValues, Attributes};
use bevy::{prelude::{default, Vec2, Vec3}, render::{mesh::{self, Mesh as BevyMesh}, render_asset::RenderAssetUsages}, utils::hashbrown::HashSet};
use itertools::Itertools;
use selection::Selection;
use slotmap::{KeyData, SlotMap};
use smallvec::SmallVec;
use traversal::Traversal;

pub mod attributes;
mod traversal;
mod selection;
pub mod vertex_ops;
pub mod edge_ops;
pub mod face_ops;
pub mod mesh_ops;


use crate::OPTIMIZE_FOR_NGONS_UNDER_SIZE;

pub type StackVec<T> = SmallVec<[T;OPTIMIZE_FOR_NGONS_UNDER_SIZE]>;

#[derive(Copy, Clone, PartialEq, Eq)]
/// A single position on a mesh. An edge, a vertex, or a face
pub enum MeshPosition{
    Vertex(VertexId),
    HalfEdge(HalfEdgeId),
    Face(FaceId)
}

#[derive(Clone, PartialEq, Eq)]
/// A mononotous selection on a mesh, several edges, vertices, or faces, but not a mix. 
pub enum MeshSelection{
    Vertices(Vec<VertexId>),
    HalfEdges(Vec<HalfEdgeId>),
    Faces(Vec<FaceId>)
}

pub trait Targettable {
    fn get_mesh_position(self, mesh:&HalfEdgeMesh) -> MeshPosition;
}

slotmap::new_key_type! { 
    /// Index of a given [`HalfEdge`] in the staging slotmap
    pub struct HalfEdgeId; 
}
slotmap::new_key_type! { 
    /// Index of a given [`Vertex`] in the staging slotmap
    pub struct VertexId;
}
slotmap::new_key_type! { 
    /// Index of a given [`Face`] in the staging slotmap
    pub struct FaceId; 
}

impl Targettable for HalfEdgeId {
    #[inline]
    fn get_mesh_position(self, _:&HalfEdgeMesh) -> MeshPosition {
        self.into()
    }
}

impl Targettable for VertexId {
    #[inline]
    fn get_mesh_position(self, _:&HalfEdgeMesh) -> MeshPosition {
        self.into()
    }
}

impl Targettable for FaceId {
    #[inline]
    fn get_mesh_position(self, _:&HalfEdgeMesh) -> MeshPosition {
        self.into()
    }
}

impl Targettable for Vec3 {
    fn get_mesh_position(self, mesh:&HalfEdgeMesh) -> MeshPosition {
        if let Some(attr) = mesh.attribute(&AttributeKind::Positions) {
            let (vertex, _) = attr.as_vertices_vec3().iter().fold((VertexId::default(), f32::MAX), |acc, p| {
                    let distance = (*p.1 - self).length();
                        if distance < acc.1 { (p.0, distance) } else { acc }
                });
                MeshPosition::Vertex(vertex)
        } else {
            MeshPosition::HalfEdge(HalfEdgeId(KeyData::default()))
        }
    }
}


impl std::fmt::Debug for MeshPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Vertex(arg0) => f.write_fmt(format_args!("{:?}", arg0)),
            Self::HalfEdge(arg0) => f.write_fmt(format_args!("{:?}", arg0)),
            Self::Face(arg0) => f.write_fmt(format_args!("{:?}", arg0)),
        }
    }
}

impl MeshPosition {
    pub fn is_valid(&self, mesh:&HalfEdgeMesh) -> bool {
        match self {
            &MeshPosition::Vertex(vertex_id) => mesh.vertices.contains_key(vertex_id),
            &MeshPosition::HalfEdge(half_edge_id) => mesh.halfedges.contains_key(half_edge_id),
            &MeshPosition::Face(face_id) => mesh.faces.contains_key(face_id),
        }
    }
}


impl Targettable for MeshPosition {
    #[inline]
    fn get_mesh_position(self, _:&HalfEdgeMesh) -> MeshPosition {
        self
    }
}

impl From<VertexId> for MeshPosition {
    #[inline]
    fn from(value: VertexId) -> Self {
        Self::Vertex(value)
    }
}

impl From<HalfEdgeId> for MeshPosition {
    #[inline]
    fn from(value: HalfEdgeId) -> Self {
        Self::HalfEdge(value)
    }
}

impl From<FaceId> for MeshPosition {
    #[inline]
    fn from(value: FaceId) -> Self {
        Self::Face(value)
    }
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


#[derive(Debug, Default, Clone, Copy)]
/// A common way to represent a polygon mesh is a shared list of vertices and a list of faces storing pointers for its vertices. 
/// This representation is both convenient and efficient for many purposes, however in some domains it proves ineffective.
/// Write up about Half-Edge Data structure can be found [here](https://www.flipcode.com/archives/The_Half-Edge_Data_Structure.shtml)
/// And [Here](https://jerryyin.info/geometry-processing-algorithms/half-edge/)1
pub struct HalfEdge {
    twin: HalfEdgeId, 
    next: HalfEdgeId,
    vertex: VertexId,
    face: Option<FaceId>,
}

#[derive(Debug, Clone, Copy, Default)]
/// Vertex stored a pointer to exactly one of the half-edges which uses the vertex as its starting point. 
/// At any given vertex there will be more than one half-edge we could choose for this, 
/// but we only need one and it doesn't matter which one it is.
pub struct Vertex {
    halfedge: HalfEdgeId,
}

#[derive(Debug, Clone, Copy, Default)]
/// The half-edge pointer in the face is similar to the pointer in the vertex structure in that although 
/// there are multiple half-edges bordering each face, we only need to store one of them, and it doesn't matter which one.
pub struct Face {
    halfedge: HalfEdgeId,
}

#[derive(Copy, Clone, Debug)]
pub enum MeshConstructionError{
    NeedPolygons,
    DuplicateVertices,
    UnknownPosition,

    /// Manifold is when each edge is contained in at most two polygons (or one polygon if it is on the boundary).
    /// NonManifold is when one edge is in more than two polygons
    NonManifold,
    DisconnectedVertex,
}

/// Editable Mesh based on Half-Edge data structure. The half-edge data structure is called that because instead of 
/// storing the edges of the mesh, we store half-edges. As the name implies, a half-edge is a half of an edge and 
/// is constructed by splitting an edge down its length. We'll call the two half-edges that make up an edge a pair. 
/// Half-edges are directed and the two edges of a pair have opposite directions.
pub struct HalfEdgeMesh {
    vertices: SlotMap<VertexId, Vertex>,
    faces: SlotMap<FaceId, Face>,
    halfedges: SlotMap<HalfEdgeId, HalfEdge>,
    attributes: Attributes,
}


macro_rules! index_mesh_with {
    ($id_type:ty, $output_type:ty, $property:ident) => {
        impl std::ops::Index<$id_type> for HalfEdgeMesh {
            type Output = $output_type;

            fn index(&self, index: $id_type) -> &Self::Output {
                match self.$property.get(index) {
                    Some(value) => value,
                    None => panic!("Broken {} pointer while reading a mesh.", stringify!($id_type)),
                }
            }
        }

        impl std::ops::IndexMut<$id_type> for HalfEdgeMesh {
            fn index_mut(&mut self, index: $id_type) -> &mut Self::Output {
                match self.$property.get_mut(index) {
                    Some(value) => value,
                    None => panic!("Broken {} pointer while writing a mesh.", stringify!($id_type)),
                }
            }
        }
    };
}

index_mesh_with!(VertexId, Vertex, vertices);
index_mesh_with!(FaceId, Face, faces);
index_mesh_with!(HalfEdgeId, HalfEdge, halfedges);

impl Default for HalfEdgeMesh {
    fn default() -> Self {
        Self::new()
    }
}

impl HalfEdgeMesh {
    pub fn new() -> Self {
        Self{
            vertices: SlotMap::with_key(),
            faces: SlotMap::with_key(),
            halfedges: SlotMap::with_key(),
            attributes:Attributes::new(),
        }
    }

    #[inline]
    /// Initiate traversal of the mesh starting at a given edge
    pub fn goto(&self, pos:impl Targettable) -> Traversal<'_> {
        Traversal::new(self, pos.get_mesh_position(self))
    }

    #[inline]
    /// Go to a position and select it, allowing you to select more items to later act on as a group
    pub fn select(&self, pos: impl Targettable) -> Selection<'_> {
        self.goto(pos).into()
    }

    pub fn new_vertex(&mut self) -> VertexId {
        self.vertices.insert(default())
    }

    /// Reconnect the mesh such that `to_edge.next` will point to newly created edge and newly created `edge.next` will point to `edge_next`
    /// At the same time, create a twin such that whatever `to_edge.next` was pointing to will end up being `twin.next` and the twin of `edge_next`
    /// will become `twin.previous`
    /// 
    ///twin_vertex(w) /twin_previous
    ///  __________./  twin_next
    ///  edge_next  \twin /
    ///          edge\   /
    ///                . - edge_vertex(v)
    ///              /   \
    ///edge_previous/     \
    pub fn new_edge(&mut self, edge_previous:MeshPosition, edge_next:MeshPosition) -> (HalfEdgeId, HalfEdgeId) {
        let edge_previous = Traversal::new(self, edge_previous);
        let edge_next = Traversal::new(self, edge_next);
        let (edge_vertex, twin_next, edge_previous) = if edge_previous.is_vertex() { 
            (
                edge_previous.get_vertex().unwrap(),
                default(),
                default(),
            )
        } else {  
            (
                edge_previous.twin().get_vertex().unwrap(),
                // Find what should be edge.next pointing to, or if we can't that means there are only to_edge and its twin, so twin should be used.
                edge_previous.next().get_halfedge().unwrap_or(edge_previous.twin().get_halfedge().unwrap()),
                edge_previous.get_halfedge().unwrap()
            )
        };

        let (twin_vertex, twin_previous, edge_next) = if edge_next.is_vertex() {
            (
                edge_next.get_vertex().unwrap(),
                default(),
                default(),
            )
        } else {
            (
                edge_next.get_vertex().unwrap(),
                // Find incoming edge that points to edge_next, or if we can't that means there are only edge_next and its twin, so twin should be used.
                edge_next.previous().get_halfedge().unwrap_or(edge_next.twin().get_halfedge().unwrap()),
                edge_next.get_halfedge().unwrap()
            )
        };

        if twin_next == twin_previous && twin_next != default() {
            let twin = self.goto(edge_previous).twin().get_halfedge().unwrap();
            (twin_next, twin)
        } else {
            let edge = self.halfedges.insert(HalfEdge { twin: default(), next:edge_next, vertex: edge_vertex, face: None });
            if edge_previous != default() {
                self[edge_previous].next = edge;
            }
            let twin = self.halfedges.insert(HalfEdge { twin: edge, next:twin_next, vertex: twin_vertex, face: None });
            self[edge].twin = twin;
            if twin_previous != default() {
                self[twin_previous].next = twin;
            }
            self[edge_vertex].halfedge = edge;
            self[twin_vertex].halfedge = twin;
            (edge, twin)
        }
    }

    /// Create a new face and attach it to correct twin edges.
    /// This method does not modify twin edges if they already exist
    pub fn new_face(&mut self, face:&[VertexId]) -> FaceId {
        let face_id = self.faces.insert(Face { halfedge: default() });

        let mut face_edges:StackVec<HalfEdgeId> = StackVec::new();
        for (&start, &end) in face.iter().circular_tuple_windows() {
            // choose previously created edge for this face, or choose a boundary edge if there are not previously created edges.
            let to_start = self.goto(start).iter_incoming().find(|pos| if !face_edges.is_empty() { pos.get_halfedge().ok() == face_edges.last().copied() } else { pos.get_face().unwrap().is_none()}).unwrap_or(self.goto(start));
            // choose the first edge of this face if only the last face-edge left, or choose a boundary edge 
            let from_end = self.goto(end).iter_outgoing().find(|pos| if face_edges.len() + 1 == face.len() { pos.get_halfedge().ok() == face_edges.first().copied()} else {pos.get_face().unwrap().is_none()}).unwrap_or(self.goto(end));
            
            let (edge, _) = self.new_edge(to_start.get_position(), from_end.get_position());
            face_edges.push(edge);
        }

        self[face_id].halfedge = face_edges[0];
        for edge in face_edges {
            self[edge].face = Some(face_id);
        }
        face_id
    }
    
    // Returns the normal of the face. The first three vertices are used to
    // compute the normal. If the vertices of the face are not coplanar,
    // the result will not be correct.
    pub fn face_normal(&self, face: FaceId) -> Vec3 {
        let positions = self.attributes[&AttributeKind::Positions].as_vertices_vec3();
        let verts:StackVec<_> = self.goto(face)
            .iter_loop()
            .map(|f| positions[f.get_vertex().unwrap()]).collect();
        for idx in 0..(verts.len()-2) {
            let v01 = verts[idx] - verts[idx+1];
            let v12 = verts[idx+1] - verts[idx+2];
            let normal = v01.cross(v12).normalize();
            if !normal.is_nan() {
                return normal
            }
        }
        Vec3::ZERO
    }

    pub fn get_normal(&self, edge:HalfEdgeId) -> Vec3 {
        // TODO: Get this from edge data instead
        self.face_normal(self[edge].face.unwrap())
    }

    pub fn get_uv(&self, edge:HalfEdgeId) -> Vec2 {
        // TODO: Oh boy, UV Mapping
        Vec2::ZERO
    }

    /// How many faces are currently allocated
    pub fn face_count(&self) -> usize {
        self.faces.len()
    }

    /// How many vertices are currently allocated
    pub fn vertex_count(&self) -> usize {
        self.vertices.len()
    }

    pub fn face_keys(&self) -> slotmap::basic::Keys<FaceId, Face>{
        self.faces.keys()
    }

    pub fn vertex_keys(&self) -> slotmap::basic::Keys<VertexId, Vertex>{
        self.vertices.keys()
    }

    pub fn edge_keys(&self) -> slotmap::basic::Keys<HalfEdgeId, HalfEdge>{
        self.halfedges.keys()
    }

    /// Insert attributes into mesh. Faces, Edges, and Vertices can have their own attribute data
    pub fn add_attribute(&mut self, kind:AttributeKind, store: impl Into<AttributeValues>) -> Option<AttributeValues> {
        self.attributes.insert(kind, store.into())
    }

    /// Get an attribute
    pub fn attribute(&self, kind:&AttributeKind) -> Option<&AttributeValues> {
        self.attributes.get(kind)
    }

    /// Get a sum of all face-edges (usefull for checks)
    pub fn count_face_edges(&self) -> usize {
        let mut count:usize = 0;
        for face in self.faces.keys() {
            count += self.goto(face).iter_loop().count();
        }
        count
    }

    /// Count mesh islands (unconnected meshes)
    pub fn count_islands(&self) -> usize {
        let mut unvisited_vertices:HashSet<_> = self.vertices.keys().collect();
        let mut island_count = 0;
        fn remove_visited(set:&mut HashSet<VertexId>, mesh:&HalfEdgeMesh, init:VertexId) {
            set.remove(&init); // iter_fan iterates over the edges (including edge of this vertex) but if this vertex has no edge this function will loop forever without removing this vertex first
            for visited in mesh.goto(init).iter_outgoing().map(|t| t.next().get_vertex().unwrap()) {
                let visited = visited;
                if set.remove(&visited) {
                    remove_visited(set, mesh, visited)
                } 
            }
        }
        loop {
            if let Some(&init_vertex) = unvisited_vertices.iter().next() {
                island_count += 1;
                remove_visited(&mut unvisited_vertices, self, init_vertex);
            } else {
                break;
            }
        }

        island_count
    }

    /// The count of outgoing edges in a vertex
    pub fn vertex_degree(&self, vertex:VertexId) -> usize {
        self.goto(vertex).iter_outgoing().count() 
    }

    fn print_mesh(&self) {
        println!("Mesh:");
        for (id, Vertex{halfedge}) in &self.vertices {
            println!("\t{id:?} -> {halfedge:?}");
        }
        for (id, Face{halfedge}) in &self.faces {
            println!("\t{id:?} -> {halfedge:?}");
        }
        for (id, HalfEdge{twin, next, vertex, face}) in &self.halfedges {
            println!("\t{id:?} -> twin:{twin:?} next:{next:?} {vertex:?} {face:?}");
        }
    }
}


impl TryFrom<BevyMesh> for HalfEdgeMesh {
    type Error = MeshConstructionError;
    fn try_from(bevy_mesh: BevyMesh) -> Result<Self, Self::Error> {
        if let (Some(positions), Some(indices)) = (bevy_mesh.attribute(BevyMesh::ATTRIBUTE_POSITION), bevy_mesh.indices()) {
            if let Some(positions) = positions.as_float3() {
                let mut mesh = HalfEdgeMesh::new();
                let mut index_to_vertex_map:Vec<Option<VertexId>> = (0..positions.len()).map(|_| None).collect();
                let mut position_attribute:AttributeStore<VertexId, Vec3> = AttributeStore::new();
                for triangle in indices.iter().batching(|it| match (it.next(), it.next(), it.next()) {
                    (Some(a), Some(b), Some(c)) => Some([a, b, c]),
                    _ => None
                }) {
                    let face = triangle.map(|idx| match index_to_vertex_map[idx] {
                        Some(v) => v,
                        None => {let v = mesh.new_vertex(); index_to_vertex_map[idx] = Some(v); v}
                    });
                    mesh.new_face(&face);
                    triangle.iter().zip(face).for_each(
                        |(&idx, vertex)| _ = position_attribute.insert(vertex, positions[idx].into()));
                    }
                mesh.add_attribute(AttributeKind::Positions, position_attribute);
                Ok(mesh)
            } else {
                todo!("Only indexed [f32;3] format supported for now");
            }
        } else {
            todo!("Only indexed [f32;3] format supported for now");
        }

    }
    
}

impl From<&HalfEdgeMesh> for BevyMesh {
    fn from(mesh: &HalfEdgeMesh) -> Self {
        let mut positions = Vec::new();
        let mut normals = Vec::new();
        let mut uvs= Vec::new();
        let mut indices = Vec::new();
        // mesh.print_mesh();
        let position_data = mesh.attribute(&AttributeKind::Positions).unwrap().as_vertices_vec3();
        for face in mesh.faces.keys() { 
            let face_start_index = positions.len() as u32;
            let halfedges:SmallVec<[_;6]> = mesh.goto(face).iter_loop().map(|f| (f.get_halfedge().unwrap(), f.get_vertex().unwrap())).collect();
            let mut index_mapping:SmallVec<[Option<usize>;6]> = SmallVec::new();
            for _ in 0..halfedges.len() {
                index_mapping.push(None);
            }
            let is_even = (halfedges.len() as i32 % 2) == 0;
            // println!("New face {face:?}, size: {}", halfedges.len());
            // println!("Halfedges: {halfedges:?}");
            // for (_,v) in &halfedges {
            //     println!("\t{v:?} - {:?}", position_data[*v]);
            // }
            let mut start_index = 0;
            let mut end_index = halfedges.len() as i32 - 1;
            // Triangulate theese polygons. TODO: Figure out better way. Most crates I found were for 2D
            while end_index > 0  {
                let triangle = [start_index, start_index+1, end_index];
                // println!("Triangle: {triangle:?}");
                for local_index in triangle {
                    if let Some(output_index) = index_mapping[local_index as usize] {
                        indices.push(output_index as u32);
                        // println!("\t output_index: {output_index:?}");
                    } else {
                        index_mapping[local_index as usize] = Some(positions.len());
                        indices.push(positions.len() as u32);
                        // println!("\t output_index: {:?}", positions.len() as u32);
                        let (edge, vertex, ) = halfedges[local_index as usize];
                        positions.push(position_data[vertex].to_array());
                        normals.push(mesh.face_normal(face).to_array());
                        uvs.push(mesh.get_uv(edge));
                    }
                }
                end_index -= 1;
                start_index += 1;
                if start_index + if is_even { 1 } else { 0 } == end_index {
                    // println!("Detected crossover");
                    start_index += 1;
                    end_index -= 1;
                }
                // println!("Positions: {positions:?}");
            } 
        }

        let indices = mesh::Indices::U32(indices);
        BevyMesh::new(
            mesh::PrimitiveTopology::TriangleList,
            RenderAssetUsages::default(),
        )
        .with_inserted_attribute(BevyMesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(BevyMesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(BevyMesh::ATTRIBUTE_UV_0, uvs)
        .with_inserted_indices(indices)
    }
}

#[cfg(test)]
mod tests {
    use bevy::prelude::{Cuboid, MeshBuilder, Meshable};
    use slotmap::KeyData;
    use smallvec::SmallVec;

    use crate::{mesh::{FaceId, HalfEdgeMesh, VertexId}, mesh_builders::HalfEdgeMeshBuilder};

    use super::HalfEdgeId;

    /// Returns a mesh in the form
    /// ```text
    /// indices    VertexId    FaceId
    /// 0--3--5    1--4--6     +--+--+
    /// |  |  |    |  |  |     |1 |2 |
    /// 1--2--4    2--3--5     +--+--+
    /// |  |  |    |  |  |     |3 |4 |
    /// 6--7--8    7--8--9     +--+--+
    /// ```
    pub fn sample_mesh() -> HalfEdgeMesh {
        let mut mesh = HalfEdgeMesh::new();
        let vertices:SmallVec<[_;9]> = (0..9).map(|_| mesh.new_vertex()).collect();
        mesh.new_face(&[vertices[0], vertices[1], vertices[2], vertices[3]]);
        mesh.new_face(&[vertices[3], vertices[2], vertices[4], vertices[5]]);
        mesh.new_face(&[vertices[1], vertices[6], vertices[7], vertices[2]]);
        mesh.new_face(&[vertices[2], vertices[7], vertices[8], vertices[4]]);
        mesh
    }

    #[test]
    fn test_sample_mesh() {
        let mesh = sample_mesh();
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.count_face_edges(), 16);
        assert_eq!(mesh.face_count(), 4);
        assert_eq!(mesh.vertex_count(), 9);
        for i in 1..(mesh.face_count()+1) {
            assert_eq!(mesh[mesh[FaceId(KeyData::from_ffi(i as u64))].halfedge].face, Some(FaceId(KeyData::from_ffi(i as u64))));
        }
        for i in 1..(mesh.vertex_count()+1) {
            assert_eq!(mesh.vertex_degree(VertexId(KeyData::from_ffi(i as u64))), mesh.goto(VertexId(KeyData::from_ffi(i as u64))).adjacent_faces().count() + if i == 3 { 0 } else { 1 });

        }
    }

    #[test]
    fn test_new_face(){
        let mut mesh = HalfEdgeMesh::new();
        let face = [mesh.new_vertex(), mesh.new_vertex(), mesh.new_vertex()];
        let face_id = mesh.new_face(&face);
        assert_eq!(mesh.vertex_count(), 3);
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.count_face_edges(), 3);
        assert_eq!(mesh.halfedges.len(), 6);
        assert_eq!(HalfEdgeId(KeyData::from_ffi(5)), mesh.goto(HalfEdgeId(KeyData::from_ffi(1))).previous().get_halfedge().unwrap());
        assert_eq!(HalfEdgeId(KeyData::from_ffi(4)), mesh.goto(HalfEdgeId(KeyData::from_ffi(2))).previous().get_halfedge().unwrap());
    }

    #[test]
    fn test_two_disjoint_faces(){
        let mut mesh = HalfEdgeMesh::new();
        let face = [mesh.new_vertex(), mesh.new_vertex(), mesh.new_vertex()];
        let face_id = mesh.new_face(&face);
        let face = [mesh.new_vertex(), mesh.new_vertex(), mesh.new_vertex()];
        let face_id = mesh.new_face(&face);
        assert_eq!(mesh.vertex_count(), 6);
        assert_eq!(mesh.count_islands(), 2);
        assert_eq!(mesh.count_face_edges(), 6);
        assert_eq!(mesh.halfedges.len(), 12);
    }

    #[test]
    fn test_two_attached_faces(){
        let mut mesh = HalfEdgeMesh::new();
        let face = [mesh.new_vertex(), mesh.new_vertex(), mesh.new_vertex()];
        let face_id = mesh.new_face(&face);
        let face = [face[1], face[0], mesh.new_vertex()];
        let face_id = mesh.new_face(&face);
        assert_eq!(mesh.vertex_count(), 4);
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.count_face_edges(), 6);
        assert_eq!(mesh.halfedges.len(), 6+4);
    }

    #[test]
    fn from_bevy_mesh() {
        let bevy_mesh = Cuboid::new(1.0, 1.0, 1.0).mesh().build();
        let mesh:HalfEdgeMesh = bevy_mesh.try_into().unwrap();
        // mesh.print_mesh();
        assert_eq!(mesh.faces.len(), 12);
        assert_eq!(mesh.vertices.len(), 24);
        assert_eq!(mesh.count_islands(), 6);
        assert_eq!(mesh.count_face_edges(), 36);
        assert_eq!(mesh.halfedges.len(), 36+4*6);
    }

    #[test]
    fn from_cuboid() {
        let mesh = Cuboid::new(1.0, 1.0, 1.0).procgen();
        assert_eq!(mesh.face_count(), 6);
        assert_eq!(mesh.vertex_count(), 8);
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.count_face_edges(), mesh.halfedges.len());
        assert_eq!(mesh.count_face_edges(), 24);
        mesh.print_mesh();
        let vertices:SmallVec<[_;8]> = mesh.vertices.keys().collect();
        let next_vertices:SmallVec<[_;8]> = vertices.iter().map(|v| (*v, mesh.goto(*v).next().get_vertex().unwrap())).collect();
        assert_eq!(next_vertices, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(1)), VertexId(KeyData::from_ffi(8))),
            (VertexId(KeyData::from_ffi(2)), VertexId(KeyData::from_ffi(7))),
            (VertexId(KeyData::from_ffi(3)), VertexId(KeyData::from_ffi(6))),
            (VertexId(KeyData::from_ffi(4)), VertexId(KeyData::from_ffi(5))),
            (VertexId(KeyData::from_ffi(5)), VertexId(KeyData::from_ffi(4))),
            (VertexId(KeyData::from_ffi(6)), VertexId(KeyData::from_ffi(3))),
            (VertexId(KeyData::from_ffi(7)), VertexId(KeyData::from_ffi(2))),
            (VertexId(KeyData::from_ffi(8)), VertexId(KeyData::from_ffi(1))),
            ]));
        let twin_vertices:SmallVec<[_;8]> = vertices.iter().map(|v| (*v, mesh.goto(*v).twin().get_vertex().unwrap())).collect();
        assert_eq!(twin_vertices, SmallVec::from_buf([
            (VertexId(KeyData::from_ffi(1)), VertexId(KeyData::from_ffi(8))),
            (VertexId(KeyData::from_ffi(2)), VertexId(KeyData::from_ffi(7))),
            (VertexId(KeyData::from_ffi(3)), VertexId(KeyData::from_ffi(6))),
            (VertexId(KeyData::from_ffi(4)), VertexId(KeyData::from_ffi(5))),
            (VertexId(KeyData::from_ffi(5)), VertexId(KeyData::from_ffi(4))),
            (VertexId(KeyData::from_ffi(6)), VertexId(KeyData::from_ffi(3))),
            (VertexId(KeyData::from_ffi(7)), VertexId(KeyData::from_ffi(2))),
            (VertexId(KeyData::from_ffi(8)), VertexId(KeyData::from_ffi(1))),
            ]));
        assert_eq!(twin_vertices, next_vertices);
    }

}
