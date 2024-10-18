

use attributes::{AttributeKind, AttributeStore, AttributeValues, Attributes, SelectionQueries, TraversalQueries};
use bevy::{prelude::{default, Vec2, Vec3}, render::{mesh::{self, Mesh as BevyMesh}, render_asset::RenderAssetUsages}, utils::hashbrown::HashSet};
use itertools::Itertools;
use selection::Selection;
use slotmap::{KeyData, SecondaryMap, SlotMap};
use smallvec::SmallVec;
use traversal::{Traversal, VertexFlow};

pub mod attributes;
pub(crate) mod traversal;
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

/// Trait that lets us convert some value to a specific HalfEdgeId
/// Useful to let rays target exact part of the mesh 
pub trait Targettable {
    /// Convert self to a [`HalfEdgeId`]. For example if a ray 
    /// points to this mesh this method is how you could aim at
    /// specific parts of a mesh
    fn get_mesh_halfedge(self, mesh:&HalfEdgeMesh) -> HalfEdgeId;
    fn get_mesh_position(self, mesh:&HalfEdgeMesh) -> MeshPosition;
}

slotmap::new_key_type! { 
    /// Index of a given [`HalfEdge`] in the staging slotmap
    /// [`HalfEdgeMesh`] assumes [`HalfEdgeId::default()`] is equivalent to a NULL pointer
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

impl HalfEdgeId {
    /// Lets the crate create custom Id. Only used for unit-tests
    pub(crate) fn from_ffi(ffi:u64) -> Self {
        Self(KeyData::from_ffi(ffi))
    }
}

impl VertexId {
    /// Lets the crate create custom Id. Only used for unit-tests
    pub(crate) fn from_ffi(ffi:u64) -> Self {
        Self(KeyData::from_ffi(ffi))
    }
}

impl FaceId {
    /// Lets the crate create custom Id. Only used for unit-tests
    pub(crate) fn from_ffi(ffi:u64) -> Self {
        Self(KeyData::from_ffi(ffi))
    }
}

impl Targettable for HalfEdgeId {
    #[inline]
    fn get_mesh_position(self, _:&HalfEdgeMesh) -> MeshPosition {
        self.into()
    }

    fn get_mesh_halfedge(self, _:&HalfEdgeMesh) -> HalfEdgeId {
        self
    }
}

impl Targettable for VertexId {
    #[inline]
    fn get_mesh_position(self, mesh:&HalfEdgeMesh) -> MeshPosition {
        self.into()
    }

    fn get_mesh_halfedge(self, mesh:&HalfEdgeMesh) -> HalfEdgeId {
        mesh[self].halfedge
    }
}

impl Targettable for FaceId {
    #[inline]
    fn get_mesh_position(self, mesh:&HalfEdgeMesh) -> MeshPosition {
        self.into()
    }
    fn get_mesh_halfedge(self, mesh:&HalfEdgeMesh) -> HalfEdgeId {
        mesh[self].halfedge
    }
}

impl Targettable for Vec3 {
    fn get_mesh_position(self, mesh:&HalfEdgeMesh) -> MeshPosition {
        if let Some(attr) = mesh.attribute(&AttributeKind::Positions) {
            // TODO: Allow aiming at edges and faces too
            let (vertex, _) = attr.as_vertices_vec3().iter().fold((VertexId::default(), f32::MAX), |acc, p| {
                    let distance = (*p.1 - self).length();
                        if distance < acc.1 { (p.0, distance) } else { acc }
                });
                vertex.into()
        } else {
            MeshPosition::HalfEdge(default())
        }
    }

    fn get_mesh_halfedge(self, mesh:&HalfEdgeMesh) -> HalfEdgeId {
        if let Some(attr) = mesh.attribute(&AttributeKind::Positions) {
            let (vertex, _) = attr.as_vertices_vec3().iter().fold((VertexId::default(), f32::MAX), |acc, p| {
                    let distance = (*p.1 - self).length();
                        if distance < acc.1 { (p.0, distance) } else { acc }
                });
                mesh[vertex].halfedge
        } else {
            default()
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
    fn get_mesh_halfedge(self, mesh:&HalfEdgeMesh) -> HalfEdgeId {
        match self {
            MeshPosition::Vertex(vertex_id) => mesh[vertex_id].halfedge,
            MeshPosition::HalfEdge(half_edge_id) => half_edge_id,
            MeshPosition::Face(face_id) => mesh[face_id].halfedge,
        }
    }
    fn get_mesh_position(self, mesh:&HalfEdgeMesh) -> MeshPosition {
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


#[derive(Default, Clone, Copy)]
/// A common way to represent a polygon mesh is a shared list of vertices and a list of faces storing pointers for its vertices. 
/// This representation is both convenient and efficient for many purposes, however in some domains it proves ineffective.
/// Write up about Half-Edge Data structure can be found [here](https://www.flipcode.com/archives/The_Half-Edge_Data_Structure.shtml)
/// And [Here](https://jerryyin.info/geometry-processing-algorithms/half-edge/)1
pub struct HalfEdge {
    pub twin: HalfEdgeId, 
    pub next: HalfEdgeId,
    pub vertex: VertexId,
    pub face: Option<FaceId>,
}

impl std::fmt::Debug for HalfEdge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output = f.debug_struct("");
        if self.twin != default() {
            output.field("twin", &self.twin);
        }
        if self.next != default() {
            output.field("next", &self.next);
        }
        output.field("vertex", &self.vertex);
        if self.face.is_some() {
            output.field("face", &self.face.unwrap());
        }
        output.finish()
    }
}


#[derive(Debug, Clone, Copy, Default)]
/// Vertex stored a pointer to exactly one of the half-edges which uses the vertex as its starting point. 
/// At any given vertex there will be more than one half-edge we could choose for this, 
/// but we only need one and it doesn't matter which one it is.
pub struct Vertex {
    pub halfedge: HalfEdgeId,
}

#[derive(Debug, Clone, Copy, Default)]
/// The half-edge pointer in the face is similar to the pointer in the vertex structure in that although 
/// there are multiple half-edges bordering each face, we only need to store one of them, and it doesn't matter which one.
pub struct Face {
    pub halfedge: HalfEdgeId,
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
    is_smooth: bool,
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
            is_smooth: true,
        }
    }

    #[inline]
    /// Initiate traversal of the mesh starting at a given edge
    pub fn goto(&self, pos:impl Targettable) -> Traversal<'_> {
        Traversal::new(self, pos.get_mesh_halfedge(self))
    }

    #[inline]
    /// Go to a position and select it, allowing you to select more items to later act on as a group
    pub fn select(&self, pos: impl Targettable + Copy) -> Selection<'_> {
        let position = pos.get_mesh_position(self);
        Selection::new(self, position)
    }

    #[inline]
    pub fn new_vertex(&mut self) -> VertexId {
        self.vertices.insert(default())
    }

    #[inline]
    pub fn new_edge(&mut self, edge:HalfEdge, mut twin:HalfEdge) -> (HalfEdgeId, HalfEdgeId) {
        let edge = self.halfedges.insert(edge);
        twin.twin = edge;
        let twin = self.halfedges.insert(twin);
        self[edge].twin = twin;
        (edge, twin)
    }

    /// Connect the mesh such that `edge_previous` will point to newly created edge and newly created `edge.next` will point to `edge_next`
    /// At the same time, create a twin such that whatever `edge_previous.next` was pointing to will end up being `twin.next` and the previous of `edge_next`
    /// will become `twin.previous` pointing to the newly created twin.
    /// `face` argument is used to constrain the flow of edges to a face (or boundary edges if face is None)
    /// ```text
    ///twin_vertex(w) /twin_previous
    ///  __________./    twin_next
    ///  edge_next  \twin /
    ///          edge\   /
    ///                . - edge_vertex(v)
    ///              /   \
    ///edge_previous/     \
    /// ```
    pub fn attach_edge(&mut self, edge_previous:HalfEdgeId, edge_next:HalfEdgeId) -> (HalfEdgeId, HalfEdgeId) {
        let edge_next = self.goto(edge_next);
        let edge_previous = self.goto(edge_previous);
        let twin_next = edge_previous.next_or_twin(); // twins will be used when both are boundary edges and we're just constructing a face
        let twin_previous = *edge_next.previous_or_twin();
        let twin_vertex = edge_next.vertex();
        let edge_vertex = twin_next.vertex();
        let edge_face = edge_previous.face();
        let twin_face = twin_next.face();

        let edge_next = *edge_next;
        let edge_previous = *edge_previous;
        let twin_next = *twin_next;
        let (edge, twin) = self.new_edge(
            HalfEdge {next: edge_next, vertex: edge_vertex, face: edge_face, ..default() }, 
            HalfEdge {next: twin_next, vertex: twin_vertex, face: twin_face, ..default() }
        );
        if let Some(edge_previous) = self.halfedges.get_mut(edge_previous) {
            edge_previous.next = edge;
        }
        if let Some(twin_previous) = self.halfedges.get_mut(twin_previous) {
            twin_previous.next = twin;
        }
        self[edge_vertex].halfedge = edge;
        self[twin_vertex].halfedge = twin;
        (edge, twin)
    }
    

    /// Create a new face and attach it to correct twin edges.
    /// This method does not modify twin edges if they already exist
    pub fn new_face(&mut self, face:&[VertexId]) -> FaceId {
        let face_id = self.faces.insert(Face { halfedge: default() });
        let mut face_edges:StackVec<HalfEdgeId> = StackVec::new();
        let mut start_flows:StackVec<VertexFlow> = StackVec::new();
        let mut end_flows:StackVec<VertexFlow> = StackVec::new();
        for (&start, &end) in face.iter().circular_tuple_windows() {
            start_flows.clear();
            end_flows.clear();
            if self.halfedges.contains_key(self[start].halfedge) {
                start_flows.extend(self.goto(start).get_flow(None));
            };
            if self.halfedges.contains_key(self[end].halfedge) {
                end_flows.extend(self.goto(end).get_flow(None));
            };
            let mut selected_start_flow:Option<VertexFlow> = None; 
            let mut selected_end_flow:Option<VertexFlow> = None;
            let mut is_existing_edge = false;
            // cross-compare possible edge flows and prefer already-connected edges
            // or edges that we previously created for this face
            for start_flow in &start_flows {
                for end_flow in &end_flows {
                    if start_flow.outgoing == end_flow.incoming && self.halfedges.contains_key(start_flow.outgoing) {
                        is_existing_edge = true;
                        face_edges.push(start_flow.outgoing);
                        break;
                    }
                    if end_flow.outgoing == start_flow.incoming && self.halfedges.contains_key(end_flow.outgoing) {
                    }
                    if let Some(&first_face_edge) = face_edges.first() {
                        if end_flow.outgoing == first_face_edge {
                            selected_end_flow = Some(*end_flow);
                        }
                    }
                }
                if let Some(&last_face_edge) = face_edges.last() {
                    if start_flow.incoming == last_face_edge {
                        selected_start_flow = Some(*start_flow);
                    }
                }
                if is_existing_edge {
                    break;
                }
            }
            if is_existing_edge {
                // face_edges.push already happened above when is_existing_edge was toggled.
                continue;
            } else {
                if selected_start_flow.is_none() && !start_flows.is_empty() {
                    selected_start_flow = Some(start_flows[0]);
                }
                if selected_end_flow.is_none() && !end_flows.is_empty() {
                    selected_end_flow = Some(end_flows[0]);
                }
                let edge = match (selected_start_flow, selected_end_flow) {
                    (Some(start_flow), Some(end_flow)) => {
                        let (edge, _) = self.attach_edge(start_flow.incoming, end_flow.outgoing);
                        edge
                    },
                    (Some(start_flow), None) => { // == * == *
                        let (edge, twin) = self.new_edge(
                            HalfEdge {vertex:start, face:self[start_flow.incoming].face, ..default()}, 
                            HalfEdge {next:start_flow.outgoing, vertex:end, face:self[start_flow.outgoing].face, ..default()}
                        );
                        #[cfg(test)]
                        assert_eq!(self[start_flow.incoming].face, None);
                        self[start_flow.incoming].next = edge;
                        self[end].halfedge = twin;
                        edge
                    },
                    (None, Some(end_flow)) => { // * == * ==
                        let (edge, twin) = self.new_edge(
                            HalfEdge{next:end_flow.outgoing, vertex:start, face:self[end_flow.outgoing].face, ..default()},
                            HalfEdge{vertex:end, face:self[end_flow.incoming].face, ..default()}
                        );
                        self[end_flow.incoming].next = twin;
                        self[start].halfedge = edge;
                        edge
                    },
                    (None, None) => {
                        if self.halfedges.contains_key(self[start].halfedge) && self[self[start].halfedge].vertex == start || self.halfedges.contains_key(self[end].halfedge) && self[self[end].halfedge].vertex == end {
                            panic!("No boundary edges found but vertices {start:?} or {end:?} contain non-boundary edges. You're trying to create a non-manifold. Unable to continue.");
                        }
                        let (edge, twin) = self.new_edge(HalfEdge{vertex:start,..default()}, HalfEdge{vertex:end,..default()});
                        self[start].halfedge = edge;
                        self[end].halfedge = twin;
                        edge
                    }
                };
                face_edges.push(edge);
            }
        }

        self[face_id].halfedge = face_edges[0];
        for edge in face_edges {
            self[edge].face = Some(face_id);
        }
        face_id
    }

    /// How many faces are currently allocated
    pub fn face_count(&self) -> usize {
        self.faces.len()
    }

    /// How many vertices are currently allocated
    pub fn vertex_count(&self) -> usize {
        self.vertices.len()
    }

    /// How many halfedges are currently allocated
    pub fn halfedge_count(&self) -> usize {
        self.halfedges.len()
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

    pub fn set_smooth(&mut self, is_smooth:bool) {
        self.is_smooth = is_smooth
    }

    /// Get an attribute
    pub fn attribute(&self, kind:&AttributeKind) -> Option<&AttributeValues> {
        self.attributes.get(kind)
    }

    /// Get an attribute
    pub fn attribute_mut(&mut self, kind:&AttributeKind) -> Option<&mut AttributeValues> {
        self.attributes.get_mut(kind)
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
            if set.remove(&init) {
                for visited in mesh.goto(init).iter_incoming().map(|t| t.vertex()) {
                    remove_visited(set, mesh, visited);
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

    pub(crate) fn print_mesh(&self) {
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

    pub fn join(&mut self, other:&HalfEdgeMesh) {
        let mut edge_map:SecondaryMap<HalfEdgeId, HalfEdgeId> = SecondaryMap::new();
        let mut face_map:SecondaryMap<FaceId, FaceId> = SecondaryMap::new();
        let mut vertex_map:SecondaryMap<VertexId, VertexId> = SecondaryMap::new();

        for face_id in other.face_keys() {
            face_map.insert(face_id, self.faces.insert(default()));
        }
        for vertex_id in other.vertex_keys() {
            vertex_map.insert(vertex_id, self.vertices.insert(default()));
        }
        for edge_id in other.edge_keys() {
            let mut halfedge = other.halfedges[edge_id];
            halfedge.vertex = vertex_map[halfedge.vertex];
            halfedge.face.iter_mut().for_each(|f| *f = face_map[*f]);
            let new_id = self.halfedges.insert(halfedge);
            self.vertices[halfedge.vertex].halfedge = new_id;
            halfedge.face.and_then(|f| Some(self.faces[f].halfedge = new_id));
            edge_map.insert(edge_id, new_id);
        }
        for edge_id in other.edge_keys() {
            let new_id = edge_map[edge_id];
            self[new_id].next = edge_map[self[new_id].next];
            self[new_id].twin = edge_map[self[new_id].twin];
        }
        for (kind, values) in &other.attributes {
            let store = self.attributes.entry(*kind).or_insert_with(|| match values {
                AttributeValues::VertexU32(_) => AttributeValues::VertexU32(SecondaryMap::new()),
                AttributeValues::VertexVec3(_) => AttributeValues::VertexVec3(SecondaryMap::new()),
                AttributeValues::VertexBool(_) => AttributeValues::VertexBool(SecondaryMap::new()),
                AttributeValues::EdgeVec2(_) => AttributeValues::EdgeVec2(SecondaryMap::new()),
                AttributeValues::EdgeVec3(_) => AttributeValues::EdgeVec3(SecondaryMap::new()),
                AttributeValues::EdgeBool(_) => AttributeValues::EdgeBool(SecondaryMap::new())
            });
            match values {
                AttributeValues::VertexU32(secondary_map) => for (old_key, &value) in secondary_map {
                    store.as_vertices_u32_mut().insert(vertex_map[old_key], value);
                },
                AttributeValues::VertexVec3(secondary_map) => for (old_key, &value) in secondary_map {
                    store.as_vertices_vec3_mut().insert(vertex_map[old_key], value);
                },
                AttributeValues::VertexBool(secondary_map) => for (old_key, &value) in secondary_map {
                    store.as_vertices_bool_mut().insert(vertex_map[old_key], value);
                },
                AttributeValues::EdgeVec2(secondary_map) => for (old_key, &value) in secondary_map {
                    store.as_edge_vec2_mut().insert(edge_map[old_key], value);
                },
                AttributeValues::EdgeVec3(secondary_map) => for (old_key, &value) in secondary_map {
                    store.as_edge_vec3_mut().insert(edge_map[old_key], value);
                },
                AttributeValues::EdgeBool(secondary_map) => for (old_key, &value) in secondary_map {
                    store.as_edge_bool_mut().insert(edge_map[old_key], value);
                }
            }
        }
        if self.is_smooth != other.is_smooth {
            todo!("Can't join two meshes with different smoothing settings.")
        }
    }
}


impl TryFrom<BevyMesh> for HalfEdgeMesh {
    type Error = ();
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
        let mut vertex_index_map:SecondaryMap<VertexId, u32> = SecondaryMap::new();
        // TODO: Check if traversing adjacent faces first is better.
        // Down side - checking adjacency now is slower on CPU
        // But having indices with better locality might be better on GPU
        for face in mesh.faces.keys() { 
            let face_vertices:SmallVec<[_;6]> = mesh.goto(face).iter_loop().map(|f| f.vertex()).collect();

            let is_even = (face_vertices.len() as i32 % 2) == 0;
            let mut start_index = 0;
            let mut end_index = face_vertices.len() - 1;
            // Triangulate theese polygons. TODO: Figure out better way. Most crates I found were for 2D
            while end_index > 0  {
                let triangle = [face_vertices[start_index], face_vertices[start_index+1], face_vertices[end_index]];
                for vertex in triangle {
                    if let Some(output_index) = vertex_index_map.get(vertex) {
                        if mesh.goto(vertex).is_smooth_normals() {
                            indices.push(*output_index);
                        } else {
                            let index = positions.len() as u32;
                            indices.push(index);
                            positions.push(mesh.goto(vertex).position().to_array());
                            let n = mesh.goto(face).calculate_normal().unwrap();
                            normals.push(n.to_array());
                            uvs.push(Vec2::ZERO);
                        }
                    } else {
                        let index = positions.len() as u32;
                        indices.push(index);
                        vertex_index_map.insert(vertex, index);
                        let v = mesh.goto(vertex);
                        let pos = v.position();
                        positions.push(pos.to_array());
                        if mesh.goto(vertex).is_smooth_normals() {
                            normals.push(v.select_vertex().to_face_selection().calculate_normal().unwrap().to_array());
                        } else {
                            let n = mesh.goto(face).calculate_normal().unwrap();

                            normals.push(n.to_array());
                        }
                        uvs.push(Vec2::ZERO);
                    }
                }
                end_index -= 1;
                start_index += 1;
                if start_index + if is_even { 1 } else { 0 } == end_index {
                    start_index += 1;
                    end_index -= 1;
                }
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
    use bevy::{math::Vec3, prelude::{Cuboid, MeshBuilder, Meshable}};
    use slotmap::{KeyData, SecondaryMap};
    use smallvec::SmallVec;

    use crate::{mesh::{FaceId, HalfEdgeMesh, VertexId}};

    use super::{attributes::AttributeKind, HalfEdgeId};


    #[test]
    fn test_new_face(){
        let mut mesh = HalfEdgeMesh::new();
        let face = [mesh.new_vertex(), mesh.new_vertex(), mesh.new_vertex()];
        let face_id = mesh.new_face(&face);
        assert_eq!(mesh.vertex_count(), 3);
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.count_face_edges(), 3);
        assert_eq!(mesh.halfedges.len(), 6);
        assert_eq!(HalfEdgeId(KeyData::from_ffi(5)), *mesh.goto(HalfEdgeId(KeyData::from_ffi(1))).previous());
        assert_eq!(HalfEdgeId(KeyData::from_ffi(4)), *mesh.goto(HalfEdgeId(KeyData::from_ffi(2))).previous());
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
        assert_eq!(mesh.faces.len(), 12);
        assert_eq!(mesh.vertices.len(), 24);
        assert_eq!(mesh.count_islands(), 6);
        assert_eq!(mesh.count_face_edges(), 36);
        assert_eq!(mesh.halfedges.len(), 36+4*6);
    }


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
        let v:SmallVec<[_;9]> = (0..9).map(|_| mesh.new_vertex()).collect();
        mesh.new_face(&[v[0], v[1], v[2], v[3]]);
        mesh.new_face(&[v[3], v[2], v[4], v[5]]);
        mesh.new_face(&[v[1], v[6], v[7], v[2]]);
        mesh.new_face(&[v[2], v[7], v[8], v[4]]);
        let positions = SecondaryMap::from_iter([
            (v[0], -Vec3::X-Vec3::Z), (v[3], -Vec3::Z+0.5*Vec3::Y), (v[5], Vec3::X-Vec3::Z),
            (v[1], -Vec3::X+0.5*Vec3::Y), (v[2], Vec3::Y), (v[4], Vec3::X+0.5*Vec3::Y),
            (v[6], -Vec3::X+Vec3::Z), (v[7], Vec3::Z+0.5*Vec3::Y), (v[8], Vec3::X+Vec3::Z),
        ]);
        mesh.add_attribute(AttributeKind::Positions, positions);
        mesh
    }

    #[test]
    fn test_sample_mesh() {
        let mesh = sample_mesh();
        assert_eq!(mesh.count_face_edges(), 16);
        assert_eq!(mesh.face_count(), 4);
        assert_eq!(mesh.vertex_count(), 9);
        assert_eq!(mesh.count_islands(), 1);
        for i in 1..(mesh.face_count()+1) {
            assert_eq!(mesh[mesh[FaceId(KeyData::from_ffi(i as u64))].halfedge].face, Some(FaceId(KeyData::from_ffi(i as u64))));
        }
        for i in 1..(mesh.vertex_count()+1) {
            assert_eq!(mesh.vertex_degree(VertexId(KeyData::from_ffi(i as u64))), mesh.goto(VertexId(KeyData::from_ffi(i as u64))).adjacent_faces().count() + if i == 3 { 0 } else { 1 });
        }
    }

}
