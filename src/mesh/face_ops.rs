use itertools::Itertools;

use super::{Face, FaceId, HalfEdgeId, HalfEdgeMesh, StackVec, VertexId};

/// Removes a vertex, either making a mesh boundary, or filling it up with a new face
pub fn delete(mesh:&mut HalfEdgeMesh, face:FaceId) {
    let r#loop:StackVec<_> = mesh.goto(face).iter_loop().map(|t| t.get_halfedge().unwrap()).collect();
    for edge in r#loop {
        mesh[edge].face = None
    }
    mesh.faces.remove(face);
}

/// Splits a face along two of its points that do not share an edge. 
/// old FaceId will be kept on the side along `v -> w` direction, and new face will be created in `w -> v` direction
/// Returned new HalfEdgeId will be the newly created edge `v -> w` 
pub fn split(mesh:&mut HalfEdgeMesh, v:VertexId, w:VertexId) -> HalfEdgeId {
    let v = mesh.goto(v);
    v.halfedge_to(w).get_halfedge().expect_err("Vertices provided to split a face already share an edge");
    //twin_vertex(w) /twin_previous
    //  __________./  twin_next
    //  edge_next  \twin /
    //          edge\   /
    //                . - edge_vertex(v)
    //              /   \
    //edge_previous/     \
    let twin_next = v.adjacent_faces().find(|p| p.iter_loop().contains(w)).expect("Vertices provided to split a face must share a face.");
    let face = twin_next.get_face().unwrap().expect("Vertices provided to split a face are boundary and don't have a face.");
    let edge_previous = twin_next.previous().get_halfedge().unwrap();
    let edge_next = twin_next.iter_loop().find(|t| t.get_vertex().unwrap() == w).unwrap();
    let edge_next = edge_next.get_halfedge().unwrap();
    let v = v.get_vertex().unwrap();
    let (edge, mut twin) = mesh.new_edge(edge_previous.into(), edge_next.into());

    let face_id = mesh.faces.insert(Face { halfedge: twin });

    // mesh[edge_previous].next = edge;
    // mesh[twin_previous].next = twin;
    mesh[twin].face = Some(face_id);
    mesh[edge].face = Some(face);
    mesh[face].halfedge = edge;
    twin = mesh[twin].next;
    while mesh[twin].twin != edge { // iterate without using `Traversal` not to have to allocate/deallocate a vector
        mesh[twin].face = Some(face_id);
        twin = mesh[twin].next;
    }
    edge
}

pub fn extrude(mesh:&mut HalfEdgeMesh, face:FaceId, length:f32) -> StackVec<FaceId> {
    let shift = length*mesh.face_normal(face);
    let face_edges = mesh.goto(face).iter_loop().map(|e| (e.get_halfedge().unwrap(), e.get_vertex().unwrap())).collect::<StackVec<_>>();
    let new_verts = (0..face_edges.len()).map(|_| mesh.new_vertex()).collect::<StackVec<_>>();
    let v = new_verts[0]; // Save a new vertex to find its boundary edge later
    let new_faces = face_edges.into_iter().zip(new_verts).circular_tuple_windows().map(|(((edge, v1), v2), ((_, v3), v4))| {
        mesh[edge].face = None;
        // We don't have remove us from next edge ahead because mesh.new_face doesn't modify or depend on twin edges
        let positions = mesh.attributes.get_mut(&super::attributes::AttributeKind::Positions).unwrap().as_vertices_vec3_mut();
        positions.insert(v2, positions[v1]+shift);
        mesh.new_face(&[v3, v4, v2, v1])
    }).collect::<StackVec<_>>();
    let old_face_goes_here = mesh.goto(v).iter_outgoing().find(|e| e.is_boundary()).unwrap().iter_loop().map(|e| e.get_halfedge().unwrap()).collect::<StackVec<_>>();
    mesh[face].halfedge = old_face_goes_here[0];
    for edge in old_face_goes_here {
        mesh[edge].face = Some(face);
    }
    new_faces
    
}




#[cfg(test)]
mod tests {
    use bevy::math::Vec3;
    use slotmap::{KeyData, SecondaryMap};
    use smallvec::SmallVec;

    use crate::mesh::{attributes::AttributeKind, tests::sample_mesh, vertex_ops, FaceId, VertexId};


    #[test]
    fn test_delete() {
        let mut mesh = sample_mesh();
        super::delete(&mut mesh, FaceId(KeyData::from_ffi(1)));
        assert!(!mesh.faces.contains_key(FaceId(KeyData::from_ffi(1))));
        assert_eq!(mesh.count_face_edges(), 12);
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.vertex_degree(VertexId(KeyData::from_ffi(3))), 4);
        assert_eq!(mesh.goto(VertexId(KeyData::from_ffi(3))).adjacent_faces().count(), 3);
        assert_eq!(mesh.goto(VertexId(KeyData::from_ffi(1))).adjacent_faces().count(), 0);
    }

    #[test]
    fn test_split() {
        let mut mesh = sample_mesh();
        let new_edge = super::split(&mut mesh, VertexId(KeyData::from_ffi(1)), VertexId(KeyData::from_ffi(3)));
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.count_face_edges(), 18);
        assert_eq!(mesh.face_count(), 5);
        assert_eq!(mesh.vertex_degree(VertexId(KeyData::from_ffi(3))), 5);
        assert_eq!(mesh[new_edge].face, Some(FaceId(KeyData::from_ffi(1))));
        assert_eq!(mesh[mesh[new_edge].twin].face, Some(FaceId(KeyData::from_ffi(5))));
        assert_eq!(mesh.goto(FaceId(KeyData::from_ffi(1))).iter_loop().count(), 3);
        assert_eq!(mesh.goto(FaceId(KeyData::from_ffi(5))).iter_loop().count(), 3);
        assert_eq!(mesh.goto(FaceId(KeyData::from_ffi(2))).iter_loop().count(), 4);
    }

    #[test]
    fn test_extrude() {
        let mut mesh = sample_mesh();
        let v = mesh.vertex_keys().collect::<SmallVec<[_;9]>>();
        // Pyramid shape with v[2] on top
        let positions = SecondaryMap::from_iter([
            (v[0], -Vec3::X-Vec3::Z), (v[3], -Vec3::Z+0.5*Vec3::Y), (v[5], Vec3::X-Vec3::Z),
            (v[1], -Vec3::X+0.5*Vec3::Y), (v[2], Vec3::Y), (v[4], Vec3::X+0.5*Vec3::Y),
            (v[6], -Vec3::X+Vec3::Z), (v[7], Vec3::Z+0.5*Vec3::Y), (v[8], Vec3::X+Vec3::Z),
        ]);
        mesh.add_attribute(AttributeKind::Positions, positions);
        let face = vertex_ops::chamfer(&mut mesh, v[2], 0.25);
        super::extrude(&mut mesh, face, 0.5);
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.count_face_edges(), 40);
        assert_eq!(mesh.face_count(), 9);
    }
}
