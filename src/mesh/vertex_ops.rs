use itertools::Itertools;


use super::{edge_ops, face_ops, traversal::{TraversalError, TraversalResult}, FaceId, HalfEdgeMesh, StackVec, VertexId};

/// Removes a vertex, either making a mesh boundary, or filling it up with a new face
pub fn delete(mesh:&mut HalfEdgeMesh, vertex:VertexId, is_fill:bool) -> StackVec<Option<FaceId>> {
    let fan:StackVec<_> = mesh.goto(vertex).iter_outgoing().map(|t| t.get_halfedge().unwrap()).collect();
    let result = fan.iter().map(|&edge| edge_ops::delete(mesh, edge, is_fill)).collect();
    mesh.vertices.remove(vertex);
    result
}

/// Chamfers a vertex by stitching a face from mid-points of the vertex's outgoing edges. Returns newly-created FaceId
pub fn chamfer(mesh:&mut HalfEdgeMesh, vertex:VertexId, factor:f32) -> FaceId {
    // Find outgoing edges
    let outgoing:StackVec<_> = mesh.goto(vertex).iter_outgoing().map(|t| t.get_halfedge().unwrap()).collect();
    // Split each edge into two, returning new vertices as an array
    let new_vertices:StackVec<_> = outgoing.iter().map(|&target| edge_ops::split(mesh, target, factor)).collect();
    // Connect pairs of vertices together to form new edges
    let new_edges:StackVec<_> = new_vertices.iter().circular_tuple_windows().map(|(&start, &end)| face_ops::split(mesh, start, end)).collect();
    // Delete the original vertex, filling its hole with a face
    delete(mesh, vertex, true);
    mesh[mesh[new_edges[0]].twin].face.unwrap()
}

/// Delete this vertex only if it lies within two faces (has two outbound half-edges)
/// Merge the half-edges together in a continous line, reducing the edge count for contained faces.
pub fn dissolve(mesh:&mut HalfEdgeMesh, vertex:VertexId) -> TraversalResult<()>{
    let outgoing = mesh.goto(vertex).iter_outgoing().map(|e| (e.get_halfedge().unwrap(), e.twin().get_halfedge().unwrap(), e.get_face().unwrap())).collect::<StackVec<_>>();
    if outgoing.len() != 2 {
        return Err(TraversalError::new(vertex, super::traversal::TraversalErrorKind::UndefinedOperation))
    }
    // outgoing[n].0 is outgoing and outgoing[n].1 is incoming
    let direction = if mesh[outgoing[0].1].next == outgoing[1].0 { 0 } else { 1 };
    mesh[outgoing[direction].1].next = mesh[outgoing[(direction + 1) % 2].0].next; // incoming.next is now whatever outgoing.next was pointing to
    mesh[outgoing[direction].1].next = mesh[outgoing[(direction + 1) % 2].0].next;
    mesh[outgoing[(direction + 1) % 2].1].next = mesh[outgoing[direction].0].next;
    mesh[outgoing[(direction + 1) % 2].1].next = mesh[outgoing[direction].0].next;
    if let Some(face) = outgoing[direction].2 {
        mesh[face].halfedge = outgoing[(direction + 1) % 2].1;
    }
    if let Some(face) = outgoing[(direction + 1) % 2].2 {
        mesh[face].halfedge = outgoing[direction].1;
    }
    mesh.halfedges.remove(outgoing[0].0);
    mesh.halfedges.remove(outgoing[1].0);
    // TODO: Figure out how to clean vertex attributes in a nice way
    mesh.vertices.remove(vertex);

    Ok(())
}

#[cfg(test)]
mod tests {
    use bevy::prelude::Vec3;
    use slotmap::{KeyData, SecondaryMap};
    use smallvec::SmallVec;

    use crate::mesh::{attributes::AttributeKind, tests::sample_mesh, FaceId, VertexId};


    #[test]
    fn test_delete() {
        let mut mesh = sample_mesh();
        super::delete(&mut mesh, VertexId(KeyData::from_ffi(3)), true);
        // 0--1--4
        // |     |
        // 3     5
        // |     |
        // 6--7--8
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.count_face_edges(), 8);
        assert_eq!(mesh.face_count(), 1);
        assert_eq!(mesh.vertex_count(), 8);
    }

    #[test]
    fn test_chamfer() {
        let mut mesh = sample_mesh();
        let v = mesh.vertex_keys().collect::<SmallVec<[_;9]>>();
        // Pyramid shape with v[2] on top
        let positions = SecondaryMap::from_iter([
            (v[0], -Vec3::X-Vec3::Z), (v[3], -Vec3::Z+0.5*Vec3::Y), (v[5], Vec3::X-Vec3::Z),
            (v[1], -Vec3::X+0.5*Vec3::Y), (v[2], Vec3::Y), (v[4], Vec3::X+0.5*Vec3::Y),
            (v[6], -Vec3::X+Vec3::Z), (v[7], Vec3::Z+0.5*Vec3::Y), (v[8], Vec3::X+Vec3::Z),
        ]);
        mesh.add_attribute(AttributeKind::Positions, positions);
        let new_face = super::chamfer(&mut mesh, v[2], 0.25);
        assert!(!mesh.vertices.contains_key(v[2]));
        assert!(!mesh.faces.contains_key(FaceId(KeyData::from_ffi(6))));
        assert!(!mesh.faces.contains_key(FaceId(KeyData::from_ffi(7))));
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.count_face_edges(), 24);
        assert_eq!(new_face, FaceId(KeyData::from_ffi(8)));
        let positions = mesh.attribute(&AttributeKind::Positions).unwrap().as_vertices_vec3();
        for vertex in mesh.vertex_keys() {
            assert!(positions.contains_key(vertex))
        }
    }
}