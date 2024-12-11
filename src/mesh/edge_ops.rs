use itertools::Itertools;
use smallvec::SmallVec;

use crate::mesh::vertex_ops;

use super::{
    attributes::{AttributeKind, TraversalQueries},
    face_ops, FaceId, HalfEdge, HalfEdgeId, HalfEdgeMesh, StackVec, VertexId,
};

/// Remove an edge, either making a mesh boundary
pub fn delete(mesh: &mut HalfEdgeMesh, target: HalfEdgeId, is_fill: bool) -> Option<FaceId> {
    // twin_vertex  /twin_previous
    //  __________./  twin_next
    //  edge_next  \twin /
    //          edge\   /
    //                . - edge_vertex
    //              /   \
    //edge_previous/     \
    let edge = mesh.goto(target);
    let edge_next = edge.next().halfedge();
    let edge_previous = edge.previous().halfedge();
    let edge_face = edge.face();
    let edge_vertex = edge.vertex();

    let twin = edge.twin();
    let twin_next = twin.next().halfedge();
    let twin_previous = twin.previous().halfedge();
    let twin_face = twin.face();
    let twin_vertex = twin.vertex();
    let twin = twin.halfedge();

    // Reconnect the mesh to bypass removed edge+twin
    mesh[twin_previous].next = edge_next;
    mesh[edge_previous].next = twin_next;
    mesh[edge_vertex].halfedge = twin_next; // make sure vertices don't point at deleted edge
    mesh[twin_vertex].halfedge = edge_next;
    mesh.halfedges.remove(target);
    mesh.halfedges.remove(twin);

    let face_edges: StackVec<_> = mesh.goto(edge_next).iter_loop().map(|t| t.halfedge()).collect();
    for edge in face_edges {
        mesh[edge].face = if is_fill { edge_face } else { None }
    }

    if twin_face != edge_face {
        if let Some(face) = twin_face {
            mesh.faces.remove(face);
        }

        if let Some(face) = edge_face {
            if !is_fill {
                mesh.faces.remove(face);
            } else {
                mesh[face].halfedge = edge_next;
            }
        }
    }

    twin_face
}

/// Split an edge and its twin, creating new vertex in-between
/// Original edge is trimmed short, and new edge is inserted after edge, with `factor` specifying
/// percent of the original length for the edge to keep.
///
/// Example: factor of 0.75 will make original edge being 0.75*edge.length() and insert
/// new edge of length 0.25*edge.length()
pub fn split(mesh: &mut HalfEdgeMesh, target: HalfEdgeId, factor: f32) -> VertexId {
    // twin_vertex  /twin_previous
    //  __________./  twin_next
    //  edge_next  \twin /
    //          edge\   /
    //                . - edge_vertex
    //              /   \
    //edge_previous/     \
    let edge = mesh.goto(target);
    let edge_next = edge.next().halfedge();
    let edge_face = edge.face();
    let start_pos = edge.position();
    let twin = edge.twin();
    let twin_previous = twin.previous().halfedge();
    let twin_face = twin.face();
    let twin_vertex = twin.vertex();
    let end_pos = twin.position();
    let twin = twin.halfedge();

    let new_vertex_pos = start_pos.lerp(end_pos, factor);
    let new_vertex = mesh.new_vertex();
    mesh.attributes
        .get_mut(&AttributeKind::Positions)
        .unwrap()
        .as_vertices_vec3_mut()
        .insert(new_vertex, new_vertex_pos);
    let new_edge = mesh.halfedges.insert(HalfEdge {
        twin: HalfEdgeId::default(),
        next: edge_next,
        vertex: new_vertex,
        face: edge_face,
    });
    let new_twin = mesh.halfedges.insert(HalfEdge {
        twin: new_edge,
        next: twin,
        vertex: twin_vertex,
        face: twin_face,
    });
    mesh[new_edge].twin = new_twin;
    // edge_previous -> edge -> (new_vertex) -> new_edge -> edge_next
    // twin_previous -> (twin_vertex) -> new_twin -> (new_vertex) -> twin -> twin_next
    mesh[target].next = new_edge;

    mesh[twin_previous].next = new_twin;
    mesh[twin].vertex = new_vertex;
    mesh[new_vertex].halfedge = new_edge;
    mesh[twin_vertex].halfedge = new_twin;
    new_vertex
}

pub fn chamfer(mesh: &mut HalfEdgeMesh, target: HalfEdgeId, factor: f32) -> FaceId {
    let start_vertex = mesh.goto(target).vertex();
    let end_vertex = mesh.goto(target).twin().vertex();
    let twin = mesh.goto(target).twin().halfedge();

    let start_outgoing_edges = mesh
        .goto(target)
        .iter_outgoing()
        .skip(1)
        .map(|e| e.halfedge())
        .collect::<StackVec<_>>();
    let end_outgoing_edges = mesh
        .goto(twin)
        .twin()
        .next()
        .iter_outgoing()
        .filter(|&e| e.halfedge() != twin)
        .map(|e| e.halfedge())
        .collect::<StackVec<_>>();
    // We need to cut all start and end outgoing edges except the target/twin and then join all new vertex pairs.
    let mut cut_vertices = start_outgoing_edges
        .iter()
        .chain(end_outgoing_edges.iter())
        .copied()
        .collect::<SmallVec<[_; 16]>>()
        .iter()
        .map(|e| split(mesh, *e, factor))
        .collect::<SmallVec<[_; 16]>>();
    cut_vertices.reverse();
    println!("start_outgoing: {start_outgoing_edges:?} end_outgoing: {end_outgoing_edges:?}");
    cut_vertices
        .iter()
        .circular_tuple_windows()
        .map(|(&start, &end)| face_ops::split(mesh, start, end))
        .count();
    // panic!("Aaaah!");
    vertex_ops::delete(mesh, start_vertex, false);
    vertex_ops::delete(mesh, end_vertex, false);
    let edge_face_sides = start_outgoing_edges.len();
    mesh.new_face(&cut_vertices[..edge_face_sides]);
    mesh.new_face(&cut_vertices[edge_face_sides..]);
    let last_face = [
        cut_vertices[0],
        cut_vertices[edge_face_sides - 1],
        cut_vertices[edge_face_sides],
        cut_vertices[cut_vertices.len() - 1],
    ];
    println!("Last_face: {last_face:?}");
    mesh.new_face(&last_face)
}

#[cfg(test)]
mod tests {
    use smallvec::SmallVec;

    use crate::mesh::HalfEdgeMesh;

    #[test]
    fn test_delete() {
        let mut mesh = HalfEdgeMesh::new();
        let vertices: SmallVec<[_; 9]> = (0..6).map(|_| mesh.new_vertex()).collect();
        // 0--1--4
        // |  |  |
        // 3--2--5
        // to_be_deleted is the first face (left on diagram above) because 2-1 edge is going up, and in CCW order that's right face.
        let to_be_deleted = mesh.new_face(&[vertices[0], vertices[1], vertices[2], vertices[3]]);
        mesh.new_face(&[vertices[1], vertices[4], vertices[5], vertices[2]]);

        let target = mesh.goto(vertices[2]).halfedge_to(vertices[1]).halfedge();
        let deleted_face = super::delete(&mut mesh, target, true);
        assert_eq!(mesh.count_face_edges(), 6);
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.face_count(), 1);
        assert_eq!(mesh.vertex_count(), 6);
        assert_eq!(deleted_face, Some(to_be_deleted));
        assert!(!mesh.halfedges.contains_key(target));
    }
}
