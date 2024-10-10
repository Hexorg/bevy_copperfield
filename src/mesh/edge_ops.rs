use bevy::prelude::default;
use itertools::Itertools;
use smallvec::SmallVec;


use crate::mesh::vertex_ops;

use super::{attributes::AttributeKind, FaceId, face_ops, HalfEdge, HalfEdgeId, HalfEdgeMesh, StackVec, VertexId};

/// Remove an edge, either making a mesh boundary
pub fn delete(mesh:&mut HalfEdgeMesh, target:HalfEdgeId, is_fill:bool) -> Option<FaceId> { 
    // twin_vertex  /twin_previous
    //  __________./  twin_next
    //  edge_next  \twin /
    //          edge\   /
    //                . - edge_vertex
    //              /   \
    //edge_previous/     \
    let edge = mesh.goto(target);
    let edge_next = edge.next().get_halfedge().unwrap();
    let edge_previous = edge.previous().get_halfedge().unwrap();
    let edge_face = edge.get_face().unwrap();
    let edge_vertex = edge.get_vertex().unwrap();

    let twin = edge.twin();
    let edge = edge.get_halfedge().unwrap();
    let twin_next = twin.next().get_halfedge().unwrap();
    let twin_previous = twin.previous().get_halfedge().unwrap();
    let twin_face = twin.get_face().unwrap();
    let twin_vertex = twin.get_vertex().unwrap();
    let twin = twin.get_halfedge().unwrap();

    // Reconnect the mesh to bypass removed edge+twin
    mesh[twin_previous].next = edge_next;
    mesh[edge_previous].next = twin_next;
    mesh[edge_vertex].halfedge = twin_next; // make sure vertices don't point at deleted edge
    mesh[twin_vertex].halfedge = edge_next;
    mesh.halfedges.remove(target);
    mesh.halfedges.remove(twin);

    let face_edges:StackVec<_> = mesh.goto(edge_next).iter_loop().map(|t| t.get_halfedge().unwrap()).collect();
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
pub fn split(mesh:&mut HalfEdgeMesh, target:HalfEdgeId, factor:f32) -> VertexId {
    // twin_vertex  /twin_previous
    //  __________./  twin_next
    //  edge_next  \twin /
    //          edge\   /
    //                . - edge_vertex
    //              /   \
    //edge_previous/     \
    let edge = mesh.goto(target);
    let edge_next = edge.next().get_halfedge().unwrap();
    let edge_face = edge.get_face().unwrap();
    let start_pos = edge.get_vertex_position().unwrap();    
    let twin = edge.twin();
    let twin_previous = twin.previous().get_halfedge().unwrap();
    let twin_face = twin.get_face().unwrap();
    let twin_vertex = twin.get_vertex().unwrap();
    let end_pos = twin.get_vertex_position().unwrap();
    let twin = twin.get_halfedge().unwrap();
    
    let new_vertex_pos = start_pos.lerp(end_pos, factor);
    let new_vertex = mesh.new_vertex();
    mesh.attributes.get_mut(&AttributeKind::Positions).unwrap().as_vertices_vec3_mut().insert(new_vertex, new_vertex_pos);
    let new_edge = mesh.halfedges.insert(HalfEdge{twin:default(), next:edge_next, vertex:new_vertex, face:edge_face});
    let new_twin = mesh.halfedges.insert(HalfEdge { twin: new_edge, next: twin, vertex: twin_vertex, face:twin_face });
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

pub fn chamfer(mesh:&mut HalfEdgeMesh, target:HalfEdgeId, factor:f32) -> FaceId {

    let start_vertex = mesh.goto(target).get_vertex().unwrap();
    let end_vertex = mesh.goto(target).twin().get_vertex().unwrap();
    let twin = mesh.goto(target).twin().get_halfedge().unwrap();

    let start_outgoing_edges = mesh.goto(start_vertex).iter_outgoing().map(|e| e.get_halfedge().unwrap()).collect::<StackVec<_>>();
    // Split on the target halfedge, making all cut_vertices flow together - makese face cutting easier
    let mut start_split = start_outgoing_edges.split(|e| e == &target);
    let mut cut_vertices = start_split.next().unwrap().iter().map(|e| split(mesh, *e, factor)).collect::<SmallVec<[_;16]>>();
    let mut start_vertex_chamfer_vertices = cut_vertices.clone();

    let mut edge_chamfer_vertices = StackVec::new();
    if let Some(last) = start_vertex_chamfer_vertices.last() {
        edge_chamfer_vertices.push(*last)
    }

    let end_outgoing_edges = mesh.goto(end_vertex).iter_outgoing().filter(|e| e.get_position() != twin.into()).map(|e| e.get_halfedge().unwrap()).collect::<StackVec<_>>();
    
    let mut end_vertex_chamfer_vertices = end_outgoing_edges.iter().map(|e| split(mesh, *e, factor)).collect::<StackVec<_>>();
    edge_chamfer_vertices.push(end_vertex_chamfer_vertices[0]);
    edge_chamfer_vertices.push(*end_vertex_chamfer_vertices.last().unwrap());
    cut_vertices.extend(end_vertex_chamfer_vertices.iter().copied());
    let leftover = start_split.next().unwrap().iter().map(|e| split(mesh, *e, factor)).collect::<StackVec<_>>();
    edge_chamfer_vertices.push(leftover[0]);
    start_vertex_chamfer_vertices.extend(leftover.iter().copied());
    cut_vertices.extend(leftover);
    start_vertex_chamfer_vertices.reverse();
    end_vertex_chamfer_vertices.reverse();
    edge_chamfer_vertices.reverse();
    let new_edges = cut_vertices.iter().circular_tuple_windows().map(|(&start, &end)| face_ops::split(mesh, start, end)).collect::<SmallVec<[_;16]>>();
    
    vertex_ops::delete(mesh, start_vertex, false);
    vertex_ops::delete(mesh, end_vertex, false);
    mesh.new_face(&start_vertex_chamfer_vertices);
    mesh.new_face(&end_vertex_chamfer_vertices);
    mesh.new_face(&edge_chamfer_vertices);

    default()
}

#[cfg(test)]
mod tests {
    use smallvec::SmallVec;

    use crate::mesh::HalfEdgeMesh;

    #[test]
    fn test_delete() {
        let mut mesh = HalfEdgeMesh::new();
        let vertices:SmallVec<[_;9]> = (0..6).map(|_| mesh.new_vertex()).collect();
        // 0--1--4
        // |  |  |
        // 3--2--5
        // to_be_deleted is the first face (left on diagram above) because 2-1 edge is going up, and in CCW order that's right face. 
        let to_be_deleted = mesh.new_face(&[vertices[0], vertices[1], vertices[2], vertices[3]]);
        mesh.new_face(&[vertices[1], vertices[4], vertices[5], vertices[2]]);

        let target = mesh.goto(vertices[2]).halfedge_to(vertices[1]).get_halfedge().unwrap();
        let deleted_face = super::delete(&mut mesh, target, true);
        assert_eq!(mesh.count_face_edges(), 6);
        assert_eq!(mesh.count_islands(), 1);
        assert_eq!(mesh.face_count(), 1);
        assert_eq!(mesh.vertex_count(), 6);
        assert_eq!(deleted_face, Some(to_be_deleted));
        assert!(!mesh.halfedges.contains_key(target));
    }
}