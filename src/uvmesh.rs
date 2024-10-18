use bevy::{prelude::{Vec2, Vec3}, utils::HashSet};
use itertools::Itertools;
use slotmap::SecondaryMap;

use crate::mesh::{attributes::{AttributeKind, AttributeValues, TraversalQueries}, traversal::Traversal, FaceId, HalfEdgeId, HalfEdgeMesh};

mod fit;

use fit::eigen_solve_symmetric3;

/// Basis class to compute tangent space basis, ortogonalizations and to transform vectors from one space to another.
pub struct Basis{
    pub tangent: Vec3,
    pub bitangent: Vec3,
    pub normal: Vec3
}

impl Basis {
    /// Project 3D vertex position onto the basis vectors
    pub fn project_vertex(&self, pos:Vec3) -> Vec2 {
        Vec2{x:self.tangent.dot(pos), y:self.bitangent.dot(pos)}
    }

    pub fn from_normal(normal:Vec3) -> Self {
        let tangent = Self::compute_tangent(normal);
        let bitangent = Self::compute_bitangent(normal, tangent);
        Self { tangent, bitangent, normal}
    }

    pub fn from_eigen_vectors(eigen_vectors:[Vec3;3]) -> Self {
        Basis{tangent:eigen_vectors[0].normalize(), bitangent:eigen_vectors[1].normalize(), normal:eigen_vectors[2].normalize()}
    }

    fn compute_tangent(normal:Vec3) -> Vec3 {
        let mut tangent = Vec3::ZERO;
        // Choose minimum axis
        if normal.x.abs() < normal.y.abs() && normal.x.abs() < normal.z.abs() {
            tangent.x = 1.0;
        } else if normal.y.abs() < normal.z.abs() {
            tangent.y = 1.0;
        } else {
            tangent.z = 1.0;
        }
        // Ortogonalize
        tangent -= normal * normal.dot(tangent);
        return tangent.normalize()
    }

    #[inline]
    fn compute_bitangent(normal:Vec3, tangent:Vec3) -> Vec3 {
        normal.cross(tangent)
    }
}



/// Set of model parts homeomorphic to discs as per 
/// [Least Squares Conformal Maps](https://dl.acm.org/doi/pdf/10.1145/566654.566590) 
/// paper by Bruno Levy
pub struct Chart {
    faces: HashSet<FaceId>, 
    // XAtlas has Material(u32) here too
}

impl Chart {
    pub fn new() -> Self {
        Self { faces: HashSet::new() }
    }
}

pub struct Charts{
    charts:Vec<Chart>,
    basis:SecondaryMap<FaceId, Basis>
}

impl Charts {

    pub fn is_face_in_chart(&self, face_id:FaceId) -> bool {
        for chart in &self.charts {
            if chart.faces.contains(&face_id) {
                return true;
            }
        }
        false
    }
}

/// Based on [Xatlas](https://github.com/jpcy/xatlas/tree/master)'s implementation and
/// https://dl.acm.org/doi/10.1145/566654.566590 paper
pub fn create_charts(mesh:&mut HalfEdgeMesh) {
    let count = mesh.face_keys().len();
    let edges = mesh.face_keys().map(|f| mesh[f].halfedge).sorted_by(|&l, &r| {
        mesh.goto(r).sharpness().partial_cmp(&mesh.goto(l).sharpness()).unwrap_or(std::cmp::Ordering::Less)
    }).take(5 * count / 100).collect::<Vec<_>>(); // take 5% of the sharpest faces

    let mut uv_seams:SecondaryMap<HalfEdgeId, bool> = SecondaryMap::new();
    for edge in edges {
        uv_seams.insert(edge, true);
    }
    mesh.add_attribute(AttributeKind::UVSeams, AttributeValues::EdgeBool(uv_seams));
}


// impl<'m> UVMeshQueries for Traversal<'m> {
//     /// This is face basis. Faces are always planar so calculate least squares should always return some
//     /// The eigen vectors solver is for Charts - collections of faces. From there we can project vertices on the bases
//     /// to compute relative UV coordinates within a chart
//     fn compute_basis(&self) -> Option<Basis> {
//         if let Some(normal) = self.calculate_least_squares_normal() {
//             Some(Basis::from_normal(normal))
//         } else {
//             if let Some((_, eigen_vectors)) = eigen_solve_symmetric3(self.calculate_face_covariance()) {
//                 Some(Basis::from_eigen_vectors(eigen_vectors))
//             } else {
//                 None
//             }
//         }
//     }
// }