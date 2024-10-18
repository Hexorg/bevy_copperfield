use bevy::{prelude::{Vec2, Vec3}, utils::HashSet};
use slotmap::SecondaryMap;

use crate::mesh::{attributes::TraversalQueries, traversal::Traversal, FaceId, HalfEdgeMesh};

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
    /// Original Chart flood fill implemented by [Xatlas](https://github.com/jpcy/xatlas/tree/master)
    pub fn flood_fill(mesh:&HalfEdgeMesh) -> Self {
        let mut charts = Vec::new();
        let mut basis: SecondaryMap<FaceId, Basis> = SecondaryMap::new();
        for face_id in mesh.face_keys() {
            basis.insert(face_id, mesh.goto(face_id).compute_basis().unwrap());
            if charts.iter().any(|c:&Chart| c.faces.contains(&face_id)) {
                continue;
            }
            // TODO: XAtlas checks if face UV area is zero... Why? We are computing UVs here from scratch
            let mut chart = Chart::new();
            chart.faces.insert(face_id);
            let mut current_face = face_id;
            // mesh.goto(current_face).adjacent_faces()
            // TODO Flood fill faces.
            charts.push(chart);
        }
        Self { charts, basis }
    }

    pub fn is_face_in_chart(&self, face_id:FaceId) -> bool {
        for chart in &self.charts {
            if chart.faces.contains(&face_id) {
                return true;
            }
        }
        false
    }
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