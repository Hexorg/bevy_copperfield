use std::mem::swap;

use bevy::prelude::{Mat3, Vec3};


pub fn eigen_solve_symmetric3(covariance:Mat3) -> Option<([f32;3], [Vec3;3])> {
    // converting covariance into tri-diagonal
    // work.y_axis.x = work.x_axis.y;
    // work.z_axis.x = work.x_axis.z;
    // work.z_axis.y = work.y_axis.z;
    let (mat, diag, subd) = eigen_solver3_tridiagonal(covariance);
    if let Some((mat, mut eigen_values, _)) = eigen_solver3_QL_algorithm(mat, diag, subd) {
        let mat = mat.transpose();
        let mut eigen_vectors = [mat.x_axis, mat.y_axis, mat.z_axis];
        		// shuffle to sort by singular value :
		if eigen_values[2] > eigen_values[0] && eigen_values[2] > eigen_values[1] {
            let tmp = eigen_values[0];
            eigen_values[2] = eigen_values[0];
            eigen_values[0] = tmp; 
            eigen_vectors[0] = mat.z_axis;
            eigen_vectors[2] = mat.x_axis;
		}
		if eigen_values[1] > eigen_values[0] {
            let tmp = eigen_values[0];
            eigen_values[1] = eigen_values[0];
            eigen_values[0] = tmp; 
            eigen_vectors[0] = mat.y_axis;
            eigen_vectors[1] = mat.x_axis;
		}
		if eigen_values[2] > eigen_values[1] {
            let tmp = eigen_values[1];
            eigen_values[2] = eigen_values[1];
            eigen_values[1] = tmp; 
            eigen_vectors[1] = mat.z_axis;
            eigen_vectors[2] = mat.y_axis;
		}

        Some((eigen_values, eigen_vectors))
    } else {
        None
    }
}

/// Householder reduction T = Q^t M Q
///   Input:
///     mat, symmetric 3x3 matrix M
///   Output:
///     mat, orthogonal matrix Q
///     diag, diagonal entries of T
///     subd, subdiagonal entries of T (T is symmetric)
fn eigen_solver3_tridiagonal(mut mat:Mat3) -> (Mat3, [f32;3], [f32;3]) {
    let mut subd = [0.0, 0.0, 0.0];
    let mut diag = [0.0, 0.0, 0.0];

    diag[0] = mat.x_axis.x;
    subd[2] = 0.0;
    if mat.x_axis.z.abs() >= f32::EPSILON {
        let ell = mat.x_axis.y.powi(2) + mat.x_axis.z.powi(2);
        mat.x_axis.y /= ell;
        mat.x_axis.z /= ell;
        let q = 2.0 * mat.x_axis.y * mat.y_axis.z + mat.x_axis.z * (mat.z_axis.z - mat.y_axis.y);
        diag[1] = mat.y_axis.y + mat.x_axis.z * q;
        diag[2] = mat.z_axis.z - mat.x_axis.z * q;
        subd[0] = ell;
        subd[1] = mat.y_axis.z - mat.x_axis.y * q;
        mat.y_axis = mat.x_axis;
        mat.y_axis.x = 0.0;
        mat.x_axis = Vec3{x:1.0, y:0.0, z:0.0};
        mat.z_axis = Vec3{x:0.0, y:mat.y_axis.z, z:-mat.y_axis.y};
    } else {
        diag[1] = mat.y_axis.y;
        diag[2] = mat.z_axis.z;
        subd[0] = mat.x_axis.y;
        subd[1] = mat.y_axis.z;
        mat = Mat3::IDENTITY;
    }

    (mat, diag, subd)
}

fn eigen_solver3_QL_algorithm(mut mat:Mat3, mut diag:[f32;3], mut subd:[f32;3]) -> Option<(Mat3, [f32;3], [f32;3])> {
    const MAX_ITER:usize = 32;
    for ell in 0..diag.len() {
        for iter in 0..MAX_ITER {
            for m in ell..2 {
                let dd = diag[m].abs() + diag[m+1].abs();
                if subd[m].abs() + dd == dd {
                    break;
                }
                if m == ell {
                    break;
                }
                let mut g = (diag[ell+1] - diag[ell]) / (2.0 * subd[ell]);
                let mut r = g.powi(2) + 1.0;
                if g < 0.0 {
                    g = diag[m] - diag[ell] + subd[ell] / (g - r);
                } else {
                    g = diag[m] - diag[ell] + subd[ell] / (g + r);
                }
                let mut s = 1.0;
                let mut c = 1.0;
                let mut p = 0.0;
                for i in ((ell+1)..(m-1)).rev() {
                    let mut f = s * subd[i];
                    let b = c * subd[i];
                    if f.abs() >= g.abs() {
						c = g / f;
						r = (c.powi(2) + 1.0).sqrt();
						subd[i + 1] = f * r;
                        s = 1.0 / r;
						c *= s;
					} else {
						s = f / g;
						r = (s.powi(2) + 1.0).sqrt();
						subd[i + 1] = g * r;
                        c = 1.0 / r;
						s *= c;
					}
					g = diag[i + 1] - p;
					r = (diag[i] - g) * s + 2.0 * b * c;
					p = s * r;
					diag[i + 1] = g + p;
					g = c * r - b;
					for mat_k in [&mut mat.x_axis, &mut mat.y_axis, &mut mat.z_axis] {
						f = mat_k[i + 1];
						mat_k[i + 1] = s * mat_k[i] + c * f;
						mat_k[i] = c * mat_k[i] - s * f;
					}
                }
                diag[ell] -= p;
				subd[ell] = g;
				subd[m] = 0.0;
            }
            if iter + 1 == MAX_ITER {
                return None;
            }
        }
    }
    Some((mat, diag, subd))
}