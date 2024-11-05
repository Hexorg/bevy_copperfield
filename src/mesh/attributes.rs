use std::collections::BTreeMap;

use bevy::prelude::{Mat3, Vec2, Vec3};
use slotmap::SecondaryMap;

use super::{selection::Selection, traversal::Traversal, HalfEdgeId, StackVec, VertexId};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AttributeKind {
    Positions,
    Normals,
    UVs,
    Indices,
    Creases,
    UVSeams,
}


pub type AttributeStore<K, T> = SecondaryMap<K, T>;
pub enum AttributeValues {
    VertexU32(AttributeStore<VertexId, u32>),
    VertexVec3(AttributeStore<VertexId, Vec3>),
    VertexVec2(AttributeStore<VertexId, Vec2>),
    VertexBool(AttributeStore<VertexId, bool>),
    EdgeVec2(AttributeStore<HalfEdgeId, Vec2>),
    EdgeVec3(AttributeStore<HalfEdgeId, Vec3>),
    EdgeBool(AttributeStore<HalfEdgeId, bool>),
}

impl AttributeValues {
    pub fn as_vertices_u32(&self) -> &AttributeStore<VertexId, u32> {
        match self {
            Self::VertexU32(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_vertices_u32_mut(&mut self) -> &mut AttributeStore<VertexId, u32> {
        match self {
            Self::VertexU32(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_vertices_vec2(&self) -> &AttributeStore<VertexId, Vec2> {
        match self {
            Self::VertexVec2(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_vertices_vec2_mut(&mut self) -> &mut AttributeStore<VertexId, Vec2> {
        match self {
            Self::VertexVec2(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_vertices_vec3(&self) -> &AttributeStore<VertexId, Vec3> {
        match self {
            Self::VertexVec3(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_vertices_vec3_mut(&mut self) -> &mut AttributeStore<VertexId, Vec3> {
        match self {
            Self::VertexVec3(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_vertices_bool(&self) -> &AttributeStore<VertexId, bool> {
        match self {
            Self::VertexBool(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_vertices_bool_mut(&mut self) -> &mut AttributeStore<VertexId, bool> {
        match self {
            Self::VertexBool(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }

    pub fn as_edge_vec2(&self) -> &AttributeStore<HalfEdgeId, Vec2> {
        match self {
            Self::EdgeVec2(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }
    pub fn as_edge_vec2_mut(&mut self) -> &mut AttributeStore<HalfEdgeId, Vec2> {
        match self {
            Self::EdgeVec2(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }
    pub fn as_edge_vec3(&self) -> &AttributeStore<HalfEdgeId, Vec3> {
        match self {
            Self::EdgeVec3(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }
    pub fn as_edge_vec3_mut(&mut self) -> &mut AttributeStore<HalfEdgeId, Vec3> {
        match self {
            Self::EdgeVec3(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }
    pub fn as_edge_bool(&self) -> &AttributeStore<HalfEdgeId, bool> {
        match self {
            Self::EdgeBool(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }
    pub fn as_edge_bool_mut(&mut self) -> &mut AttributeStore<HalfEdgeId, bool> {
        match self {
            Self::EdgeBool(v) => v,
            _ => panic!("Unexpected attribute kind")
        }
    }
}

pub type Attributes = BTreeMap<AttributeKind, AttributeValues>;


impl From<AttributeStore<VertexId, u32>> for AttributeValues {
    fn from(value: AttributeStore<VertexId, u32>) -> Self {
        Self::VertexU32(value)
    }
}

impl From<AttributeStore<VertexId, Vec3>> for AttributeValues {
    fn from(value: AttributeStore<VertexId, Vec3>) -> Self {
        Self::VertexVec3(value)
    }
}

impl From<AttributeStore<HalfEdgeId, Vec2>> for AttributeValues {
    fn from(value: AttributeStore<HalfEdgeId, Vec2>) -> Self {
        Self::EdgeVec2(value)
    }
}

impl From<AttributeStore<HalfEdgeId, Vec3>> for AttributeValues {
    fn from(value: AttributeStore<HalfEdgeId, Vec3>) -> Self {
        Self::EdgeVec3(value)
    }
}

pub trait TraversalQueries{
    /// Get position of the associated vertex
    fn position(&self) -> Vec3;
    /// Uses AttributeKind::Creases to tell if the vertex is supposed to use smooth normals or sharp
    fn is_smooth_normals(&self) -> bool;
    /// Uses AttributeKind::UVSeams to tell if the vertex is supposed to use smooth normals or sharp
    fn is_uv_seam(&self) -> bool;
    /// Returns how sharp this edge is by taking a dot product of adjacent face normals.
    fn sharpness(&self) -> f32;
    /// Returns UV coordinates of the associated vertex.
    fn uv(&self) -> Vec2;
    /// Use earcutr to triangulage a polygon. Panics if the normal can't be calculated or 
    /// polygon can't be triangulated. Returns an array of vertices, in CCW winding, three for each triangle.
    fn triangulate(&self) -> Vec<VertexId>;
}

pub trait SelectionQueries {
    /// Get the average of all vertices of this face
    fn calculate_centroid(&self) -> Vec3;
    /// Fit a plane to a collection of points using least squares method.
	/// Fast, and accurate to within a few degrees.
	/// Returns None if the points do not span a plane.
	/// https://www.ilikebigbits.com/2015_03_04_plane_from_points.html
    fn calculate_normal(&self) -> Option<Vec3>;
    /// Calculate full 3x3 covariance matrix, excluding symmetries
    fn calculate_covariance(&self) -> Mat3;
}

impl<'m> TraversalQueries for Traversal<'m> {
    fn position(&self) -> Vec3 {
        let vertex = self.vertex();
        let values = self.mesh.attribute(&super::attributes::AttributeKind::Positions).expect("Vertices don't have position attribute.");
        values.as_vertices_vec3().get(vertex).copied().unwrap()
    }

    fn uv(&self) -> Vec2 {
        let values = self.mesh.attribute(&super::attributes::AttributeKind::UVs).expect("Vertices don't have UV attribute.");
        values.as_edge_vec2().get(**self).copied().unwrap()
    }

    fn is_smooth_normals(&self) -> bool {
        if let Some(store) = self.mesh.attribute(&super::attributes::AttributeKind::Creases) {
            store.as_vertices_bool().get(self.vertex()).copied().unwrap_or(self.mesh.is_smooth)
        } else {
            self.mesh.is_smooth
        }
    }

    fn is_uv_seam(&self) -> bool {
        if let Some(store) = self.mesh.attribute(&super::attributes::AttributeKind::UVSeams) {
            store.as_edge_bool().get(**self).copied().unwrap_or(false)
        } else {
            false
        }
    }
    
    fn sharpness(&self) -> f32 {
        let n = self.calculate_normal();
        let n_other = self.twin().calculate_normal();
        1.0 - match (n, n_other) { // invert cosine so 1.0 is "sharp" and 0 is "flat"
            (Some(n), Some(n_other)) => {
                n.dot(n_other).abs() / n.length() / n_other.length() // return cos() of angle between normals
            },
            _ => 1.0 // cos(0) = 1.0
        }
    }

    fn triangulate(&self) -> Vec<VertexId> {
        let normal = self.calculate_normal().unwrap();
        let u = (self.position() - self.calculate_centroid()).normalize();
        let v = u.cross(normal);
        let mut vertex_ids = StackVec::new();
        let vertices = self.iter_loop().flat_map(|p| {vertex_ids.push(p.vertex()); let p = p.position(); [p.dot(u), p.dot(v)]}).collect::<Vec<_>>();
        let result = earcutr::earcut(&vertices, &[], 2).unwrap();
        // earcutr by default returns CW order winding. Call Iterator::rev() to change it to CCW
        result.into_iter().map(|idx| vertex_ids[idx]).rev().collect()
    }
}

impl<'m> SelectionQueries for Traversal<'m> {
    fn calculate_centroid(&self) -> Vec3 {
        let (sum, count) = self.iter_loop().fold((Vec3::ZERO, 0.0), |acc, i| (acc.0 + i.position(), acc.1 + 1.0));
        sum / count
    }

    /// Fit a plane to a collection of points.
	/// Fast, and accurate to within a few degrees.
	/// Returns None if the points do not span a plane.
	/// https://www.ilikebigbits.com/2017_09_25_plane_from_points_2.html
    fn calculate_normal(&self) -> Option<Vec3> {
        let points = self
        .iter_loop()
        .map(|f| f.position()).collect::<StackVec<_>>();
        let n = points.len() as f32;
        let mut sum = Vec3{x:0.0, y:0.0, z:0.0};
        for &p in &points {
            sum += p;
        }
        let centroid = sum * (1.0 / n);
    
        // Calculate full 3x3 covariance matrix, excluding symmetries:
        let mut xx = 0.0; let mut xy = 0.0; let mut xz = 0.0;
        let mut yy = 0.0; let mut yz = 0.0; let mut zz = 0.0;

        let a = centroid - points[0];
        let b = centroid - points[1];
        let simple_normal = a.cross(b);
    
        for p in points {
            let r = p - centroid;
            xx += r.x * r.x;
            xy += r.x * r.y;
            xz += r.x * r.z;
            yy += r.y * r.y;
            yz += r.y * r.z;
            zz += r.z * r.z;
        }
    
        xx /= n;
        xy /= n;
        xz /= n;
        yy /= n;
        yz /= n;
        zz /= n;
    
        let mut weighted_dir = Vec3{x: 0.0, y: 0.0, z: 0.0};
    
        {
            let det_x = yy*zz - yz*yz;
            let axis_dir = Vec3{
                x: det_x,
                y: xz*yz - xy*zz,
                z: xy*yz - xz*yy,
            };
            let mut weight = det_x * det_x;
            if weighted_dir.dot(axis_dir) < 0.0 { weight = -weight; }
            weighted_dir += axis_dir * weight;
        }
    
        {
            let det_y = xx*zz - xz*xz;
            let axis_dir = Vec3{
                x: xz*yz - xy*zz,
                y: det_y,
                z: xy*xz - yz*xx,
            };
            let mut weight = det_y * det_y;
            if weighted_dir.dot(axis_dir) < 0.0 { weight = -weight; }
            weighted_dir += axis_dir * weight;
        }
    
        {
            let det_z = xx*yy - xy*xy;
            let axis_dir = Vec3{
                x: xy*yz - xz*yy,
                y: xy*xz - yz*xx,
                z: det_z,
            };
            let mut weight = det_z * det_z;
            if weighted_dir.dot(axis_dir) < 0.0 { weight = -weight; }
            weighted_dir += axis_dir * weight;
        }
    
        let normal = weighted_dir.normalize();
        if normal.is_finite() {
            let sign = simple_normal.dot(normal).signum();
            Some(sign*normal)
        } else {
            None
        }
    }

    fn calculate_covariance(&self) -> Mat3 {
        let centroid = self.calculate_centroid();
        self.iter_loop().fold(Mat3::ZERO, |acc, i| {
            let r = Mat3::from_cols(i.position() - centroid, Vec3::ZERO, Vec3::ZERO);
            acc + r * r.transpose()
        })
    }

}

impl<'m> SelectionQueries for Selection<'m> {
    fn calculate_centroid(&self) -> Vec3 {
        // We keep count since we don't know ahead of time the amount of face edges
        let (sum, count) = self.iter().map(|t| t.calculate_centroid()).fold((Vec3::ZERO, 0.0), |acc, i| (acc.0 + i, acc.1 + 1.0));
        sum / count
    }
    fn calculate_covariance(&self) -> Mat3 {
        self.iter().map(|t| t.calculate_covariance()).sum()
    }
    fn calculate_normal(&self) -> Option<Vec3> {
        let (sum, count) = self.iter().filter_map(|t| t.calculate_normal()).fold((Vec3::ZERO, 0.0), |acc, i| (acc.0 + i, acc.1 + 1.0));
        if count == 0.0 {
            None
        } else {
            Some(sum / count)
        }
    }
}


#[cfg(test)]
mod tests {
    use bevy::prelude::{Mat3, Vec3};

    #[test]
    fn test_quad() {
        let v1 = Vec3::new(0.0, 0.875, -0.25);  //  v1 === v4
        let v2 = Vec3::new(-0.25, 0.875, 0.0);  //   |      |
        let v3 = Vec3::new(0.0, 0.875, 0.25);   //   |      |
        let v4 = Vec3::new(0.25, 0.875, 0.0);   //  v2 === v3
        let n = (v1 - v2).cross(v3 - v2).normalize();
        let n2 = (v3 - v4).cross(v1 - v4).normalize();
        assert_eq!(n, n2);
    }   

    #[test]
    fn test_matrix() {
        let v1 = Vec3::new(0.0, 0.875, -0.25);  //  v1 === v4
        let v2 = Vec3::new(-0.25, 0.875, 0.0);  //   |      |
        let v3 = Vec3::new(0.0, 0.875, 0.25);   //   |      |
        let v4 = Vec3::new(0.25, 0.875, 0.0);   //  v2 === v3
        let centroid = 0.25*(v1 + v2 + v3 + v4);
        println!("Centroid: {centroid:?}");
        let r1 = v1 - centroid;
        let r2 = v2 - centroid;
        let r3 = v3 - centroid;
        let r4 = v4 - centroid;
        let m1 = Mat3::from_cols(r1, Vec3::ZERO, Vec3::ZERO);
        let m2 = Mat3::from_cols(r2, Vec3::ZERO, Vec3::ZERO);
        let m3 = Mat3::from_cols(r3, Vec3::ZERO, Vec3::ZERO);
        let m4 = Mat3::from_cols(r4, Vec3::ZERO, Vec3::ZERO);
        let covariance = m1 * m1.transpose() + m2 * m2.transpose() + m3 * m3.transpose() + m4 * m4.transpose();
        // covariance.y_axis.x = 0.0;
        // covariance.z_axis.x = 0.0;
        // covariance.z_axis.y = 0.0;
        let mut other_way = Mat3::ZERO;
        other_way.x_axis.x = r1.x * r1.x + r2.x * r2.x + r3.x * r3.x + r4.x * r4.x;
        other_way.x_axis.y = r1.x * r1.y + r2.x * r2.y + r3.x * r3.y + r4.x * r4.y;
        other_way.x_axis.z = r1.x * r1.z + r2.x * r2.z + r3.x * r3.z + r4.x * r4.z;
        other_way.y_axis.y = r1.y * r1.y + r2.y * r2.y + r3.y * r3.y + r4.y * r4.y;
        other_way.y_axis.z = r1.y * r1.z + r2.y * r2.z + r3.y * r3.z + r4.y * r4.z;
        other_way.z_axis.z = r1.z * r1.z + r2.z * r2.z + r3.z * r3.z + r4.z * r4.z;
        println!("Covariance: {covariance:?} other: {other_way:?}");
        assert_eq!(covariance, other_way);

		let det_x = other_way.y_axis.y * other_way.z_axis.z - other_way.y_axis.z * other_way.y_axis.z;
		let det_y = other_way.x_axis.x * other_way.z_axis.z - other_way.x_axis.z * other_way.x_axis.z;
		let det_z = other_way.x_axis.x * other_way.y_axis.y - other_way.x_axis.y * other_way.x_axis.y;

        let cdx = covariance.y_axis.cross(covariance.z_axis);
        let cdy = covariance.z_axis.cross(covariance.x_axis);
        let cdz = covariance.y_axis.cross(covariance.y_axis);

        println!("cdy: {cdy:?}");
        assert_eq!(det_x, cdx.x);
        assert_eq!(det_y, cdy.y);
        assert_eq!(det_z, cdz.x);

        let dir = Vec3::new(det_x,other_way.x_axis.z * other_way.y_axis.z - other_way.x_axis.y * other_way.z_axis.z,other_way.x_axis.y * other_way.y_axis.z - other_way.x_axis.z * other_way.y_axis.y);
        assert_eq!(dir, cdx);
        let dir = Vec3::new(other_way.x_axis.z * other_way.y_axis.z - other_way.x_axis.y * other_way.z_axis.z, det_y, other_way.x_axis.y * other_way.x_axis.z - other_way.y_axis.z * other_way.x_axis.x);
        assert_eq!(dir, cdy);
        let dir = Vec3::new(other_way.x_axis.y * other_way.y_axis.z - other_way.x_axis.z * other_way.y_axis.y, other_way.x_axis.y * other_way.x_axis.z - other_way.y_axis.z * other_way.x_axis.x, det_z);
        assert_eq!(dir, cdz);

    }
}