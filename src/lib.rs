pub mod mesh;
pub mod mesh_builders;

pub(crate) const OPTIMIZE_FOR_NGONS_UNDER_SIZE:usize = 4; // optimize for quads (reduces used stack space)