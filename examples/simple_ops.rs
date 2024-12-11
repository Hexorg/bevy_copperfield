//! A simple 3D scene with light shining over a cube sitting on a plane.

use bevy::{
    pbr::wireframe::{Wireframe, WireframePlugin},
    prelude::*,
};
use bevy_copperfield::{
    mesh::vertex_ops,
    mesh_builders::HalfEdgeMeshBuilder,
};
// use bevy_copperfield::{mesh::{vertex_ops::chamfer, VertexId}, mesh_builders::HalfEdgeMeshBuilder};

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, WireframePlugin))
        .add_systems(Startup, setup)
        .add_systems(Update, update)
        .run();
}

/// set up a simple 3D scene
fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // circular base
    commands.spawn((
        Mesh3d(meshes.add(Circle::new(4.0))),
        MeshMaterial3d(materials.add(Color::WHITE)),
        Transform::from_rotation(Quat::from_rotation_x(-std::f32::consts::FRAC_PI_2)),
    ));
    // cube
    let mut cube = Cuboid::new(1.0, 1.0, 1.0).procgen();
    cube.is_smooth = false;
    let vertex = cube.goto(Vec3::ONE).vertex();
    vertex_ops::chamfer(&mut cube, vertex, 0.25);
    // chamfer(&mut cube, vertex, 0.1).unwrap();
    // let other_vertex = cube.goto(Vec3{x:-0.5, y:0.5, z:0.5}).get_vertex().unwrap();
    // chamfer(&mut cube, other_vertex, 0.3).unwrap();
    cube.calculate_uvs();
    commands.spawn((
        Wireframe,
        Mesh3d(meshes.add(&cube)),
        MeshMaterial3d(materials.add(Color::srgb_u8(124, 144, 255))),
        Transform::from_xyz(0.0, 0.5, 0.0),

    ));
    // light
    commands.spawn((
        PointLight {
            shadows_enabled: true,
            ..default()
        },
        Transform::from_xyz(4.0, 8.0, 4.0),
    ));
    // camera
    commands.spawn((
        Camera3d::default(),
        Transform::from_xyz(2.5, 4.5, 9.0).looking_at(Vec3::ZERO, Vec3::Y),
    ));
}

fn update(time: Res<Time>, mut camera: Query<&mut Transform, With<Camera>>) {
    let (x, z) = time.elapsed_secs().sin_cos();
    let pos = Vec3 { x, y: 0.45, z } * 10.0;
    let mut transform = camera.single_mut();
    transform.translation = pos;
    transform.look_at(Vec3::ZERO, Vec3::Y);
}
