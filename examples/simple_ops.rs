//! A simple 3D scene with light shining over a cube sitting on a plane.

use bevy::{
    pbr::wireframe::{Wireframe, WireframePlugin},
    prelude::*,
};
use bevy_copperfield::{
    mesh::{edge_ops, vertex_ops},
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
    commands.spawn(PbrBundle {
        mesh: meshes.add(Circle::new(4.0)),
        material: materials.add(Color::WHITE),
        transform: Transform::from_rotation(Quat::from_rotation_x(-std::f32::consts::FRAC_PI_2)),
        ..default()
    });
    // cube
    let mut cube = Cuboid::new(1.0, 1.0, 1.0).procgen();

    let edge = *cube.goto(Vec3::ONE);
    let vertex = edge_ops::split(&mut cube, edge, 0.33);
    vertex_ops::chamfer(&mut cube, vertex, 0.25);
    // chamfer(&mut cube, vertex, 0.1).unwrap();
    // let other_vertex = cube.goto(Vec3{x:-0.5, y:0.5, z:0.5}).get_vertex().unwrap();
    // chamfer(&mut cube, other_vertex, 0.3).unwrap();
    commands.spawn((
        Wireframe,
        PbrBundle {
            mesh: meshes.add(&cube),
            material: materials.add(Color::srgb_u8(124, 144, 255)),
            transform: Transform::from_xyz(0.0, 0.5, 0.0),
            ..default()
        },
    ));
    // light
    commands.spawn(PointLightBundle {
        point_light: PointLight {
            shadows_enabled: true,
            ..default()
        },
        transform: Transform::from_xyz(4.0, 8.0, 4.0),
        ..default()
    });
    // camera
    commands.spawn(Camera3dBundle {
        transform: Transform::from_xyz(2.5, 4.5, 9.0).looking_at(Vec3::ZERO, Vec3::Y),
        ..default()
    });
}

fn update(time: Res<Time>, mut camera: Query<&mut Transform, With<Camera>>) {
    let (x, z) = time.elapsed_seconds().sin_cos();
    let pos = Vec3 { x, y: 0.45, z } * 10.0;
    let mut transform = camera.single_mut();
    transform.translation = pos;
    transform.look_at(Vec3::ZERO, Vec3::Y);
}
