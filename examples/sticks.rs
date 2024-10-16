//! A simple 3D scene with light shining over a cube sitting on a plane.

use core::f32;

use bevy::{color, math::VectorSpace, pbr::wireframe::{Wireframe, WireframePlugin}, prelude::*};
use bevy_copperfield::{mesh::{edge_ops, face_ops, mesh_ops, vertex_ops, HalfEdgeMesh}, mesh_builders::HalfEdgeMeshBuilder};
use noise::{NoiseFn, Simplex};
// use bevy_copperfield::{mesh::{vertex_ops::chamfer, VertexId}, mesh_builders::HalfEdgeMeshBuilder};


fn make_stick(length:f32, transform:Transform) -> HalfEdgeMesh {
    let mut mesh = Circle::new(0.02).mesh().resolution(5).procgen();
    let noise = noise::Turbulence::<noise::Simplex, noise::Simplex>::new(Simplex::new(1));
    mesh_ops::transform(&mut mesh, Transform::from_rotation(Quat::from_rotation_x(-f32::consts::FRAC_PI_2)));
    mesh_ops::transform(&mut mesh, transform);
    let noise_point = transform.transform_point(Vec3::ZERO);
    let noise_point = [noise_point.x as f64, noise_point.y as f64, noise_point.z as f64];
    mesh_ops::transform(&mut mesh, Transform::from_translation(0.2*noise.get(noise_point) as f32 * Vec3{x:1.0, y:0.0, z:1.0}));
    let face = mesh.face_keys().next().unwrap();
    let mut height = 0.0;
    while height <= length {
        face_ops::extrude(&mut mesh, face, 0.50);
        let noise_point = transform.transform_point(height*Vec3::Y);
        let noise_point = [noise_point.x as f64, noise_point.y as f64, noise_point.z as f64];
        face_ops::transform(&mut mesh, face, Transform::from_rotation(Quat::from_rotation_x(0.01*noise.get(noise_point) as f32)).with_translation(0.05*noise.get(noise_point) as f32 * Vec3{x:1.0, y:0.0, z:1.0}));
        height += 0.5;
    }
    mesh
}

fn make_fence(length:f32) -> HalfEdgeMesh {
    let mut pos = 0.0;
    let mut mesh = make_stick(2.0, Transform::IDENTITY);
    while pos <= length {
        pos += 0.04;
        let new_stick = make_stick(2.0, Transform::from_translation(pos*Vec3::X));
        mesh.join(&new_stick);
    }
    println!("Face count: {}", mesh.face_count());
    mesh
}

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
        material: materials.add(<bevy::prelude::Srgba as Into<Color>>::into(color::palettes::basic::GREEN)),
        transform: Transform::from_rotation(Quat::from_rotation_x(-std::f32::consts::FRAC_PI_2)),
        ..default()
    });

    commands.spawn((PbrBundle {
        mesh: meshes.add(&make_fence(8.0)),
        material: materials.add(Color::srgb_u8(124, 144, 255)),
        transform: Transform::from_xyz(-4.0, 0.0, 0.0),
        ..default()
    }));
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

fn update(time:Res<Time>, mut camera:Query<&mut Transform, With<Camera>>) {
    let (x, z) = time.elapsed_seconds().sin_cos();
    let pos = Vec3{x, y:0.45, z}*10.0;
    let mut transform = camera.single_mut();
    transform.translation = pos;
    transform.look_at(Vec3::ZERO, Vec3::Y);
}