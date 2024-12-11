//! 3D chess scene with procedurally generated pieces
//! Sizes are taken from https://www.measuringhow.com/chess-piece-sizes/

use core::f32;

use bevy::prelude::*;
use bevy_copperfield::{
    mesh::{face_ops, FaceId, HalfEdgeMesh},
    mesh_builders::HalfEdgeMeshBuilder,
};

fn make_base(diameter: f32, resolution: u32) -> (HalfEdgeMesh, FaceId) {
    // Shapes are generally x2 height of the diameter
    let mut mesh = Circle::new(0.5 * diameter)
        .mesh()
        .resolution(resolution)
        .procgen();
    mesh.is_smooth = false;
    let face = match mesh.goto(Vec3::ZERO).face() {
        Some(f) => f,
        None => mesh.goto(Vec3::ZERO).twin().face().unwrap(),
    };
    face_ops::transform(
        &mut mesh,
        face,
        Transform::from_translation(-0.5 * diameter * Vec3::Y)
            .with_rotation(Quat::from_rotation_x(-f32::consts::FRAC_PI_2)),
    );
    face_ops::extrude(&mut mesh, face, 0.1 * diameter);
    face_ops::extrude(&mut mesh, face, 0.05 * diameter);
    face_ops::transform(
        &mut mesh,
        face,
        Transform::from_scale(Vec3 {
            x: 0.8,
            y: 1.0,
            z: 0.8,
        }),
    );
    face_ops::extrude(&mut mesh, face, 0.05 * diameter);
    face_ops::transform(
        &mut mesh,
        face,
        Transform::from_scale(Vec3 {
            x: 1.1,
            y: 1.0,
            z: 1.1,
        }),
    );
    face_ops::extrude(&mut mesh, face, 0.05 * diameter);
    face_ops::transform(
        &mut mesh,
        face,
        Transform::from_scale(Vec3 {
            x: 0.9,
            y: 1.0,
            z: 0.9,
        }),
    );
    face_ops::extrude(&mut mesh, face, 0.05 * diameter);
    face_ops::transform(
        &mut mesh,
        face,
        Transform::from_scale(Vec3 {
            x: 0.8,
            y: 1.0,
            z: 0.8,
        }),
    );
    face_ops::extrude(&mut mesh, face, 0.05 * diameter);
    face_ops::transform(
        &mut mesh,
        face,
        Transform::from_scale(Vec3 {
            x: 0.8,
            y: 1.0,
            z: 0.8,
        }),
    );
    face_ops::extrude(&mut mesh, face, 0.1 * diameter);
    face_ops::transform(
        &mut mesh,
        face,
        Transform::from_scale(Vec3 {
            x: 1.2,
            y: 1.0,
            z: 1.2,
        }),
    );
    face_ops::extrude(&mut mesh, face, 0.05 * diameter);
    // Height: 0.5*diameter here
    face_ops::transform(
        &mut mesh,
        face,
        Transform::from_scale(Vec3 {
            x: 0.8,
            y: 1.0,
            z: 0.8,
        }),
    );
    face_ops::extrude(&mut mesh, face, 0.3 * diameter);
    face_ops::transform(
        &mut mesh,
        face,
        Transform::from_scale(Vec3 {
            x: 0.8,
            y: 1.0,
            z: 0.8,
        }),
    );
    face_ops::extrude(&mut mesh, face, 0.3 * diameter);
    face_ops::transform(
        &mut mesh,
        face,
        Transform::from_scale(Vec3 {
            x: 0.7,
            y: 1.0,
            z: 0.7,
        }),
    );
    face_ops::extrude(&mut mesh, face, 0.3 * diameter);
    // Height: 1.1*diameter
    // face_ops::transform(&mut mesh, face, Transform::from_scale(Vec3{x:0.8, y:1.0, z:0.8}));
    (mesh, face)
}

fn add_pawn_top(mesh: &mut HalfEdgeMesh, top_face: FaceId) {
    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::transform(
        mesh,
        top_face,
        Transform::from_scale(Vec3 {
            x: 1.5,
            y: 1.0,
            z: 1.5,
        }),
    );
    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::transform(
        mesh,
        top_face,
        Transform::from_scale(Vec3 {
            x: 0.3,
            y: 1.0,
            z: 0.3,
        }),
    );
    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::transform(
        mesh,
        top_face,
        Transform::from_scale(Vec3 {
            x: 2.0,
            y: 1.0,
            z: 2.0,
        }),
    );
    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::transform(
        mesh,
        top_face,
        Transform::from_scale(Vec3 {
            x: 1.2,
            y: 1.0,
            z: 1.2,
        }),
    );
    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::transform(
        mesh,
        top_face,
        Transform::from_scale(Vec3 {
            x: 1.1,
            y: 1.0,
            z: 1.1,
        }),
    );
    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::extrude(mesh, top_face, 0.3);
    face_ops::transform(
        mesh,
        top_face,
        Transform::from_scale(Vec3 {
            x: 0.9,
            y: 1.0,
            z: 0.9,
        }),
    );

    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::transform(
        mesh,
        top_face,
        Transform::from_scale(Vec3 {
            x: 0.9,
            y: 1.0,
            z: 0.9,
        }),
    );
    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::transform(
        mesh,
        top_face,
        Transform::from_scale(Vec3 {
            x: 0.8,
            y: 1.0,
            z: 0.8,
        }),
    );
    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::transform(
        mesh,
        top_face,
        Transform::from_scale(Vec3 {
            x: 0.5,
            y: 1.0,
            z: 0.5,
        }),
    );
    face_ops::extrude(mesh, top_face, 0.05);
    face_ops::transform(
        mesh,
        top_face,
        Transform::from_scale(Vec3 {
            x: 0.5,
            y: 1.0,
            z: 0.5,
        }),
    );
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup)
        .add_systems(Update, update)
        .run();
}

#[derive(Component)]
struct Pawn;

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

    let (mut pawn, top_face) = make_base(2.75, 5);
    add_pawn_top(&mut pawn, top_face);
    // let edge = *cube.goto(Vec3::ONE);
    // let vertex = edge_ops::split(&mut cube, edge, 0.33);
    // vertex_ops::chamfer(&mut cube, vertex, 0.25);
    // chamfer(&mut cube, vertex, 0.1).unwrap();
    // let other_vertex = cube.goto(Vec3{x:-0.5, y:0.5, z:0.5}).get_vertex().unwrap();
    // chamfer(&mut cube, other_vertex, 0.3).unwrap();
    pawn.calculate_uvs();
    commands.spawn((
        Pawn,
        Mesh3d(meshes.add(&pawn)),
        MeshMaterial3d(materials.add(Color::srgb_u8(124, 144, 255))),
        Transform::from_xyz(0.0, 0.5, 0.0)
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
       Transform::from_xyz(2.5, 4.5, 9.0).looking_at(Vec3::Y, Vec3::Y)
    ));
}

fn update(
    time: Res<Time>,
    mut camera: Query<&mut Transform, With<Camera>>,
    mut meshes: ResMut<Assets<Mesh>>,
    pawn: Query<&Mesh3d, With<Pawn>>,
) {
    let count = (4.0 + 6.0 * time.elapsed_secs().sin().abs()).ceil() as u32;
    let (mut mesh, top_face) = make_base(2.75, count);
    add_pawn_top(&mut mesh, top_face);
    mesh.calculate_uvs();
    if let Some(pawn) = meshes.get_mut(pawn.single()) {
        *pawn = (&mesh).into()
    }
    let (x, z) = (0.8 * time.elapsed_secs()).sin_cos();
    let pos = Vec3 { x, y: 0.45, z } * 10.0;
    let mut transform = camera.single_mut();
    transform.translation = pos;
    transform.look_at(Vec3::Y, Vec3::Y);
}
