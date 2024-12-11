use core::f32;

use bevy::{
    color, image::{ImageAddressMode, ImageLoaderSettings, ImageSampler, ImageSamplerDescriptor}, prelude::*, render::view::screenshot::{save_to_disk, Screenshot}
};
use bevy_copperfield::{
    mesh::{
        attributes::{AttributeKind, SelectionQueries, TraversalQueries},
        face_ops, mesh_ops, vertex_ops, HalfEdgeId, HalfEdgeMesh, MeshPosition, StackVec,
    },
    mesh_builders::HalfEdgeMeshBuilder,
    uvmesh::{Chart, ProjectionMethod},
};
use camera_controls::{capture_mouse, FlyingCamera};
use itertools::Itertools;
use line_drawing::Bresenham;
use slotmap::SecondaryMap;
use smallvec::SmallVec;

#[derive(Resource)]
pub struct Charts {
    charts: Vec<Chart>,
}

pub fn cuboid_tests() -> (Vec<Chart>, HalfEdgeMesh) {
    let mut mesh = Cuboid::new(1.0, 1.0, 1.0).procgen();
    let faces = mesh.face_keys().collect::<StackVec<_>>();
    let face = faces[1];
    face_ops::extrude(&mut mesh, face, 1.0);
    face_ops::extrude(&mut mesh, face, 1.0);
    let middle_face = mesh.goto(face).twin().next().face().unwrap();
    face_ops::extrude(&mut mesh, middle_face, 1.0);
    mesh_ops::subdivide(&mut mesh);
    mesh_ops::subdivide(&mut mesh);
    mesh_ops::invert_normals(&mut mesh);
    // let charts = create_charts(&mut mesh);
    mesh.uv_projection = ProjectionMethod::Cube {
        center: Vec3 {
            x: 0.0,
            y: 1.0,
            z: 0.5,
        },
        scale: Vec3 {
            x: 1.0,
            y: 3.0,
            z: 2.0,
        },
    };
    // mesh.uv_projection = ProjectionMethod::Sphere {center: Vec3{x:0.0, y:1.0, z:0.0}, radius: Vec3{x:1.0, y:1.5, z:1.0} };
    // mesh.uv_projection = ProjectionMethod::LSCM;
    mesh.calculate_uvs();
    (Vec::new(), mesh)
}

pub fn sample_mesh_tests() -> HalfEdgeMesh {
    let mut mesh = sample_mesh();
    mesh.is_smooth = false;
    let v = mesh.vertex_keys().collect::<SmallVec<[_; 9]>>();
    let face = vertex_ops::chamfer(&mut mesh, v[2], 0.25);
    face_ops::extrude(&mut mesh, face, 0.5);
    // mesh_ops::subdivide(&mut mesh);
    // mesh_ops::subdivide(&mut mesh);
    mesh
}

pub fn builder_tests() -> HalfEdgeMesh {
    let mut mesh = Circle::new(0.5).mesh().resolution(8).procgen();
    mesh.is_smooth = false;
    let face = mesh.goto(Vec3::ZERO).twin().face().unwrap();
    face_ops::transform(
        &mut mesh,
        face,
        Transform::from_translation(-0.5 * Vec3::Y)
            .with_rotation(Quat::from_rotation_x(-f32::consts::FRAC_PI_2)),
    );

    face_ops::extrude(&mut mesh, face, 1.0);
    mesh
}

pub fn build_mesh() -> (Vec<Chart>, HalfEdgeMesh) {
    cuboid_tests()
    // sample_mesh_tests()
    // builder_tests()
}

pub mod camera_controls {
    use core::f32;

    use bevy::{
        input::mouse::{MouseMotion, MouseWheel},
        prelude::*,
        window::{CursorGrabMode, PrimaryWindow},
    };

    #[derive(Component, Default)]
    pub struct FlyingCamera {
        pub pitch: f32,
        pub yaw: f32,
    }

    pub fn capture_mouse(
        mut q_windows: Query<&mut Window, With<PrimaryWindow>>,
        mut motion_events: ResMut<Events<MouseMotion>>,
    ) {
        let mut primary_window = q_windows.single_mut();
        primary_window.cursor_options.grab_mode = CursorGrabMode::Locked;
        primary_window.cursor_options.visible = false;
        motion_events.clear();
    }

    pub fn player_controller(
        time: Res<Time>,
        keyboard: Res<ButtonInput<KeyCode>>,
        mut transform_query: Query<(&mut Transform, &mut FlyingCamera, &mut Projection)>,
        mut motion_evr: EventReader<MouseMotion>,
        mut scroll_evr: EventReader<MouseWheel>,
        mut exit: EventWriter<AppExit>,
    ) {
        const MOUSE_SENSITIVITY: f32 = 0.001;
        const CAMERA_SPEED: f32 = 10.0;

        let w = keyboard.pressed(KeyCode::KeyW);
        let s = keyboard.pressed(KeyCode::KeyS);
        let a = keyboard.pressed(KeyCode::KeyA);
        let d = keyboard.pressed(KeyCode::KeyD);
        let up = keyboard.pressed(KeyCode::ShiftLeft);
        let down = keyboard.pressed(KeyCode::ControlLeft);
        let escape = keyboard.just_pressed(KeyCode::Escape);
        let zero = keyboard.just_pressed(KeyCode::Digit0);
        let change_projection = keyboard.just_pressed(KeyCode::Tab);

        for (mut transform, mut looking_at, projection) in &mut transform_query {
            let p = projection.into_inner();
            for ev in scroll_evr.read() {
                let change_value = ev.y
                    * match ev.unit {
                        bevy::input::mouse::MouseScrollUnit::Line => 0.03,
                        bevy::input::mouse::MouseScrollUnit::Pixel => 0.01,
                    };
                match p {
                    Projection::Orthographic(orthographic_projection) => {
                        orthographic_projection.scale += 0.01*change_value
                    }
                    Projection::Perspective(perspective_projection) => {
                        perspective_projection.fov += change_value
                    }
                }
            }
            if change_projection {
                match p {
                    Projection::Perspective(_) => {
                        *p = Projection::Orthographic(OrthographicProjection{scale:0.005, ..OrthographicProjection::default_3d()})
                    }
                    Projection::Orthographic(_) => *p = Projection::Perspective(default()),
                }
            }
            for ev in motion_evr.read() {
                // TODO: Add support for axis input reassignment
                looking_at.pitch -= MOUSE_SENSITIVITY * ev.delta.y;
                looking_at.yaw -= MOUSE_SENSITIVITY * ev.delta.x;
            }
            if zero {
                looking_at.pitch = -0.5 * f32::consts::PI;
                looking_at.yaw = 0.0;
            }
            transform.rotation =
                Quat::from_euler(EulerRot::YXZ, looking_at.yaw, looking_at.pitch, 0.0);
            let mut shift = Vec3::ZERO;
            if s {
                shift -= (Vec3 {
                    x: 1.0,
                    y: 0.0,
                    z: 1.0,
                } * *transform.forward())
                .normalize();
            } else if w {
                shift += (Vec3 {
                    x: 1.0,
                    y: 0.0,
                    z: 1.0,
                } * *transform.forward())
                .normalize();
            }
            if d {
                shift -= (Vec3 {
                    x: 1.0,
                    y: 0.0,
                    z: 1.0,
                } * *transform.left())
                .normalize();
            } else if a {
                shift += (Vec3 {
                    x: 1.0,
                    y: 0.0,
                    z: 1.0,
                } * *transform.left())
                .normalize();
            }
            if down {
                shift += -Vec3::Y;
            } else if up {
                shift += Vec3::Y;
            }
            if shift != Vec3::ZERO {
                transform.translation += time.delta_secs() * CAMERA_SPEED * shift;
            }
            if zero {
                transform.translation = 3.0 * Vec3::Y;
            }
        }

        if escape {
            exit.send(AppExit::Success);
        }
    }
}

#[derive(Resource, Deref)]
struct DebugMesh(HalfEdgeMesh);

#[derive(Component, Copy, Clone, Deref)]
struct HalfMeshLabelKey(MeshPosition);

#[derive(Component)]
struct HelpText;

const VERTEX_COLOR: Srgba = color::palettes::basic::WHITE;
const EDGE_COLOR: Srgba = color::palettes::basic::AQUA;
const UV_SEAM_EDGE_COLOR: Srgba = color::palettes::basic::RED;
const RIB_COLOR: Srgba = color::palettes::basic::BLACK;
const FACE_COLOR: Srgba = color::palettes::basic::BLUE;

#[derive(States, Debug, Hash, Default, PartialEq, Eq, Clone, Copy)]
pub enum GizmoState {
    NoGizmos,
    #[default]
    LinesOnly,
    LinesAndLabels,
}

// #[derive(States, Debug, Hash, Default, PartialEq, Eq, Clone, Copy)]
// pub enum ScreenshotState {
//     #[default]
//     NotTakingScreenshot,
//     ReadyForScreenshot,
// }
/// set up a simple 3D scene
fn setup(
    debug_mesh: Res<DebugMesh>,
    charts: Res<Charts>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    assets: Res<AssetServer>,
) {
    commands.spawn((
        Mesh3d(meshes.add(Mesh::from(&debug_mesh.0))),
        MeshMaterial3d(materials.add(StandardMaterial {
            base_color_texture: Some(assets.load_with_settings(
                "uv.png",
                |s: &mut ImageLoaderSettings| {
                    s.sampler = ImageSampler::Descriptor(ImageSamplerDescriptor {
                        address_mode_u: ImageAddressMode::Repeat,
                        address_mode_v: ImageAddressMode::Repeat,
                        ..default()
                    })
                },
            )),
            ..default()
        })),
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
        FlyingCamera {
            pitch: -0.5 * f32::consts::PI,
            yaw: 0.0,
        },
        Camera3d::default(),
        Transform::from_xyz(0.0, 3.0, 0.0).looking_at(Vec3::ZERO, Vec3::Y),
    ));

    commands.spawn((
        HelpText,
        Text("Controls
W,A,S,D,Ctrl,Shift - move; Mouse movement - look around
Mouse wheel: Zoom
Escape: Exit
Digit 0: Reset view
Backslash: Toggle gizmo state
Tab: Toggle projection
Enter: Take Screenshot
Space: Apply Mesh mod function
".to_owned())
    ));

    for chart in &charts.charts {
        if chart.faces.len() < 5 {
            for &face in &chart.faces {
                commands.spawn((
                    HalfMeshLabelKey(face.into()),
                    Text(String::from(&format!("{:?}", face)[6..])),
                    TextColor(FACE_COLOR.into()),
                    Node{position_type:PositionType::Absolute,..default()},
                ));
            }
        }
    }
    // for vertex in debug_mesh.vertex_keys().take(2) {
    //     commands.spawn((HalfMeshLabelKey(vertex.into()), TextBundle{text:Text::from_section(String::from(&format!("{:?}", vertex)[8..]), TextNode {color: VERTEX_COLOR.into(), ..default() }), Node:Node{position_type:PositionType::Absolute,..default()}, ..default()}));
    // }
}

fn spawn_labels(debug_mesh: Res<DebugMesh>, mut commands: Commands) {
    for vertex in debug_mesh.vertex_keys() {
        commands.spawn((
            HalfMeshLabelKey(vertex.into()),
            Text(String::from(&format!("{:?}", vertex)[8..])),
            TextColor(VERTEX_COLOR.into()),
            Node{position_type:PositionType::Absolute,..default()},
        ));
    }
    for edge in debug_mesh.edge_keys() {
        commands.spawn((
            HalfMeshLabelKey(edge.into()),
            Text(String::from(&format!("{:?}", edge)[10..])),
            TextColor(EDGE_COLOR.into()),
            Node{position_type:PositionType::Absolute,..default()},
        ));
    }
    for face in debug_mesh.face_keys() {
        commands.spawn((
            HalfMeshLabelKey(face.into()),
            Text(String::from(&format!("{:?}", face)[6..])),
            TextColor(FACE_COLOR.into()),
            Node{position_type:PositionType::Absolute,..default()},
        ));
    }
}

fn despawn_labels(labels: Query<Entity, With<HalfMeshLabelKey>>, mut commands: Commands) {
    for label in &labels {
        commands.entity(label).despawn();
    }
}

fn draw_origin_gizmos(mut gizmos: Gizmos) {
    // use color::palettes::basic::{RED, GREEN, BLUE};
    // gizmos.line(Vec3::ZERO, Vec3::X, RED);
    // gizmos.line(Vec3::ZERO, Vec3::Y, GREEN);
    // gizmos.line(Vec3::ZERO, Vec3::Z, BLUE);
    gizmos.axes(Transform::IDENTITY, 1.0);
    // gizmos.axes(Transform::from_translation(Vec3::Y), 1.0);
    // gizmos.cuboid(Transform::from_translation(Vec3::Y+0.5*Vec3::Z).with_scale(Vec3{x:1.0, y:3.0, z:2.0}), color::palettes::css::ORANGE);
}

fn draw_vertex_gizmos(mesh: Res<DebugMesh>, mut gizmos: Gizmos) {
    let positions = mesh
        .attribute(&AttributeKind::Positions)
        .unwrap()
        .as_vertices_vec3();
    for vertex in mesh.vertex_keys() {
        if let Some(&pos) = positions.get(vertex) {
            gizmos.sphere(Isometry3d::from_translation(pos), 0.01, VERTEX_COLOR);
        }
    }
}

fn get_face_center_pos(edge: HalfEdgeId, mesh: &HalfEdgeMesh) -> Vec3 {
    let edge = mesh.goto(edge);
    let (sum, count) = edge.iter_loop().fold((Vec3::ZERO, 0.0_f32), |acc, v| {
        (acc.0 + v.position(), acc.1 + 1.0)
    });
    sum / count
}

fn get_edge_pos(edge: HalfEdgeId, mesh: &HalfEdgeMesh) -> (Vec3, Vec3) {
    const LENGTH: f32 = 0.25;
    const OFFSET: f32 = 0.1;
    let edge = mesh.goto(edge);
    let start = edge.position();
    let to = edge.twin().position();
    let distance_to_end = to - start;
    let direction = distance_to_end.normalize();
    let distance_to_end = distance_to_end.length();
    let shift = OFFSET
        * if edge.face().is_some() {
            get_face_center_pos(edge.halfedge(), mesh) - start + OFFSET * edge.calculate_normal().unwrap()
        } else {
            -(get_face_center_pos(edge.twin().halfedge(), mesh) - start)
        };
    // let  end = start.lerp(to, LENGTH);
    (
        start + shift,
        start + shift + LENGTH * distance_to_end * direction,
    )
}

fn draw_edge_gizmos(mesh: Res<DebugMesh>, mut gizmos: Gizmos) {
    for edge in mesh.edge_keys() {
        let (start, end) = get_edge_pos(edge, &mesh.0);
        gizmos.arrow(start, end, EDGE_COLOR);
        // Draw lines towards face center
        // if mesh[edge].face.is_some() {
        //     let face_center = get_face_center_pos(edge, &mesh.0);
        //     gizmos.line(start, face_center, FACE_COLOR);
        // }
        let start = mesh.goto(edge).position();
        let end = mesh.goto(edge).twin().position();
        let is_seam = mesh.goto(edge).is_uv_seam();
        gizmos.line(
            start,
            end,
            if is_seam {
                UV_SEAM_EDGE_COLOR
            } else {
                RIB_COLOR
            },
        );
    }
}

fn move_labels_to_with_camera(
    mut transform: Query<(&mut Node, &HalfMeshLabelKey)>,
    camera: Query<(&GlobalTransform, &Camera)>,
    mesh: Res<DebugMesh>,
) {
    let (camera_transform, camera) = camera.single();
    for (mut t, &key) in &mut transform {
        let pos = match key.0 {
            MeshPosition::Vertex(vertex_id) => mesh
                .attribute(&AttributeKind::Positions)
                .unwrap()
                .as_vertices_vec3()
                .get(vertex_id)
                .copied()
                .unwrap_or_default(),
            MeshPosition::HalfEdge(half_edge_id) => get_edge_pos(half_edge_id, &mesh.0).1,
            MeshPosition::Face(face_id) => get_face_center_pos(mesh.goto(face_id).halfedge(), &mesh.0),
        };
        if let Ok(viewport_pos) = camera.world_to_viewport(camera_transform, pos) {
            t.top = Val::Px(viewport_pos.y);
            t.left = Val::Px(viewport_pos.x);
        }
    }
}

fn screenshot_on_enter(
    input: Res<ButtonInput<KeyCode>>,
    mesh: Res<DebugMesh>,
    texture: Res<Assets<Image>>,
    loader: Res<AssetServer>,
    mut commands: Commands,
    mut counter: Local<u32>,
) {
    if input.just_pressed(KeyCode::Enter) {
        let screnshot_path = format!("./debug_screenshot-{}.png", *counter);
        let uv_path = format!("./uv-{}.png", *counter);
        *counter += 1;
        commands.spawn(Screenshot::primary_window()).observe(save_to_disk(screnshot_path));
        let image = texture.get(&loader.load("uv.png")).unwrap().clone();
        match image.try_into_dynamic() {
            Ok(dyn_img) => {
                // discard the alpha channel which stores brightness values when HDR is enabled to make sure
                // the screenshot looks right
                let mut img = dyn_img.to_rgb8();
                let width = img.width();
                let uvs = mesh.attribute(&AttributeKind::UVs).unwrap().as_edge_vec2();
                for face in mesh.face_keys() {
                    let verts = mesh
                        .goto(face)
                        .iter_loop()
                        .map(|p| uvs[p.halfedge()])
                        .collect::<StackVec<_>>();
                    for (&from, &to) in verts.iter().circular_tuple_windows() {
                        let w = width as f32;
                        let from = from * w; //(0.5 + 0.5*from)*w;
                        let to = to * w; //(0.5 + 0.5*to)*w;
                        let x1 = from.x as i32;
                        let y1 = from.y as i32;
                        let x2 = to.x as i32;
                        let y2 = to.y as i32;
                        for (x, y) in Bresenham::new((x1, y1), (x2, y2)) {
                            if (x as u32) < width && (y as u32) < width {
                                img.get_pixel_mut(x as u32, y as u32).0 = [0, 0, 0];
                            }
                        }
                    }
                }
                #[cfg(not(target_arch = "wasm32"))]
                match img.save(&uv_path) {
                    Ok(_) => info!("uv map saved to {}", uv_path),
                    Err(e) => error!("Cannot save screenshot, IO error: {e}"),
                }
            }
            Err(e) => error!("Cannot save uv map, requested format not recognized: {e}"),
        }
    }
}

fn gizmo_state_changes(
    state: Res<State<GizmoState>>,
    input: Res<ButtonInput<KeyCode>>,
    mut next: ResMut<NextState<GizmoState>>,
) {
    if input.just_pressed(KeyCode::Backslash) {
        next.set(match state.get() {
            GizmoState::NoGizmos => {
                if input.pressed(KeyCode::ShiftRight) {
                    GizmoState::LinesAndLabels
                } else {
                    GizmoState::LinesOnly
                }
            }
            GizmoState::LinesOnly => {
                if input.pressed(KeyCode::ShiftRight) {
                    GizmoState::NoGizmos
                } else {
                    GizmoState::LinesAndLabels
                }
            }
            GizmoState::LinesAndLabels => {
                if input.pressed(KeyCode::ShiftRight) {
                    GizmoState::LinesOnly
                } else {
                    GizmoState::NoGizmos
                }
            }
        });
    }
}

/// Returns a mesh in the form
/// ```text
/// indices    VertexId    FaceId
/// 0--3--5    1--4--6     +--+--+
/// |  |  |    |  |  |     |1 |2 |
/// 1--2--4    2--3--5     +--+--+
/// |  |  |    |  |  |     |3 |4 |
/// 6--7--8    7--8--9     +--+--+
/// ```
pub fn sample_mesh() -> HalfEdgeMesh {
    let mut mesh = HalfEdgeMesh::new();
    let v: SmallVec<[_; 9]> = (0..9).map(|_| mesh.new_vertex()).collect();
    mesh.new_face(&[v[0], v[1], v[2], v[3]]);
    mesh.new_face(&[v[3], v[2], v[4], v[5]]);
    mesh.new_face(&[v[1], v[6], v[7], v[2]]);
    mesh.new_face(&[v[2], v[7], v[8], v[4]]);
    let positions = SecondaryMap::from_iter([
        (v[0], -Vec3::X - Vec3::Z),
        (v[3], -Vec3::Z + 0.5 * Vec3::Y),
        (v[5], Vec3::X - Vec3::Z),
        (v[1], -Vec3::X + 0.5 * Vec3::Y),
        (v[2], Vec3::Y),
        (v[4], Vec3::X + 0.5 * Vec3::Y),
        (v[6], -Vec3::X + Vec3::Z),
        (v[7], Vec3::Z + 0.5 * Vec3::Y),
        (v[8], Vec3::X + Vec3::Z),
    ]);
    mesh.add_attribute(AttributeKind::Positions, positions);
    mesh
}

fn main() {
    // let mut mesh = sample_mesh();
    // let v = mesh.vertex_keys().collect::<SmallVec<[_;9]>>();
    // let face = vertex_ops::chamfer(&mut mesh, v[2], 0.33);
    // face_ops::extrude(&mut mesh, face, 1.0);
    // let edge = mesh.goto(face).halfedge().twin().next().next().get_halfedge().unwrap();
    // edge_ops::chamfer(&mut mesh, edge, 0.25);
    // mesh_ops::subdivide(&mut mesh);

    let mut app = App::new();
    app.add_plugins(DefaultPlugins);
    let mut r = app.world_mut().resource_mut::<GizmoConfigStore>();
    let (config, _) = r.config_mut::<DefaultGizmoConfigGroup>();
    config.line_perspective = false;
    config.line_width = 5.0;
    let (charts, mesh) = build_mesh();
    app.insert_resource(DebugMesh(mesh))
        .insert_resource(Charts { charts })
        .init_state::<GizmoState>()
        // .init_state::<ScreenshotState>()
        .add_systems(Startup, (setup, capture_mouse))
        .add_systems(
            Update,
            (
                screenshot_on_enter,
                camera_controls::player_controller,
                gizmo_state_changes,
                move_labels_to_with_camera,
            ),
        )
        .add_systems(
            Update,
            (draw_origin_gizmos, draw_vertex_gizmos, draw_edge_gizmos)
                .run_if(not(in_state(GizmoState::NoGizmos))),
        )
        // .add_systems(
        //     OnEnter(ScreenshotState::ReadyForScreenshot),
        //     take_screenshot,
        // )
        // .add_systems(
        //     OnEnter(ScreenshotState::NotTakingScreenshot),
        //     |mut v: Query<&mut Visibility, With<HelpText>>| {
        //         for mut v in &mut v {
        //             *v = Visibility::Inherited
        //         }
        //     },
        // )
        .add_systems(OnEnter(GizmoState::LinesAndLabels), spawn_labels)
        .add_systems(OnExit(GizmoState::LinesAndLabels), despawn_labels)
        .run();
}
