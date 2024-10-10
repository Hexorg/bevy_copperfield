use core::f32;

use bevy_copperfield::{mesh::{attributes::AttributeKind, edge_ops, face_ops, vertex_ops, HalfEdgeId, HalfEdgeMesh, MeshPosition, StackVec}, mesh_builders::HalfEdgeMeshBuilder};
use camera_controls::{capture_mouse, FlyingCamera};
use slotmap::{SecondaryMap};
use smallvec::SmallVec;
use bevy::{color, math::VectorSpace, prelude::*, render::view::screenshot::ScreenshotManager, window::PrimaryWindow};

pub mod camera_controls {
    use core::f32;

    use bevy::{input::mouse::{MouseMotion, MouseWheel}, prelude::*, window::{CursorGrabMode, PrimaryWindow}};

    #[derive(Component, Default)]
    pub struct FlyingCamera{
        pub pitch: f32,
        pub yaw: f32
    }

    pub fn capture_mouse(mut q_windows: Query<&mut Window, With<PrimaryWindow>>, mut motion_events:ResMut<Events<MouseMotion>>) {
        let mut primary_window = q_windows.single_mut();
        primary_window.cursor.grab_mode = CursorGrabMode::Locked;
        primary_window.cursor.visible = false;
        motion_events.clear();
    }

    pub fn player_controller(
        time:Res<Time>, 
        keyboard: Res<ButtonInput<KeyCode>>,
        mut transform_query: Query<(&mut Transform, &mut FlyingCamera, &mut Projection)>,
        mut motion_evr: EventReader<MouseMotion>,
        mut scroll_evr: EventReader<MouseWheel>,
        mut exit: EventWriter<AppExit>,
    ){
        const MOUSE_SENSITIVITY:f32 = 0.001;
        const CAMERA_SPEED:f32 = 10.0;
    
        let w = keyboard.pressed(KeyCode::KeyW);
        let s = keyboard.pressed(KeyCode::KeyS);
        let a = keyboard.pressed(KeyCode::KeyA);
        let d = keyboard.pressed(KeyCode::KeyD);
        let up = keyboard.pressed(KeyCode::ShiftLeft);
        let down = keyboard.pressed(KeyCode::ControlLeft);
        let escape = keyboard.just_pressed(KeyCode::Escape);
        let zero = keyboard.just_pressed(KeyCode::Digit0);
    
        for (mut transform, mut looking_at,projection) in &mut transform_query {
            let p = projection.into_inner();
            for ev in scroll_evr.read() {
                if let Projection::Orthographic(orthographic_projection) = p {
                    orthographic_projection.scale += ev.y*match ev.unit {
                        bevy::input::mouse::MouseScrollUnit::Line => 0.03,
                        bevy::input::mouse::MouseScrollUnit::Pixel => 0.01,
                    }
                }
            }
            for ev in motion_evr.read() {
                // TODO: Add support for axis input reassignment
                looking_at.pitch -= MOUSE_SENSITIVITY*ev.delta.y;
                looking_at.yaw -= MOUSE_SENSITIVITY*ev.delta.x;
            }
            if zero {
                looking_at.pitch = -0.5*f32::consts::PI;
                looking_at.yaw = 0.0;
            }
            transform.rotation = Quat::from_euler(EulerRot::YXZ, looking_at.yaw, looking_at.pitch, 0.0);
            let mut shift = Vec3::ZERO;
            if s {
                shift -= (Vec3{x:1.0, y:0.0, z:1.0}**transform.forward()).normalize();
            } else if w {
                shift += (Vec3{x:1.0, y:0.0, z:1.0}**transform.forward()).normalize();
            }
            if d {
                shift -= (Vec3{x:1.0, y:0.0, z:1.0}**transform.left()).normalize();
            } else if a {
                shift += (Vec3{x:1.0, y:0.0, z:1.0}**transform.left()).normalize();
            }
            if down {
                shift += -Vec3::Y;
            } else if up {
                shift += Vec3::Y;
            }
            if shift != Vec3::ZERO {
                transform.translation += time.delta_seconds()*CAMERA_SPEED*shift;
            }
            if zero {
                transform.translation = 10.0*Vec3::Y;
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

const VERTEX_COLOR:Srgba = color::palettes::basic::WHITE;
const EDGE_COLOR:Srgba = color::palettes::basic::AQUA;
const RIB_COLOR:Srgba = color::palettes::basic::BLACK;
const FACE_COLOR:Srgba = color::palettes::basic::BLUE;

#[derive(States, Debug, Hash, Default, PartialEq, Eq, Clone, Copy)]
pub enum GizmoState{
    #[default]
    Draw,
    NoDraw,
}
/// set up a simple 3D scene
fn setup(
    debug_mesh:Res<DebugMesh>,
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    commands.spawn(PbrBundle {
        mesh: meshes.add(Mesh::from(&debug_mesh.0)),
        material: materials.add(Color::srgb_u8(124, 144, 255)),
        transform: Transform::IDENTITY,
        ..default()
    });
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
    commands.spawn((FlyingCamera{pitch:-0.5*f32::consts::PI, yaw:0.0}, Camera3dBundle {
        transform: Transform::from_xyz(0.0, 3.0, 0.0).looking_at(Vec3::ZERO, Vec3::Y),
        projection:Projection::Orthographic(OrthographicProjection { near: 0.1, far: 10.0, scaling_mode: bevy::render::camera::ScalingMode::WindowSize(300.0), scale: 1.0, ..default() }),
        ..default()
    }));

    for vertex in debug_mesh.vertex_keys() {
        commands.spawn((HalfMeshLabelKey(vertex.into()), TextBundle{text:Text::from_section(String::from(&format!("{:?}", vertex)[8..]), TextStyle {color: VERTEX_COLOR.into(), ..default() }), style:Style{position_type:PositionType::Absolute,..default()}, ..default()}));
    }
    for edge in debug_mesh.edge_keys() {
        commands.spawn((HalfMeshLabelKey(edge.into()), TextBundle{text:Text::from_section(String::from(&format!("{:?}", edge)[10..]), TextStyle {color: EDGE_COLOR.into(), ..default() }), style:Style{position_type:PositionType::Absolute,..default()}, ..default()}));
    }
    for face in debug_mesh.face_keys() {
        commands.spawn((HalfMeshLabelKey(face.into()), TextBundle{text:Text::from_section(String::from(&format!("{:?}", face)[6..]), TextStyle {color: FACE_COLOR.into(), ..default() }), style:Style{position_type:PositionType::Absolute,..default()}, ..default()}));
    }
}

fn draw_origin_gizmos(mut gizmos:Gizmos) {
    use color::palettes::basic::{RED, GREEN, BLUE};
    gizmos.line(Vec3::ZERO, Vec3::X, RED);
    gizmos.line(Vec3::ZERO, Vec3::Y, GREEN);
    gizmos.line(Vec3::ZERO, Vec3::Z, BLUE);
}

fn draw_vertex_gizmos(mesh:Res<DebugMesh>, mut gizmos:Gizmos) {
    let positions = mesh.attribute(&AttributeKind::Positions).unwrap().as_vertices_vec3();
    for vertex in mesh.vertex_keys() {
        let pos = positions[vertex];
        gizmos.sphere(pos, Quat::IDENTITY, 0.01, VERTEX_COLOR);
    }
}

fn get_face_center_pos(edge:HalfEdgeId, mesh:&HalfEdgeMesh) -> Vec3 {
    let edge = mesh.goto(edge);
    let (sum, count) = edge.face().iter_loop().fold((Vec3::ZERO, 0.0_f32), |acc, v| (acc.0 + v.get_vertex_position().unwrap(), acc.1 + 1.0));
    sum / count
}

fn get_edge_pos(edge:HalfEdgeId, mesh:&HalfEdgeMesh) -> (Vec3, Vec3) {
    const LENGTH:f32 = 0.25;
    const OFFSET:f32 = 0.1;
    let edge = mesh.goto(edge);
    let start = edge.get_vertex_position().unwrap();
    let to = edge.twin().get_vertex_position().unwrap();
    let distance_to_end = to-start;
    let direction = distance_to_end.normalize();
    let distance_to_end = distance_to_end.length();
    let shift = OFFSET*if !edge.is_boundary() {
        get_face_center_pos(edge.get_halfedge().unwrap(), mesh) - start //+ mesh.face_normal(edge.get_face().unwrap().unwrap())
    } else {
        start - get_face_center_pos(edge.twin().get_halfedge().unwrap(), mesh)
    }.normalize();
    // let  end = start.lerp(to, LENGTH);
    (start+shift, start+shift+LENGTH*distance_to_end*direction)
}

fn draw_edge_gizmos(mesh:Res<DebugMesh>, mut gizmos:Gizmos) {
    for edge in mesh.edge_keys() {
        let (start, end) = get_edge_pos(edge, &mesh.0);
        gizmos.arrow(start, end, EDGE_COLOR);
        let start = mesh.goto(edge).get_vertex_position().unwrap();
        let end = mesh.goto(edge).twin().get_vertex_position().unwrap();
        gizmos.line(start, end, RIB_COLOR);
    }
}

fn move_labels_to_with_camera(mut transform:Query<(&mut Style, &HalfMeshLabelKey)>, 
    state:Res<State<GizmoState>>,
    camera:Query<(&GlobalTransform, &Camera)>, 
    mesh:Res<DebugMesh>) {
    let (camera_transform, camera) = camera.single();
    // let window = window.single();
    // let width = window.width();
    // let height = window.height();
    for (mut t, &key) in &mut transform {
        if state.get() == &GizmoState::Draw {
            let pos = match key.0 {
                MeshPosition::Vertex(vertex_id) => mesh.goto(vertex_id).get_vertex_position().unwrap(),
                MeshPosition::HalfEdge(half_edge_id) => get_edge_pos(half_edge_id, &mesh.0).1,
                MeshPosition::Face(face_id) => get_face_center_pos(mesh.goto(face_id).get_halfedge().unwrap(), &mesh.0),
            };
            if let Some(viewport_pos) = camera.world_to_viewport(camera_transform, pos) {
                t.top = Val::Px(viewport_pos.y);
                t.left = Val::Px(viewport_pos.x);
    
            }
        } else {
            t.top = Val::Px(-10.0);
            t.left = Val::Px(-10.0);
        }
    }
}

fn screenshot_on_enter(
    input: Res<ButtonInput<KeyCode>>,
    main_window: Query<Entity, With<PrimaryWindow>>,
    mut screenshot_manager: ResMut<ScreenshotManager>,
    mut counter: Local<u32>,
) {
    if input.just_pressed(KeyCode::Enter) {
        let path = format!("./debug_screenshot-{}.png", *counter);
        *counter += 1;
        screenshot_manager
            .save_screenshot_to_disk(main_window.single(), path)
            .unwrap();
    }
}

fn gizmo_state_changes(state:Res<State<GizmoState>>, input: Res<ButtonInput<KeyCode>>, mut next:ResMut<NextState<GizmoState>>) {
    if input.just_pressed(KeyCode::Backslash) {
        next.set(match state.get() {
            GizmoState::Draw => GizmoState::NoDraw,
            GizmoState::NoDraw => GizmoState::Draw,
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
    let v:SmallVec<[_;9]> = (0..9).map(|_| mesh.new_vertex()).collect();
    mesh.new_face(&[v[0], v[1], v[2], v[3]]);
    mesh.new_face(&[v[3], v[2], v[4], v[5]]);
    mesh.new_face(&[v[1], v[6], v[7], v[2]]);
    mesh.new_face(&[v[2], v[7], v[8], v[4]]);
    let positions = SecondaryMap::from_iter([
        (v[0], -Vec3::X-Vec3::Z), (v[3], -Vec3::Z+0.5*Vec3::Y), (v[5], Vec3::X-Vec3::Z),
        (v[1], -Vec3::X+0.5*Vec3::Y), (v[2], Vec3::Y), (v[4], Vec3::X+0.5*Vec3::Y),
        (v[6], -Vec3::X+Vec3::Z), (v[7], Vec3::Z+0.5*Vec3::Y), (v[8], Vec3::X+Vec3::Z),
    ]);
    mesh.add_attribute(AttributeKind::Positions, positions);
    mesh
}

fn main() {
    let mut mesh = Cuboid::new(1.0, 1.0, 1.0).procgen();
    let faces = mesh.face_keys().collect::<StackVec<_>>();
    let face = faces[2];
    face_ops::extrude(&mut mesh, face, 1.0);
    let middle_halfedge = mesh.goto(face).twin().next();
    let middle_vertex = middle_halfedge.get_vertex().unwrap();
    let middle_halfedge = middle_halfedge.get_halfedge().unwrap();
    face_ops::extrude(&mut mesh, face, 1.0);
    println!("{middle_halfedge:?}");
    edge_ops::chamfer(&mut mesh, middle_halfedge, 0.25);

    // let mut mesh = sample_mesh();
    // let v = mesh.vertex_keys().collect::<SmallVec<[_;9]>>();
    // let face = vertex_ops::chamfer(&mut mesh, v[2], 0.33);
    // face_ops::extrude(&mut mesh, face, 1.0);
    // let edge = mesh.goto(face).get_halfedge().unwrap();
    // edge_ops::chamfer(&mut mesh, edge, 0.25);


    let mut app = App::new();
    app.add_plugins(DefaultPlugins);
    let mut r = app.world_mut().resource_mut::<GizmoConfigStore>();
    let (config, _) = r.config_mut::<DefaultGizmoConfigGroup>();
        config.line_perspective = false;
        config.line_width = 5.0;
    app
        .insert_resource(DebugMesh(mesh))
        .init_state::<GizmoState>()
        .add_systems(Startup, (setup, capture_mouse))
        .add_systems(Update, (screenshot_on_enter, camera_controls::player_controller, gizmo_state_changes, move_labels_to_with_camera))
        .add_systems(Update, (draw_origin_gizmos, draw_vertex_gizmos, draw_edge_gizmos,).run_if(in_state(GizmoState::Draw)))
        .run()
        ;
    
}