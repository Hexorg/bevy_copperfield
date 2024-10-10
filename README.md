`bevy_copperfield` is a [Bevy](https://github.com/bevyengine/bevy) plugin for procedural modelling, inspired by [Blender's geometry nodes](https://docs.blender.org/manual/en/latest/modeling/geometry_nodes/index.html). Currently at its infancy, but `bevy_coperfield` allows you to create and edit meshes in non-destructive manner. 

Example box from Cuboid:
![Example extruded and chamfered Cuboid](https://github.com/Hexorg/bevy_copperfield/blob/main/extrude_bevel_debug.png)

# Approach

`bevy_copperfield` implements a [Half-Edge Mesh data-structure](https://www.flipcode.com/archives/The_Half-Edge_Data_Structure.shtml) which allows us to quickly navigate and edit the mesh, providing methods to extrude, subdivide, and bevel parts of the mesh. The debug eample provides a nice visualization of the internal data-structure implemented, as each drawn edge is a pointer to the next one.

![Simple half-edge mesh](https://github.com/Hexorg/bevy_copperfield/blob/main/sample_mesh.png)

# Usage
`bevy_copperfield` is still is its early stages of development, but its key goals is to enable seamless use in Bevy. Upon adding `bevy_copperfield` to your repository, supported Bevy 3D primitives (currently just `Cuboid`) will allow you to spawn editable mesh with `.procgen()`. From there you will be able to chain series of edit nodes to turn primitives into objects you want. See [examples](https://github.com/hexorg/bevy_copperfield/tree/latest/examples) for sample use.