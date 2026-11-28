module loaders

import gg

// load_model loads the first mesh of an OBJ, glTF or GLB file into a Model3D.
// Meshes from multi-primitive assets beyond the first are not loaded.
pub fn load_model(path string) !gg.Model3D {
	mut meshes := []gg.Mesh3D{}
	if path.ends_with('.glb') {
		meshes = load_mesh_glb(path)!
	} else if path.ends_with('.gltf') {
		meshes = load_mesh_gltf(path)!
	} else if path.ends_with('.obj') {
		meshes << load_mesh_obj(path)!
	} else {
		return error('gg.E0013: load_model supports .obj, .gltf and .glb: ${path}')
	}
	if meshes.len == 0 {
		return error('gg: no meshes found in "${path}"')
	}
	first := meshes[0]
	return gg.Model3D{ mesh: &first }
}
