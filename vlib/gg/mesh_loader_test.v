module gg

import os
import encoding.base64
import encoding.binary

fn test_load_mesh_obj() {
	dir := os.vtmp_dir()
	defer { os.rmdir(dir) or {} }
	path := os.join_path(dir, 'tri.obj')
	content := 'v 0 0 0\nv 1 0 0\nv 0 1 0\nvt 0 0\nvt 1 0\nvt 0 1\nvn 0 0 1\nf 1/1/1 2/2/1 3/3/1\n'
	os.write_file(path, content)!
	mesh := load_mesh_obj(path)!
	assert mesh.valid()
	assert mesh.vertices.len == 3
	assert mesh.indices == [u32(0), 1, 2]!
	assert mesh.normals[0].z == 1
	assert mesh.uvs.len == 6
}

fn test_load_mesh_obj_quad_is_triangulated() {
	dir := os.vtmp_dir()
	defer { os.rmdir(dir) or {} }
	path := os.join_path(dir, 'quad.obj')
	content := 'v 0 0 0\nv 1 0 0\nv 1 1 0\nv 0 1 0\nf 1 2 3 4\n'
	os.write_file(path, content)!
	mesh := load_mesh_obj(path)!
	assert mesh.vertices.len == 6
	assert mesh.indices.len == 6
}

fn test_load_mesh_glb() {
	dir := os.vtmp_dir()
	defer { os.rmdir(dir) or {} }
	mut bin := []u8{len: 42}
	for i, value in [f32(0), 0, 0, 1, 0, 0, 0, 1, 0] {
		binary.little_endian_put_f32_at(mut bin, value, i * 4)
	}
	for i, value in [u16(0), 1, 2] {
		binary.little_endian_put_u16_at(mut bin, value, 36 + i * 2)
	}
	json_text := '{"buffers":[{"byteLength":42}],' +
		'"bufferViews":[{"buffer":0,"byteOffset":0,"byteLength":36},' +
		'{"buffer":0,"byteOffset":36,"byteLength":6}],' +
		'"accessors":[{"bufferView":0,"componentType":5126,"count":3,"type":"VEC3"},' +
		'{"bufferView":1,"componentType":5123,"count":3,"type":"SCALAR"}],' +
		'"meshes":[{"primitives":[{"attributes":{"POSITION":0},"indices":1}]}]}'
	json_data := json_text.bytes()
	mut padded_json := json_data.clone()
	for padded_json.len % 4 != 0 {
		padded_json << u8(0x20)
	}
	mut chunk_hdr := []u8{len: 8}
	binary.little_endian_put_u32_at(mut chunk_hdr, u32(padded_json.len), 0)
	binary.little_endian_put_u32_at(mut chunk_hdr, u32(0x4e4f534a), 4)
	mut bin_hdr := []u8{len: 8}
	binary.little_endian_put_u32_at(mut bin_hdr, u32(bin.len), 0)
	binary.little_endian_put_u32_at(mut bin_hdr, u32(0x004e4942), 4)
	total := 12 + chunk_hdr.len + padded_json.len + bin_hdr.len + bin.len
	mut glb := []u8{len: 12}
	binary.little_endian_put_u32_at(mut glb, u32(0x46546c67), 0)
	binary.little_endian_put_u32_at(mut glb, 2, 4)
	binary.little_endian_put_u32_at(mut glb, u32(total), 8)
	glb << chunk_hdr
	glb << padded_json
	glb << bin_hdr
	glb << bin
	path := os.join_path(dir, 'tri.glb')
	os.write_file(path, glb.bytestr())!
	meshes := load_mesh_glb(path)!
	assert meshes.len == 1
	assert meshes[0].vertices.len == 3
	assert meshes[0].indices == [u32(0), 1, 2]!
}

fn test_load_mesh_gltf() {
	dir := os.vtmp_dir()
	defer { os.rmdir(dir) or {} }
	mut bin := []u8{len: 42}
	for i, value in [f32(0), 0, 0, 1, 0, 0, 0, 1, 0] {
		binary.little_endian_put_f32_at(mut bin, value, i * 4)
	}
	for i, value in [u16(0), 1, 2] {
		binary.little_endian_put_u16_at(mut bin, value, 36 + i * 2)
	}
	uri := base64.encode(bin)
	json_text := '{"buffers":[{"uri":"data:application/octet-stream;base64,${uri}","byteLength":42}],' +
		'"bufferViews":[{"buffer":0,"byteOffset":0,"byteLength":36},' +
		'{"buffer":0,"byteOffset":36,"byteLength":6}],' +
		'"accessors":[{"bufferView":0,"componentType":5126,"count":3,"type":"VEC3"},' +
		'{"bufferView":1,"componentType":5123,"count":3,"type":"SCALAR"}],' +
		'"meshes":[{"primitives":[{"attributes":{"POSITION":0},"indices":1}]}]}'
	path := os.join_path(dir, 'tri.gltf')
	os.write_file(path, json_text)!
	meshes := load_mesh_gltf(path)!
	assert meshes.len == 1
	mesh := meshes[0]
	assert mesh.valid()
	assert mesh.vertices.len == 3
	assert mesh.indices == [u32(0), 1, 2]!
	assert mesh.vertices[1].x == 1
}
