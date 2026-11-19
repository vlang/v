// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import os
import json2
import encoding.base64
import encoding.binary

struct ObjIndex {
	position int
	uv       int
	normal   int
}

fn obj_index(raw string, count int) !int {
	if raw == '' {
		return -1
	}
	value := raw.int()
	if value > 0 {
		return value - 1
	}
	if value < 0 {
		return count + value
	}
	return -1
}

// load_mesh_obj loads a static triangle mesh from a Wavefront .obj file.
// Surface and quad faces are triangulated as fans. Missing normals become
// (0,1,0) and missing UVs become (0,0).
pub fn load_mesh_obj(path string) !Mesh3D {
	text := os.read_file(path) or { return error('gg: cannot read "${path}": ${err}') }
	mut positions := []Vec3{}
	mut normals := []Vec3{}
	mut uvs := []f32{}
	mut vertices := []Vec3{}
	mut out_normals := []Vec3{}
	mut out_uvs := []f32{}
	mut indices := []u32{}
	for raw_line in text.split('\n') {
		line := raw_line.trim_space()
		if line.len == 0 || line.starts_with('#') {
			continue
		}
		parts := line.fields()
		match parts[0] {
			'v' {
				if parts.len < 4 {
					continue
				}
				positions << Vec3{parts[1].f32(), parts[2].f32(), parts[3].f32()}
			}
			'vn' {
				if parts.len < 4 {
					continue
				}
				normals << Vec3{parts[1].f32(), parts[2].f32(), parts[3].f32()}
			}
			'vt' {
				if parts.len < 3 {
					continue
				}
				uvs << parts[1].f32()
				uvs << parts[2].f32()
			}
			'f' {
				if parts.len < 4 {
					continue
				}
				mut corners := []ObjIndex{}
				for token in parts[1..] {
					bits := token.split('/')
					pos := obj_index(bits[0], positions.len) or { return err }
					uv := if bits.len > 1 { obj_index(bits[1], uvs.len / 2) or { -1 } } else { -1 }
					norm := if bits.len > 2 {
						obj_index(bits[2], normals.len) or { -1 }
					} else {
						-1
					}
					if pos < 0 || pos >= positions.len {
						return error('gg: OBJ face references missing vertex')
					}
					corners << ObjIndex{pos, uv, norm}
				}
				for k in 1 .. corners.len - 1 {
					for corner in [corners[0], corners[k], corners[k + 1]] {
						vertices << positions[corner.position]
						out_normals << if corner.normal >= 0 && corner.normal < normals.len {
							normals[corner.normal]
						} else {
							Vec3{0, 1, 0}
						}
						if corner.uv >= 0 && corner.uv * 2 + 1 < uvs.len {
							out_uvs << uvs[corner.uv * 2]
							out_uvs << uvs[corner.uv * 2 + 1]
						} else {
							out_uvs << 0
							out_uvs << 0
						}
						indices << u32(vertices.len - 1)
					}
				}
			}
			else {}
		}
	}
	if vertices.len == 0 {
		return error('gg: no triangles found in "${path}"')
	}
	return Mesh3D{ vertices: vertices, normals: out_normals, uvs: out_uvs, indices: indices }
}

struct GltfBuffer {
	uri string
}

struct GltfBufferView {
	buffer      int
	byte_offset int @[json: 'byteOffset']
	byte_length int @[json: 'byteLength']
	byte_stride int @[json: 'byteStride']
}

struct GltfAccessor {
	buffer_view    int @[json: 'bufferView']
	byte_offset    int @[json: 'byteOffset']
	component_type int @[json: 'componentType']
	count          int
	typ            string @[json: 'type']
}

struct GltfAttributes {
	position  int = -1 @[json: 'POSITION']
	normal    int = -1 @[json: 'NORMAL']
	texcoord0 int = -1 @[json: 'TEXCOORD_0']
}

struct GltfPrimitive {
	attributes GltfAttributes
	indices    int = -1
}

struct GltfMesh {
	primitives []GltfPrimitive
}

struct GltfDoc {
	buffers      []GltfBuffer
	buffer_views []GltfBufferView @[json: 'bufferViews']
	accessors    []GltfAccessor
	meshes       []GltfMesh
}

fn gltf_buffer_data(doc &GltfDoc, dir string, index int) ![]u8 {
	if index < 0 || index >= doc.buffers.len {
		return error('gg: glTF buffer index out of range')
	}
	uri := doc.buffers[index].uri
	if uri.starts_with('data:') {
		comma := uri.index(',') or { return error('gg: malformed glTF data URI') }
		return base64.decode(uri[comma + 1..])
	}
	data := os.read_file(os.join_path(dir, uri)) or { return error('gg: cannot read glTF buffer: ${err}') }
	return data.bytes()
}

fn accessor_offset(doc &GltfDoc, accessor_index int) !(int, int, int) {
	if accessor_index < 0 || accessor_index >= doc.accessors.len {
		return error('gg: glTF accessor index out of range')
	}
	acc := doc.accessors[accessor_index]
	if acc.buffer_view < 0 || acc.buffer_view >= doc.buffer_views.len {
		return error('gg: glTF bufferView index out of range')
	}
	view := doc.buffer_views[acc.buffer_view]
	components := match acc.typ {
		'SCALAR' { 1 }
		'VEC2' { 2 }
		'VEC3' { 3 }
		else { return error('gg: unsupported glTF accessor type "${acc.typ}"') }
	}
	component_size := match acc.component_type {
		5121 { 1 }
		5123 { 2 }
		5125 { 4 }
		5126 { 4 }
		else { return error('gg: unsupported glTF component type') }
	}
	stride := if view.byte_stride > 0 { view.byte_stride } else { components * component_size }
	return view.buffer, view.byte_offset + acc.byte_offset, stride
}

fn accessor_vec3(doc &GltfDoc, buffers [][]u8, index int) ![]Vec3 {
	buffer_index, offset, stride := accessor_offset(doc, index)!
	if buffer_index < 0 || buffer_index >= buffers.len {
		return error('gg: glTF buffer missing')
	}
	acc := doc.accessors[index]
	if acc.component_type != 5126 || acc.typ != 'VEC3' {
		return error('gg: glTF VEC3 accessor must use float components')
	}
	data := buffers[buffer_index]
	mut out := []Vec3{cap: acc.count}
	for i in 0 .. acc.count {
		at := offset + i * stride
		if at + 12 > data.len {
			return error('gg: glTF accessor data truncated')
		}
		out << Vec3{binary.little_endian_f32_at(data, at), binary.little_endian_f32_at(data, at + 4), binary.little_endian_f32_at(data, at + 8)}
	}
	return out
}

fn accessor_uvs(doc &GltfDoc, buffers [][]u8, index int) ![]f32 {
	buffer_index, offset, stride := accessor_offset(doc, index)!
	if buffer_index < 0 || buffer_index >= buffers.len {
		return error('gg: glTF buffer missing')
	}
	acc := doc.accessors[index]
	data := buffers[buffer_index]
	mut out := []f32{cap: acc.count * 2}
	for i in 0 .. acc.count {
		at := offset + i * stride
		if at + 8 > data.len {
			return error('gg: glTF accessor data truncated')
		}
		out << binary.little_endian_f32_at(data, at)
		out << binary.little_endian_f32_at(data, at + 4)
	}
	return out
}

fn accessor_indices(doc &GltfDoc, buffers [][]u8, index int) ![]u32 {
	buffer_index, offset, stride := accessor_offset(doc, index)!
	if buffer_index < 0 || buffer_index >= buffers.len {
		return error('gg: glTF buffer missing')
	}
	acc := doc.accessors[index]
	data := buffers[buffer_index]
	mut out := []u32{cap: acc.count}
	for i in 0 .. acc.count {
		at := offset + i * stride
		match acc.component_type {
			5121 {
				if at >= data.len {
					return error('gg: glTF index data truncated')
				}
				out << u32(data[at])
			}
			5123 {
				if at + 2 > data.len {
					return error('gg: glTF index data truncated')
				}
				out << u32(binary.little_endian_u16_at(data, at))
			}
			5125 {
				if at + 4 > data.len {
					return error('gg: glTF index data truncated')
				}
				out << binary.little_endian_u32_at(data, at)
			}
			else { return error('gg: unsupported glTF index component type') }
		}
	}
	return out
}

// load_mesh_gltf loads static mesh primitives from a glTF 2.0 .gltf file.
// It supports embedded/base64 or external buffers, float POSITION/NORMAL/
// TEXCOORD_0 attributes and 8/16/32-bit indices. Animations, skins and
// textures are not loaded.
pub fn load_mesh_gltf(path string) ![]Mesh3D {
	text := os.read_file(path) or { return error('gg: cannot read "${path}": ${err}') }
	doc := json2.decode[GltfDoc](text) or { return error('gg: invalid glTF JSON: ${err}') }
	dir := os.dir(path)
	mut buffers := [][]u8{}
	for i in 0 .. doc.buffers.len {
		buffers << gltf_buffer_data(&doc, dir, i)!
	}
	mut result := []Mesh3D{}
	for mesh in doc.meshes {
		for prim in mesh.primitives {
			if prim.attributes.position < 0 {
				continue
			}
			verts := accessor_vec3(&doc, buffers, prim.attributes.position)!
			mut normals := []Vec3{}
			if prim.attributes.normal >= 0 {
				normals = accessor_vec3(&doc, buffers, prim.attributes.normal)!
			}
			mut uvs := []f32{}
			if prim.attributes.texcoord0 >= 0 {
				uvs = accessor_uvs(&doc, buffers, prim.attributes.texcoord0)!
			}
			mut indices := []u32{}
			if prim.indices >= 0 {
				indices = accessor_indices(&doc, buffers, prim.indices)!
			} else {
				for i in 0 .. verts.len {
					indices << u32(i)
				}
			}
			result << Mesh3D{ vertices: verts, normals: normals, uvs: uvs, indices: indices }
		}
	}
	if result.len == 0 {
		return error('gg: no mesh primitives found in "${path}"')
	}
	return result
}
