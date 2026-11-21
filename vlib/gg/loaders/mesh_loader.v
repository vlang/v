// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module loaders

import gg
import os
import math
import json2
import encoding.base64
import encoding.binary

struct ObjIndex {
	position int
	uv       int
	normal   int
}

// parse_obj_float validates the token and rejects values that overflow f32
// (e.g. "1e400" becomes +Inf, which would poison later math with NaN).
fn parse_obj_float(raw string) !f32 {
	if !is_float_token(raw) {
		return error('gg: invalid OBJ number "${raw}"')
	}
	value := raw.f32()
	if !math.is_finite(f64(value)) {
		return error('gg: OBJ number "${raw}" is not finite')
	}
	return value
}

// is_integer_token rejects V's lenient int() behaviour, which accepts a
// numeric prefix and trailing garbage ("12x" -> 12, "abc" -> 0).
fn is_integer_token(s string) bool {
	if s.len == 0 {
		return false
	}
	mut i := 0
	if s[0] == 45 || s[0] == 43 { // '-' or '+'
		i = 1
	}
	if i >= s.len {
		return false
	}
	for i < s.len {
		c := s[i]
		if c < 48 || c > 57 { // '0'..'9'
			return false
		}
		i++
	}
	return true
}

// is_float_token guards .f32() against the same lenient prefix parsing.
fn is_float_token(s string) bool {
	if s.len == 0 {
		return false
	}
	mut i := 0
	if s[0] == 45 || s[0] == 43 {
		i = 1
	}
	mut digits := 0
	mut dot := false
	mut exp := false
	for i < s.len {
		c := s[i]
		if c >= 48 && c <= 57 {
			digits++
			i++
		} else if c == 46 && !dot && !exp { // '.'
			dot = true
			i++
		} else if (c == 101 || c == 69) && !exp && digits > 0 { // 'e' or 'E'
			exp = true
			i++
			if i < s.len && (s[i] == 45 || s[i] == 43) {
				i++
			}
			if i >= s.len {
				return false
			}
		} else {
			return false
		}
	}
	return digits > 0
}

fn obj_index(raw string, count int) !int {
	if raw == '' {
		return -1
	}
	if !is_integer_token(raw) {
		return error('gg: invalid OBJ face index "${raw}"')
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
pub fn load_mesh_obj(path string) !gg.Mesh3D {
	text := os.read_file(path) or { return error('gg: cannot read "${path}": ${err}') }
	mut positions := []gg.Vec3{}
	mut normals := []gg.Vec3{}
	mut uvs := []f32{}
	mut vertices := []gg.Vec3{}
	mut out_normals := []gg.Vec3{}
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
				x := parse_obj_float(parts[1]) or { return err }
				y := parse_obj_float(parts[2]) or { return err }
				z := parse_obj_float(parts[3]) or { return err }
				positions << gg.Vec3{x, y, z}
			}
			'vn' {
				if parts.len < 4 {
					continue
				}
				x := parse_obj_float(parts[1]) or { return err }
				y := parse_obj_float(parts[2]) or { return err }
				z := parse_obj_float(parts[3]) or { return err }
				normals << gg.Vec3{x, y, z}
			}
			'vt' {
				if parts.len < 3 {
					continue
				}
				u := parse_obj_float(parts[1]) or { return err }
				v := parse_obj_float(parts[2]) or { return err }
				uvs << u
				uvs << v
			}
			'f' {
				if parts.len < 4 {
					continue
				}
				mut corners := []ObjIndex{}
				for token in parts[1..] {
					bits := token.split('/')
					pos := obj_index(bits[0], positions.len) or { return err }
					uv := if bits.len > 1 {
						obj_index(bits[1], uvs.len / 2) or { return err }
					} else {
						-1
					}
					norm := if bits.len > 2 {
						obj_index(bits[2], normals.len) or { return err }
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
							gg.Vec3{0, 1, 0}
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
		return error('gg.E0010: no triangles found in "${path}" — check that the file has f lines')
	}
	return gg.Mesh3D{ vertices: vertices, normals: out_normals, uvs: out_uvs, indices: indices }
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
	// -1 means the attribute is absent. Zero is a valid accessor index and
	// must not be used as a presence sentinel.
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
	// A glTF buffer URI must stay inside the asset's directory: reject
	// absolute paths and parent-directory traversal.
	if uri.starts_with('/') || uri.contains('..') || uri.contains('\\') {
		return error('gg: glTF buffer URI escapes the asset directory')
	}
	data := os.read_file(os.join_path(dir, uri)) or { return error('gg: cannot read glTF buffer: ${err}') }
	return data.bytes()
}

fn accessor_offset(doc &GltfDoc, accessor_index int) !(int, int, int) {
	if accessor_index < 0 || accessor_index >= doc.accessors.len {
		return error('gg: glTF accessor index out of range')
	}
	acc := doc.accessors[accessor_index]
	if acc.count < 0 {
		return error('gg: glTF accessor has a negative count')
	}
	if acc.buffer_view < 0 || acc.buffer_view >= doc.buffer_views.len {
		return error('gg: glTF bufferView index out of range')
	}
	view := doc.buffer_views[acc.buffer_view]
	if view.buffer < 0 || view.buffer >= doc.buffers.len {
		return error('gg: glTF buffer index out of range')
	}
	if view.byte_offset < 0 || acc.byte_offset < 0 || view.byte_stride < 0 {
		return error('gg: glTF accessor has a negative byte offset or stride')
	}
	offset := view.byte_offset + acc.byte_offset
	// byteLength is relative to the view's own byteOffset, not the buffer.
	if view.byte_length > 0 && acc.byte_offset >= view.byte_length {
		return error('gg: glTF accessor starts outside its bufferView')
	}
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
	// The whole accessor must fit inside its bufferView, not just its start.
	// This also bounds count, which prevents a huge count from driving an
	// enormous allocation before the per-element data check runs.
	if acc.count > 0 && view.byte_length > 0 {
		last := acc.byte_offset + (acc.count - 1) * stride + components * component_size
		if last > view.byte_length {
			return error('gg: glTF accessor extends outside its bufferView')
		}
	}
	return view.buffer, offset, stride
}

fn accessor_vec3(doc &GltfDoc, buffers [][]u8, index int) ![]gg.Vec3 {
	buffer_index, offset, stride := accessor_offset(doc, index)!
	if buffer_index < 0 || buffer_index >= buffers.len {
		return error('gg: glTF buffer missing')
	}
	acc := doc.accessors[index]
	if acc.component_type != 5126 || acc.typ != 'VEC3' {
		return error('gg: glTF VEC3 accessor must use float components')
	}
	data := buffers[buffer_index]
	if acc.count > data.len / 4 + 1 {
		return error('gg: glTF accessor count exceeds the buffer size')
	}
	mut out := []gg.Vec3{cap: acc.count}
	for i in 0 .. acc.count {
		at := offset + i * stride
		if at < 0 || at + 12 > data.len {
			return error('gg: glTF accessor data truncated')
		}
		out << gg.Vec3{binary.little_endian_f32_at(data, at), binary.little_endian_f32_at(data, at + 4), binary.little_endian_f32_at(data, at + 8)}
	}
	return out
}

fn accessor_uvs(doc &GltfDoc, buffers [][]u8, index int) ![]f32 {
	buffer_index, offset, stride := accessor_offset(doc, index)!
	if buffer_index < 0 || buffer_index >= buffers.len {
		return error('gg: glTF buffer missing')
	}
	acc := doc.accessors[index]
	if acc.component_type != 5126 || acc.typ != 'VEC2' {
		return error('gg: glTF TEXCOORD_0 accessor must use float VEC2 components')
	}
	data := buffers[buffer_index]
	if acc.count > data.len / 4 + 1 {
		return error('gg: glTF accessor count exceeds the buffer size')
	}
	mut out := []f32{cap: acc.count * 2}
	for i in 0 .. acc.count {
		at := offset + i * stride
		if at < 0 || at + 8 > data.len {
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
	if acc.typ != 'SCALAR' {
		return error('gg: glTF index accessor must be SCALAR')
	}
	data := buffers[buffer_index]
	if acc.count > data.len + 1 {
		return error('gg: glTF accessor count exceeds the buffer size')
	}
	mut out := []u32{cap: acc.count}
	for i in 0 .. acc.count {
		at := offset + i * stride
		if at < 0 {
			return error('gg: glTF index data has a negative offset')
		}
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
pub fn load_mesh_gltf(path string) ![]gg.Mesh3D {
	text := os.read_file(path) or { return error('gg: cannot read "${path}": ${err}') }
	doc := json2.decode[GltfDoc](text) or { return error('gg.E0011: invalid glTF JSON in "${path}": ${err}') }
	dir := os.dir(path)
	mut buffers := [][]u8{}
	for i in 0 .. doc.buffers.len {
		buffers << gltf_buffer_data(&doc, dir, i)!
	}
	return meshes_from_gltf(&doc, buffers, path)
}

fn meshes_from_gltf(doc &GltfDoc, buffers [][]u8, path string) ![]gg.Mesh3D {
	mut result := []gg.Mesh3D{}
	for mesh in doc.meshes {
		for prim in mesh.primitives {
			if prim.attributes.position < 0 {
				continue
			}
			verts := accessor_vec3(doc, buffers, prim.attributes.position)!
			mut normals := []gg.Vec3{}
			if prim.attributes.normal >= 0 {
				normals = accessor_vec3(doc, buffers, prim.attributes.normal)!
			}
			mut uvs := []f32{}
			if prim.attributes.texcoord0 >= 0 {
				uvs = accessor_uvs(doc, buffers, prim.attributes.texcoord0)!
			}
			mut indices := []u32{}
			if prim.indices >= 0 {
				indices = accessor_indices(doc, buffers, prim.indices)!
			} else {
				for i in 0 .. verts.len {
					indices << u32(i)
				}
			}
			result << gg.Mesh3D{ vertices: verts, normals: normals, uvs: uvs, indices: indices }
		}
	}
	if result.len == 0 {
		return error('gg: no mesh primitives found in "${path}"')
	}
	return result
}

// load_mesh_glb loads static meshes from a binary glTF 2.0 .glb file.
// The JSON and BIN chunks are read from the same file; no external resources
// are required. Skins, animations and texture images are not loaded.
pub fn load_mesh_glb(path string) ![]gg.Mesh3D {
	bytes := os.read_file(path) or { return error('gg: cannot read "${path}": ${err}') }
	data := bytes.bytes()
	if data.len < 20 {
		return error('gg: GLB file is too short')
	}
	magic := binary.little_endian_u32_at(data, 0)
	if magic != u32(0x46546c67) {
		return error('gg.E0012: "${path}" is not a GLB file (bad magic) — use load_mesh_gltf for .gltf')
	}
	version := binary.little_endian_u32_at(data, 4)
	if version != 2 {
		return error('gg: unsupported GLB version ${version}')
	}
	mut json_bytes := []u8{}
	mut bin_bytes := []u8{}
	mut pos := 12
	for pos + 8 <= data.len {
		chunk_len := int(binary.little_endian_u32_at(data, pos))
		chunk_type := binary.little_endian_u32_at(data, pos + 4)
		chunk_start := i64(pos) + 8
		chunk_end := chunk_start + i64(chunk_len)
		if chunk_len < 0 || chunk_end > i64(data.len) {
			return error('gg: truncated GLB chunk')
		}
		start := int(chunk_start)
		end := int(chunk_end)
		chunk := data[start..end]
		if chunk_type == u32(0x4e4f534a) {
			json_bytes = chunk.clone()
		} else if chunk_type == u32(0x004e4942) {
			bin_bytes = chunk.clone()
		}
		pos = end
	}
	if json_bytes.len == 0 {
		return error('gg: GLB has no JSON chunk')
	}
	doc := json2.decode[GltfDoc](json_bytes.bytestr()) or { return error('gg: invalid GLB JSON: ${err}') }
	mut buffers := [][]u8{}
	for i in 0 .. doc.buffers.len {
		if i == 0 && bin_bytes.len > 0 {
			buffers << bin_bytes
		} else {
			buffers << gltf_buffer_data(&doc, os.dir(path), i)!
		}
	}
	return meshes_from_gltf(&doc, buffers, path)
}
