// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module loaders

import gg
import gg.anim
import json2
import math
import os
import encoding.base64

struct AnimBuffer {
	uri string
}

struct AnimBufferView {
	buffer      int
	byte_offset int @[json: 'byteOffset']
	byte_length int @[json: 'byteLength']
}

struct AnimAccessor {
	buffer_view    int @[json: 'bufferView']
	byte_offset    int @[json: 'byteOffset']
	component_type int @[json: 'componentType']
	count          int
	typ            string @[json: 'type']
}

struct AnimNode {
	name     string
	children []int
	skin     int = -1
}

struct AnimSkin {
	joints                []int
	inverse_bind_matrices int @[json: 'inverseBindMatrices']
}

struct AnimSampler {
	input         int
	output        int
	interpolation string = 'LINEAR'
}

struct AnimChannel {
	sampler int
	target  AnimTarget
}

struct AnimTarget {
	node int
	path string
}

struct AnimAnimation {
	name     string
	samplers []AnimSampler
	channels []AnimChannel
}

struct AnimDoc {
	buffers      []AnimBuffer
	buffer_views []AnimBufferView @[json: 'bufferViews']
	accessors    []AnimAccessor
	nodes        []AnimNode
	skins        []AnimSkin
	animations   []AnimAnimation
}

// Skin is an imported skeleton: joints with parent indices and 4x4 inverse
// bind matrices stored as 16 floats each in column-major order.
pub struct Skin {
pub:
	joints       []anim.Joint
	inverse_bind []f32
}

fn anim_buffer_data(doc &AnimDoc, dir string, index int) ![]u8 {
	if index < 0 || index >= doc.buffers.len {
		return error('gg: glTF buffer index out of range')
	}
	uri := doc.buffers[index].uri
	if uri.starts_with('data:') {
		comma := uri.index(',') or { return error('gg: malformed glTF data URI') }
		return base64.decode(uri[comma + 1..])
	}
	// A glTF buffer URI must stay inside the asset's directory: reject
	// absolute paths and parent-directory traversal, as mesh_loader.v does.
	if uri.starts_with('/') || uri.contains('..') || uri.contains('\\') {
		return error('gg: glTF buffer URI escapes the asset directory')
	}
	return os.read_bytes(os.join_path(dir, uri))
}

fn read_float_accessor(doc &AnimDoc, dir string, index int, components int) ![]f32 {
	if index < 0 || index >= doc.accessors.len {
		return error('gg: glTF accessor index out of range')
	}
	acc := doc.accessors[index]
	if acc.component_type != 5126 {
		return error('gg: only FLOAT animation accessors are supported')
	}
	if acc.count < 0 || components < 0 {
		return error('gg: glTF accessor has a negative count')
	}
	if acc.buffer_view < 0 || acc.buffer_view >= doc.buffer_views.len {
		return error('gg: glTF bufferView index out of range')
	}
	view := doc.buffer_views[acc.buffer_view]
	if view.byte_offset < 0 || view.byte_length < 0 || acc.byte_offset < 0 {
		return error('gg: glTF accessor has a negative byte offset or length')
	}
	start := i64(view.byte_offset) + i64(acc.byte_offset)
	// `count` comes from untrusted JSON. Bound the whole accessor before
	// allocating so a huge count cannot drive an enormous allocation first.
	needed := i64(acc.count) * i64(components) * 4
	if view.byte_length > 0 {
		if i64(acc.byte_offset) >= i64(view.byte_length) {
			return error('gg: glTF accessor starts outside its bufferView')
		}
		if needed > i64(view.byte_length) - i64(acc.byte_offset) {
			return error('gg: glTF accessor extends outside its bufferView')
		}
	}
	data := anim_buffer_data(doc, dir, view.buffer)!
	if start < 0 || start + needed > i64(data.len) {
		return error('gg: glTF accessor data out of bounds')
	}
	mut out := []f32{len: acc.count * components}
	for i in 0 .. out.len {
		at := int(start) + i * 4
		if at + 4 > data.len {
			return error('gg: glTF accessor data out of bounds')
		}
		bits := u32(data[at]) | (u32(data[at + 1]) << 8) | (u32(data[at + 2]) << 16) |
			(u32(data[at + 3]) << 24)
		out[i] = math.f32_from_bits(bits)
	}
	return out
}

fn anim_doc(path string) !AnimDoc {
	text := os.read_file(path)!
	doc := json2.decode[AnimDoc](text) or { return error('gg: invalid glTF animation JSON: ${err}') }
	return doc
}

// load_gltf_skin imports the first skin of a glTF file, or none.
pub fn load_gltf_skin(path string) !Skin {
	doc := anim_doc(path)!
	if doc.skins.len == 0 {
		return error('gg: glTF has no skins')
	}
	skin := doc.skins[0]
	mut joints := []anim.Joint{}
	for node_index in skin.joints {
		if node_index < 0 || node_index >= doc.nodes.len {
			return error('gg: glTF skin joint out of range')
		}
		node := doc.nodes[node_index]
		mut parent := -1
		for i, candidate in doc.nodes {
			if node_index in candidate.children {
				parent = i
				break
			}
		}
		joints << anim.Joint{ name: node.name, parent: parent }
	}
	bind := if skin.inverse_bind_matrices >= 0 {
		read_float_accessor(&doc, os.dir(path), skin.inverse_bind_matrices, 16)!
	} else {
		[]f32{}
	}
	if bind.len % 16 != 0 {
		return error('gg: glTF inverse bind matrices must contain 16 floats per matrix')
	}
	return Skin{ joints: joints, inverse_bind: bind }
}

// load_gltf_clips imports every animation clip of a glTF file. Rotation
// tracks are converted from quaternion keyframes to Euler angles for the
// lightweight anim.AnimClip model.
pub fn load_gltf_clips(path string) ![]&anim.AnimClip {
	doc := anim_doc(path)!
	mut clips := []&anim.AnimClip{}
	dir := os.dir(path)
	for source in doc.animations {
		mut duration := f32(0)
		for channel in source.channels {
			if channel.sampler < 0 || channel.sampler >= source.samplers.len {
				continue
			}
			sampler := source.samplers[channel.sampler]
			// One malformed accessor must not abort every clip in the file.
			times := read_float_accessor(&doc, dir, sampler.input, 1) or { continue }
			if times.len > 0 && times[times.len - 1] > duration {
				duration = times[times.len - 1]
			}
		}
		mut tracks := []anim.AnimTrack{}
		for channel in source.channels {
			if channel.sampler < 0 || channel.sampler >= source.samplers.len {
				continue
			}
			// A track pointing at a missing node cannot be animated.
			if channel.target.node < 0 || channel.target.node >= doc.nodes.len {
				continue
			}
			sampler := source.samplers[channel.sampler]
			times := read_float_accessor(&doc, dir, sampler.input, 1) or { continue }
			components := if channel.target.path == 'rotation' { 4 } else { 3 }
			values := read_float_accessor(&doc, dir, sampler.output, components) or { continue }
			if times.len == 0 || values.len < times.len * components {
				continue
			}
			mut keys := []anim.AnimKey{}
			for i, t in times {
				mut translation := gg.Vec3{}
				mut rotation := gg.Vec3{}
				mut scale := gg.Vec3{1, 1, 1}
				if channel.target.path == 'translation' {
					translation = gg.Vec3{values[i * 3], values[i * 3 + 1], values[i * 3 + 2]}
				} else if channel.target.path == 'scale' {
					scale = gg.Vec3{values[i * 3], values[i * 3 + 1], values[i * 3 + 2]}
				} else if channel.target.path == 'rotation' {
					q := anim.Quat{values[i * 4], values[i * 4 + 1], values[i * 4 + 2], values[i * 4 + 3]}
					rotation = anim.quat_to_euler(q)
				}
				keys << anim.AnimKey{t, translation, rotation, scale}
			}
			interp := if sampler.interpolation == 'STEP' {
				anim.AnimInterp.step
			} else {
				anim.AnimInterp.linear
			}
			tracks << anim.AnimTrack{ joint: channel.target.node, keys: keys, interp: interp }
		}
		clips << &anim.AnimClip{ name: source.name, duration: duration, tracks: tracks }
	}
	return clips
}
