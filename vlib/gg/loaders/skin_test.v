module loaders

import encoding.base64
import os
import time

fn test_gltf_skin_and_clips() {
	// Buffer: times [0, 1], translation [0,0,0, 1,0,0].
	mut bytes := []u8{}
	bytes << [u8(0), 0, 0, 0]
	bytes << [u8(0), 0, 0x80, 0x3F]
	bytes << [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
	bytes << [u8(0), 0, 0x80, 0x3F, 0, 0, 0, 0, 0, 0, 0, 0]
	uri := 'data:application/octet-stream;base64,' + base64.encode(bytes)
	json := '{
		"buffers":[{"uri":"${uri}"}],
		"bufferViews":[{"buffer":0,"byteOffset":0,"byteLength":32}],
		"accessors":[
			{"bufferView":0,"byteOffset":0,"componentType":5126,"count":2,"type":"SCALAR"},
			{"bufferView":0,"byteOffset":8,"componentType":5126,"count":2,"type":"VEC3"}
		],
		"nodes":[{"name":"root"},{"name":"child"}],
		"skins":[{"joints":[0,1],"inverseBindMatrices":-1}],
		"animations":[{
			"name":"move",
			"samplers":[{"input":0,"output":1,"interpolation":"LINEAR"}],
			"channels":[{"sampler":0,"target":{"node":0,"path":"translation"}}]
		}]
	}'
	path := os.join_path(os.temp_dir(), 'gg_skin_${time.now().unix_nano()}.gltf')
	os.write_file(path, json) or { panic('write failed') }
	defer { os.rm(path) or {} }

	s := load_gltf_skin(path) or { panic('skin failed') }
	assert s.joints.len == 2
	assert s.joints[0].name == 'root'

	clips := load_gltf_clips(path) or { panic('clips failed') }
	assert clips.len == 1
	assert clips[0].name == 'move'
	assert clips[0].duration == 1
	assert clips[0].tracks.len == 1
	assert clips[0].tracks[0].keys.len == 2
}

fn test_clips_skip_bad_channels_without_dropping_clip() {
	mut bytes := []u8{}
	bytes << [u8(0), 0, 0, 0]
	bytes << [u8(0), 0, 0x80, 0x3F]
	bytes << [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
	bytes << [u8(0), 0, 0x80, 0x3F, 0, 0, 0, 0, 0, 0, 0, 0]
	uri := 'data:application/octet-stream;base64,' + base64.encode(bytes)
	json := '{
		"buffers":[{"uri":"${uri}"}],
		"bufferViews":[{"buffer":0,"byteOffset":0,"byteLength":32}],
		"accessors":[
			{"bufferView":0,"byteOffset":0,"componentType":5126,"count":2,"type":"SCALAR"},
			{"bufferView":0,"byteOffset":8,"componentType":5126,"count":2,"type":"VEC3"}
		],
		"nodes":[{"name":"root"}],
		"animations":[{
			"name":"mostly_valid",
			"samplers":[
				{"input":0,"output":1,"interpolation":"LINEAR"},
				{"input":99,"output":99}
			],
			"channels":[
				{"sampler":1,"target":{"node":0,"path":"translation"}},
				{"sampler":0,"target":{"node":0,"path":"translation"}},
				{"sampler":0,"target":{"node":99,"path":"translation"}}
			]
		}]
	}'
	path := os.join_path(os.temp_dir(), 'gg_skin_skip_${time.now().unix_nano()}.gltf')
	os.write_file(path, json) or { panic('write failed') }
	defer { os.rm(path) or {} }
	clips := load_gltf_clips(path) or { panic('clips failed') }
	assert clips.len == 1
	assert clips[0].name == 'mostly_valid'
	// The bad sampler and the out-of-range target node are both skipped.
	assert clips[0].tracks.len == 1
	assert clips[0].tracks[0].keys.len == 2
}

fn test_skin_rejects_unsafe_gltf() {
	dir := os.join_path(os.temp_dir(), 'gg_skin_unsafe_${time.now().unix_nano()}')
	os.mkdir_all(dir) or { panic('mkdir failed') }
	defer { os.rmdir_all(dir) or {} }

	// A parent-directory buffer URI must not escape the asset directory.
	escape_json := '{
		"buffers":[{"uri":"../secret.bin"}],
		"bufferViews":[{"buffer":0,"byteOffset":0,"byteLength":64}],
		"accessors":[{"bufferView":0,"byteOffset":0,"componentType":5126,"count":1,"type":"MAT4"}],
		"nodes":[{"name":"j"}],
		"skins":[{"joints":[0],"inverseBindMatrices":0}]
	}'
	escape_path := os.join_path(dir, 'escape.gltf')
	os.write_file(escape_path, escape_json) or { panic('write failed') }
	if _ := load_gltf_skin(escape_path) {
		assert false, 'path traversal was accepted'
	}

	// A huge untrusted count must be rejected before allocating count*16 floats.
	uri := 'data:application/octet-stream;base64,' + base64.encode([u8(0), 0, 0, 0])
	huge_json := '{
		"buffers":[{"uri":"${uri}"}],
		"bufferViews":[{"buffer":0,"byteOffset":0,"byteLength":4}],
		"accessors":[{"bufferView":0,"byteOffset":0,"componentType":5126,"count":100000000,"type":"MAT4"}],
		"nodes":[{"name":"j"}],
		"skins":[{"joints":[0],"inverseBindMatrices":0}]
	}'
	huge_path := os.join_path(dir, 'huge.gltf')
	os.write_file(huge_path, huge_json) or { panic('write failed') }
	if _ := load_gltf_skin(huge_path) {
		assert false, 'huge accessor count was accepted'
	}
}
