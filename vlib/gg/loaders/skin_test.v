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
