module loaders

import os
import time

fn test_scene_gltf_import() {
	json := '{
		"buffers":[{"uri":"data:application/octet-stream;base64,AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}],
		"bufferViews":[{"buffer":0,"byteOffset":0,"byteLength":36}],
		"accessors":[{"bufferView":0,"byteOffset":0,"componentType":5126,"count":3,"type":"VEC3"}],
		"meshes":[{"primitives":[{"attributes":{"POSITION":0}}]}],
		"materials":[{
			"name":"red",
			"pbrMetallicRoughness":{"baseColorFactor":[1,0,0,1],"metallicFactor":0.2,"roughnessFactor":0.7},
			"emissiveFactor":[0.1,0.1,0.1]
		}],
		"nodes":[{"name":"root","mesh":0,"translation":[1,2,3],"scale":[2,2,2],"children":[]}],
		"cameras":[{"name":"main","type":"perspective","perspective":{"yfov":50,"znear":0.2,"zfar":500}}],
		"images":[{"uri":"tex.png"}],
		"KHR_lights_punctual":[{"name":"sun","color":[1,1,1],"intensity":2,"type":"directional"}]
	}'
	path := os.join_path(os.temp_dir(), 'gg_scene_${time.now().unix_nano()}.gltf')
	os.write_file(path, json) or { panic('write failed') }
	defer { os.rm(path) or {} }
	asset := load_scene_gltf(path) or { panic(err.msg()) }
	assert asset.meshes.len == 1
	assert asset.materials.len == 1
	assert asset.materials[0].name == 'red'
	assert asset.materials[0].base_color.r == 255
	assert asset.materials[0].metallic == 0.2
	assert asset.nodes.len == 1
	assert asset.nodes[0].translation.x == 1
	assert asset.nodes[0].scale.x == 2
	assert asset.cameras.len == 1
	assert asset.cameras[0].fovy == 50
	assert asset.lights.len == 1
	assert asset.lights[0].intensity == 2
	assert asset.textures.len == 1
}
