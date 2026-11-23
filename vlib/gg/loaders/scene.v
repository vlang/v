module loaders

import gg
import json2
import os

// SceneMaterial is a glTF 2.0 PBR material reduced to the fields gg can use.
pub struct SceneMaterial {
pub:
	name       string
	base_color gg.Color
	emissive   gg.Color
	metallic   f32
	roughness  f32
	texture    int = -1
}

// SceneNode is one imported node with a local transform and optional mesh.
pub struct SceneNode {
pub:
	name        string
	mesh        int = -1
	skin        int = -1
	camera      int = -1
	light       int = -1
	translation gg.Vec3
	rotation    [4]f32  = [f32(0), 0, 0, 1]!
	scale       gg.Vec3 = gg.Vec3{1, 1, 1}
	children    []int
}

// SceneCamera is a perspective or orthographic camera definition.
pub struct SceneCamera {
pub:
	name  string
	fovy  f32 = 60
	near  f32 = 0.1
	far   f32 = 1000
	ortho bool
}

// SceneLight is a punctual light from KHR_lights_punctual.
pub struct SceneLight {
pub:
	name      string
	color     gg.Color = gg.white
	intensity f32      = 1
	kind      string
}

// SceneAsset bundles everything needed to instantiate a glTF scene.
pub struct SceneAsset {
pub:
	meshes    []gg.Mesh3D
	materials []SceneMaterial
	nodes     []SceneNode
	cameras   []SceneCamera
	lights    []SceneLight
	textures  []string
}

struct DocMaterial {
	name                   string
	pbr_metallic_roughness DocPbr @[json: 'pbrMetallicRoughness']
	emissive_factor        []f32  @[json: 'emissiveFactor']
	base_color_texture     DocTex @[json: 'baseColorTexture']
}

struct DocPbr {
	base_color_factor  []f32  @[json: 'baseColorFactor']
	metallic_factor    f32    @[json: 'metallicFactor']
	roughness_factor   f32    @[json: 'roughnessFactor']
	base_color_texture DocTex @[json: 'baseColorTexture']
}

struct DocTex {
	index int = -1
}

struct DocNode {
	name        string
	mesh        int = -1
	skin        int = -1
	camera      int = -1
	translation []f32
	rotation    []f32
	scale       []f32
	children    []int
	extensions  DocExtensions
}

struct DocExtensions {
	khr_lights_punctual DocLightRef @[json: 'KHR_lights_punctual']
}

struct DocLightRef {
	light int = -1
}

struct DocCamera {
	name         string
	type_        string @[json: 'type']
	perspective  DocPerspective
	orthographic DocOrtho
}

struct DocPerspective {
	yfov  f32 = 60   @[json: 'yfov']
	znear f32 = 0.1  @[json: 'znear']
	zfar  f32 = 1000 @[json: 'zfar']
}

struct DocOrtho {
	ymag f32 = 1    @[json: 'ymag']
	zfar f32 = 1000 @[json: 'zfar']
}

struct DocLight {
	name      string
	color     []f32
	intensity f32 = 1
	type_     string @[json: 'type']
}

// KHR_lights_punctual stores its lights under extensions, never at the root.
struct DocLightExt {
	lights []DocLight
}

struct SceneDocExtensions {
	khr_lights_punctual DocLightExt @[json: 'KHR_lights_punctual']
}

struct SceneDoc {
	materials  []DocMaterial
	nodes      []DocNode
	cameras    []DocCamera
	images     []GltfBuffer
	extensions SceneDocExtensions
}

fn color_from_factors(v []f32, fallback f32) gg.Color {
	if v.len < 3 {
		return gg.Color{u8(fallback * 255), u8(fallback * 255), u8(fallback * 255), 255}
	}
	a := if v.len > 3 { v[3] } else { 1 }
	return gg.Color{u8(math_clip(v[0]) * 255), u8(math_clip(v[1]) * 255), u8(math_clip(v[2]) * 255), u8(math_clip(a) * 255)}
}

fn math_clip(v f32) f32 {
	if v < 0 {
		return 0
	}
	if v > 1 {
		return 1
	}
	return v
}

// load_scene_gltf imports meshes, materials, nodes, cameras and punctual
// lights from a glTF 2.0 file in one pass. Animations and skins remain
// available through load_gltf_skin and load_gltf_clips.
pub fn load_scene_gltf(path string) !SceneAsset {
	text := os.read_file(path) or { return error('gg: cannot read "${path}": ${err}') }
	doc := json2.decode[SceneDoc](text) or { return error('gg: invalid glTF scene JSON: ${err}') }
	meshes := load_mesh_gltf(path) or { return err }
	mut materials := []SceneMaterial{}
	for m in doc.materials {
		idx := m.pbr_metallic_roughness.base_color_texture.index
		candidate := if m.base_color_texture.index >= 0 { m.base_color_texture.index } else { idx }
		materials << SceneMaterial{
			name:       m.name
			base_color: color_from_factors(m.pbr_metallic_roughness.base_color_factor, 1)
			emissive:   color_from_factors(m.emissive_factor, 0)
			metallic:   m.pbr_metallic_roughness.metallic_factor
			roughness:  m.pbr_metallic_roughness.roughness_factor
			// An out-of-range image index is reported as "no texture" (-1) so
			// consumers cannot index past doc.images.
			texture:    if candidate >= 0 && candidate < doc.images.len { candidate } else { -1 }
		}
	}
	mut nodes := []SceneNode{}
	for n in doc.nodes {
		mut pos := gg.Vec3{}
		if n.translation.len >= 3 {
			pos = gg.Vec3{n.translation[0], n.translation[1], n.translation[2]}
		}
		mut scale := gg.Vec3{1, 1, 1}
		if n.scale.len >= 3 {
			scale = gg.Vec3{n.scale[0], n.scale[1], n.scale[2]}
		}
		mut rot := [f32(0), 0, 0, 1]!
		if n.rotation.len >= 4 {
			for i in 0 .. 4 {
				rot[i] = n.rotation[i]
			}
		}
		nodes << SceneNode{
			name:        n.name
			mesh:        n.mesh
			skin:        n.skin
			camera:      n.camera
			light:       n.extensions.khr_lights_punctual.light
			translation: pos
			rotation:    rot
			scale:       scale
			children:    n.children
		}
	}
	mut cameras := []SceneCamera{}
	for c in doc.cameras {
		if c.type_ == 'orthographic' {
			cameras << SceneCamera{ name: c.name, ortho: true, far: c.orthographic.zfar }
		} else {
			cameras << SceneCamera{
				name: c.name
				fovy: c.perspective.yfov
				near: c.perspective.znear
				far:  c.perspective.zfar
			}
		}
	}
	mut lights := []SceneLight{}
	for l in doc.extensions.khr_lights_punctual.lights {
		lights << SceneLight{
			name:      l.name
			color:     color_from_factors(l.color, 1)
			intensity: l.intensity
			kind:      l.type_
		}
	}
	mut textures := []string{}
	for image in doc.images {
		textures << image.uri
	}
	return SceneAsset{meshes, materials, nodes, cameras, lights, textures}
}
