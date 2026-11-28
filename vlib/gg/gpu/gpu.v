// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gpu

import gg

// ShaderSource holds GLSL 330 vertex and fragment sources. sokol's backend
// descriptors are filled from these; other backends need their own sources.
@[params]
pub struct ShaderSource {
pub:
	vs    string
	fs    string
	label string = 'gg.shader'
}

// UniformSlot is a named float uniform in a material's uniform block.
pub struct UniformSlot {
pub mut:
	name   string
	values []f32
}

// ShaderMaterial groups named uniform slots for one shader. Values are kept
// as floats so the block can be uploaded to a GPU uniform buffer in one call.
pub struct ShaderMaterial {
pub mut:
	name  string
	slots []UniformSlot
}

// new_material returns an empty material.
pub fn new_material(name string) ShaderMaterial {
	return ShaderMaterial{ name: name }
}

fn set_slot(mut m ShaderMaterial, name string, values []f32) {
	for i in 0 .. m.slots.len {
		if m.slots[i].name == name {
			m.slots[i].values = values.clone()
			return
		}
	}
	m.slots << UniformSlot{ name: name, values: values.clone() }
}

// set_float stores a scalar uniform.
pub fn (mut m ShaderMaterial) set_float(name string, value f32) {
	set_slot(mut m, name, [value])
}

// set_vec3 stores a three-component uniform.
pub fn (mut m ShaderMaterial) set_vec3(name string, value gg.Vec3) {
	set_slot(mut m, name, [value.x, value.y, value.z])
}

// set_color stores an RGBA color as four normalized floats.
pub fn (mut m ShaderMaterial) set_color(name string, color gg.Color) {
	set_slot(mut m, name, [f32(color.r) / 255, f32(color.g) / 255, f32(color.b) / 255,
		f32(color.a) / 255])
}

// uniform_block packs all slots into one flat float array in registration
// order. Keep slot order stable when uploading to a shader.
pub fn (m &ShaderMaterial) uniform_block() []f32 {
	mut out := []f32{}
	for slot in m.slots {
		out << slot.values
	}
	return out
}

// slot_index returns the position of a named slot or -1.
pub fn (m &ShaderMaterial) slot_index(name string) int {
	for i, slot in m.slots {
		if slot.name == name {
			return i
		}
	}
	return -1
}

// default_source returns a minimal GLSL330 pair using a 128-byte vertex
// uniform block, matching the layout used by gg/render's standard shader.
pub fn default_source() ShaderSource {
	return ShaderSource{
		vs:    '
layout(location=0) in vec4 position;
layout(location=1) in vec2 texcoord0;
layout(location=2) in vec4 color0;
layout(std140) uniform vs_params { mat4 mvp; };
out vec4 color;
out vec2 uv;
void main() {
	gl_Position = mvp * position;
	color = color0;
	uv = texcoord0;
}'
		fs:    '
in vec4 color;
in vec2 uv;
out vec4 frag_color;
void main() { frag_color = color; }'
		label: 'gg.default'
	}
}
