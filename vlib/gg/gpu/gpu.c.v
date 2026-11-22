module gpu

import sokol.gfx

// ShaderHandle owns a compiled sokol shader.
@[heap]
pub struct ShaderHandle {
pub:
	shader gfx.Shader
}

// create_shader compiles a ShaderSource into a native GPU shader. It requires
// an initialized sokol.gfx context (normally set up by gg.new_context).
pub fn create_shader(source ShaderSource) !&ShaderHandle {
	return create_shader_with_images(source, [])
}

// create_shader_with_images also declares fragment texture slots: sokol needs
// explicit image/sampler reflection info before those bindings can be applied.
fn create_shader_with_images(source ShaderSource, image_names []string) !&ShaderHandle {
	if !gfx.is_valid() {
		return error('gg.gpu: sokol.gfx is not initialized')
	}
	mut desc := gfx.ShaderDesc{}
	desc.attrs[0].name = c'position'
	desc.attrs[1].name = c'texcoord0'
	desc.attrs[2].name = c'color0'
	desc.set_vert_uniform_block_size(0, 128)
	desc.set_vert_src(source.vs)
	desc.set_frag_src(source.fs)
	for i, name in image_names {
		desc.fs.images[i].used = true
		desc.fs.images[i].image_type = ._2d
		desc.fs.images[i].sample_type = .float
		desc.fs.samplers[i].used = true
		desc.fs.samplers[i].sampler_type = .filtering
		desc.fs.image_sampler_pairs[i].used = true
		desc.fs.image_sampler_pairs[i].image_slot = i
		desc.fs.image_sampler_pairs[i].sampler_slot = i
		desc.fs.image_sampler_pairs[i].glsl_name = &char(name.str)
	}
	shader := gfx.make_shader(&desc)
	if gfx.query_shader_state(shader) != .valid {
		// make_shader succeeded, so release the failed handle before returning.
		gfx.destroy_shader(shader)
		return error('gg.gpu: could not compile shader "${source.label}"')
	}
	return &ShaderHandle{ shader: shader }
}

// destroy releases the GPU shader. It is safe to call after gfx teardown.
pub fn (h &ShaderHandle) destroy() {
	if gfx.is_valid() {
		gfx.destroy_shader(h.shader)
	}
}
