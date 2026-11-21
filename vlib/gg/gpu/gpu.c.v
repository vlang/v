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
	shader := gfx.make_shader(&desc)
	if gfx.query_shader_state(shader) != .valid {
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
