module splat

import gg
import sokol.gfx

// SplatVertex is one instanced Gaussian quad for the GPU renderer.
pub struct SplatVertex {
pub:
	center_x f32
	center_y f32
	radius_x f32
	radius_y f32
	r        f32
	g        f32
	b        f32
	a        f32
}

// GpuSplatRenderer draws projected Gaussians with sokol instancing. The CPU
// rasterizer stays available for headless and WASM/JS targets.
@[heap]
pub struct GpuSplatRenderer {
pub mut:
	vertices    []SplatVertex
	buffer      gfx.Buffer
	shader      gfx.Shader
	pipeline    gfx.Pipeline
	has_buffer  bool
	has_pipe    bool
	has_shader  bool
	buffer_size int
}

pub fn new_gpu_splat_renderer() !&GpuSplatRenderer {
	if !gfx.is_valid() {
		return error('splat: sokol.gfx is not initialized')
	}
	shader_source := shader_desc()
	shader := gfx.make_shader(&shader_source)
	if gfx.query_shader_state(shader) != .valid {
		gfx.destroy_shader(shader)
		return error('splat: could not compile Gaussian shader')
	}
	mut pdesc := gfx.PipelineDesc{
		shader:         shader
		primitive_type: .triangle_strip
		cull_mode:      .none
		sample_count:   1
	}
	pdesc.colors[0].pixel_format = .rgba8
	pdesc.colors[0].write_mask = .rgba
	pdesc.colors[0].blend.enabled = true
	pdesc.colors[0].blend.src_factor_rgb = .src_alpha
	pdesc.colors[0].blend.dst_factor_rgb = .one_minus_src_alpha
	pdesc.layout.buffers[0].stride = int(sizeof(SplatVertex))
	pdesc.layout.buffers[0].step_func = .per_instance
	pdesc.layout.attrs[0] = gfx.VertexAttrDesc{ buffer_index: 0, offset: 0, format: .float2 }
	pdesc.layout.attrs[1] = gfx.VertexAttrDesc{ buffer_index: 0, offset: 8, format: .float2 }
	pdesc.layout.attrs[2] = gfx.VertexAttrDesc{ buffer_index: 0, offset: 16, format: .float4 }
	return &GpuSplatRenderer{
		shader:     shader
		pipeline:   gfx.make_pipeline(&pdesc)
		has_pipe:   true
		has_shader: true
	}
}

fn shader_desc() gfx.ShaderDesc {
	mut desc := gfx.ShaderDesc{}
	desc.attrs[0].name = c'center'
	desc.attrs[1].name = c'extent'
	desc.attrs[2].name = c'color0'
	desc.set_vert_src('
layout(location=0) in vec2 center;
layout(location=1) in vec2 extent;
layout(location=2) in vec4 color0;
out vec2 uv;
out vec4 color;
void main() {
    vec2 corner = vec2((gl_VertexID & 1) == 0 ? -1.0 : 1.0, (gl_VertexID & 2) == 0 ? -1.0 : 1.0);
    uv = corner;
    color = color0;
    gl_Position = vec4(center + corner * extent, 0.0, 1.0);
}')
	desc.set_frag_src('
in vec2 uv;
in vec4 color;
out vec4 frag_color;
void main() {
    float d2 = dot(uv, uv);
    float a = color.a * exp(-2.0 * d2);
    frag_color = vec4(color.rgb * a, a);
}')
	return desc
}

// set replaces the instance buffer from projected CPU-side splats.
pub fn (mut r GpuSplatRenderer) set(cloud &SplatCloud, cam gg.Camera3D, width int, height int) {
	r.vertices = []
	if width <= 0 || height <= 0 {
		return
	}
	basis := make_splat_basis(cam, f32(width), f32(height)) or { return }
	for g in cloud.points {
		if p := project_with_basis(g, basis) {
			r.vertices << SplatVertex{p.x, p.y, p.rx, p.ry, p.color.r, p.color.g, p.color.b, p.opacity}
		}
	}
}

pub fn (mut r GpuSplatRenderer) upload() {
	if r.vertices.len == 0 {
		return
	}
	size := r.vertices.len * int(sizeof(SplatVertex))
	// The instance count changes every frame; recreate the buffer when the
	// existing one is too small instead of overflowing it on update_buffer.
	if r.has_buffer && size > r.buffer_size {
		gfx.destroy_buffer(r.buffer)
		r.has_buffer = false
	}
	if !r.has_buffer {
		r.buffer = gfx.make_buffer(&gfx.BufferDesc{
			size:  size
			type:  .vertexbuffer
			usage: .stream
		})
		r.has_buffer = true
		r.buffer_size = size
	}
	gfx.update_buffer(r.buffer, &gfx.Range{ ptr: r.vertices.data, size: u32(size) })
}

pub fn (mut r GpuSplatRenderer) draw() {
	if !r.has_buffer || r.vertices.len == 0 {
		return
	}
	gfx.apply_pipeline(r.pipeline)
	mut bindings := gfx.Bindings{}
	bindings.vertex_buffers[0] = r.buffer
	gfx.apply_bindings(&bindings)
	gfx.draw(0, 4, r.vertices.len)
}

pub fn (mut r GpuSplatRenderer) destroy() {
	if !gfx.is_valid() {
		return
	}
	if r.has_buffer {
		gfx.destroy_buffer(r.buffer)
		r.has_buffer = false
		r.buffer_size = 0
	}
	if r.has_pipe {
		gfx.destroy_pipeline(r.pipeline)
		r.has_pipe = false
	}
	if r.has_shader {
		gfx.destroy_shader(r.shader)
		r.has_shader = false
	}
}
