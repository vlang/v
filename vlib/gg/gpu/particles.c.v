module gpu

import sokol.gfx

// particle_vertex is one instanced quad: position, size, color.
pub struct ParticleVertex {
pub:
	x    f32
	y    f32
	size f32
	r    f32
	g    f32
	b    f32
	a    f32
}

// ParticleGPU uploads a particle buffer and draws it with an instanced shader.
@[heap]
pub struct ParticleGPU {
pub mut:
	vertices    []ParticleVertex
	buffer      gfx.Buffer
	buffer_size int
	shader      &ShaderHandle
	pipeline    gfx.Pipeline
	has_buffer  bool
}

pub fn new_particle_gpu() !&ParticleGPU {
	src := ShaderSource{
		vs:    '
layout(location=0) in vec2 position;
layout(location=1) in vec4 color0;
layout(location=2) in vec4 particle;
out vec4 color;
void main() {
    vec2 p = position * particle.z + particle.xy;
    gl_Position = vec4(p, 0.0, 1.0);
    color = color0;
}'
		fs:    '
in vec4 color;
out vec4 frag_color;
void main() { frag_color = color; }'
		label: 'gg.particles'
	}
	shader := create_shader(src)!
	mut desc := gfx.PipelineDesc{
		shader:         shader.shader
		primitive_type: .triangle_strip
		cull_mode:      .none
		sample_count:   1
	}
	desc.layout.buffers[0].stride = int(sizeof(ParticleVertex))
	desc.layout.attrs[0] = gfx.VertexAttrDesc{ buffer_index: 0, format: .float2, offset: 0 }
	desc.layout.attrs[2] = gfx.VertexAttrDesc{ buffer_index: 0, format: .float4, offset: 8 }
	desc.layout.attrs[1] = gfx.VertexAttrDesc{ buffer_index: 0, format: .float4, offset: 24 }
	pipeline := gfx.make_pipeline(&desc)
	return &ParticleGPU{ shader: shader, pipeline: pipeline }
}

// set replaces the particle data.
pub fn (mut p ParticleGPU) set(vertices []ParticleVertex) {
	p.vertices = vertices.clone()
}

// upload creates or updates the GPU buffer, growing it when needed.
pub fn (mut p ParticleGPU) upload() {
	if p.vertices.len == 0 {
		return
	}
	// Guard the byte count in i64: a later set() may supply many more
	// vertices than the buffer was originally sized for.
	required64 := i64(p.vertices.len) * i64(sizeof(ParticleVertex))
	if required64 <= 0 || required64 > i64(1) << 31 {
		return
	}
	required := int(required64)
	if !p.has_buffer || required > p.buffer_size {
		if p.has_buffer && gfx.is_valid() {
			gfx.destroy_buffer(p.buffer)
		}
		p.buffer = gfx.make_buffer(&gfx.BufferDesc{
			size:  required
			type:  .vertexbuffer
			usage: .stream
		})
		p.has_buffer = true
		p.buffer_size = required
	}
	gfx.update_buffer(p.buffer, &gfx.Range{ ptr: p.vertices.data, size: u32(required) })
}

// draw renders the uploaded particles as quads.
pub fn (mut p ParticleGPU) draw() {
	if !p.has_buffer || p.vertices.len == 0 {
		return
	}
	gfx.apply_pipeline(p.pipeline)
	mut bindings := gfx.Bindings{}
	bindings.vertex_buffers[0] = p.buffer
	gfx.apply_bindings(&bindings)
	gfx.draw(0, 4, p.vertices.len)
}

pub fn (mut p ParticleGPU) destroy() {
	if gfx.is_valid() {
		if p.has_buffer {
			gfx.destroy_buffer(p.buffer)
			p.has_buffer = false
			p.buffer_size = 0
		}
		gfx.destroy_pipeline(p.pipeline)
	}
	p.shader.destroy()
}
