module raytrace

import gg
import sokol.gfx

// GpuSphere is one sphere for the GPU tracer.
pub struct GpuSphere {
pub:
	center gg.Vec3
	radius f32
	color  gg.Color
}

const max_gpu_spheres = 8

struct TracerParams {
mut:
	cam_pos   [4]f32
	cam_fwd   [4]f32
	cam_right [4]f32
	cam_up    [4]f32
	viewport  [4]f32
	spheres   [max_gpu_spheres][4]f32
	colors    [max_gpu_spheres][4]f32
}

// GpuTracer ray marches a small sphere scene on the GPU with sokol. It is a
// native path for simple scenes; the CPU path tracer remains the general one.
@[heap]
pub struct GpuTracer {
pub mut:
	shader   gfx.Shader
	pipeline gfx.Pipeline
	params   TracerParams
	ready    bool
}

pub fn new_gpu_tracer() !&GpuTracer {
	if !gfx.is_valid() {
		return error('raytrace: sokol.gfx is not initialized')
	}
	mut sd := gfx.ShaderDesc{}
	sd.set_vert_src('
out vec2 uv;
void main() {
    vec2 p = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
    uv = p;
    gl_Position = vec4(p * 2.0 - 1.0, 0.0, 1.0);
}')
	sd.set_frag_src('
layout(std140) uniform params {
    vec4 cam_pos;
    vec4 cam_fwd;
    vec4 cam_right;
    vec4 cam_up;
    vec4 viewport;
    vec4 spheres[8];
    vec4 colors[8];
};
in vec2 uv;
out vec4 frag_color;
vec3 trace(vec3 ro, vec3 rd) {
    float best = 9999.0;
    vec3 color = vec3(0.02, 0.02, 0.05);
    int count = int(viewport.w);
    for (int i = 0; i < 8; i++) {
        if (i >= count) break;
        vec3 oc = ro - spheres[i].xyz;
        float b = dot(oc, rd);
        float c = dot(oc, oc) - spheres[i].w * spheres[i].w;
        float h = b * b - c;
        if (h < 0.0) continue;
        float t = -b - sqrt(h);
        if (t < 0.001 || t >= best) continue;
        best = t;
        vec3 n = normalize(ro + rd * t - spheres[i].xyz);
        float light = max(dot(n, normalize(vec3(0.5, 0.8, -0.3))), 0.0);
        color = colors[i].rgb * (0.2 + 0.8 * light);
    }
    return color;
}
void main() {
    vec2 ndc = uv * 2.0 - 1.0;
    vec3 rd = normalize(cam_fwd.xyz + cam_right.xyz * ndc.x * viewport.z * cam_up.w + cam_up.xyz * ndc.y * cam_up.w);
    frag_color = vec4(trace(cam_pos.xyz, rd), 1.0);
}')
	sd.set_frag_uniform_block_size(0, u32(sizeof(TracerParams)))
	shader := gfx.make_shader(&sd)
	if gfx.query_shader_state(shader) != .valid {
		return error('raytrace: could not compile GPU tracer shader')
	}
	mut pd := gfx.PipelineDesc{
		shader:         shader
		primitive_type: .triangles
		cull_mode:      .none
		sample_count:   1
	}
	pd.colors[0].write_mask = .rgba
	return &GpuTracer{ shader: shader, pipeline: gfx.make_pipeline(&pd), ready: true }
}

// set_scene uploads camera, viewport and up to eight spheres.
pub fn (mut t GpuTracer) set_scene(cam gg.Camera3D, width int, height int, spheres []GpuSphere) {
	fwd := cam.forward()
	right := cam.right()
	up := right.cross(fwd).normalized()
	mut params := t.params
	params.cam_pos = [f32(cam.position.x), cam.position.y, cam.position.z, 0]!
	params.cam_fwd = [f32(fwd.x), fwd.y, fwd.z, 0]!
	params.cam_right = [f32(right.x), right.y, right.z, 0]!
	tan_half := f32(0.57735027) // tan(30 degrees) for the default 60-degree fov
	params.cam_up = [f32(up.x), up.y, up.z, tan_half]!
	aspect := if height > 0 { f32(width) / f32(height) } else { f32(1) }
	count := if spheres.len < max_gpu_spheres { spheres.len } else { max_gpu_spheres }
	params.viewport = [f32(width), f32(height), aspect, f32(count)]!
	for i in 0 .. count {
		params.spheres[i] = [f32(spheres[i].center.x), spheres[i].center.y, spheres[i].center.z,
			spheres[i].radius]!
		c := spheres[i].color
		params.colors[i] = [f32(c.r) / 255, f32(c.g) / 255, f32(c.b) / 255, 1]!
	}
	t.params = params
}

pub fn (mut t GpuTracer) draw() {
	if !t.ready {
		return
	}
	gfx.apply_pipeline(t.pipeline)
	rng := gfx.Range{ ptr: &t.params, size: u32(sizeof(TracerParams)) }
	gfx.apply_uniforms(.fs, 0, &rng)
	gfx.draw(0, 3, 1)
}

pub fn (mut t GpuTracer) destroy() {
	if t.ready && gfx.is_valid() {
		gfx.destroy_pipeline(t.pipeline)
		gfx.destroy_shader(t.shader)
		t.ready = false
	}
}
