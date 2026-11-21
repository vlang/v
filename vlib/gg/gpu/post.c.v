module gpu

import sokol.gfx

// Fullscreen GLSL330 shaders. They use gl_VertexID so no vertex buffer is
// needed; bind the source texture at slot 0 and draw 3 vertices.
fn fullscreen_vs() string {
	return '
out vec2 uv;
void main() {
    vec2 p = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
    uv = p;
    gl_Position = vec4(p * 2.0 - 1.0, 0.0, 1.0);
}'
}

// tonemap_source darkens bright HDR samples with an ACES curve and encodes sRGB.
pub fn tonemap_source(exposure f32) ShaderSource {
	return ShaderSource{
		vs:    fullscreen_vs()
		fs:    '
uniform sampler2D src_tex;
uniform float exposure;
in vec2 uv;
out vec4 frag_color;
vec3 aces(vec3 x) { return clamp((x*(2.51*x+0.03))/(x*(2.43*x+0.59)+0.14), 0.0, 1.0); }
vec3 srgb(vec3 c) { return mix(c*12.92, 1.055*pow(c, vec3(1.0/2.4))-0.055, step(0.0031308, c)); }
void main() {
    vec3 c = aces(texture(src_tex, uv).rgb * ${exposure});
    frag_color = vec4(srgb(c), 1.0);
}'
		label: 'gg.tonemap'
	}
}

// blur_source is a separable 9-tap box blur. direction is in texel units.
pub fn blur_source(dir_x f32, dir_y f32) ShaderSource {
	return ShaderSource{
		vs:    fullscreen_vs()
		fs:    '
uniform sampler2D src_tex;
in vec2 uv;
out vec4 frag_color;
void main() {
    vec4 sum = vec4(0.0);
    for (int i = -4; i <= 4; i++) {
        sum += texture(src_tex, uv + vec2(${dir_x}, ${dir_y}) * float(i));
    }
    frag_color = sum / 9.0;
}'
		label: 'gg.blur'
	}
}

// bloom_source adds a thresholded blurred pass to the original image.
pub fn bloom_source(threshold f32, strength f32) ShaderSource {
	return ShaderSource{
		vs:    fullscreen_vs()
		fs:    '
uniform sampler2D src_tex;
uniform sampler2D bloom_tex;
in vec2 uv;
out vec4 frag_color;
void main() {
    vec3 base = texture(src_tex, uv).rgb;
    vec3 bloom = texture(bloom_tex, uv).rgb;
    float l = dot(base, vec3(0.2126, 0.7152, 0.0722));
    vec3 bright = l > ${threshold} ? base : vec3(0.0);
    frag_color = vec4(base + bloom * ${strength} + bright * ${strength} * 0.25, 1.0);
}'
		label: 'gg.bloom'
	}
}

// FullscreenPass owns a compiled shader and its pipeline for one effect.
@[heap]
pub struct FullscreenPass {
pub mut:
	shader   &ShaderHandle
	pipeline gfx.Pipeline
}

pub fn new_fullscreen_pass(source ShaderSource) !&FullscreenPass {
	shader := create_shader(source)!
	mut desc := gfx.PipelineDesc{
		shader:         shader.shader
		primitive_type: .triangles
		cull_mode:      .none
		sample_count:   1
	}
	desc.colors[0].write_mask = .rgba
	pipeline := gfx.make_pipeline(&desc)
	return &FullscreenPass{ shader: shader, pipeline: pipeline }
}

pub fn (p &FullscreenPass) draw(image gfx.Image) {
	gfx.apply_pipeline(p.pipeline)
	mut bindings := gfx.Bindings{}
	bindings.set_frag_image(0, image)
	gfx.apply_bindings(&bindings)
	gfx.draw(0, 3, 1)
}

pub fn (p &FullscreenPass) destroy() {
	if gfx.is_valid() {
		gfx.destroy_pipeline(p.pipeline)
	}
	p.shader.destroy()
}
