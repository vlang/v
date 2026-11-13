module canvas

import crypto.sha256

fn sha_prefix(data []u8) string {
	return sha256.sum(data).hex()[..16]
}

struct GradientShaderCase {
	step     RenderStep
	clipped  bool
	shading  Shading
	vertex   string
	fragment string
}

// The SHA-256 prefixes of the shaders Skia Graphite (Chromium 155) compiles
// for dithered gradients on rects, rrects, convex path fills and strokes,
// with opaque and translucent paints, captured with a Metal trace. Stencil-and-cover fills cover with the
// cover-bounds shaders.
const gradient_shader_cases = [
	GradientShaderCase{.cover_bounds, false, .linear_4, '635a618cbe4139a4', 'be5d04eb806189e9'},
	GradientShaderCase{.cover_bounds, false, .linear_8, '052f9387d8fa7319', '4c187b0fb3b94f66'},
	GradientShaderCase{.cover_bounds, false, .radial_4, '635a618cbe4139a4', 'f469385d0d5ff43a'},
	GradientShaderCase{.cover_bounds, false, .radial_8, '052f9387d8fa7319', '0dd21065756456e2'},
	GradientShaderCase{.cover_bounds, true, .linear_4, 'a17c17b73dddab56', 'fc21b0910446f37c'},
	GradientShaderCase{.cover_bounds, true, .linear_8, '808597bd14b5de3d', '02e19a5218a324d8'},
	GradientShaderCase{.cover_bounds, true, .radial_4, 'a17c17b73dddab56', '1fd8dfccdc1ac38b'},
	GradientShaderCase{.cover_bounds, true, .radial_8, '808597bd14b5de3d', 'b623d8eda1735960'},
	GradientShaderCase{.analytic_rrect, false, .linear_4, '6836bf9edf56603f', 'df4dc06ae0367328'},
	GradientShaderCase{.analytic_rrect, false, .linear_8, '2274c91edfffc37d', '32d5358d46b24d16'},
	GradientShaderCase{.analytic_rrect, false, .radial_4, '6836bf9edf56603f', 'ea680630abba2da5'},
	GradientShaderCase{.analytic_rrect, false, .radial_8, '2274c91edfffc37d', 'efedc65b7882719c'},
	GradientShaderCase{.analytic_rrect, true, .linear_4, 'ff779aff08c25dd2', '0e5f542abc287d07'},
	GradientShaderCase{.analytic_rrect, true, .linear_8, 'd6622548aa5918d5', '9fb3ec9fc0521b4f'},
	GradientShaderCase{.analytic_rrect, true, .radial_4, 'ff779aff08c25dd2', '2bcd659530d23a7d'},
	GradientShaderCase{.analytic_rrect, true, .radial_8, 'd6622548aa5918d5', '6ea6371a1100479c'},
	GradientShaderCase{.wedges_shaded, false, .linear_4, '6e94f048af97772d', '07633829c11506f2'},
	GradientShaderCase{.wedges_shaded, false, .linear_8, '59002f4895da517a', 'c969c3ceec7dc5f0'},
	GradientShaderCase{.wedges_shaded, false, .radial_4, '6e94f048af97772d', '3494ed368d09f3fa'},
	GradientShaderCase{.wedges_shaded, false, .radial_8, '59002f4895da517a', '4bae965a64e5c6be'},
	GradientShaderCase{.wedges_shaded, true, .linear_4, '9eb92903ada80483', 'b1cba2b4a86567dc'},
	GradientShaderCase{.wedges_shaded, true, .linear_8, '39374615eaf8bc67', 'caccd4067ea447b9'},
	GradientShaderCase{.wedges_shaded, true, .radial_4, '9eb92903ada80483', 'ed2669950469045b'},
	GradientShaderCase{.wedges_shaded, true, .radial_8, '39374615eaf8bc67', '489de9fed25b3fdd'},
	GradientShaderCase{.tessellate_strokes, false, .linear_4, 'cde4b7564d8bea1a', 'fa66760ffd4b5c41'},
	GradientShaderCase{.tessellate_strokes, false, .linear_8, '80b97845f7d004d7', '97cf4dbd751b8411'},
	GradientShaderCase{.tessellate_strokes, false, .radial_4, 'cde4b7564d8bea1a', 'a995c7dd6104972a'},
	GradientShaderCase{.tessellate_strokes, false, .radial_8, '80b97845f7d004d7', '9779abaeec60398c'},
	GradientShaderCase{.tessellate_strokes, true, .linear_4, 'd15a6633e979d9df', 'e276f9a5546f4af9'},
	GradientShaderCase{.tessellate_strokes, true, .linear_8, 'c23798e7cf17c4e9', 'c13c4fdf579b5fee'},
	GradientShaderCase{.tessellate_strokes, true, .radial_4, 'd15a6633e979d9df', '5cabdbb32963d986'},
	GradientShaderCase{.tessellate_strokes, true, .radial_8, 'c23798e7cf17c4e9', '8e3991386a011e67'},
	GradientShaderCase{.cover_bounds, false, .linear_4_alpha, 'f3b2e3e0db9f20dc', '7c47590e4ae91fc1'},
	GradientShaderCase{.cover_bounds, false, .linear_8_alpha, 'b3ef0200b6ae274d', '8253c119f2cba826'},
	GradientShaderCase{.cover_bounds, false, .radial_4_alpha, 'f3b2e3e0db9f20dc', '8d82c0d0a3ddfc5d'},
	GradientShaderCase{.analytic_rrect, false, .linear_4_alpha, '7cebdcd470dd90ef', '1f84ec4cd32d6eb0'},
	GradientShaderCase{.analytic_rrect, false, .linear_8_alpha, '6a2fc7f5b08e5f79', '03fde2e8267d4173'},
	GradientShaderCase{.analytic_rrect, false, .radial_4_alpha, '7cebdcd470dd90ef', 'c762ef09ab5226b8'},
	GradientShaderCase{.analytic_rrect, true, .linear_4_alpha, '9db4020d8a250189', 'b20050519a587e8c'},
	GradientShaderCase{.analytic_rrect, true, .linear_8_alpha, '6c99bc685a02cbfd', '6abd2db2bfb7ee0c'},
	GradientShaderCase{.analytic_rrect, true, .radial_4_alpha, '9db4020d8a250189', 'daa14d48c6fb5b51'},
	GradientShaderCase{.wedges_shaded, false, .linear_4_alpha, '609e2f67b4082a32', 'dadb4bacfa2b9964'},
	GradientShaderCase{.wedges_shaded, false, .linear_8_alpha, '5724497f41526540', '7511e4d812ee1686'},
	GradientShaderCase{.wedges_shaded, false, .radial_4_alpha, '609e2f67b4082a32', '39caa69a8fed8ab3'},
	GradientShaderCase{.wedges_shaded, true, .linear_4_alpha, '279aaa44b8dd04bd', '5f37c37721bed730'},
	GradientShaderCase{.wedges_shaded, true, .linear_8_alpha, '1013a8ea696ff2d6', '7441be65d7f0bac3'},
	GradientShaderCase{.wedges_shaded, true, .radial_4_alpha, '279aaa44b8dd04bd', '40bf2bf108289f15'},
	GradientShaderCase{.tessellate_strokes, false, .linear_4_alpha, 'a910870eb0b3bce5', 'cc37761069a95731'},
	GradientShaderCase{.tessellate_strokes, false, .linear_8_alpha, 'fefdac84fdd0075b', '12ebb6ce81164c20'},
	GradientShaderCase{.tessellate_strokes, false, .radial_4_alpha, 'a910870eb0b3bce5', '1eccbd35c37d0a71'},
	GradientShaderCase{.tessellate_strokes, true, .linear_4_alpha, '766201b36f27afac', '1fa2e896daafad8e'},
	GradientShaderCase{.tessellate_strokes, true, .linear_8_alpha, '76f810b8d7a2f36a', '1e3d03fda237ae0f'},
	GradientShaderCase{.tessellate_strokes, true, .radial_4_alpha, '766201b36f27afac', '07d8a1c13aba0355'},
]

fn test_gradient_shaders_match_graphite() {
	for c in gradient_shader_cases {
		vert, frag := solid_shader_sources(c.step, c.clipped)
		v := gradient_vertex_source(vert, c.step, c.shading, c.clipped)
		f := gradient_fragment_source(frag, c.shading, c.clipped)
		assert sha_prefix(v.bytes()) == c.vertex, '${c.step} ${c.clipped} ${c.shading} vertex'
		assert sha_prefix(f.bytes()) == c.fragment, '${c.step} ${c.clipped} ${c.shading} fragment'
	}
}

fn test_gradient_uniform_sizes_follow_the_struct_layout() {
	// The gradient, the analytic clip, then the step's transform.
	assert step_uniform_size(.cover_bounds, false, .linear_4) == 144
	assert step_uniform_size(.analytic_rrect, true, .radial_8) == 272
	assert step_uniform_size(.wedges_shaded, false, .linear_4) == 208
	assert step_uniform_size(.tessellate_strokes, true, .linear_4) == 224
	// A translucent paint's color follows `mode`.
	assert step_uniform_size(.cover_bounds, false, .linear_4_alpha) == 160
	assert step_uniform_size(.analytic_rrect, true, .radial_8_alpha) == 288
}

// Graphite's uniforms for the linear gradient above with a paint alpha of 0.5.
fn test_translucent_gradient_uniforms_match_graphite() {
	_, u := Paint{
		color:    Color4f{0, 0, 0, 0.5}
		dither:   true
		gradient: Gradient{
			kind:      .linear
			p0:        Point{4, 4}
			p1:        Point{60, 40}
			colors:    three_stops
			positions: [f32(0), 0.5, 1]
		}
	}.shading()!
	assert uniform_hex(.linear_4_alpha, u) == 'a0044f3c421505bc4215053ca0044f3cf10caabdbdde93bc00000000000000000000803f00000000000000000000803f000000000000803f000000000000803f00000000000000000000803f0000803f00000000000000000000803f0000803f000000000000003f0000803f0000803f0000000000000000000000000000803f0000000000000000000000000000003f8180803b000000000000000000000000'
	assert !u.opaque
}

fn test_dither_lut_matches_graphite_upload() {
	assert sha_prefix(dither_lut()) == 'ff021e7e555b39d7'
}

fn uniform_hex(shading Shading, u GradientUniforms) string {
	mut floats := []f32{}
	u.serialize(shading, mut floats)
	mut bytes := []u8{}
	for f in floats {
		bits := math_f32_bits(f)
		bytes << u8(bits)
		bytes << u8(bits >> 8)
		bytes << u8(bits >> 16)
		bytes << u8(bits >> 24)
	}
	return bytes.hex()
}

const three_stops = [Color4f{1, 0, 0, 1}, Color4f{0, 1, 0, 1}, Color4f{0, 0, 1, 1}]
const six_stops = [Color4f{1, 0, 0, 1}, Color4f{1, 1, 0, 1}, Color4f{0, 1, 0, 1}, Color4f{0, 1, 1, 1},
	Color4f{0, 0, 1, 1}, Color4f{1, 0, 1, 1}]

// The uniform data Graphite writes for the same gradients (Metal trace).
fn test_gradient_uniforms_match_graphite() {
	for premul in [false, true] {
		shading, u := gradient_uniforms(Gradient{
			kind:      .linear
			p0:        Point{4, 4}
			p1:        Point{60, 40}
			colors:    three_stops
			positions: [f32(0), 0.5, 1]
			premul:    premul
		})!
		assert shading == .linear_4
		assert u.opaque
		expected := if premul {
			'a0044f3c421505bc4215053ca0044f3cf10caabdbdde93bc00000000000000000000803f00000000000000000000803f000000000000803f000000000000803f00000000000000000000803f0000803f00000000000000000000803f0000803f000000000000003f0000803f0000803f000000000000000001000000000000008180803b000000000000000000000000'
		} else {
			'a0044f3c421505bc4215053ca0044f3cf10caabdbdde93bc00000000000000000000803f00000000000000000000803f000000000000803f000000000000803f00000000000000000000803f0000803f00000000000000000000803f0000803f000000000000003f0000803f0000803f0000000000000000000000000000803f8180803b000000000000000000000000'
		}
		assert uniform_hex(shading, u) == expected
	}
	shading, u := gradient_uniforms(Gradient{
		kind:      .radial
		p0:        Point{32, 24}
		radius:    30
		colors:    six_stops
		positions: [f32(0), 0.2, 0.4, 0.6, 0.8, 1]
	})!
	assert shading == .radial_8
	assert uniform_hex(shading, u) == '8988083d00000000000000008988083d898888bfcecc4cbf00000000000000000000803f00000000000000000000803f0000803f0000803f000000000000803f000000000000803f000000000000803f000000000000803f0000803f0000803f00000000000000000000803f0000803f0000803f000000000000803f0000803f0000803f000000000000803f0000803f0000803f000000000000803f0000803f00000000cdcc4c3ecdcccc3e9a99193fcdcc4c3f0000803f0000803f0000803f0000000000000000000000000000803f8180803b000000000000000000000000'
}

fn test_gradient_stops_follow_skia() {
	// Implicit end stops are added and evenly spaced positions dropped.
	mut colors, mut positions, opaque := gradient_stops(Gradient{
		colors:    [Color4f{1, 0, 0, 1}, Color4f{0, 0, 1, f32(0.5)}]
		positions: [f32(0.25), 0.75]
	})
	assert colors.len == 4
	assert positions == [f32(0), 0.25, 0.75, 1]
	assert !opaque
	colors, positions, _ = gradient_stops(Gradient{
		colors:    three_stops
		positions: [f32(0), 0.5, 1]
	})
	assert colors.len == 3
	assert positions.len == 0
	// Runs of equal positions keep their outer stops; positions are pinned.
	colors, positions, _ = gradient_stops(Gradient{
		colors:    [Color4f{1, 0, 0, 1}, Color4f{0, 1, 0, 1}, Color4f{0, 0, 1, 1}, Color4f{1, 1, 1, 1}]
		positions: [f32(0), 0.3, 0.3, 0.2]
	})
	assert positions == [f32(0), 0.3, 0.3, 1]
	assert colors[3] == Color4f{1, 1, 1, 1}
}

fn test_degenerate_gradients_are_unsupported() {
	gradient_uniforms(Gradient{
		kind:   .linear
		p0:     Point{4, 4}
		p1:     Point{4, 4}
		colors: three_stops
	}) or { return }
	assert false
}
