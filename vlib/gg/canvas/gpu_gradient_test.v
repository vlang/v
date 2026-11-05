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
// for dithered gradients on rects and rrects, captured with a Metal trace.
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
]

fn test_gradient_shaders_match_graphite() {
	for c in gradient_shader_cases {
		vert, frag := solid_shader_sources(c.step, c.clipped)
		v := gradient_vertex_source(vert, c.shading, c.clipped)
		f := gradient_fragment_source(frag, c.shading, c.clipped)
		assert sha_prefix(v.bytes()) == c.vertex, '${c.step} ${c.clipped} ${c.shading} vertex'
		assert sha_prefix(f.bytes()) == c.fragment, '${c.step} ${c.clipped} ${c.shading} fragment'
	}
}

fn test_dither_lut_matches_graphite_upload() {
	assert sha_prefix(dither_lut()) == 'ff021e7e555b39d7'
}

fn uniform_hex(shading Shading, u GradientUniforms) string {
	mut floats := []f32{}
	u.serialize(shading.stops(), mut floats)
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
