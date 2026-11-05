module canvas

import math

pub enum GradientKind {
	linear
	radial
}

pub enum TileMode {
	clamp
	repeat
	mirror
	decal
}

// Gradient is a linear or radial gradient shader, like
// SkShaders::LinearGradient() and SkShaders::RadialGradient() with colors in
// sRGB, interpolated in the destination color space.
pub struct Gradient {
pub:
	kind GradientKind
	// The start and end points of a linear gradient, or the center of a
	// radial gradient in `p0`.
	p0     Point
	p1     Point
	radius f32
	// Unpremultiplied colors, and their positions in [0, 1]; without
	// positions the colors are evenly spaced.
	colors    []Color4f
	positions []f32
	tile_mode TileMode
	// Interpolates premultiplied colors, like
	// SkGradient::Interpolation::InPremul::kYes.
	premul       bool
	local_matrix Matrix = Matrix.identity()
}

// Gradient.two_point_conical mirrors the shader that
// SkShaders::TwoPointConicalGradient() makes for `g`'s colors: concentric
// gradients that start at a point are radial gradients. Other two-point
// conical gradients are not supported yet.
pub fn Gradient.two_point_conical(start Point, start_radius f32, end Point, end_radius f32, g Gradient) !Gradient {
	if start_radius < 0 || end_radius < 0 {
		return error('gg.canvas: invalid gradient radius')
	}
	if point_length(start.x - end.x, start.y - end.y) <= gradient_degenerate_threshold
		&& abs_f32(start_radius - end_radius) > gradient_degenerate_threshold
		&& start_radius <= gradient_degenerate_threshold {
		return Gradient{
			...g
			kind:   .radial
			p0:     start
			radius: end_radius
		}
	}
	return error('gg.canvas: two-point conical gradients are not supported')
}

// Shading selects the paint part of a draw's shaders: a solid color, or a
// dithered gradient with up to 4 or 8 stops, multiplied by the paint's alpha
// for the `_alpha` variants.
enum Shading {
	solid
	linear_4
	linear_8
	radial_4
	radial_8
	linear_4_alpha
	linear_8_alpha
	radial_4_alpha
	radial_8_alpha
}

fn (s Shading) is_gradient() bool {
	return s != .solid
}

fn (s Shading) stops() int {
	return if s in [.linear_8, .radial_8, .linear_8_alpha, .radial_8_alpha] { 8 } else { 4 }
}

fn (s Shading) is_linear() bool {
	return s in [.linear_4, .linear_8, .linear_4_alpha, .linear_8_alpha]
}

// is_translucent reports the gradients that Graphite blends with the paint's
// alpha (SrcIn with an AlphaOnlyPaintColor block).
fn (s Shading) is_translucent() bool {
	return s in [.linear_4_alpha, .linear_8_alpha, .radial_4_alpha, .radial_8_alpha]
}

// with_alpha returns the translucent variant of a gradient shading.
fn (s Shading) with_alpha() Shading {
	return match s {
		.linear_4 { .linear_4_alpha }
		.linear_8 { .linear_8_alpha }
		.radial_4 { .radial_4_alpha }
		.radial_8 { .radial_8_alpha }
		else { s }
	}
}

// gradient_uniform_size is the size of a gradient paint's uniforms in
// CombinedUniformData: upper2x2, translation, the colors and offsets,
// tilemode, colorSpace, doUnPremul, mode, the paint color of translucent
// paints and range, padded to 16 bytes.
fn gradient_uniform_size(shading Shading) int {
	size := if shading.stops() == 4 { 144 } else { 224 }
	return if shading.is_translucent() { size + 16 } else { size }
}

// GradientUniforms are the uniforms of Graphite's LocalMatrix, gradient,
// ColorSpaceTransform and Dither blocks.
struct GradientUniforms {
mut:
	upper2x2    [4]f32
	translation [2]f32
	colors      [8]Color4f
	offsets     [8]f32
	tile_mode   int
	color_space int
	do_unpremul int
	mode        f32
	// The paint's RGB and alpha (AlphaOnlyPaintColor) for translucent paints.
	paint_color Color4f
	range       f32
	// The paint does not read the destination: the colors are opaque and the
	// tile mode is not decal.
	opaque bool
}

fn same_gradient_uniforms(a GradientUniforms, b GradientUniforms) bool {
	if a.upper2x2 != b.upper2x2 || a.translation != b.translation || a.offsets != b.offsets
		|| a.tile_mode != b.tile_mode || a.color_space != b.color_space
		|| a.do_unpremul != b.do_unpremul || a.mode != b.mode || a.range != b.range
		|| a.paint_color.r != b.paint_color.r || a.paint_color.g != b.paint_color.g
		|| a.paint_color.b != b.paint_color.b || a.paint_color.a != b.paint_color.a {
		return false
	}
	for i in 0 .. 8 {
		x := a.colors[i]
		y := b.colors[i]
		if x.r != y.r || x.g != y.g || x.b != y.b || x.a != y.a {
			return false
		}
	}
	return true
}

fn (u GradientUniforms) serialize(shading Shading, mut out []f32) {
	stops := shading.stops()
	start := out.len
	for v in u.upper2x2 {
		out << v
	}
	out << u.translation[0]
	out << u.translation[1]
	out << 0
	out << 0
	for c in u.colors[..stops] {
		out << c.r
		out << c.g
		out << c.b
		out << c.a
	}
	for v in u.offsets[..stops] {
		out << v
	}
	out << math.f32_from_bits(u32(u.tile_mode))
	out << math.f32_from_bits(u32(u.color_space))
	out << math.f32_from_bits(u32(u.do_unpremul))
	out << u.mode
	if shading.is_translucent() {
		// float4 paintColor, already 16-byte aligned after mode.
		out << u.paint_color.r
		out << u.paint_color.g
		out << u.paint_color.b
		out << u.paint_color.a
	}
	out << u.range
	for out.len - start < gradient_uniform_size(shading) / 4 {
		out << 0
	}
}

// gradient_degenerate_threshold is SkGradientBaseShader::kDegenerateThreshold.
const gradient_degenerate_threshold = f32(1) / f32(1 << 15)

// gradient_uniforms computes a gradient paint's shading and uniforms like
// SkShaders::LinearGradient()/RadialGradient(), SkGradientBaseShader,
// SkColor4fXformer and Graphite's add_gradient_to_key() for an sRGB
// destination. Degenerate gradients, which Skia replaces with other
// shaders, and gradients with more than 8 stops return an error.
fn gradient_uniforms(g Gradient) !(Shading, GradientUniforms) {
	if g.colors.len < 2 {
		return error('gg.canvas: gradients need at least two colors')
	}
	if g.positions.len > 0 && g.positions.len != g.colors.len {
		return error('gg.canvas: gradient positions must match the colors')
	}
	mut pts_to_unit := RasterMatrix.identity()
	match g.kind {
		.linear {
			dx := g.p1.x - g.p0.x
			dy := g.p1.y - g.p0.y
			mag := point_length(dx, dy)
			if !is_finite_f32(mag) {
				return error('gg.canvas: invalid gradient points')
			}
			if mag <= gradient_degenerate_threshold {
				return error('gg.canvas: degenerate gradients are not supported')
			}
			// pts_to_unit_matrix()
			inv := f32(1) / mag
			vx := dx * inv
			vy := dy * inv
			pts_to_unit = RasterMatrix.sin_cos(-vy, vx, g.p0.x, g.p0.y)
			pts_to_unit.post_translate(-g.p0.x, -g.p0.y)
			pts_to_unit.post_scale(inv, inv)
		}
		.radial {
			if g.radius < 0 {
				return error('gg.canvas: invalid gradient radius')
			}
			if g.radius <= gradient_degenerate_threshold {
				return error('gg.canvas: degenerate gradients are not supported')
			}
			// rad_to_unit_matrix()
			inv := f32(1) / g.radius
			pts_to_unit = RasterMatrix.translate(-g.p0.x, -g.p0.y)
			pts_to_unit.post_scale(inv, inv)
		}
	}
	// add_local_matrix_to_key(): the inverse local matrix followed by the
	// gradient matrix.
	mut local := RasterMatrix.from(g.local_matrix)
	mut lm_inverse := local.invert() or {
		return error('gg.canvas: the gradient local matrix is not invertible')
	}
	lm_inverse.post_concat(pts_to_unit)
	if lm_inverse.has_perspective() {
		return error('gg.canvas: perspective gradient matrices are not supported')
	}
	colors, positions, opaque := gradient_stops(g)
	if colors.len > 8 {
		return error('gg.canvas: gradients with more than 8 stops are not supported')
	}
	stops := if colors.len <= 4 { 4 } else { 8 }
	mut u := GradientUniforms{
		upper2x2:    [lm_inverse.m[mat_scale_x], lm_inverse.m[mat_skew_y], lm_inverse.m[mat_skew_x],
			lm_inverse.m[mat_scale_y]]!
		translation: [lm_inverse.m[mat_trans_x], lm_inverse.m[mat_trans_y]]!
		tile_mode:   int(g.tile_mode)
		do_unpremul: if g.premul { 1 } else { 0 }
		// ColorSpaceTransform's pre-alpha mode: premultiply unpremultiplied
		// interpolation results.
		mode:        if g.premul { f32(0) } else { f32(1) }
		range:       f32(1) / f32(255)
		opaque:      opaque && g.tile_mode != .decal
	}
	// GradientShaderBlocks::GradientData: the stops, padded with the last.
	n := colors.len
	for i in 0 .. 8 {
		j := if i < n { i } else { n - 1 }
		c := colors[j]
		u.colors[i] = if g.premul { c.premul() } else { c }
		u.offsets[i] = if positions.len > 0 {
			positions[j]
		} else {
			f32(j) / f32(n - 1)
		}
	}
	shading := match g.kind {
		.linear {
			if stops == 4 { Shading.linear_4 } else { Shading.linear_8 }
		}
		.radial {
			if stops == 4 { Shading.radial_4 } else { Shading.radial_8 }
		}
	}
	return shading, u
}

// gradient_stops mirrors SkGradientBaseShader's constructor: implicit stops
// at 0 and 1, positions pinned to be monotonic, evenly spaced positions
// dropped, and runs of equal positions reduced to their outer stops. It
// returns the colors, the positions (empty when even) and whether all
// colors are opaque.
fn gradient_stops(g Gradient) ([]Color4f, []f32, bool) {
	pos := g.positions
	mut first_implicit := false
	mut last_implicit := false
	mut count := g.colors.len
	if pos.len > 0 {
		first_implicit = pos[0] > 0
		last_implicit = pos[pos.len - 1] != 1
		count += int(first_implicit) + int(last_implicit)
	}
	mut colors := []Color4f{cap: count}
	if first_implicit {
		colors << g.colors[0]
	}
	mut opaque := true
	for c in g.colors {
		colors << c
		opaque = opaque && c.a == 1
	}
	if last_implicit {
		colors << g.colors[g.colors.len - 1]
	}
	if pos.len == 0 {
		return colors, []f32{}, opaque
	}
	mut positions := []f32{cap: count}
	mut prev := f32(0)
	positions << prev
	start := if first_implicit { 0 } else { 1 }
	end := pos.len + int(last_implicit)
	mut uniform := true
	uniform_step := pos[start] - prev
	for i in start .. end {
		mut curr := f32(1)
		if i != pos.len {
			// SkTPin(pos[i], prev, 1.0f)
			curr = max_f32(prev, min_f32(pos[i], 1))
			if curr == 1 && last_implicit {
				last_implicit = false
			}
		}
		uniform = uniform && abs_f32(uniform_step - (curr - prev)) <= scalar_nearly_zero
		positions << curr
		prev = curr
	}
	if uniform {
		return colors, []f32{}, opaque
	}
	clamp := g.tile_mode == .clamp
	mut out_colors := []Color4f{cap: count}
	mut out_positions := []f32{cap: count}
	mut i := 0
	for j in 1 .. count + 1 {
		if j == count || positions[i] != positions[j] {
			dup := j - i > 1
			if !(dup && !clamp && positions[i] == 0) {
				out_positions << positions[i]
				out_colors << colors[i]
			}
			if dup && !(!clamp && positions[j - 1] == 1) {
				out_positions << positions[j - 1]
				out_colors << colors[j - 1]
			}
			i = j
		}
	}
	return out_colors, out_positions, opaque
}

// point_length mirrors SkPoint::Length().
fn point_length(dx f32, dy f32) f32 {
	xx := dx * dx
	yy := dy * dy
	mag2 := xx + yy
	if is_finite_f32(mag2) {
		return f32(math.sqrt(f64(mag2)))
	}
	x := f64(dx)
	y := f64(dy)
	x2 := x * x
	y2 := y * y
	return f32(math.sqrt(x2 + y2))
}

// dither_lut is skgpu::MakeDitherLUT(): the 8x8 ordered dither matrix that
// Graphite's Dither block samples with repeat tiling.
fn dither_lut() []u8 {
	mut data := []u8{len: 64}
	for x in 0 .. 8 {
		for y in 0 .. 8 {
			m := (y & 1) << 5 | (x & 1) << 4 | (y & 2) << 2 | (x & 2) << 1 | (y & 4) >> 1 | (x & 4) >> 2
			value := f32(m) * (f32(1) / f32(64)) - (f32(63) / f32(128))
			data[y * 8 + x] = u8((value + f32(0.5)) * f32(255) + f32(0.5))
		}
	}
	return data
}

const gradient_snippet_file = $embed_file('shaders/gradient_snippets.metal')

// gradient_snippets splits the pieces of Graphite's gradient MSL by name.
fn gradient_snippets() map[string]string {
	mut out := map[string]string{}
	text := gradient_snippet_file.to_string()
	for part in text.split('// @@ ')[1..] {
		name := part.all_before('\n')
		out[name] = part.all_after_first('\n')
	}
	// The preamble shares its line with the uniform struct.
	out['preamble'] = out['preamble'].trim_right('\n')
	return out
}

const gradient_local_matrix_line = '_out.LocalMatrix_2_Var = float2x2(_globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].upper2x2_2.xy, _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].upper2x2_2.zw) * stepLocalCoords + _globals._anonInterface0->combinedUniformData[_globals.uniformSsboIndex].translation_2;'

// The analytic clip block's index after the gradient blocks.
const gradient_clip_block = '10'

// gradient_vertex_source turns a solid-color vertex shader into the one
// Graphite generates for the same render step with a gradient paint: the
// gradient uniforms replace the color, and the local coordinates mapped by
// the LocalMatrix block replace the color varying.
fn gradient_vertex_source(solid string, step RenderStep, shading Shading, clipped bool) string {
	snippets := gradient_snippets()
	mut t := solid
	if !t.contains('stepLocalCoords') {
		// The wedge and stroke steps only compute local coordinates for
		// paints that read them.
		anchor, assign := if step == .tessellate_strokes {
			'float4 devPosition = float4(devAndLocalCoords.xy, _in.depth, 1.0);\n', 'stepLocalCoords = devAndLocalCoords.zw;\n'
		} else {
			'devPosition.z = _in.depth;\n', 'stepLocalCoords = localCoord;\n'
		}
		t = t.replace_once('(void)_out;\n', '(void)_out;\nfloat2 stepLocalCoords = float2(0.0);\n')
		t = t.replace_once(anchor, anchor + assign)
	}
	t = t.replace('float4 color_1;\n', snippets['fields_${shading.stops()}'])
	t = gradient_varyings(t, clipped)
	mut lines := []string{}
	for line in t.split('\n') {
		if line.starts_with('_out.SolidColor_1_Var = ') {
			lines << gradient_local_matrix_line
			if !clipped {
				lines << '_out.ssboIndexVar = _globals.uniformSsboIndex;'
			}
			continue
		}
		lines << line
	}
	return translucent_blocks(rename_clip_block(lines.join('\n')), shading)
}

// gradient_fragment_source turns a solid-color fragment shader into the one
// Graphite generates for the same render step with a gradient paint. The
// gradient code precedes the step's coverage code, whose inlined variables
// are renumbered after the gradient's, and the helper functions are merged
// in Graphite's order.
fn gradient_fragment_source(solid string, shading Shading, clipped bool) string {
	snippets := gradient_snippets()
	stops := shading.stops()
	mut t := solid.replace('struct CombinedUniformData {',
		snippets['preamble'] + 'struct CombinedUniformData {')
	t = t.replace('float4 color_1;\n', snippets['fields_${stops}'])
	t = gradient_varyings(t, clipped)
	if !t.contains('uint uniformSsboIndex;') {
		t = t.replace('    const device CombinedUniforms* _anonInterface0;\n',
			'    const device CombinedUniforms* _anonInterface0;\n    uint uniformSsboIndex;\n')
	}
	t = t.replace('    constant IntrinsicUniforms* _anonInterface1;\n',
		'    constant IntrinsicUniforms* _anonInterface1;\n    sampler2D ditherLUT_6;\n')
	// The step's own helpers keep their temporaries apart from the gradient's.
	protos_color := snippets['protos_color']
	protos_tile := snippets['protos_tile'] + snippets['proto_colorize_${stops}']
	bodies_color := snippets['bodies_color'].replace('_skTemp', '_skTempA')
	bodies_tile := (snippets['bodies_tile'] + snippets['body_colorize_${stops}']).replace('_skTemp',
		'_skTempB')
	helpers := helper_prototypes(solid)
	if helpers.len == 0 {
		t = t.replace_once('};\nfragment Outputs',
			'};\n' + protos_color + protos_tile + bodies_color + bodies_tile + 'fragment Outputs')
	} else {
		// The color space helpers follow the step's first helper, the tiling
		// and colorizing helpers its last.
		first := helpers[0] + ';\n'
		last := helpers[helpers.len - 1] + ';\n'
		t = t.replace_once(first, first + protos_color)
		t = t.replace_once(last, last + protos_tile)
		next := if helpers.len > 1 { helpers[1] + ' {\n' } else { 'fragment Outputs' }
		i := t.index(next) or { 0 }
		t = t[..i] + bodies_color + t[i..]
		k := t.index('fragment Outputs') or { 0 }
		t = t[..k] + bodies_tile + t[k..]
	}
	t = t.replace('bool _frontFacing [[front_facing]]',
		'texture2d<float> ditherLUT_6_Tex [[texture(0)]], sampler ditherLUT_6_Smplr [[sampler(0)]], bool _frontFacing [[front_facing]]')
	sampler_globals := 'Globals _globals{&_anonInterface0, {}, &_anonInterface1, {ditherLUT_6_Tex, ditherLUT_6_Smplr}};'
	t = t.replace('Globals _globals{&_anonInterface0, &_anonInterface1};', sampler_globals)
	t = t.replace('Globals _globals{&_anonInterface0, {}, &_anonInterface1};', sampler_globals)
	k := t.index('fragment Outputs') or { 0 }
	head := t[..k]
	mut main := t[k..]
	kind := if shading.is_linear() { 'linear' } else { 'radial' }
	alpha := if shading.is_translucent() { '_alpha' } else { '' }
	mut body := snippets['main_${kind}${alpha}']
	if stops == 8 {
		body = body.replace('w_Qh4f4f4f2', 'x_Qh4f4f4f2')
	}
	main = shift_inlined_variables(main, max_inlined_variable(body) + 1)
	ssbo := '_globals.uniformSsboIndex = _in.ssboIndexVar;\n'
	main = main.replace(ssbo, '')
	main = main.replace('float4 outColor_1 = _in.SolidColor_1_Var;\n', ssbo + body)
	return translucent_blocks(renumber_temporaries(rename_clip_block(head + main)), shading)
}

// The gradient blocks' uniforms before and after a translucent paint's
// alpha blend renumbers the paint blocks.
const translucent_renames = [['upper2x2_2', 'upper2x2_3'], ['translation_2', 'translation_3'],
	['colors_4', 'colors_5'], ['offsets_4', 'offsets_5'], ['tilemode_4', 'tilemode_5'],
	['colorSpace_4', 'colorSpace_5'], ['doUnPremul_4', 'doUnPremul_5'], ['mode_5', 'mode_6'],
	['range_6', 'range_9'], ['LocalMatrix_2_Var', 'LocalMatrix_3_Var'], ['rect_10', 'rect_13'],
	['radiusPlusHalf_10', 'radiusPlusHalf_13'], ['edgeSelect_10', 'edgeSelect_13'],
	['outColor_10', 'outColor_13']]

// translucent_blocks renumbers a gradient shader's blocks for a translucent
// paint, whose alpha follows `mode` as the paintColor uniform.
fn translucent_blocks(t string, shading Shading) string {
	if !shading.is_translucent() {
		return t
	}
	mut out := t.replace('float range_6;\n', 'float4 paintColor;\nfloat range_6;\n')
	for r in translucent_renames {
		out = replace_identifier(out, r[0], r[1])
	}
	// ditherLUT_6 also prefixes the texture and sampler parameter names.
	return out.replace('ditherLUT_6', 'ditherLUT_9')
}

// gradient_varyings replaces the solid color varying with the flat uniform
// index and the gradient's local coordinates.
fn gradient_varyings(t string, clipped bool) string {
	if clipped {
		// The uniform index is already a varying of clipped draws.
		return t.replace('    float4 SolidColor_1_Var [[user(locn1) center_no_perspective]];\n',
			'    float2 LocalMatrix_2_Var [[user(locn1)]];\n')
	}
	mut out := []u8{cap: t.len + 128}
	marker := '[[user(locn'
	mut i := 0
	for i < t.len {
		if has_at(t, i, marker) {
			j := i + marker.len
			mut k := j
			for k < t.len && t[k].is_digit() {
				k++
			}
			if has_at(t, k, ')]]') {
				out << marker.bytes()
				out << (t[j..k].int() + 1).str().bytes()
				i = k
				continue
			}
		}
		out << t[i]
		i++
	}
	return out.bytestr().replace('    float4 SolidColor_1_Var [[user(locn0) center_no_perspective]];\n',
		'    uint ssboIndexVar [[user(locn0) flat]];\n    float2 LocalMatrix_2_Var [[user(locn1)]];\n')
}

// rename_clip_block renames the analytic clip block's uniforms and output,
// whose block index follows the paint's blocks.
fn rename_clip_block(t string) string {
	start := t.index('float4 rect_') or { return t }
	end := t.index_after(';', start) or { return t }
	old := t[start + 'float4 rect_'.len..end]
	mut out := t
	for name in ['rect', 'radiusPlusHalf', 'edgeSelect', 'outColor'] {
		out = replace_identifier(out, '${name}_${old}', '${name}_${gradient_clip_block}')
	}
	return out
}

// has_at reports whether `s` occurs in `t` at byte index `i`.
fn has_at(t string, i int, s string) bool {
	if i + s.len > t.len {
		return false
	}
	for j in 0 .. s.len {
		if t[i + j] != s[j] {
			return false
		}
	}
	return true
}

fn is_identifier_byte(c u8) bool {
	return c.is_letter() || c.is_digit() || c == `_`
}

// replace_identifier replaces whole-identifier occurrences of `old`.
fn replace_identifier(t string, old string, new string) string {
	mut out := []u8{cap: t.len}
	mut i := 0
	for i < t.len {
		if has_at(t, i, old) && (i == 0 || !is_identifier_byte(t[i - 1]))
			&& (i + old.len == t.len || !is_identifier_byte(t[i + old.len])) {
			out << new.bytes()
			i += old.len
			continue
		}
		out << t[i]
		i++
	}
	return out.bytestr()
}

// helper_prototypes returns the prototypes of a shader's helper functions,
// whose minified names carry a `_Q` signature suffix.
fn helper_prototypes(t string) []string {
	mut out := []string{}
	for line in t.split('\n') {
		if line.starts_with('fragment ') {
			break
		}
		if line.len > 0 && is_identifier_byte(line[0]) && line.ends_with(');')
			&& line.all_before('(').contains('_Q') {
			out << line[..line.len - 1]
		}
	}
	return out
}

// inlined_variable reads an inliner variable name `_<n>_<name>` at `i`,
// returning its number and the index after the number.
fn inlined_variable(t string, i int) ?(int, int) {
	if t[i] != `_` || (i > 0 && is_identifier_byte(t[i - 1])) {
		return none
	}
	mut k := i + 1
	for k < t.len && t[k].is_digit() {
		k++
	}
	if k == i + 1 || k + 1 >= t.len || t[k] != `_` || !t[k + 1].is_letter() {
		return none
	}
	return t[i + 1..k].int(), k
}

fn max_inlined_variable(t string) int {
	mut n := -1
	for i in 0 .. t.len {
		if v, _ := inlined_variable(t, i) {
			n = math.max(n, v)
		}
	}
	return n
}

// shift_inlined_variables renumbers the inliner's variables by `delta`.
fn shift_inlined_variables(t string, delta int) string {
	mut out := []u8{cap: t.len + 64}
	mut i := 0
	for i < t.len {
		if v, k := inlined_variable(t, i) {
			out << `_`
			out << (v + delta).str().bytes()
			i = k
			continue
		}
		out << t[i]
		i++
	}
	return out.bytestr()
}

// renumber_temporaries numbers the `_skTemp` variables in order of
// appearance, like the code generator does for the combined program.
fn renumber_temporaries(t string) string {
	mut numbers := map[string]int{}
	mut out := []u8{cap: t.len}
	marker := '_skTemp'
	mut i := 0
	for i < t.len {
		if has_at(t, i, marker) {
			mut k := i + marker.len
			if k < t.len && t[k].is_capital() {
				k++
			}
			for k < t.len && t[k].is_digit() {
				k++
			}
			key := t[i + marker.len..k]
			if key !in numbers {
				numbers[key] = numbers.len
			}
			out << marker.bytes()
			out << numbers[key].str().bytes()
			i = k
			continue
		}
		out << t[i]
		i++
	}
	return out.bytestr()
}
