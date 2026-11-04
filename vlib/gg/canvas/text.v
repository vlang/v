module canvas

// CPU glyph rendering that reproduces Skia's raster text path on macOS:
// SkScalerContext record construction, SkScalerContext_Mac glyph masks
// (CoreGraphics output with Skia's linearization and mask gamma), glyph
// positioning with subpixel buckets and the legacy ARGB32 mask blitters.

pub enum FontEdging {
	alias
	antialias
	subpixel_antialias
}

pub enum FontHinting {
	none
	slight
	normal
	full
}

// Font holds the settings of an SkFont. The defaults are SkFont's.
pub struct Font {
pub mut:
	typeface         &Typeface = unsafe { nil }
	size             f32       = 12
	scale_x          f32       = 1
	skew_x           f32
	edging           FontEdging  = .antialias
	hinting          FontHinting = .normal
	subpixel         bool
	linear_metrics   bool
	embolden         bool
	baseline_snap    bool = true
	embedded_bitmaps bool
}

// SurfaceProps mirrors the text-related parts of SkSurfaceProps. Only an
// unknown pixel geometry is supported (no LCD masks).
pub struct SurfaceProps {
pub:
	text_contrast f32 = 0.5
	text_gamma    f32
}

// Mask formats, as SkMask::Format.
const mask_bw = 0
const mask_a8 = 1
const mask_argb32 = 3
const mask_lcd16 = 4

// SkScalerContext flags.
const flag_embedded_bitmap_text = u16(0x0002)
const flag_embolden = u16(0x0004)
const flag_subpixel_positioning = u16(0x0008)
const flag_force_autohinting = u16(0x0010)
const flag_hinting_shift = 7
const flag_hinting_mask = u16(0x0180)
const flag_lcd_vertical = u16(0x0200)
const flag_lcd_bgr_order = u16(0x0400)
const flag_gen_a8_from_lcd = u16(0x0800)
const flag_linear_metrics = u16(0x1000)
const flag_baseline_snap = u16(0x2000)

// ScalerRec holds the fields of SkScalerContextRec that decide glyph images.
struct ScalerRec {
mut:
	typeface_id  u32
	text_size    f32
	pre_scale_x  f32
	pre_skew_x   f32
	post2x2      [4]f32 // [0][0], [0][1], [1][0], [1][1]
	frame_width  f32
	lum_bits     u32
	device_gamma u8
	contrast     u8
	mask_format  int
	flags        u16
}

fn (r &ScalerRec) hinting() FontHinting {
	return unsafe { FontHinting(int((r.flags & flag_hinting_mask) >> flag_hinting_shift)) }
}

fn (mut r ScalerRec) set_hinting(h FontHinting) {
	r.flags = (r.flags & ~flag_hinting_mask) | u16(int(h) << flag_hinting_shift)
}

fn (mut r ScalerRec) set_luminance_color(c u32) {
	r.lum_bits = canonical_color(c)
}

fn internal_gamma(g f32) u8 {
	return u8(g * f32(1 << 6))
}

fn internal_contrast(c f32) u8 {
	scaled := c * f32(255)
	return u8(scaled + f32(0.5))
}

fn (r &ScalerRec) key() string {
	return '${r.typeface_id}|${f32_bits(r.text_size)}|${f32_bits(r.pre_scale_x)}|${f32_bits(r.pre_skew_x)}|${f32_bits(r.post2x2[0])}|${f32_bits(r.post2x2[1])}|${f32_bits(r.post2x2[2])}|${f32_bits(r.post2x2[3])}|${f32_bits(r.frame_width)}|${r.lum_bits}|${r.device_gamma}|${r.contrast}|${r.mask_format}|${r.flags}'
}

fn f32_bits(x f32) u32 {
	return math_f32_bits(x)
}

// sk_relax mirrors SkScalerContext.cpp's sk_relax(): round to 1/1024.
fn sk_relax(x f32) f32 {
	n := f32(C.floor(f64(x * f32(1024)) + 0.5))
	return n / f32(1024)
}

// make_text_matrix mirrors SkFontPriv::MakeTextMatrix().
fn make_text_matrix(size f32, scale_x f32, skew_x f32) RasterMatrix {
	mut m := RasterMatrix.scale(size * scale_x, size)
	if skew_x != 0 {
		m.post_concat(RasterMatrix.skew(skew_x, 0))
	}
	return m
}

// make_rec mirrors SkScalerContext::MakeRecAndEffects(), followed by
// SkTypeface_Mac::onFilterRec() and SkScalerContext::PreprocessRec().
fn make_rec(font Font, color u32, props SurfaceProps, device_matrix_ RasterMatrix) !ScalerRec {
	mut device_matrix := device_matrix_
	mut rec := ScalerRec{
		typeface_id: font.typeface.id
		text_size:   font.size
		pre_scale_x: font.scale_x
		pre_skew_x:  font.skew_x
	}
	mut check_post2x2 := false
	mask := device_matrix.get_type()
	if mask & type_scale != 0 {
		rec.post2x2[0] = sk_relax(device_matrix.m[mat_scale_x])
		rec.post2x2[3] = sk_relax(device_matrix.m[mat_scale_y])
		check_post2x2 = true
	} else {
		rec.post2x2[0] = 1
		rec.post2x2[3] = 1
	}
	if mask & type_affine != 0 {
		rec.post2x2[1] = sk_relax(device_matrix.m[mat_skew_x])
		rec.post2x2[2] = sk_relax(device_matrix.m[mat_skew_y])
		check_post2x2 = true
	}
	mut flags := u16(0)
	if font.embolden {
		return error('gg.canvas: synthetic bold text is not supported yet')
	}
	rec.frame_width = -1
	if font.edging == .alias {
		return error('gg.canvas: aliased (BW) text is not supported yet')
	}
	rec.mask_format = match font.edging {
		.alias { mask_bw }
		.antialias { mask_a8 }
		.subpixel_antialias { mask_lcd16 }
	}
	if rec.mask_format == mask_lcd16 {
		// Too big for LCD or an unknown pixel geometry: both make A8 from LCD.
		rec.mask_format = mask_a8
		flags |= flag_gen_a8_from_lcd
	}
	_ = check_post2x2
	if font.embedded_bitmaps {
		flags |= flag_embedded_bitmap_text
	}
	if font.subpixel {
		flags |= flag_subpixel_positioning
	}
	if font.linear_metrics {
		flags |= flag_linear_metrics
	}
	if font.baseline_snap {
		flags |= flag_baseline_snap
	}
	rec.flags = flags
	rec.set_hinting(font.hinting)
	rec.set_luminance_color(color)
	rec.device_gamma = internal_gamma(props.text_gamma)
	rec.contrast = internal_contrast(props.text_contrast)

	// SkTypeface_Mac::onFilterRec().
	rec.flags &= ~(flag_force_autohinting | flag_lcd_bgr_order | flag_lcd_vertical)
	smooth := C.v_canvas_ct_smooth_behavior()
	if rec.hinting() != .none {
		rec.set_hinting(.normal)
	}
	if smooth == 0 {
		rec.set_hinting(.none)
	}
	if font.typeface.has_color_glyphs {
		return error('gg.canvas: color glyphs are not supported yet')
	}
	if rec.mask_format == mask_a8 && rec.hinting() == .none {
		// Skia ignores the pre-blend here unless SK_GAMMA_APPLY_TO_A8 is
		// defined; LibWeb's Skia defines it, so unhinted A8 keeps its gamma.
	} else {
		mut lum := rec.lum_bits
		r := lum >> 16 & 0xff
		g := lum >> 8 & 0xff
		b := lum & 0xff
		if smooth == 1 {
			lum = r * 1 / 2 << 16 | g * 1 / 2 << 8 | b * 1 / 2
		} else if smooth == 2 {
			lum = r * 3 / 4 << 16 | g * 3 / 4 << 8 | b * 3 / 4
		}
		rec.set_luminance_color(lum)
		rec.contrast = 0
	}
	// SkScalerContext::PreprocessRec().
	if rec.mask_format == mask_a8 {
		lc := rec.lum_bits
		l := compute_luminance(lc >> 16 & 0xff, lc >> 8 & 0xff, lc & 0xff)
		rec.set_luminance_color(l << 16 | l << 8 | l)
	}
	return rec
}

// single_matrix mirrors SkScalerContextRec::getSingleMatrix().
fn (r &ScalerRec) single_matrix() RasterMatrix {
	mut local := make_text_matrix(r.text_size, r.pre_scale_x, r.pre_skew_x)
	from2x2 := RasterMatrix.affine(r.post2x2[0], r.post2x2[2], r.post2x2[1], r.post2x2[3],
		0, 0)
	local.post_concat(from2x2)
	return local
}

// givens_rotation mirrors SkComputeGivensRotation().
fn givens_rotation(h Point) RasterMatrix {
	a := h.x
	b := h.y
	mut c := f32(0)
	mut s := f32(0)
	if b == 0 {
		c = copysign_f32(1, a)
		s = 0
	} else if a == 0 {
		c = 0
		s = -copysign_f32(1, b)
	} else if abs_f32(b) > abs_f32(a) {
		t := a / b
		tt := t * t
		u := copysign_f32(C.sqrtf(f32(1) + tt), b)
		s = f32(-1) / u
		c = -s * t
	} else {
		t := b / a
		tt := t * t
		u := copysign_f32(C.sqrtf(f32(1) + tt), a)
		c = f32(1) / u
		s = -c * t
	}
	return RasterMatrix{
		m:         [c, -s, 0, s, c, 0, 0, 0, 1]!
		type_mask: type_unknown | type_only_perspective_valid
	}
}

fn copysign_f32(x f32, y f32) f32 {
	return C.copysignf(x, y)
}

fn C.copysignf(x f32, y f32) f32

// compute_matrices_vertical mirrors SkScalerContextRec::computeMatrices()
// with PreMatrixScale::kVertical. It returns the scale and the remaining
// matrix sA, or false for a singular matrix.
fn (r &ScalerRec) compute_matrices_vertical() (Point, RasterMatrix, bool) {
	mut a := r.single_matrix()
	skewed_or_flipped := a.m[mat_skew_x] != 0 || a.m[mat_skew_y] != 0 || a.m[mat_scale_x] < 0
		|| a.m[mat_scale_y] < 0
	mut ga := a
	if skewed_or_flipped {
		h := a.map_point(Point{1, 0})
		mut g := givens_rotation(h)
		g.pre_concat(a)
		ga = g
	}
	if abs_f32(ga.m[mat_scale_x]) <= f32(1) / f32(4096)
		|| abs_f32(ga.m[mat_scale_y]) <= f32(1) / f32(4096) || !matrix_is_finite(ga) {
		return Point{1, 1}, RasterMatrix.scale(0, 0), false
	}
	y_scale := abs_f32(ga.m[mat_scale_y])
	s := Point{y_scale, y_scale}
	mut sa := RasterMatrix.identity()
	if !skewed_or_flipped && a.m[mat_scale_x] == a.m[mat_scale_y] {
		// sA is identity.
	} else if !skewed_or_flipped {
		// SkMatrix::setScaleX() leaves the type to be computed.
		sa.m[mat_scale_x] = a.m[mat_scale_x] / s.y
		sa.type_mask = type_unknown | type_only_perspective_valid
	} else {
		sa = a
		sa.pre_scale(f32(1) / s.x, f32(1) / s.y)
	}
	return s, sa, true
}

fn matrix_is_finite(m RasterMatrix) bool {
	mut acc := f32(0)
	for v in m.m {
		acc *= v
	}
	return acc == 0
}

// Glyph is a rendered glyph image with Skia's SkGlyph bounds.
struct Glyph {
mut:
	left      int
	top       int
	width     int
	height    int
	advance_x f32
	advance_y f32
	image     []u8
	has_image bool
}

fn (g &Glyph) is_empty() bool {
	return g.width == 0
}

// Strike is a scaler context and its glyph cache, keyed by the ScalerRec.
@[heap]
struct Strike {
	rec ScalerRec
mut:
	ct_font     voidptr
	transform   [6]f64
	inverse     [6]f64
	offscreen   voidptr
	pre_blend   []u8
	sub_pos     bool
	glyph_index map[u32]int
	glyphs      []Glyph
}

struct StrikeCache {
mut:
	keys    []string
	strikes []&Strike
}

const strike_cache = &StrikeCache{}

fn find_or_create_strike(rec ScalerRec, typeface &Typeface) !&Strike {
	key := rec.key()
	mut cache := unsafe { strike_cache }
	for i, k in cache.keys {
		if k == key {
			return cache.strikes[i]
		}
	}
	strike := new_strike(rec, typeface)!
	cache.keys << key
	cache.strikes << strike
	return strike
}

// new_strike mirrors the SkScalerContext_Mac constructor.
fn new_strike(rec ScalerRec, typeface &Typeface) !&Strike {
	scale, sa, invertible := rec.compute_matrices_vertical()
	t := [f64(sa.m[mat_scale_x]), -f64(sa.m[mat_skew_y]), -f64(sa.m[mat_skew_x]),
		f64(sa.m[mat_scale_y]), f64(sa.m[mat_trans_x]), f64(sa.m[mat_trans_y])]!
	mut inv := [6]f64{}
	if invertible {
		C.v_canvas_cg_invert(&t[0], &inv[0])
	} else {
		inv = t
	}
	ct := C.v_canvas_ct_font_exact_copy(typeface.ct_font, f64(scale.y), 0, 0)
	if ct == unsafe { nil } {
		return error('gg.canvas: CoreText could not size the font')
	}
	mut pre_blend := []u8{}
	if !(rec.contrast == 0 && rec.device_gamma == internal_gamma(1)) {
		gamma := new_mask_gamma(f32(rec.contrast) / f32(255), f32(rec.device_gamma) / f32(1 << 6))
		pre_blend = gamma.green_table(rec.lum_bits).clone()
	}
	return &Strike{
		rec:       rec
		ct_font:   ct
		transform: t
		inverse:   inv
		offscreen: C.v_canvas_ct_offscreen_new()
		pre_blend: pre_blend
		sub_pos:   rec.flags & flag_subpixel_positioning != 0
	}
}

// sk_fixed_to_float mirrors SkFixedToFloat().
@[inline]
fn sk_fixed_to_float(x int) f32 {
	return f32(x) * f32(1.52587890625e-5)
}

// glyph returns the glyph for a packed ID (glyph | sub_x << 0 | sub_y << 18,
// as SkPackedGlyphID), generating its metrics and image on first use.
fn (mut s Strike) glyph(packed u32) &Glyph {
	if i := s.glyph_index[packed] {
		return &s.glyphs[i]
	}
	mut g := s.make_glyph(packed)
	s.glyphs << g
	s.glyph_index[packed] = s.glyphs.len - 1
	return &s.glyphs[s.glyphs.len - 1]
}

fn packed_glyph_id(packed u32) u16 {
	return u16((packed >> 2) & 0xffff)
}

fn packed_sub_x_fixed(packed u32) int {
	return int(packed & 3) << 14
}

fn packed_sub_y_fixed(packed u32) int {
	return int((packed >> 18) & 3) << 14
}

// make_glyph mirrors SkScalerContext::internalMakeGlyph() with
// SkScalerContext_Mac::generateMetrics() and generateImage().
fn (mut s Strike) make_glyph(packed u32) Glyph {
	mut g := Glyph{}
	glyph_id := packed_glyph_id(packed)
	mut advance := [2]f32{}
	mut cg := [4]f64{}
	if C.v_canvas_ct_glyph_metrics(s.ct_font, &s.transform[0], glyph_id, &advance[0],
		&cg[0]) == 0 {
		g.advance_x = advance[0]
		g.advance_y = advance[1]
		return g
	}
	g.advance_x = advance[0]
	g.advance_y = advance[1]
	// SkRect::MakeXYWH(x, -y - h, w, h) from CGFloat values.
	x := f32(cg[0])
	y := f32(-cg[1] - cg[3])
	w := f32(cg[2])
	h := f32(cg[3])
	left := x
	top := y
	mut right := x + w
	mut bottom := y + h
	if s.sub_pos {
		right += sk_fixed_to_float(packed_sub_x_fixed(packed))
		bottom += sk_fixed_to_float(packed_sub_y_fixed(packed))
	}
	// roundOut, then outset by one pixel for CoreGraphics antialiasing.
	l := int(C.floorf(left)) - 1
	t := int(C.floorf(top)) - 1
	r := int(C.ceilf(right)) + 1
	b := int(C.ceilf(bottom)) + 1
	g.left = int(i16(l))
	g.top = int(i16(t))
	g.width = int(u16(r - l))
	g.height = int(u16(b - t))
	if g.width == 0 || g.height == 0 {
		g.left = 0
		g.top = 0
		g.width = 0
		g.height = 0
		return g
	}
	s.generate_image(mut g, packed)
	return g
}

// The linearization of CoreGraphics' smoothed coverage (gamma 2.0).
const linear_from_cg_lcd = build_linear_from_cg_lcd()

fn build_linear_from_cg_lcd() []u8 {
	mut t := []u8{len: 256}
	for i in 0 .. 256 {
		t[i] = u8((i * i + 128) / 255)
	}
	return t
}

fn (mut s Strike) generate_image(mut g Glyph, packed u32) {
	request_smooth := s.rec.hinting() != .none
	mut do_aa := false
	mut do_lcd := false
	if s.rec.mask_format != mask_bw {
		do_lcd = true
		do_aa = true
	}
	if !request_smooth && s.rec.mask_format == mask_a8 {
		do_lcd = false
		do_aa = true
	}
	mut sub_x := f32(0)
	mut sub_y := f32(0)
	if s.sub_pos {
		sub_x = sk_fixed_to_float(packed_sub_x_fixed(packed))
		sub_y = sk_fixed_to_float(packed_sub_y_fixed(packed))
	}
	mut cg_row_bytes := u64(0)
	pixels := C.v_canvas_ct_draw_glyph(s.offscreen, s.ct_font, &s.transform[0], &s.inverse[0],
		packed_glyph_id(packed), g.left, g.top, g.width, g.height, sub_x, sub_y, int(do_aa),
		int(do_lcd), &cg_row_bytes)
	stride := int(cg_row_bytes / 4)
	linearize := s.rec.mask_format == mask_a8 && request_smooth
		&& C.v_canvas_ct_smooth_behavior() != 0
	g.image = []u8{len: g.width * g.height}
	for y in 0 .. g.height {
		row := unsafe { pixels + y * stride }
		for x in 0 .. g.width {
			mut p := unsafe { row[x] }
			if linearize {
				p = u32(linear_from_cg_lcd[(p >> 16) & 0xff]) << 16 | u32(linear_from_cg_lcd[(p >> 8) & 0xff]) << 8 | u32(linear_from_cg_lcd[p & 0xff])
			}
			r := 0xff - ((p >> 16) & 0xff)
			gr := 0xff - ((p >> 8) & 0xff)
			b := 0xff - (p & 0xff)
			mut lum := compute_luminance(r, gr, b)
			if s.pre_blend.len > 0 {
				lum = s.pre_blend[lum]
			}
			g.image[y * g.width + x] = u8(lum)
		}
	}
	g.has_image = true
}

fn C.v_canvas_ct_smooth_behavior() int
fn C.v_canvas_ct_font_exact_copy(base voidptr, size f64, opsz_set int, opsz f64) voidptr
fn C.v_canvas_cg_invert(t &f64, out &f64)
fn C.v_canvas_ct_offscreen_new() voidptr
fn C.v_canvas_ct_glyph_metrics(font voidptr, transform &f64, glyph u16, advance &f32, bounds &f64) int
fn C.v_canvas_ct_draw_glyph(offscreen voidptr, font voidptr, transform &f64, inverse &f64, glyph u16, left int, top int, width int, height int, sub_x f32, sub_y f32, do_aa int, do_lcd int, row_bytes &u64) &u32
