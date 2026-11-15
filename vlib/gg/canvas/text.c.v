module canvas

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

fn (r &ScalerRec) key() [14]u32 {
	return [r.typeface_id, f32_bits(r.text_size), f32_bits(r.pre_scale_x), f32_bits(r.pre_skew_x),
		f32_bits(r.post2x2[0]), f32_bits(r.post2x2[1]), f32_bits(r.post2x2[2]), f32_bits(r.post2x2[3]),
		f32_bits(r.frame_width), r.lum_bits, u32(r.device_gamma), u32(r.contrast), u32(r.mask_format),
		u32(r.flags)]!
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

// make_rec builds the shared scaler record, applies the native backend filter,
// then canonicalizes A8 luminance for the mask preblend.
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

	rec.filter_rec(font)!
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
	native_font voidptr
	transform   [6]f64
	inverse     [6]f64
	offscreen   voidptr
	pre_blend   []u8
	sub_pos     bool
	native_size f32
	glyph_index map[u32]int
	glyphs      []Glyph
	image_bytes int
}

// Text APIs and this process-wide cache are used serially on the drawing thread.
const max_text_strikes = 32
const max_strike_glyphs = 256
const max_strike_image_bytes = 256 * 1024
const max_glyph_dimension = 512

struct StrikeCache {
mut:
	// Least recently used first; a hit moves the strike to the end.
	strikes []&Strike
}

const strike_cache = &StrikeCache{}

fn (mut s Strike) clear_glyphs() {
	for glyph in s.glyphs {
		unsafe { glyph.image.free() }
	}
	s.glyphs.clear()
	s.glyph_index.clear()
	s.image_bytes = 0
}

fn (mut s Strike) release() {
	s.clear_glyphs()
	s.release_native()
	unsafe {
		s.pre_blend.free()
		s.glyphs.free()
		s.glyph_index.free()
	}
}

fn (mut cache StrikeCache) remove(index int) {
	mut strike := cache.strikes[index]
	cache.strikes.delete(index)
	strike.release()
	unsafe { free(strike) }
}

// clear_text_cache releases cached glyph masks and each backend's strike resources.
// Call serially with text drawing; live typefaces remain usable afterwards.
pub fn clear_text_cache() {
	mut cache := unsafe { strike_cache }
	for cache.strikes.len > 0 {
		cache.remove(cache.strikes.len - 1)
	}
}

fn release_typeface_strikes(id u32) {
	mut cache := unsafe { strike_cache }
	for i := cache.strikes.len - 1; i >= 0; i-- {
		if cache.strikes[i].rec.typeface_id == id {
			cache.remove(i)
		}
	}
}

fn find_or_create_strike(rec ScalerRec, typeface &Typeface) !&Strike {
	key := rec.key()
	mut cache := unsafe { strike_cache }
	for i, strike in cache.strikes {
		if strike.rec.key() == key {
			cache.strikes.delete(i)
			cache.strikes << strike
			return strike
		}
	}
	strike := new_strike(rec, typeface)!
	if cache.strikes.len == max_text_strikes {
		cache.remove(0)
	}
	cache.strikes << strike
	return strike
}

// sk_fixed_to_float mirrors SkFixedToFloat().
@[inline]
fn sk_fixed_to_float(x int) f32 {
	return f32(x) * f32(1.52587890625e-5)
}

// glyph returns the glyph for a packed ID (glyph | sub_x << 0 | sub_y << 18,
// as SkPackedGlyphID), generating its metrics and image on first use.
fn (mut s Strike) glyph(packed u32) !&Glyph {
	if i := s.glyph_index[packed] {
		return &s.glyphs[i]
	}
	mut g := s.make_glyph(packed)!
	if s.glyphs.len >= max_strike_glyphs || s.image_bytes + g.image.len > max_strike_image_bytes {
		s.clear_glyphs()
	}
	s.image_bytes += g.image.len
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

fn (rec &ScalerRec) preblend() []u8 {
	if rec.contrast == 0 && rec.device_gamma == internal_gamma(1) {
		return []u8{}
	}
	gamma := new_mask_gamma(f32(rec.contrast) / f32(255), f32(rec.device_gamma) / f32(1 << 6))
	table := gamma.green_table(rec.lum_bits).clone()
	unsafe { gamma.tables.free() }
	return table
}
