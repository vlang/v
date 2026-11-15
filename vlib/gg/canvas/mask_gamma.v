module canvas

// A port of SkMaskGamma (SkTMaskGamma<3, 3, 3>): tables that adjust glyph
// coverage for the luminance of the text color, so blending in non-linear
// space approximates a perceptually correct result.

const lum_bits = 3
const lum_tables = 1 << lum_bits

// srgb_to_luma mirrors SkColorSpaceLuminance::toLuma() for each gamma kind:
// 0 is sRGB, 1 is linear and other values are a power curve.
fn to_luma(gamma f32, luminance f32) f32 {
	if gamma == 0 {
		if luminance <= f32(0.04045) {
			return luminance / f32(12.92)
		}
		shifted := luminance + f32(0.055)
		return C.powf(shifted / f32(1.055), f32(2.4))
	}
	if gamma == 1 {
		return luminance
	}
	return C.powf(luminance, gamma)
}

fn from_luma(gamma f32, luma f32) f32 {
	if gamma == 0 {
		if luma <= f32(0.0031308) {
			return luma * f32(12.92)
		}
		p := C.powf(luma, f32(1) / f32(2.4))
		scaled := f32(1.055) * p
		return scaled - f32(0.055)
	}
	if gamma == 1 {
		return luma
	}
	return C.powf(luma, f32(1) / gamma)
}

fn apply_contrast(srca f32, contrast f32) f32 {
	inv := f32(1) - srca
	a := inv * contrast
	b := a * srca
	return srca + b
}

// sk_float_round2int mirrors sk_float_round2int: floor(x + 0.5) saturated to int.
fn float_round2int(x f32) int {
	return int(C.floor(f64(x) + 0.5))
}

// build_correcting_lut mirrors SkTMaskGamma_build_correcting_lut().
fn build_correcting_lut(mut table []u8, offset int, src_i int, contrast f32, gamma f32) {
	src := f32(src_i) / f32(255)
	lin_src := to_luma(gamma, src)
	dst := f32(1) - src
	lin_dst := to_luma(gamma, dst)
	adjusted_contrast := contrast * lin_dst
	if math_fabs(src - dst) < f32(1) / f32(256) {
		mut ii := f32(0)
		for i in 0 .. 256 {
			raw_srca := ii / f32(255)
			srca := apply_contrast(raw_srca, adjusted_contrast)
			scaled := f32(255) * srca
			table[offset + i] = u8(float_round2int(scaled))
			ii += f32(1)
		}
		return
	}
	mut ii := f32(0)
	for i in 0 .. 256 {
		raw_srca := ii / f32(255)
		srca := apply_contrast(raw_srca, adjusted_contrast)
		dsta := f32(1) - srca
		a := lin_src * srca
		b := dsta * lin_dst
		lin_out := a + b
		out := from_luma(gamma, lin_out)
		result := (out - dst) / (src - dst)
		scaled := f32(255) * result
		table[offset + i] = u8(float_round2int(scaled))
		ii += f32(1)
	}
}

@[inline]
fn math_fabs(x f32) f64 {
	// `fabs(src - dst)` in Skia promotes to double.
	return C.fabs(f64(x))
}

// MaskGamma holds the eight correcting tables of SkTMaskGamma<3, 3, 3>, or
// none when it is linear.
struct MaskGamma {
	tables []u8
}

fn new_mask_gamma(contrast f32, gamma f32) MaskGamma {
	mut tables := []u8{len: lum_tables * 256}
	for i in 0 .. lum_tables {
		// sk_t_scale255<3>(i): replicate the 3-bit value into 8 bits.
		base := u32(i) << (8 - lum_bits)
		mut lum := base
		for shift := lum_bits; shift < 8; shift += lum_bits {
			lum |= base >> u32(shift)
		}
		build_correcting_lut(mut tables, i * 256, int(lum), contrast, gamma)
	}
	return MaskGamma{
		tables: tables
	}
}

// green_table returns the preblend table for the green channel of the
// luminance color, which is all an A8 mask uses (SkMaskGamma::preBlend().fG).
fn (g &MaskGamma) green_table(lum_color u32) []u8 {
	if g.tables.len == 0 {
		return []u8{}
	}
	index := int((lum_color >> 8 & 0xff) >> (8 - lum_bits))
	return g.tables[index * 256..index * 256 + 256]
}

// canonical_color mirrors SkTMaskGamma<3, 3, 3>::CanonicalColor() for an
// 0xAARRGGBB color, dropping the alpha like SkColorSetRGB().
fn canonical_color(color u32) u32 {
	r := scale255_3((color >> 16 & 0xff) >> 5)
	g := scale255_3((color >> 8 & 0xff) >> 5)
	b := scale255_3((color & 0xff) >> 5)
	return 0xff000000 | r << 16 | g << 8 | b
}

fn scale255_3(v u32) u32 {
	base := v << 5
	mut lum := base
	lum |= base >> 3
	lum |= base >> 6
	return lum
}

// compute_luminance mirrors SkComputeLuminance(): BT.709 weights in 1.8 fixed point.
fn compute_luminance(r u32, g u32, b u32) u32 {
	return (r * 54 + g * 183 + b * 19) >> 8
}

fn C.powf(x f32, y f32) f32
fn C.floor(x f64) f64
fn C.fabs(x f64) f64
