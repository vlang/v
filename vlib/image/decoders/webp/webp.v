/**********************************************************************
*
* WEBP image decoder (VP8 lossy, VP8L lossless, ALPH, animation)
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module webp

import image.decoders.common

// ============================================================================
// Container / chunk constants
// ============================================================================

const chunk_vp8 = 'VP8 '
const chunk_vp8l = 'VP8L'
const chunk_vp8x = 'VP8X'
const chunk_alph = 'ALPH'
const chunk_iccp = 'ICCP'
const chunk_exif = 'EXIF'
const chunk_xmp = 'XMP '
const chunk_anim = 'ANIM'
const chunk_anmf = 'ANMF'

const vp8x_flag_anim = 0x02
const vp8x_flag_xmp = 0x04
const vp8x_flag_exif = 0x08
const vp8x_flag_alpha = 0x10
const vp8x_flag_icc = 0x20
const vp8x_reserved_flags = 0xC1
const vp8x_chunk_size = 10

// Fixed payload sizes of the animation chunks
// (src/webp/format_constants.h ANIM_CHUNK_SIZE / ANMF_CHUNK_SIZE).
const anim_chunk_size = 6
const anmf_chunk_size = 16

const layout_lossy = 'lossy'
const layout_lossless = 'lossless'
const layout_extended = 'extended'
const layout_animated = 'animated'

// ALPH alpha-plane constants
const alpha_header_len = 1
const alpha_no_compression = 0
const alpha_lossless_compression = 1
const alpha_preprocessed_levels = 1

const webp_filter_none = 0
const webp_filter_horizontal = 1
const webp_filter_vertical = 2
const webp_filter_gradient = 3
const webp_filter_last = webp_filter_gradient + 1

// VP8L lossless bitstream constants
const vp8l_signature = 0x2f
const num_literal_codes = 256
const num_length_codes = 24
const distance_alpha_size = 40
const color_cache_alpha_base = num_literal_codes + num_length_codes
const color_cache_multiplier = 0x1e35a7bd

// VP8 lossy structural limits
const vp8_keyframe_signature = [u8(0x9d), u8(0x01), u8(0x2a)]
const vp8_frame_header_size = 3
const vp8_picture_header_size = 7
const vp8_num_mb_segments = 4
const vp8_mb_feature_tree_probs = 3
const vp8_num_ref_lf_deltas = 4
const vp8_num_mode_lf_deltas = 4
const vp8_num_types = 4
const vp8_num_bands = 8
const vp8_num_ctx = 3
const vp8_num_probas = 11

// Intra prediction modes
const vp8_b_dc_pred = 0
const vp8_b_tm_pred = 1
const vp8_b_ve_pred = 2
const vp8_b_he_pred = 3
const vp8_b_rd_pred = 4
const vp8_b_vr_pred = 5
const vp8_b_ld_pred = 6
const vp8_b_vl_pred = 7
const vp8_b_hd_pred = 8
const vp8_b_hu_pred = 9
const vp8_b_dc_pred_notop = 4
const vp8_b_dc_pred_noleft = 5
const vp8_b_dc_pred_notopleft = 6
const vp8_dc_pred = vp8_b_dc_pred
const vp8_v_pred = vp8_b_ve_pred
const vp8_h_pred = vp8_b_he_pred
const vp8_tm_pred = vp8_b_tm_pred

const mask32 = (1 << 32) - 1

// ============================================================================
// Small helpers
// ============================================================================

fn div_round_up(num int, den int) int {
	return (num + den - 1) / den
}

fn to_int8(value int) int {
	if value >= 128 {
		return value - 256
	}
	return value
}

fn clampi(value int) int {
	if value < 0 {
		return 0
	}
	if value > 255 {
		return 255
	}
	return value
}

fn clipi(value int, max_val int) int {
	if value < 0 {
		return 0
	}
	if value > max_val {
		return max_val
	}
	return value
}

fn absi(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

fn idct_mul1(a int) int {
	return ((a * 20091) >> 16) + a
}

fn idct_mul2(a int) int {
	return (a * 35468) >> 16
}

fn sclip1(x int) int {
	if x < -128 {
		return -128
	}
	if x > 127 {
		return 127
	}
	return x
}

fn sclip2(x int) int {
	if x < -16 {
		return -16
	}
	if x > 15 {
		return 15
	}
	return x
}

fn mult_hi(v int, coeff int) int {
	return (v * coeff) >> 8
}

fn clip8(v int) int {
	if (v & ~16383) == 0 {
		return v >> 6
	}
	if v < 0 {
		return 0
	}
	return 255
}

fn yuv_to_rgb(mut out []int, off int, y int, u int, v int) {
	out[off] = clip8(mult_hi(y, 19077) + mult_hi(v, 26149) - 14234)
	out[off + 1] = clip8(mult_hi(y, 19077) - mult_hi(u, 6419) - mult_hi(v, 13320) + 8708)
	out[off + 2] = clip8(mult_hi(y, 19077) + mult_hi(u, 33050) - 17685)
	out[off + 3] = 255
}

fn trunc_div(a int, b int) int {
	q := absi(a) / absi(b)
	if (a < 0) == (b < 0) {
		return q
	}
	return -q
}

fn argb_add_mod(a int, b int) int {
	return ((a + b) & 0xff) | (((((a >> 8) + (b >> 8)) & 0xff) << 8) | (((((a >> 16) + (b >> 16)) & 0xff) << 16) | ((((a >> 24) + (b >> 24)) & 0xff) << 24)))
}

fn avg2_argb(a int, b int) int {
	return (((a & 0xff) + (b & 0xff)) >> 1) | (((((a >> 8) & 0xff) + ((b >> 8) & 0xff)) >> 1) << 8) | (((((a >> 16) & 0xff) + ((b >> 16) & 0xff)) >> 1) << 16) | (((((a >> 24) & 0xff) + ((b >> 24) & 0xff)) >> 1) << 24)
}

fn select_argb(left int, top int, top_left int) int {
	pa := ((left >> 24) & 0xff) + ((top >> 24) & 0xff) - ((top_left >> 24) & 0xff)
	pr := ((left >> 16) & 0xff) + ((top >> 16) & 0xff) - ((top_left >> 16) & 0xff)
	pg := ((left >> 8) & 0xff) + ((top >> 8) & 0xff) - ((top_left >> 8) & 0xff)
	pb := (left & 0xff) + (top & 0xff) - (top_left & 0xff)
	p_left := (absi(pa - ((left >> 24) & 0xff)) + absi(pr - ((left >> 16) & 0xff)) + absi(pg - ((left >> 8) & 0xff)) + absi(pb - (left & 0xff)))
	p_top := (absi(pa - ((top >> 24) & 0xff)) + absi(pr - ((top >> 16) & 0xff)) + absi(pg - ((top >> 8) & 0xff)) + absi(pb - (top & 0xff)))
	if p_left < p_top {
		return left
	}
	return top
}

fn clamp_add_sub_full(left int, top int, top_left int) int {
	mut out := 0
	mut shift := 0
	for _ in 0 .. 4 {
		v := clampi(((left >> shift) & 0xff) + ((top >> shift) & 0xff) - ((top_left >> shift) & 0xff))
		out |= v << shift
		shift += 8
	}
	return out
}

fn clamp_add_sub_half(a int, b int) int {
	mut out := 0
	mut shift := 0
	for _ in 0 .. 4 {
		ca := (a >> shift) & 0xff
		cb := (b >> shift) & 0xff
		v := clampi(ca + trunc_div(ca - cb, 2))
		out |= v << shift
		shift += 8
	}
	return out
}

fn predict(mode int, left int, top int, top_right int, top_left int) !int {
	if mode == 0 {
		return 0xff000000
	}
	if mode == 1 {
		return left
	}
	if mode == 2 {
		return top
	}
	if mode == 3 {
		return top_right
	}
	if mode == 4 {
		return top_left
	}
	if mode == 5 {
		return avg2_argb(avg2_argb(left, top_right), top)
	}
	if mode == 6 {
		return avg2_argb(left, top_left)
	}
	if mode == 7 {
		return avg2_argb(left, top)
	}
	if mode == 8 {
		return avg2_argb(top_left, top)
	}
	if mode == 9 {
		return avg2_argb(top, top_right)
	}
	if mode == 10 {
		return avg2_argb(avg2_argb(left, top_left), avg2_argb(top, top_right))
	}
	if mode == 11 {
		return select_argb(left, top, top_left)
	}
	if mode == 12 {
		return clamp_add_sub_full(left, top, top_left)
	}
	if mode == 13 {
		return clamp_add_sub_half(avg2_argb(left, top), top_left)
	}
	return error('Invalid predictor mode: ${mode}')
}

fn color_transform_delta(t int, c int) int {
	return (t * c) >> 5
}

fn bits_log2_floor(value int) int {
	mut v := value
	mut n := 0
	for v > 1 {
		v >>= 1
		n++
	}
	return n
}

fn filled(n int, val int) []int {
	mut a := []int{len: n}
	for i in 0 .. n {
		a[i] = val
	}
	return a
}

fn fc_eq(a []u8, s string) bool {
	return a.len == 4 && a[0] == s[0] && a[1] == s[1] && a[2] == s[2] && a[3] == s[3]
}

fn cat_chain(cat int) []int {
	match cat {
		0 { return vp8_cat3 }
		1 { return vp8_cat4 }
		2 { return vp8_cat5 }
		else { return vp8_cat6 }
	}
}

// ============================================================================
// Generated constant tables (flat, indexed by computed offsets)
// ============================================================================

const vp8_coeffs_proba_0 = [
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	253,
	136,
	254,
	255,
	228,
	219,
	128,
	128,
	128,
	128,
	128,
	189,
	129,
	242,
	255,
	227,
	213,
	255,
	219,
	128,
	128,
	128,
	106,
	126,
	227,
	252,
	214,
	209,
	255,
	255,
	128,
	128,
	128,
	1,
	98,
	248,
	255,
	236,
	226,
	255,
	255,
	128,
	128,
	128,
	181,
	133,
	238,
	254,
	221,
	234,
	255,
	154,
	128,
	128,
	128,
	78,
	134,
	202,
	247,
	198,
	180,
	255,
	219,
	128,
	128,
	128,
	1,
	185,
	249,
	255,
	243,
	255,
	128,
	128,
	128,
	128,
	128,
	184,
	150,
	247,
	255,
	236,
	224,
	128,
	128,
	128,
	128,
	128,
	77,
	110,
	216,
	255,
	236,
	230,
	128,
	128,
	128,
	128,
	128,
	1,
	101,
	251,
	255,
	241,
	255,
	128,
	128,
	128,
	128,
	128,
	170,
	139,
	241,
	252,
	236,
	209,
	255,
	255,
	128,
	128,
	128,
	37,
	116,
	196,
	243,
	228,
	255,
	255,
	255,
	128,
	128,
	128,
	1,
	204,
	254,
	255,
	245,
	255,
	128,
	128,
	128,
	128,
	128,
	207,
	160,
	250,
	255,
	238,
	128,
	128,
	128,
	128,
	128,
	128,
	102,
	103,
	231,
	255,
	211,
	171,
	128,
	128,
	128,
	128,
	128,
	1,
	152,
	252,
	255,
	240,
	255,
	128,
	128,
	128,
	128,
	128,
	177,
	135,
	243,
	255,
	234,
	225,
	128,
	128,
	128,
	128,
	128,
	80,
	129,
	211,
	255,
	194,
	224,
	128,
	128,
	128,
	128,
	128,
	1,
	1,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	246,
	1,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	198,
	35,
	237,
	223,
	193,
	187,
	162,
	160,
	145,
	155,
	62,
	131,
	45,
	198,
	221,
	172,
	176,
	220,
	157,
	252,
	221,
	1,
	68,
	47,
	146,
	208,
	149,
	167,
	221,
	162,
	255,
	223,
	128,
	1,
	149,
	241,
	255,
	221,
	224,
	255,
	255,
	128,
	128,
	128,
	184,
	141,
	234,
	253,
	222,
	220,
	255,
	199,
	128,
	128,
	128,
	81,
	99,
	181,
	242,
	176,
	190,
	249,
	202,
	255,
	255,
	128,
	1,
	129,
	232,
	253,
	214,
	197,
	242,
	196,
	255,
	255,
	128,
	99,
	121,
	210,
	250,
	201,
	198,
	255,
	202,
	128,
	128,
	128,
	23,
	91,
	163,
	242,
	170,
	187,
	247,
	210,
	255,
	255,
	128,
	1,
	200,
	246,
	255,
	234,
	255,
	128,
	128,
	128,
	128,
	128,
	109,
	178,
	241,
	255,
	231,
	245,
	255,
	255,
	128,
	128,
	128,
	44,
	130,
	201,
	253,
	205,
	192,
	255,
	255,
	128,
	128,
	128,
	1,
	132,
	239,
	251,
	219,
	209,
	255,
	165,
	128,
	128,
	128,
	94,
	136,
	225,
	251,
	218,
	190,
	255,
	255,
	128,
	128,
	128,
	22,
	100,
	174,
	245,
	186,
	161,
	255,
	199,
	128,
	128,
	128,
	1,
	182,
	249,
	255,
	232,
	235,
	128,
	128,
	128,
	128,
	128,
	124,
	143,
	241,
	255,
	227,
	234,
	128,
	128,
	128,
	128,
	128,
	35,
	77,
	181,
	251,
	193,
	211,
	255,
	205,
	128,
	128,
	128,
	1,
	157,
	247,
	255,
	236,
	231,
	255,
	255,
	128,
	128,
	128,
	121,
	141,
	235,
	255,
	225,
	227,
	255,
	255,
	128,
	128,
	128,
	45,
	99,
	188,
	251,
	195,
	217,
	255,
	224,
	128,
	128,
	128,
	1,
	1,
	251,
	255,
	213,
	255,
	128,
	128,
	128,
	128,
	128,
	203,
	1,
	248,
	255,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	137,
	1,
	177,
	255,
	224,
	255,
	128,
	128,
	128,
	128,
	128,
	253,
	9,
	248,
	251,
	207,
	208,
	255,
	192,
	128,
	128,
	128,
	175,
	13,
	224,
	243,
	193,
	185,
	249,
	198,
	255,
	255,
	128,
	73,
	17,
	171,
	221,
	161,
	179,
	236,
	167,
	255,
	234,
	128,
	1,
	95,
	247,
	253,
	212,
	183,
	255,
	255,
	128,
	128,
	128,
	239,
	90,
	244,
	250,
	211,
	209,
	255,
	255,
	128,
	128,
	128,
	155,
	77,
	195,
	248,
	188,
	195,
	255,
	255,
	128,
	128,
	128,
	1,
	24,
	239,
	251,
	218,
	219,
	255,
	205,
	128,
	128,
	128,
	201,
	51,
	219,
	255,
	196,
	186,
	128,
	128,
	128,
	128,
	128,
	69,
	46,
	190,
	239,
	201,
	218,
	255,
	228,
	128,
	128,
	128,
	1,
	191,
	251,
	255,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	223,
	165,
	249,
	255,
	213,
	255,
	128,
	128,
	128,
	128,
	128,
	141,
	124,
	248,
	255,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	1,
	16,
	248,
	255,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	190,
	36,
	230,
	255,
	236,
	255,
	128,
	128,
	128,
	128,
	128,
	149,
	1,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	1,
	226,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	247,
	192,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	240,
	128,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	1,
	134,
	252,
	255,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	213,
	62,
	250,
	255,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	55,
	93,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	202,
	24,
	213,
	235,
	186,
	191,
	220,
	160,
	240,
	175,
	255,
	126,
	38,
	182,
	232,
	169,
	184,
	228,
	174,
	255,
	187,
	128,
	61,
	46,
	138,
	219,
	151,
	178,
	240,
	170,
	255,
	216,
	128,
	1,
	112,
	230,
	250,
	199,
	191,
	247,
	159,
	255,
	255,
	128,
	166,
	109,
	228,
	252,
	211,
	215,
	255,
	174,
	128,
	128,
	128,
	39,
	77,
	162,
	232,
	172,
	180,
	245,
	178,
	255,
	255,
	128,
	1,
	52,
	220,
	246,
	198,
	199,
	249,
	220,
	255,
	255,
	128,
	124,
	74,
	191,
	243,
	183,
	193,
	250,
	221,
	255,
	255,
	128,
	24,
	71,
	130,
	219,
	154,
	170,
	243,
	182,
	255,
	255,
	128,
	1,
	182,
	225,
	249,
	219,
	240,
	255,
	224,
	128,
	128,
	128,
	149,
	150,
	226,
	252,
	216,
	205,
	255,
	171,
	128,
	128,
	128,
	28,
	108,
	170,
	242,
	183,
	194,
	254,
	223,
	255,
	255,
	128,
	1,
	81,
	230,
	252,
	204,
	203,
	255,
	192,
	128,
	128,
	128,
	123,
	102,
	209,
	247,
	188,
	196,
	255,
	233,
	128,
	128,
	128,
	20,
	95,
	153,
	243,
	164,
	173,
	255,
	203,
	128,
	128,
	128,
	1,
	222,
	248,
	255,
	216,
	213,
	128,
	128,
	128,
	128,
	128,
	168,
	175,
	246,
	252,
	235,
	205,
	255,
	255,
	128,
	128,
	128,
	47,
	116,
	215,
	255,
	211,
	212,
	255,
	255,
	128,
	128,
	128,
	1,
	121,
	236,
	253,
	212,
	214,
	255,
	255,
	128,
	128,
	128,
	141,
	84,
	213,
	252,
	201,
	202,
	255,
	219,
	128,
	128,
	128,
	42,
	80,
	160,
	240,
	162,
	185,
	255,
	205,
	128,
	128,
	128,
	1,
	1,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	244,
	1,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	238,
	1,
	255,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
	128,
]

const vp8_coeffs_update_proba = [
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	176,
	246,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	223,
	241,
	252,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	249,
	253,
	253,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	244,
	252,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	234,
	254,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	253,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	246,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	239,
	253,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	248,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	251,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	253,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	251,
	254,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	253,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	250,
	255,
	254,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	217,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	225,
	252,
	241,
	253,
	255,
	255,
	254,
	255,
	255,
	255,
	255,
	234,
	250,
	241,
	250,
	253,
	255,
	253,
	254,
	255,
	255,
	255,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	223,
	254,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	238,
	253,
	254,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	248,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	249,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	253,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	247,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	253,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	252,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	253,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	253,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	250,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	186,
	251,
	250,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	234,
	251,
	244,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	251,
	251,
	243,
	253,
	254,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	253,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	236,
	253,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	251,
	253,
	253,
	254,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	254,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	248,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	250,
	254,
	252,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	248,
	254,
	249,
	253,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	253,
	253,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	246,
	253,
	253,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	252,
	254,
	251,
	254,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	252,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	248,
	254,
	253,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	253,
	255,
	254,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	251,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	245,
	251,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	253,
	253,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	251,
	253,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	252,
	253,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	252,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	249,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	253,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	250,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	254,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
	255,
]

const vp8_bmodes_proba = [
	231,
	120,
	48,
	89,
	115,
	113,
	120,
	152,
	112,
	152,
	179,
	64,
	126,
	170,
	118,
	46,
	70,
	95,
	175,
	69,
	143,
	80,
	85,
	82,
	72,
	155,
	103,
	56,
	58,
	10,
	171,
	218,
	189,
	17,
	13,
	152,
	114,
	26,
	17,
	163,
	44,
	195,
	21,
	10,
	173,
	121,
	24,
	80,
	195,
	26,
	62,
	44,
	64,
	85,
	144,
	71,
	10,
	38,
	171,
	213,
	144,
	34,
	26,
	170,
	46,
	55,
	19,
	136,
	160,
	33,
	206,
	71,
	63,
	20,
	8,
	114,
	114,
	208,
	12,
	9,
	226,
	81,
	40,
	11,
	96,
	182,
	84,
	29,
	16,
	36,
	134,
	183,
	89,
	137,
	98,
	101,
	106,
	165,
	148,
	72,
	187,
	100,
	130,
	157,
	111,
	32,
	75,
	80,
	66,
	102,
	167,
	99,
	74,
	62,
	40,
	234,
	128,
	41,
	53,
	9,
	178,
	241,
	141,
	26,
	8,
	107,
	74,
	43,
	26,
	146,
	73,
	166,
	49,
	23,
	157,
	65,
	38,
	105,
	160,
	51,
	52,
	31,
	115,
	128,
	104,
	79,
	12,
	27,
	217,
	255,
	87,
	17,
	7,
	87,
	68,
	71,
	44,
	114,
	51,
	15,
	186,
	23,
	47,
	41,
	14,
	110,
	182,
	183,
	21,
	17,
	194,
	66,
	45,
	25,
	102,
	197,
	189,
	23,
	18,
	22,
	88,
	88,
	147,
	150,
	42,
	46,
	45,
	196,
	205,
	43,
	97,
	183,
	117,
	85,
	38,
	35,
	179,
	61,
	39,
	53,
	200,
	87,
	26,
	21,
	43,
	232,
	171,
	56,
	34,
	51,
	104,
	114,
	102,
	29,
	93,
	77,
	39,
	28,
	85,
	171,
	58,
	165,
	90,
	98,
	64,
	34,
	22,
	116,
	206,
	23,
	34,
	43,
	166,
	73,
	107,
	54,
	32,
	26,
	51,
	1,
	81,
	43,
	31,
	68,
	25,
	106,
	22,
	64,
	171,
	36,
	225,
	114,
	34,
	19,
	21,
	102,
	132,
	188,
	16,
	76,
	124,
	62,
	18,
	78,
	95,
	85,
	57,
	50,
	48,
	51,
	193,
	101,
	35,
	159,
	215,
	111,
	89,
	46,
	111,
	60,
	148,
	31,
	172,
	219,
	228,
	21,
	18,
	111,
	112,
	113,
	77,
	85,
	179,
	255,
	38,
	120,
	114,
	40,
	42,
	1,
	196,
	245,
	209,
	10,
	25,
	109,
	88,
	43,
	29,
	140,
	166,
	213,
	37,
	43,
	154,
	61,
	63,
	30,
	155,
	67,
	45,
	68,
	1,
	209,
	100,
	80,
	8,
	43,
	154,
	1,
	51,
	26,
	71,
	142,
	78,
	78,
	16,
	255,
	128,
	34,
	197,
	171,
	41,
	40,
	5,
	102,
	211,
	183,
	4,
	1,
	221,
	51,
	50,
	17,
	168,
	209,
	192,
	23,
	25,
	82,
	138,
	31,
	36,
	171,
	27,
	166,
	38,
	44,
	229,
	67,
	87,
	58,
	169,
	82,
	115,
	26,
	59,
	179,
	63,
	59,
	90,
	180,
	59,
	166,
	93,
	73,
	154,
	40,
	40,
	21,
	116,
	143,
	209,
	34,
	39,
	175,
	47,
	15,
	16,
	183,
	34,
	223,
	49,
	45,
	183,
	46,
	17,
	33,
	183,
	6,
	98,
	15,
	32,
	183,
	57,
	46,
	22,
	24,
	128,
	1,
	54,
	17,
	37,
	65,
	32,
	73,
	115,
	28,
	128,
	23,
	128,
	205,
	40,
	3,
	9,
	115,
	51,
	192,
	18,
	6,
	223,
	87,
	37,
	9,
	115,
	59,
	77,
	64,
	21,
	47,
	104,
	55,
	44,
	218,
	9,
	54,
	53,
	130,
	226,
	64,
	90,
	70,
	205,
	40,
	41,
	23,
	26,
	57,
	54,
	57,
	112,
	184,
	5,
	41,
	38,
	166,
	213,
	30,
	34,
	26,
	133,
	152,
	116,
	10,
	32,
	134,
	39,
	19,
	53,
	221,
	26,
	114,
	32,
	73,
	255,
	31,
	9,
	65,
	234,
	2,
	15,
	1,
	118,
	73,
	75,
	32,
	12,
	51,
	192,
	255,
	160,
	43,
	51,
	88,
	31,
	35,
	67,
	102,
	85,
	55,
	186,
	85,
	56,
	21,
	23,
	111,
	59,
	205,
	45,
	37,
	192,
	55,
	38,
	70,
	124,
	73,
	102,
	1,
	34,
	98,
	125,
	98,
	42,
	88,
	104,
	85,
	117,
	175,
	82,
	95,
	84,
	53,
	89,
	128,
	100,
	113,
	101,
	45,
	75,
	79,
	123,
	47,
	51,
	128,
	81,
	171,
	1,
	57,
	17,
	5,
	71,
	102,
	57,
	53,
	41,
	49,
	38,
	33,
	13,
	121,
	57,
	73,
	26,
	1,
	85,
	41,
	10,
	67,
	138,
	77,
	110,
	90,
	47,
	114,
	115,
	21,
	2,
	10,
	102,
	255,
	166,
	23,
	6,
	101,
	29,
	16,
	10,
	85,
	128,
	101,
	196,
	26,
	57,
	18,
	10,
	102,
	102,
	213,
	34,
	20,
	43,
	117,
	20,
	15,
	36,
	163,
	128,
	68,
	1,
	26,
	102,
	61,
	71,
	37,
	34,
	53,
	31,
	243,
	192,
	69,
	60,
	71,
	38,
	73,
	119,
	28,
	222,
	37,
	68,
	45,
	128,
	34,
	1,
	47,
	11,
	245,
	171,
	62,
	17,
	19,
	70,
	146,
	85,
	55,
	62,
	70,
	37,
	43,
	37,
	154,
	100,
	163,
	85,
	160,
	1,
	63,
	9,
	92,
	136,
	28,
	64,
	32,
	201,
	85,
	75,
	15,
	9,
	9,
	64,
	255,
	184,
	119,
	16,
	86,
	6,
	28,
	5,
	64,
	255,
	25,
	248,
	1,
	56,
	8,
	17,
	132,
	137,
	255,
	55,
	116,
	128,
	58,
	15,
	20,
	82,
	135,
	57,
	26,
	121,
	40,
	164,
	50,
	31,
	137,
	154,
	133,
	25,
	35,
	218,
	51,
	103,
	44,
	131,
	131,
	123,
	31,
	6,
	158,
	86,
	40,
	64,
	135,
	148,
	224,
	45,
	183,
	128,
	22,
	26,
	17,
	131,
	240,
	154,
	14,
	1,
	209,
	45,
	16,
	21,
	91,
	64,
	222,
	7,
	1,
	197,
	56,
	21,
	39,
	155,
	60,
	138,
	23,
	102,
	213,
	83,
	12,
	13,
	54,
	192,
	255,
	68,
	47,
	28,
	85,
	26,
	85,
	85,
	128,
	128,
	32,
	146,
	171,
	18,
	11,
	7,
	63,
	144,
	171,
	4,
	4,
	246,
	35,
	27,
	10,
	146,
	174,
	171,
	12,
	26,
	128,
	190,
	80,
	35,
	99,
	180,
	80,
	126,
	54,
	45,
	85,
	126,
	47,
	87,
	176,
	51,
	41,
	20,
	32,
	101,
	75,
	128,
	139,
	118,
	146,
	116,
	128,
	85,
	56,
	41,
	15,
	176,
	236,
	85,
	37,
	9,
	62,
	71,
	30,
	17,
	119,
	118,
	255,
	17,
	18,
	138,
	101,
	38,
	60,
	138,
	55,
	70,
	43,
	26,
	142,
	146,
	36,
	19,
	30,
	171,
	255,
	97,
	27,
	20,
	138,
	45,
	61,
	62,
	219,
	1,
	81,
	188,
	64,
	32,
	41,
	20,
	117,
	151,
	142,
	20,
	21,
	163,
	112,
	19,
	12,
	61,
	195,
	128,
	48,
	4,
	24,
]

const vp8_bands = [
	0,
	1,
	2,
	3,
	6,
	4,
	5,
	6,
	6,
	6,
	6,
	6,
	6,
	6,
	6,
	7,
	0,
]

const vp8_zigzag = [
	0,
	1,
	4,
	8,
	5,
	2,
	3,
	6,
	9,
	12,
	13,
	10,
	7,
	11,
	14,
	15,
]

const vp8_cat3 = [
	173,
	148,
	140,
	0,
]

const vp8_cat4 = [
	176,
	155,
	140,
	135,
	0,
]

const vp8_cat5 = [
	180,
	157,
	141,
	134,
	130,
	0,
]

const vp8_cat6 = [
	254,
	254,
	243,
	230,
	196,
	177,
	153,
	140,
	133,
	130,
	129,
	0,
]

const vp8_dc_table = [
	4,
	5,
	6,
	7,
	8,
	9,
	10,
	10,
	11,
	12,
	13,
	14,
	15,
	16,
	17,
	17,
	18,
	19,
	20,
	20,
	21,
	21,
	22,
	22,
	23,
	23,
	24,
	25,
	25,
	26,
	27,
	28,
	29,
	30,
	31,
	32,
	33,
	34,
	35,
	36,
	37,
	37,
	38,
	39,
	40,
	41,
	42,
	43,
	44,
	45,
	46,
	46,
	47,
	48,
	49,
	50,
	51,
	52,
	53,
	54,
	55,
	56,
	57,
	58,
	59,
	60,
	61,
	62,
	63,
	64,
	65,
	66,
	67,
	68,
	69,
	70,
	71,
	72,
	73,
	74,
	75,
	76,
	76,
	77,
	78,
	79,
	80,
	81,
	82,
	83,
	84,
	85,
	86,
	87,
	88,
	89,
	91,
	93,
	95,
	96,
	98,
	100,
	101,
	102,
	104,
	106,
	108,
	110,
	112,
	114,
	116,
	118,
	122,
	124,
	126,
	128,
	130,
	132,
	134,
	136,
	138,
	140,
	143,
	145,
	148,
	151,
	154,
	157,
]

const vp8_ac_table = [
	4,
	5,
	6,
	7,
	8,
	9,
	10,
	11,
	12,
	13,
	14,
	15,
	16,
	17,
	18,
	19,
	20,
	21,
	22,
	23,
	24,
	25,
	26,
	27,
	28,
	29,
	30,
	31,
	32,
	33,
	34,
	35,
	36,
	37,
	38,
	39,
	40,
	41,
	42,
	43,
	44,
	45,
	46,
	47,
	48,
	49,
	50,
	51,
	52,
	53,
	54,
	55,
	56,
	57,
	58,
	60,
	62,
	64,
	66,
	68,
	70,
	72,
	74,
	76,
	78,
	80,
	82,
	84,
	86,
	88,
	90,
	92,
	94,
	96,
	98,
	100,
	102,
	104,
	106,
	108,
	110,
	112,
	114,
	116,
	119,
	122,
	125,
	128,
	131,
	134,
	137,
	140,
	143,
	146,
	149,
	152,
	155,
	158,
	161,
	164,
	167,
	170,
	173,
	177,
	181,
	185,
	189,
	193,
	197,
	201,
	205,
	209,
	213,
	217,
	221,
	225,
	229,
	234,
	239,
	245,
	249,
	254,
	259,
	264,
	269,
	274,
	279,
	284,
]

const code_length_code_order = [
	17,
	18,
	0,
	1,
	2,
	3,
	4,
	5,
	16,
	6,
	7,
	8,
	9,
	10,
	11,
	12,
	13,
	14,
	15,
]

const distance_map_dx = [
	0,
	1,
	1,
	-1,
	0,
	2,
	1,
	-1,
	2,
	-2,
	2,
	-2,
	0,
	3,
	1,
	-1,
	3,
	-3,
	2,
	-2,
	3,
	-3,
	0,
	4,
	1,
	-1,
	4,
	-4,
	3,
	-3,
	2,
	-2,
	4,
	-4,
	0,
	3,
	-3,
	4,
	-4,
	5,
	1,
	-1,
	5,
	-5,
	2,
	-2,
	5,
	-5,
	4,
	-4,
	3,
	-3,
	5,
	-5,
	0,
	6,
	1,
	-1,
	6,
	-6,
	2,
	-2,
	6,
	-6,
	4,
	-4,
	5,
	-5,
	3,
	-3,
	6,
	-6,
	0,
	7,
	1,
	-1,
	5,
	-5,
	7,
	-7,
	4,
	-4,
	6,
	-6,
	2,
	-2,
	7,
	-7,
	3,
	-3,
	7,
	-7,
	5,
	-5,
	6,
	-6,
	8,
	4,
	-4,
	7,
	-7,
	8,
	8,
	6,
	-6,
	8,
	5,
	-5,
	7,
	-7,
	8,
	6,
	-6,
	7,
	-7,
	8,
	7,
	-7,
	8,
	8,
]

const distance_map_dy = [
	1,
	0,
	1,
	1,
	2,
	0,
	2,
	2,
	1,
	1,
	2,
	2,
	3,
	0,
	3,
	3,
	1,
	1,
	3,
	3,
	2,
	2,
	4,
	0,
	4,
	4,
	1,
	1,
	3,
	3,
	4,
	4,
	2,
	2,
	5,
	4,
	4,
	3,
	3,
	0,
	5,
	5,
	1,
	1,
	5,
	5,
	2,
	2,
	4,
	4,
	5,
	5,
	3,
	3,
	6,
	0,
	6,
	6,
	1,
	1,
	6,
	6,
	2,
	2,
	5,
	5,
	4,
	4,
	6,
	6,
	3,
	3,
	7,
	0,
	7,
	7,
	5,
	5,
	1,
	1,
	6,
	6,
	4,
	4,
	7,
	7,
	2,
	2,
	7,
	7,
	3,
	3,
	6,
	6,
	5,
	5,
	0,
	7,
	7,
	4,
	4,
	1,
	2,
	6,
	6,
	3,
	7,
	7,
	5,
	5,
	4,
	7,
	7,
	6,
	6,
	5,
	7,
	7,
	6,
	7,
]

// ============================================================================
// Arena allocator
// ============================================================================
//
// The Windows viewer build is effectively `-gc none` (no Boehm collector), so
// every array the decoder allocates must be released explicitly or it leaks for
// the life of the process. The WEBP decode path allocates a large, heterogeneous
// set of short-lived arrays: Huffman trees, prefix groups, transform images, the
// pixel chain, the VP8 planes, per-macroblock coefficient scratch, and the
// nested probability bands. Rather than pre-size a single typed region (the
// element types vary from `[]int` to `[][][][]int` to `[]Transform`), the arena
// TRACKS every owned allocation and releases them all in one `free_all()` at the
// end of each public decode call, on both the success and the error path.
//
// Tracking retains each owned array header and delegates release to `array.free()`,
// which handles the current managed allocation layout and alignment. Only OWNED arrays (created with `[]T{len: n}`, array literals,
// or `.clone()`) are tracked; slices that view into `d.data` or into another
// buffer are never tracked (freeing a slice base would corrupt the parent block).
//
// A separate bump `scratch` region backs the hot per-macroblock scratch (the
// 16-element IDCT/predict temporaries and the coefficient block) so those
// thousands of tiny allocations are reused between macroblocks instead of each
// being tracked and held until the end. The returned 8-bit image ([]u8 produced
// in decode()) is allocated OUTSIDE the arena and owned by the caller.
struct Arena {
mut:
	blocks  [][]u8 // owned array headers, released with the builtin array method
	n       int    // number of tracked arrays in `blocks`
	scratch []int  // per-macroblock bump region
	spos    int    // bump cursor into `scratch`
}

// push_block appends an owned array header, growing the tracking array and freeing the old
// backing block so the growth itself never leaks (a plain `<<` on a growing
// `[][]u8` would leak each intermediate block under `-gc none`).
fn (mut a Arena) push_block(block []u8) {
	if a.n >= a.blocks.len {
		mut newcap := if a.blocks.len == 0 { 1024 } else { a.blocks.len * 2 }
		if newcap <= a.n {
			newcap = a.n + 1
		}
		mut np := [][]u8{len: newcap}
		for i in 0 .. a.n {
			unsafe { np[i] = a.blocks[i] }
		}
		unsafe { a.blocks.free() }
		unsafe { a.blocks = np }
	}
	unsafe { a.blocks[a.n] = block }
	a.n++
}

// track registers an owned array so its backing block is freed in free_all().
// Retain the header without copying the elements. Never call this on a slice view.
@[unsafe]
fn (mut a Arena) track[T](arr []T) {
	if arr.data == unsafe { nil } {
		return
	}
	// Generic arrays share the builtin header layout; the arena owns this copy.
	a.push_block(unsafe { *(&[]u8(voidptr(&arr))) })
}

// push appends `val` to `arr`, growing it by allocating a fresh backing block and
// freeing the old one. A plain `arr << val` leaks the previous backing block for
// slice/pointer-bearing element types under `-gc none`; this frees it. The caller
// still `track`s the final array so the live backing is released in free_all().
fn (mut a Arena) push[T](mut arr []T, val T) {
	if arr.len + 1 > arr.cap {
		mut newcap := if arr.cap == 0 { 8 } else { arr.cap * 2 }
		if newcap <= arr.len {
			newcap = arr.len + 1
		}
		mut na := []T{len: arr.len, cap: newcap}
		for i in 0 .. arr.len {
			na[i] = arr[i]
		}
		if arr.cap > 0 {
			unsafe { arr.free() }
		}
		// Shallow move: a plain `arr = na` makes V insert a deep `.clone()`
		// (array__clone_to_depth) for pointer-bearing element types, which leaks
		// `na`'s backing block. `unsafe` performs the header copy we actually want.
		unsafe { arr = na }
	}
	// `arr << val` deep-clones pointer-bearing element fields (e.g. Chunk.data),
	// leaking a copy of the slice. Write the element through a typed pointer
	// (shallow) instead.
	mut p := &T(arr.data)
	p[arr.len] = val
	arr.len++
}

// alloc_scratch returns a bump sub-slice of the per-macroblock scratch region.
fn (mut a Arena) alloc_scratch(n int) ![]int {
	if n <= 0 {
		return []int{}
	}
	if a.scratch.len == 0 {
		return error('arena: scratch region not initialised')
	}
	if a.spos + n > a.scratch.len {
		return error('arena: scratch region exhausted (${a.spos + n} > ${a.scratch.len})')
	}
	res := unsafe { a.scratch[a.spos..a.spos + n] }
	a.spos += n
	return res
}

// reset_scratch rewinds the bump cursor so the per-macroblock temporaries are
// reused between macroblocks. The previously used region is zeroed first: the
// original code allocated these buffers with `[]int{len: n}` (zero-initialised),
// and the coefficient/IDCT writers only overwrite part of each buffer, so the
// untouched positions must read back as zero rather than the prior block's data.
fn (mut a Arena) reset_scratch() {
	for i in 0 .. a.spos {
		a.scratch[i] = 0
	}
	a.spos = 0
}

// init_scratch sizes the per-macroblock scratch region (idempotent for a page).
fn (mut a Arena) init_scratch(cap int) {
	if a.scratch.len < cap {
		unsafe { a.scratch.free() }
		a.scratch = []int{len: cap}
		a.spos = 0
	}
}

// free_all releases every tracked backing block and the scratch region.
fn (mut a Arena) free_all() {
	$if dbg_webp_leak ? {
		eprintln('arena free_all: tracked=${a.n}')
	}
	for i in 0 .. a.n {
		unsafe { a.blocks[i].free() }
	}
	unsafe {
		a.blocks.free()
		a.scratch.free()
	}
	a.blocks = [][]u8{}
	a.scratch = []int{}
	a.n = 0
	a.spos = 0
}

// ============================================================================
// Data structures
// ============================================================================

struct Chunk {
mut:
	fourcc  []u8
	payload []u8
}

// AnimFrameSpec holds one parsed ANMF frame. The payload/alph slices view
// into d.data, so the spec array is tracked but its elements never are.
struct AnimFrameSpec {
mut:
	x          int
	y          int
	w          int
	h          int
	duration   int
	dispose_bg bool
	blend      bool
	fourcc     []u8
	payload    []u8
	alph       []u8
}

// WebPFrame is one fully composited animation frame. The rgba buffer is the
// complete canvas after drawing this frame; the caller owns it and must free
// it (same rule as common.Image.rgba).
pub struct WebPFrame {
pub:
	rgba         []u8
	width        int
	height       int
	duration_ms  int
	dispose_bg   bool
	blend        bool
	x            int
	y            int
	frame_width  int
	frame_height int
}

// WebPAnimation is a decoded, composited animation ready for a viewer to
// play: full-canvas frames plus loop, timing, and background helpers that
// mirror the GIF animation API.
pub struct WebPAnimation {
pub:
	frames     []WebPFrame
	loop_count int
	width      int
	height     int
	bg_r       int
	bg_g       int
	bg_b       int
	bg_a       int
}

pub fn (a &WebPAnimation) frame_count() int {
	return a.frames.len
}

pub fn (a &WebPAnimation) total_duration_ms() int {
	mut total := 0
	for f in a.frames {
		total += f.duration_ms
	}
	return total
}

pub fn (a &WebPAnimation) is_animated() bool {
	return a.frames.len > 1
}

// frame_index_at maps an elapsed time to the frame to display. loop_count 0
// wraps forever; a finite loop clamps to the last frame at the end.
pub fn (a &WebPAnimation) frame_index_at(elapsed_ms int) int {
	n := a.frames.len
	if n == 0 {
		return 0
	}
	total := a.total_duration_ms()
	if total <= 0 {
		return 0
	}
	mut t := elapsed_ms % total
	if a.loop_count != 0 {
		end := total * a.loop_count
		if elapsed_ms >= end {
			return n - 1
		}
	}
	mut acc := 0
	for i, f in a.frames {
		acc += f.duration_ms
		if t < acc {
			return i
		}
	}
	return n - 1
}

struct BooleanDecoder {
mut:
	data  []u8
	pos   int
	end   int
	range int
	value int
	bits  int
	eof   int
}

struct HuffmanTree {
mut:
	single         int
	max_len        int
	counts         []int
	sorted_symbols []int
}

struct PrefixGroup {
mut:
	green HuffmanTree
	red   HuffmanTree
	blue  HuffmanTree
	alpha HuffmanTree
	dist  HuffmanTree
}

struct QuantMat {
mut:
	y1       []int
	y2       []int
	uv       []int
	uv_quant int
}

struct FilterInfo {
mut:
	f_ilevel   int
	f_limit    int
	hev_thresh int
	f_inner    int
}

struct NzState {
mut:
	mb_x        int
	above_nz    []int
	above_nz_dc []int
	left_nz     int
	left_nz_dc  int
}

struct MbBlock {
mut:
	segment int
	skip    int
	is_i4x4 int
	imodes  []int
	uvmode  int
}

struct Proba {
mut:
	bands          [][][][]int
	bands_ptr      [][][][]int
	use_skip_proba int
	skip_p         int
}

struct Transform {
mut:
	kind         int // 0 predictor, 1 color, 2 subtract_green, 3 color_indexing
	size_bits    int
	tw           int
	img          []int
	table        []int
	width_bits   int
	width_before int
}

// ============================================================================
// BooleanDecoder (VP8 lossy arithmetic decoder)
// ============================================================================

fn new_boolean_decoder(data []u8, pos int, size int) BooleanDecoder {
	mut end := pos + size
	if size < 0 {
		end = data.len
	}
	mut br := BooleanDecoder{
		data:  data
		pos:   pos
		end:   end
		range: 254
		value: 0
		bits:  -8
		eof:   0
	}
	br.load_new_bytes()
	return br
}

fn (mut br BooleanDecoder) load_new_bytes() {
	if br.pos < br.end {
		br.value = (int(br.data[br.pos]) | (br.value << 8)) & mask32
		br.pos++
		br.bits += 8
	} else if br.eof == 0 {
		br.value = (br.value << 8) & mask32
		br.bits += 8
		br.eof = 1
	} else {
		br.bits = 0
	}
}

fn (mut br BooleanDecoder) get_bit(prob int) int {
	mut range_ := br.range
	if br.bits < 0 {
		br.load_new_bytes()
	}
	pos := br.bits
	split := (range_ * prob) >> 8
	value := (br.value >> pos) & mask32
	mut bit := 0
	if value > split {
		bit = 1
	}
	if bit != 0 {
		range_ -= split
		br.value = (br.value - ((split + 1) << pos)) & mask32
	} else {
		range_ = split + 1
	}
	shift := 7 ^ bits_log2_floor(range_)
	range_ <<= shift
	br.bits -= shift
	br.range = range_ - 1
	return bit
}

fn (mut br BooleanDecoder) get_value(n int) int {
	mut v := 0
	for _ in 0 .. n {
		v = (v * 2) | br.get_bit(0x80)
	}
	return v
}

fn (mut br BooleanDecoder) get_signed_value(n int) int {
	value := br.get_value(n)
	if br.get_bit(0x80) != 0 {
		return -value
	}
	return value
}

fn (mut br BooleanDecoder) get_signed(v int) int {
	if br.bits < 0 {
		br.load_new_bytes()
	}
	pos := br.bits
	split := br.range >> 1
	value := (br.value >> pos) & mask32
	mask := (split - value) >> 31
	br.bits -= 1
	br.range = (br.range + mask) | 1
	br.value = (br.value - (((split + 1) & mask) << pos)) & mask32
	return (v ^ mask) - mask
}

// ============================================================================
// HuffmanTree (canonical prefix code)
// ============================================================================

fn new_huffman_tree(mut a Arena, code_lengths []int) !HuffmanTree {
	mut nonzero := []int{len: code_lengths.len}
	a.track(nonzero)
	mut nnz := 0
	for i, length in code_lengths {
		if length != 0 {
			nonzero[nnz] = i
			nnz++
		}
	}
	if nnz == 1 && code_lengths[nonzero[0]] == 1 {
		mut counts := [0, 1]
		mut sorted := [nonzero[0]]
		a.track(counts)
		a.track(sorted)
		return HuffmanTree{
			single:         nonzero[0]
			max_len:        1
			counts:         counts
			sorted_symbols: sorted
		}
	}
	if nnz == 0 {
		return error('Empty Huffman code (no symbols)')
	}
	mut max_len := 0
	for length in code_lengths {
		if length > max_len {
			max_len = length
		}
	}
	if max_len > 15 {
		return error('Huffman code length too large: ${max_len}')
	}
	mut counts := []int{len: max_len + 1}
	a.track(counts)
	for length in code_lengths {
		if length != 0 {
			counts[length]++
		}
	}
	mut sorted_symbols := []int{len: nnz}
	a.track(sorted_symbols)
	mut si := 0
	for length in 1 .. max_len + 1 {
		for sym in 0 .. code_lengths.len {
			if code_lengths[sym] == length {
				sorted_symbols[si] = sym
				si++
			}
		}
	}
	return HuffmanTree{
		single:         -1
		max_len:        max_len
		counts:         counts
		sorted_symbols: sorted_symbols
	}
}

fn huff_decode(t &HuffmanTree, mut br common.BitReader) !int {
	if t.single != -1 {
		return t.single
	}
	mut code := 0
	mut first := 0
	mut index := 0
	mut length := 0
	for {
		code = (code * 2) | int(br.read_bits(1)!)
		length++
		if length > t.max_len {
			return error('Invalid Huffman code: overlong code')
		}
		count := t.counts[length]
		if code - first < count {
			return t.sorted_symbols[index + (code - first)]
		}
		index += count
		first = (first + count) << 1
	}
}

// ============================================================================
// VP8L lossless helpers
// ============================================================================

fn (mut d WebpDecoder) read_color_cache_info(mut br common.BitReader) !int {
	if int(br.read_bits(1)!) == 0 {
		return 0
	}
	bits := int(br.read_bits(4)!)
	if bits < 1 || bits > 11 {
		return error('Invalid VP8L color cache bits: ${bits}')
	}
	return bits
}

fn (mut d WebpDecoder) read_prefix_code(mut br common.BitReader, alphabet_size int) !HuffmanTree {
	if int(br.read_bits(1)!) != 0 {
		num_symbols := int(br.read_bits(1)!) + 1
		is_first_8bits := int(br.read_bits(1)!)
		symbol0 := int(br.read_bits(1 + 7 * is_first_8bits)!)
		mut code_lengths := []int{len: alphabet_size}
		d.arena.track(code_lengths)
		if symbol0 >= alphabet_size {
			return error('VP8L simple symbol ${symbol0} exceeds alphabet ${alphabet_size}')
		}
		code_lengths[symbol0] = 1
		if num_symbols == 2 {
			symbol1 := int(br.read_bits(8)!)
			if symbol1 >= alphabet_size {
				return error('VP8L simple symbol ${symbol1} exceeds alphabet ${alphabet_size}')
			}
			code_lengths[symbol1] = 1
		}
		return new_huffman_tree(mut d.arena, code_lengths)
	}
	num_code_lengths := 4 + int(br.read_bits(4)!)
	mut cl_lengths := []int{len: 19}
	d.arena.track(cl_lengths)
	for i in 0 .. num_code_lengths {
		cl_lengths[code_length_code_order[i]] = int(br.read_bits(3)!)
	}
	cl_tree := new_huffman_tree(mut d.arena, cl_lengths)!
	mut max_symbol := 0
	if int(br.read_bits(1)!) == 0 {
		max_symbol = alphabet_size
	} else {
		length_nbits := 2 + 2 * int(br.read_bits(3)!)
		max_symbol = 2 + int(br.read_bits(length_nbits)!)
	}
	if max_symbol > alphabet_size {
		return error('VP8L max_symbol ${max_symbol} exceeds alphabet ${alphabet_size}')
	}
	mut code_lengths := []int{len: alphabet_size}
	d.arena.track(code_lengths)
	mut prev := 8
	mut i := 0
	for i < alphabet_size {
		if max_symbol == 0 {
			break
		}
		max_symbol--
		sym := huff_decode(&cl_tree, mut br)!
		if sym < 16 {
			code_lengths[i] = sym
			if sym != 0 {
				prev = sym
			}
			i++
		} else {
			slot := sym - 16
			extra_bits := [2, 3, 7][slot]
			repeat_offset := [3, 3, 11][slot]
			repeat := int(br.read_bits(extra_bits)!) + repeat_offset
			if i + repeat > alphabet_size {
				return error('VP8L code length repeat overflow')
			}
			mut length := 0
			if sym == 16 {
				length = prev
			}
			for _ in 0 .. repeat {
				code_lengths[i] = length
				i++
			}
		}
	}
	return new_huffman_tree(mut d.arena, code_lengths)
}

fn (mut d WebpDecoder) read_prefix_group(mut br common.BitReader, cache_size int) !PrefixGroup {
	green := d.read_prefix_code(mut br, num_literal_codes + num_length_codes + cache_size)!
	red := d.read_prefix_code(mut br, num_literal_codes)!
	blue := d.read_prefix_code(mut br, num_literal_codes)!
	alpha := d.read_prefix_code(mut br, num_literal_codes)!
	dist := d.read_prefix_code(mut br, distance_alpha_size)!
	return PrefixGroup{
		green: green
		red:   red
		blue:  blue
		alpha: alpha
		dist:  dist
	}
}

fn (mut d WebpDecoder) read_prefix_value(mut br common.BitReader, prefix_code int) !int {
	if prefix_code < 4 {
		return prefix_code + 1
	}
	extra := (prefix_code - 2) >> 1
	offset := (2 + (prefix_code & 1)) << extra
	return offset + int(br.read_bits(extra)!) + 1
}

fn (mut d WebpDecoder) read_distance_value(mut br common.BitReader, dist_prefix int, width int) !int {
	code := d.read_prefix_value(mut br, dist_prefix)!
	if code > 120 {
		return code - 120
	}
	dx := distance_map_dx[code - 1]
	dy := distance_map_dy[code - 1]
	mut dist := dx + dy * width
	if dist < 1 {
		dist = 1
	}
	return dist
}

fn (mut d WebpDecoder) decode_pixels(mut br common.BitReader, width int, height int, groups []PrefixGroup, cache_bits int, entropy []int, has_entropy bool, prefix_bits int, prefix_image_width int) ![]int {
	total := width * height
	mut pixels := []int{len: total}
	d.arena.track(pixels)
	mut cache_size := 0
	if cache_bits != 0 {
		cache_size = 1 << cache_bits
	}
	mut cache := []int{len: cache_size}
	d.arena.track(cache)
	single_group := !has_entropy
	group0 := groups[0]
	mut shift := 0
	if cache_bits != 0 {
		shift = 32 - cache_bits
	}
	mut idx := 0
	for idx < total {
		mut group := group0
		if !single_group {
			x := idx % width
			y := idx / width
			position := (y >> prefix_bits) * prefix_image_width + (x >> prefix_bits)
			meta := (entropy[position] >> 8) & 0xffff
			group = groups[meta]
		}
		s := huff_decode(&group.green, mut br)!
		if s < num_literal_codes {
			red := huff_decode(&group.red, mut br)!
			blue := huff_decode(&group.blue, mut br)!
			alpha := huff_decode(&group.alpha, mut br)!
			argb := (alpha << 24) | (red << 16) | (s << 8) | blue
			pixels[idx] = argb
			if cache_size > 0 {
				ci := ((color_cache_multiplier * argb) & mask32) >> shift
				cache[ci] = argb
			}
			idx++
		} else if s < color_cache_alpha_base {
			length := d.read_prefix_value(mut br, s - num_literal_codes)!
			dist_prefix := huff_decode(&group.dist, mut br)!
			dist := d.read_distance_value(mut br, dist_prefix, width)!
			src := idx - dist
			if src < 0 {
				return error('VP8L backward reference before start of image')
			}
			end := idx + length
			if end > total {
				return error('VP8L backward reference exceeds image size')
			}
			for j in 0 .. length {
				v := pixels[src + j]
				pixels[idx + j] = v
				if cache_size > 0 {
					ci := ((color_cache_multiplier * v) & mask32) >> shift
					cache[ci] = v
				}
			}
			idx = end
		} else {
			if cache_size == 0 {
				return error('VP8L color cache symbol but no color cache present')
			}
			ci := s - color_cache_alpha_base
			if ci >= cache_size {
				return error('VP8L color cache index out of range')
			}
			pixels[idx] = cache[ci]
			idx++
		}
	}
	return pixels
}

fn (mut d WebpDecoder) decode_entropy_image(mut br common.BitReader, width int, height int) ![]int {
	cache_bits := d.read_color_cache_info(mut br)!
	mut cache_size := 0
	if cache_bits != 0 {
		cache_size = 1 << cache_bits
	}
	group := d.read_prefix_group(mut br, cache_size)!
	mut groups := []PrefixGroup{len: 1}
	groups[0] = group
	d.arena.track(groups)
	return d.decode_pixels(mut br, width, height, groups, cache_bits, []int{}, false, 0, 0)!
}

fn (mut d WebpDecoder) decode_main_image(mut br common.BitReader, width int, height int) ![]int {
	cache_bits := d.read_color_cache_info(mut br)!
	mut cache_size := 0
	if cache_bits != 0 {
		cache_size = 1 << cache_bits
	}
	if int(br.read_bits(1)!) == 0 {
		mut groups := []PrefixGroup{len: 1}
		groups[0] = d.read_prefix_group(mut br, cache_size)!
		d.arena.track(groups)
		return d.decode_pixels(mut br, width, height, groups, cache_bits, []int{}, false, 0, 0)!
	}
	prefix_bits := int(br.read_bits(3)!) + 2
	piw := div_round_up(width, 1 << prefix_bits)
	pih := div_round_up(height, 1 << prefix_bits)
	entropy := d.decode_entropy_image(mut br, piw, pih)!
	mut max_meta := 0
	for p in entropy {
		m := (p >> 8) & 0xffff
		if m > max_meta {
			max_meta = m
		}
	}
	num_groups := max_meta + 1
	mut groups := []PrefixGroup{len: num_groups}
	d.arena.track(groups)
	for gi in 0 .. num_groups {
		groups[gi] = d.read_prefix_group(mut br, cache_size)!
	}
	return d.decode_pixels(mut br, width, height, groups, cache_bits, entropy, true, prefix_bits, piw)!
}

fn (mut d WebpDecoder) decode_vp8l(payload []u8) ![]int {
	mut br := common.new_bitreader_lsb(payload)
	sig := int(br.read_bits(8)!)
	if sig != vp8l_signature {
		return error('Invalid VP8L signature: 0x${sig:x}')
	}
	width := int(br.read_bits(14)!) + 1
	height := int(br.read_bits(14)!) + 1
	alpha_is_used := int(br.read_bits(1)!)
	version := int(br.read_bits(3)!)
	if version != 0 {
		return error('Unsupported VP8L version: ${version}')
	}
	d.width = width
	d.height = height
	d.has_alpha = alpha_is_used != 0
	return d.decode_vp8l_body(mut br, width, height)
}

fn (mut d WebpDecoder) decode_vp8l_body(mut br common.BitReader, width int, height int) ![]int {
	check_rgba_dimensions(width, height)!
	mut cur_w := width
	mut transforms := []Transform{}
	for int(br.read_bits(1)!) != 0 {
		ttype := int(br.read_bits(2)!)
		if ttype == 0 {
			size_bits := int(br.read_bits(3)!) + 2
			tw := div_round_up(cur_w, 1 << size_bits)
			th := div_round_up(height, 1 << size_bits)
			img := d.decode_entropy_image(mut br, tw, th)!
			d.arena.push(mut transforms, Transform{ kind: 0, size_bits: size_bits, tw: tw, img: img })
		} else if ttype == 1 {
			size_bits := int(br.read_bits(3)!) + 2
			tw := div_round_up(cur_w, 1 << size_bits)
			th := div_round_up(height, 1 << size_bits)
			img := d.decode_entropy_image(mut br, tw, th)!
			d.arena.push(mut transforms, Transform{ kind: 1, size_bits: size_bits, tw: tw, img: img })
		} else if ttype == 2 {
			d.arena.push(mut transforms, Transform{ kind: 2 })
		} else {
			color_table_size := int(br.read_bits(8)!) + 1
			mut table := d.decode_entropy_image(mut br, color_table_size, 1)!
			for i in 1 .. color_table_size {
				table[i] = argb_add_mod(table[i], table[i - 1])
			}
			mut width_bits := 0
			if color_table_size <= 2 {
				width_bits = 3
			} else if color_table_size <= 4 {
				width_bits = 2
			} else if color_table_size <= 16 {
				width_bits = 1
			} else {
				width_bits = 0
			}
			d.arena.push(mut transforms, Transform{ kind: 3, table: table, width_bits: width_bits, width_before: cur_w })
			cur_w = div_round_up(cur_w, 1 << width_bits)
		}
	}
	d.arena.track(transforms)
	mut pixels := d.decode_main_image(mut br, cur_w, height)!
	for ti in 0 .. transforms.len {
		t := transforms[transforms.len - 1 - ti]
		if t.kind == 0 {
			pixels = d.inverse_predictor(pixels, cur_w, height, t.size_bits, t.tw, t.img)!
		} else if t.kind == 1 {
			pixels = d.inverse_color(pixels, cur_w, height, t.size_bits, t.tw, t.img)!
		} else if t.kind == 2 {
			pixels = d.inverse_subtract_green(pixels)
		} else if t.kind == 3 {
			pixels = d.inverse_color_indexing(pixels, cur_w, height, t.table, t.width_bits, t.width_before)
			cur_w = t.width_before
		}
	}
	return pixels
}

fn (mut d WebpDecoder) inverse_predictor(pixels []int, width int, height int, size_bits int, tw int, pred_img []int) ![]int {
	mut out := pixels.clone()
	d.arena.track(out)
	for y in 0 .. height {
		row := y * width
		for x in 0 .. width {
			idx := row + x
			residual := pixels[idx]
			mut pred := 0
			if y == 0 && x == 0 {
				pred = 0xff000000
			} else if y == 0 {
				pred = out[idx - 1]
			} else if x == 0 {
				pred = out[idx - width]
			} else {
				block := (y >> size_bits) * tw + (x >> size_bits)
				mode := (pred_img[block] >> 8) & 0xff
				left := out[idx - 1]
				top := out[idx - width]
				top_left := out[idx - width - 1]
				mut top_right := 0
				if x == width - 1 {
					top_right = out[row]
				} else {
					top_right = out[idx - width + 1]
				}
				pred = predict(mode, left, top, top_right, top_left)!
			}
			out[idx] = argb_add_mod(residual, pred)
		}
	}
	return out
}

fn (mut d WebpDecoder) inverse_color(pixels []int, width int, height int, size_bits int, tw int, color_img []int) ![]int {
	mut out := pixels.clone()
	d.arena.track(out)
	for y in 0 .. height {
		row := y * width
		for x in 0 .. width {
			idx := row + x
			argb := pixels[idx]
			block := (y >> size_bits) * tw + (x >> size_bits)
			cte := color_img[block]
			green_to_red := to_int8(cte & 0xff)
			green_to_blue := to_int8((cte >> 8) & 0xff)
			red_to_blue := to_int8((cte >> 16) & 0xff)
			green := (argb >> 8) & 0xff
			red := (argb >> 16) & 0xff
			blue := argb & 0xff
			tmp_red := red + color_transform_delta(green_to_red, to_int8(green))
			mut tmp_blue := blue + color_transform_delta(green_to_blue, to_int8(green))
			tmp_blue += color_transform_delta(red_to_blue, to_int8(tmp_red & 0xff))
			out[idx] = ((argb & 0xff00ff00) | ((tmp_red & 0xff) << 16) | (tmp_blue & 0xff))
		}
	}
	return out
}

fn (mut d WebpDecoder) inverse_subtract_green(pixels []int) []int {
	mut out := pixels.clone()
	d.arena.track(out)
	for i, argb in pixels {
		green := (argb >> 8) & 0xff
		red := ((argb >> 16) + green) & 0xff
		blue := (argb + green) & 0xff
		out[i] = (argb & 0xff00ff00) | (red << 16) | blue
	}
	return out
}

fn (mut d WebpDecoder) inverse_color_indexing(pixels []int, reduced_w int, height int, table []int, width_bits int, full_w int) []int {
	n := 1 << width_bits
	bits_per := 8 >> width_bits
	mask := (1 << bits_per) - 1
	table_len := table.len
	mut out := []int{len: full_w * height}
	d.arena.track(out)
	for y in 0 .. height {
		for xr in 0 .. reduced_w {
			g := (pixels[y * reduced_w + xr] >> 8) & 0xff
			base := xr * n
			for k in 0 .. n {
				x := base + k
				if x >= full_w {
					break
				}
				index := (g >> (k * bits_per)) & mask
				if index < table_len {
					out[y * full_w + x] = table[index]
				} else {
					out[y * full_w + x] = 0
				}
			}
		}
	}
	return out
}

// ============================================================================
// ALPH alpha plane
// ============================================================================

fn alph_unfilter_row(mut a Arena, filter_type int, prev []int, has_prev bool, deltas []int, width int) []int {
	mut out := []int{len: width}
	a.track(out)
	if filter_type == webp_filter_none {
		for i in 0 .. width {
			out[i] = deltas[i]
		}
		return out
	}
	if filter_type == webp_filter_horizontal {
		mut pred := 0
		if has_prev {
			pred = prev[0]
		}
		for i in 0 .. width {
			pred = (pred + deltas[i]) & 0xff
			out[i] = pred
		}
		return out
	}
	if filter_type == webp_filter_vertical {
		if !has_prev {
			return alph_unfilter_row(mut a, webp_filter_horizontal, []int{}, false, deltas, width)
		}
		for i in 0 .. width {
			out[i] = (prev[i] + deltas[i]) & 0xff
		}
		return out
	}
	// gradient
	if !has_prev {
		return alph_unfilter_row(mut a, webp_filter_horizontal, []int{}, false, deltas, width)
	}
	mut top := prev[0]
	mut top_left := top
	mut left := top
	for i in 0 .. width {
		top = prev[i]
		g := left + top - top_left
		mut pred := 0
		if g < 0 {
			pred = 0
		} else if g > 255 {
			pred = 255
		} else {
			pred = g
		}
		left = (deltas[i] + pred) & 0xff
		top_left = top
		out[i] = left
	}
	return out
}

fn (mut d WebpDecoder) decode_alph(payload []u8, width int, height int) ![]int {
	if payload.len < alpha_header_len {
		return error('ALPH chunk too short for header')
	}
	header := int(payload[0])
	method := header & 0x03
	filter_type := (header >> 2) & 0x03
	pre_processing := (header >> 4) & 0x03
	reserved := (header >> 6) & 0x03
	if method != alpha_no_compression && method != alpha_lossless_compression {
		return error('Unknown ALPH compression method: ${method}')
	}
	if filter_type >= webp_filter_last {
		return error('Unknown ALPH filter type: ${filter_type}')
	}
	if pre_processing > alpha_preprocessed_levels {
		return error('Unknown ALPH pre-processing: ${pre_processing}')
	}
	if reserved != 0 {
		return error('ALPH reserved bits not zero: ${reserved}')
	}
	data := payload[alpha_header_len..]
	mut alpha := []int{len: width * height}
	d.arena.track(alpha)
	if method == alpha_no_compression {
		needed := width * height
		if data.len < needed {
			return error('ALPH uncompressed data too short: ${data.len} < ${needed}')
		}
		mut prev := []int{}
		mut has_prev := false
		for y in 0 .. height {
			row := data[y * width..(y + 1) * width]
			mut rowi := []int{len: width}
			d.arena.track(rowi)
			for i in 0 .. width {
				rowi[i] = int(row[i])
			}
			out := alph_unfilter_row(mut d.arena, filter_type, prev, has_prev, rowi, width)
			for i in 0 .. width {
				alpha[y * width + i] = out[i]
			}
			mut prevnext := out.clone()
			d.arena.track(prevnext)
			unsafe { prev = prevnext }
			has_prev = true
		}
	} else {
		mut br := common.new_bitreader_lsb(data)
		argb := d.decode_vp8l_body(mut br, width, height)!
		for i in 0 .. width * height {
			alpha[i] = (argb[i] >> 8) & 0xff
		}
		if filter_type != webp_filter_none {
			mut prev := []int{}
			mut has_prev := false
			for y in 0 .. height {
				mut rowi := []int{len: width}
				d.arena.track(rowi)
				for i in 0 .. width {
					rowi[i] = alpha[y * width + i]
				}
				out := alph_unfilter_row(mut d.arena, filter_type, prev, has_prev, rowi, width)
				for i in 0 .. width {
					alpha[y * width + i] = out[i]
				}
				mut prevnext := out.clone()
				d.arena.track(prevnext)
				unsafe { prev = prevnext }
				has_prev = true
			}
		}
	}
	return alpha
}

// ============================================================================
// Container / VP8X / layout / metadata
// ============================================================================

fn (mut d WebpDecoder) parse_container() ! {
	data := d.data
	if data.len < 12 {
		return error('WEBP file is too short for a RIFF/WEBP header')
	}
	if !fc_eq(data[0..4], 'RIFF') {
		return error('Invalid WEBP RIFF magic')
	}
	if !fc_eq(data[8..12], 'WEBP') {
		return error('Invalid WEBP FourCC')
	}
	riff_size := int(data[4]) | (int(data[5]) << 8) | (int(data[6]) << 16) | (int(data[7]) << 24)
	if riff_size + 8 > data.len {
		return error('WEBP RIFF size ${riff_size} exceeds file length ${data.len - 8}')
	}
	end := riff_size + 8
	mut pos := 12
	mut chunks := []Chunk{}
	for pos + 8 <= end {
		fourcc := data[pos..pos + 4]
		size := int(data[pos + 4]) | (int(data[pos + 5]) << 8) | (int(data[pos + 6]) << 16) | (int(data[pos + 7]) << 24)
		pos += 8
		if pos + size > end {
			return error('WEBP chunk extends beyond EOF')
		}
		payload := data[pos..pos + size]
		pos += size
		if size % 2 == 1 && pos < end {
			pos++
		}
		d.arena.push(mut chunks, Chunk{ fourcc: fourcc, payload: payload })
	}
	d.arena.track(chunks)
	d.chunks = chunks
}

// check_rgba_dimensions bounds both integer samples and their backing byte size.
fn check_rgba_dimensions(width int, height int) ! {
	if width <= 0 || height <= 0 || width > max_int / int(sizeof(int)) / height / 4 {
		return error('WEBP dimensions exceed the supported buffer size')
	}
}

fn (mut d WebpDecoder) parse_vp8x(payload []u8) ! {
	if payload.len < vp8x_chunk_size {
		return error('VP8X chunk too small: ${payload.len} bytes')
	}
	flags := int(payload[0])
	if payload[1] != 0 || payload[2] != 0 || payload[3] != 0 {
		return error('VP8X reserved bytes must be zero')
	}
	if flags & vp8x_reserved_flags != 0 {
		return error('VP8X reserved flag bits must be zero, got 0x${flags:x}')
	}
	d.vp8x_flags = flags
	d.width = (int(payload[4]) | (int(payload[5]) << 8) | (int(payload[6]) << 16)) + 1
	d.height = (int(payload[7]) | (int(payload[8]) << 8) | (int(payload[9]) << 16)) + 1
	check_rgba_dimensions(d.width, d.height)!
	d.has_alpha = (flags & vp8x_flag_alpha) != 0
}

fn (mut d WebpDecoder) detect_layout() ! {
	if d.chunks.len == 0 {
		return error('WEBP file contains no chunks')
	}
	mut image_chunks := []Chunk{}
	for c in d.chunks {
		if fc_eq(c.fourcc, chunk_vp8) || fc_eq(c.fourcc, chunk_vp8l) {
			d.arena.push(mut image_chunks, c)
		}
	}
	d.arena.track(image_chunks)
	if image_chunks.len != 1 {
		return error('WEBP static image requires exactly one image chunk, found ${image_chunks.len}')
	}
	image_fourcc := image_chunks[0].fourcc
	mut has_vp8x := false
	mut has_alph := false
	for c in d.chunks {
		if fc_eq(c.fourcc, chunk_vp8x) {
			has_vp8x = true
		}
		if fc_eq(c.fourcc, chunk_alph) {
			has_alph = true
		}
	}
	if has_vp8x && !fc_eq(d.chunks[0].fourcc, chunk_vp8x) {
		return error('WEBP VP8X chunk must appear before image chunks')
	}
	if has_alph {
		if !has_vp8x {
			return error('WEBP ALPH chunk requires a VP8X extended header')
		}
		if !fc_eq(image_fourcc, chunk_vp8) {
			return error('WEBP ALPH chunk requires a lossy VP8 chunk')
		}
	}
	if has_vp8x {
		d.layout = layout_extended
		if has_alph && (d.vp8x_flags & vp8x_flag_alpha) == 0 {
			return error('WEBP VP8X alpha flag not set but ALPH chunk present')
		}
		if (d.vp8x_flags & vp8x_flag_alpha) != 0 && fc_eq(image_fourcc, chunk_vp8) && !has_alph {
			return error('WEBP VP8X alpha flag set but no ALPH chunk present')
		}
	} else {
		if fc_eq(image_fourcc, chunk_vp8) {
			d.layout = layout_lossy
		} else {
			d.layout = layout_lossless
		}
	}
}

fn (mut d WebpDecoder) parse_metadata() {
	for c in d.chunks {
		if fc_eq(c.fourcc, chunk_iccp) {
			d.icc_profile = c.payload
		} else if fc_eq(c.fourcc, chunk_exif) {
			d.exif = c.payload
		} else if fc_eq(c.fourcc, chunk_xmp) {
			d.xmp = c.payload
		}
	}
}

// ============================================================================
// Animation (ANIM / ANMF) - follows libwebp anim_decode.c
// ============================================================================

fn le32_at(data []u8, off int) int {
	return int(data[off]) | (int(data[off + 1]) << 8) | (int(data[off + 2]) << 16) |
		(int(data[off + 3]) << 24)
}

fn read_int24(data []u8, offset int) int {
	mut v := int(data[offset]) | (int(data[offset + 1]) << 8) | (int(data[offset + 2]) << 16)
	if v & 0x800000 != 0 {
		v -= 1 << 24
	}
	return v
}

// blend_row_over alpha-blends `count` canvas pixels over the previous canvas
// in place. Port of libwebp BlendPixelRowNonPremult / BlendPixelNonPremult:
// opaque canvas pixels are kept, fully transparent pixels take the previous
// value, and partial alpha uses libwebp's integer approximation.
fn blend_row_over(mut canvas []int, prev []int, offset int, count int) {
	for i := offset; i < offset + count; i++ {
		j := i * 4
		src_a := canvas[j + 3]
		if src_a == 255 {
			continue
		}
		dst_a := prev[j + 3]
		if src_a == 0 {
			canvas[j] = prev[j]
			canvas[j + 1] = prev[j + 1]
			canvas[j + 2] = prev[j + 2]
			canvas[j + 3] = dst_a
			continue
		}
		dst_factor_a := (dst_a * (256 - src_a)) >> 8
		blend_a := src_a + dst_factor_a
		scale := (1 << 24) / blend_a
		mut r := ((canvas[j] * src_a + prev[j] * dst_factor_a) * scale) >> 24
		canvas[j] = r
		r = ((canvas[j + 1] * src_a + prev[j + 1] * dst_factor_a) * scale) >> 24
		canvas[j + 1] = r
		r = ((canvas[j + 2] * src_a + prev[j + 2] * dst_factor_a) * scale) >> 24
		canvas[j + 2] = r
		canvas[j + 3] = blend_a
	}
}

// blend_ranges_at_row computes up to two blend ranges on one row excluding
// the previous frame's rectangle (cleared to transparent by dispose-to-
// background, so blending there is a no-op). Port of FindBlendRangeAtRow.
fn blend_ranges_at_row(src_x int, src_w int, dst_x int, dst_w int, dst_y int,
	dst_h int, canvas_y int
) (int, int, int, int) {
	src_max_x := src_x + src_w
	dst_max_x := dst_x + dst_w
	dst_max_y := dst_y + dst_h
	if canvas_y < dst_y || canvas_y >= dst_max_y || src_x >= dst_max_x
		|| src_max_x <= dst_x {
		return src_x, src_w, -1, 0
	}
	mut left1 := -1
	mut width1 := 0
	mut left2 := -1
	mut width2 := 0
	if src_x < dst_x {
		left1 = src_x
		width1 = dst_x - src_x
	}
	if src_max_x > dst_max_x {
		left2 = dst_max_x
		width2 = src_max_x - dst_max_x
	}
	return left1, width1, left2, width2
}

fn (mut d WebpDecoder) parse_animation() ! {
	mut has_vp8x := false
	for c in d.chunks {
		if fc_eq(c.fourcc, chunk_vp8x) {
			has_vp8x = true
			break
		}
	}
	if !has_vp8x {
		return error('WEBP animation requires a VP8X extended header')
	}
	if d.chunks.len == 0 || !fc_eq(d.chunks[0].fourcc, chunk_vp8x) {
		return error('WEBP VP8X chunk must appear before image chunks')
	}
	if (d.vp8x_flags & vp8x_flag_anim) == 0 {
		return error('WEBP ANMF chunk present but VP8X animation flag not set')
	}
	mut anim_seen := false
	mut frames := []AnimFrameSpec{}
	for i := 1; i < d.chunks.len; i++ {
		c := &d.chunks[i]
		if fc_eq(c.fourcc, chunk_anim) {
			if anim_seen {
				return error('WEBP file contains multiple ANIM chunks')
			}
			if frames.len > 0 {
				return error('WEBP ANIM chunk must precede ANMF frames')
			}
			if c.payload.len < anim_chunk_size {
				return error('ANIM chunk too small: ${c.payload.len} bytes')
			}
			// libwebp writes 6 bytes: BGcolor as little-endian 0xRRGGBBAA
			// followed by the 16-bit loop count.
			d.bgcolor = u32(le32_at(c.payload, 0))
			d.loop_count = int(c.payload[4]) | (int(c.payload[5]) << 8)
			anim_seen = true
		} else if fc_eq(c.fourcc, chunk_anmf) {
			if !anim_seen {
				return error('WEBP ANMF chunk before ANIM chunk')
			}
			if c.payload.len < anmf_chunk_size {
				return error('ANMF chunk too small: ${c.payload.len} bytes')
			}
			mut spec := AnimFrameSpec{}
			spec.x = read_int24(c.payload, 0) * 2
			spec.y = read_int24(c.payload, 3) * 2
			spec.w = read_int24(c.payload, 6) + 1
			spec.h = read_int24(c.payload, 9) + 1
			spec.duration = int(c.payload[12]) | (int(c.payload[13]) << 8) |
				(int(c.payload[14]) << 16)
			bits := int(c.payload[15])
			spec.dispose_bg = (bits & 1) != 0
			spec.blend = (bits & 2) == 0
			if spec.x < 0 || spec.y < 0 || spec.w <= 0 || spec.h <= 0
				|| spec.x + spec.w > d.width || spec.y + spec.h > d.height {
				return error('WEBP ANMF frame ${spec.x},${spec.y},${spec.w}x${spec.h} ' +
					'exceeds canvas ${d.width}x${d.height}')
			}
			// The frame's image chunks are nested inside the ANMF payload
			// after the 16-byte header (libwebp stores this region as the
			// frame fragment passed to the decoder).
			rest := c.payload[anmf_chunk_size..]
			mut pos := 0
			for pos + 8 <= rest.len {
				nf := rest[pos..pos + 4]
				nsz := le32_at(rest, pos + 4)
				pos += 8
				if pos + nsz > rest.len {
					return error('WEBP ANMF frame chunk extends beyond the ANMF chunk')
				}
				npay := rest[pos..pos + nsz]
				pos += nsz
				if nsz % 2 == 1 && pos < rest.len {
					pos += 1
				}
				if fc_eq(nf, chunk_vp8l) || fc_eq(nf, chunk_vp8) {
					if spec.fourcc.len > 0 {
						return error('WEBP ANMF frame contains multiple image chunks')
					}
					spec.fourcc = nf
					spec.payload = npay
				} else if fc_eq(nf, chunk_alph) {
					if spec.alph.len > 0 {
						return error('WEBP ANMF frame contains multiple ALPH chunks')
					}
					spec.alph = npay
				}
			}
			frames << spec
		} else if fc_eq(c.fourcc, chunk_vp8l) || fc_eq(c.fourcc, chunk_vp8)
			|| fc_eq(c.fourcc, chunk_alph) {
			return error('WEBP image or ALPH chunk outside an ANMF frame')
		}
	}
	if !anim_seen {
		return error('WEBP VP8X animation flag set but no ANIM chunk present')
	}
	if frames.len == 0 {
		return error('WEBP animation contains no ANMF frames')
	}
	for f in frames {
		if f.payload.len == 0 {
			return error('WEBP ANMF frame contains no image chunk')
		}
		if f.alph.len > 0 && !fc_eq(f.fourcc, chunk_vp8) {
			return error('WEBP ANMF ALPH chunk requires a lossy VP8 frame')
		}
	}
	d.arena.track(frames)
	d.anim_frames = frames
	d.animated = true
	d.layout = layout_animated
}

fn (d &WebpDecoder) frame_has_alpha(spec AnimFrameSpec) bool {
	if fc_eq(spec.fourcc, chunk_vp8) {
		return spec.alph.len > 0
	}
	if spec.payload.len < 5 {
		return false
	}
	// VP8L header: 8-bit signature, 14-bit width-1, 14-bit height-1, then
	// the alpha_is_used bit (bit 36 of the little-endian word).
	v := u64(spec.payload[0]) | (u64(spec.payload[1]) << 8) |
		(u64(spec.payload[2]) << 16) | (u64(spec.payload[3]) << 24) |
		(u64(spec.payload[4]) << 32)
	return ((v >> 36) & 1) != 0
}

fn (mut d WebpDecoder) decode_frame_rgba(spec AnimFrameSpec) ![]int {
	saved_w := d.width
	saved_h := d.height
	saved_a := d.has_alpha
	if fc_eq(spec.fourcc, chunk_vp8l) {
		argb := d.decode_vp8l(spec.payload)!
		frame_w := d.width
		frame_h := d.height
		d.width = saved_w
		d.height = saved_h
		d.has_alpha = saved_a
		if frame_w != spec.w || frame_h != spec.h {
			return error('WEBP ANMF frame ${spec.w}x${spec.h} does not match ' +
				'payload size ${frame_w}x${frame_h}')
		}
		mut rgba := []int{len: argb.len * 4}
		d.arena.track(rgba)
		for i, p in argb {
			rgba[i * 4] = (p >> 16) & 0xff
			rgba[i * 4 + 1] = (p >> 8) & 0xff
			rgba[i * 4 + 2] = p & 0xff
			rgba[i * 4 + 3] = (p >> 24) & 0xff
		}
		return rgba
	}
	fh := d.parse_vp8_frame_header(spec.payload)!
	mut rgba := d.decode_vp8(spec.payload)!
	d.arena.track(rgba)
	if spec.alph.len > 0 {
		alpha := d.decode_alph(spec.alph, fh.width, fh.height)!
		for i in 0 .. fh.width * fh.height {
			rgba[i * 4 + 3] = alpha[i]
		}
	}
	d.width = saved_w
	d.height = saved_h
	d.has_alpha = saved_a
	if fh.width != spec.w || fh.height != spec.h {
		return error('WEBP ANMF frame ${spec.w}x${spec.h} does not match ' +
			'payload size ${fh.width}x${fh.height}')
	}
	return rgba
}

// composite_frames decodes and composites every animation frame onto the
// canvas. Port of WebPAnimDecoderGetNext: the canvas starts transparent,
// each frame replaces its rectangle, blending applies only to non-key frames
// with the blend flag, and dispose-to-background clears the frame rectangle
// (to transparent) in the saved canvas. Each snapshot is arena-tracked; the
// caller converts them to caller-owned buffers before cleanup.
fn (mut d WebpDecoder) composite_frames() ![][]int {
	w := d.width
	h := d.height
	n := w * h * 4
	specs := d.anim_frames
	mut frames := [][]int{}
	mut curr := []int{len: n}
	d.arena.track(curr)
	mut prev_disp := []int{len: n}
	d.arena.track(prev_disp)
	mut prev_exists := false
	mut prev_x := 0
	mut prev_y := 0
	mut prev_w := 0
	mut prev_h := 0
	mut prev_dispose_bg := false
	mut prev_was_key := false
	for idx in 0 .. specs.len {
		spec := specs[idx]
		frame_num := idx + 1
		full := spec.w == w && spec.h == h
		has_alpha := d.frame_has_alpha(spec)
		mut is_key := frame_num == 1
		if !is_key && (!has_alpha || !spec.blend) && full {
			is_key = true
		}
		if !is_key && prev_exists && prev_dispose_bg
			&& ((prev_w == w && prev_h == h) || prev_was_key) {
			is_key = true
		}
		if is_key {
			for i in 0 .. n {
				curr[i] = 0
			}
		} else {
			for i in 0 .. n {
				curr[i] = prev_disp[i]
			}
		}
		plane := d.decode_frame_rgba(spec)!
		x := spec.x
		y := spec.y
		sw := spec.w
		sh := spec.h
		for row in 0 .. sh {
			src := row * sw * 4
			dst := ((y + row) * w + x) * 4
			for k in 0 .. sw * 4 {
				curr[dst + k] = plane[src + k]
			}
		}
		if frame_num > 1 && spec.blend && !is_key {
			if !prev_dispose_bg {
				for row in 0 .. sh {
					blend_row_over(mut curr, prev_disp, (y + row) * w + x, sw)
				}
			} else {
				for row in 0 .. sh {
					cy := y + row
					l1, w1, l2, w2 := blend_ranges_at_row(x, sw, prev_x, prev_w,
						prev_y, prev_h, cy)
					if w1 > 0 {
						blend_row_over(mut curr, prev_disp, cy * w + l1, w1)
					}
					if w2 > 0 {
						blend_row_over(mut curr, prev_disp, cy * w + l2, w2)
					}
				}
			}
		}
		snap := curr.clone()
		d.arena.track(snap)
		frames << snap
		for i in 0 .. n {
			prev_disp[i] = curr[i]
		}
		if spec.dispose_bg {
			for row in 0 .. sh {
				dst := ((y + row) * w + x) * 4
				for k in 0 .. sw * 4 {
					prev_disp[dst + k] = 0
				}
			}
		}
		prev_exists = true
		prev_x = x
		prev_y = y
		prev_w = sw
		prev_h = sh
		prev_dispose_bg = spec.dispose_bg
		prev_was_key = is_key
	}
	return frames
}

fn (mut d WebpDecoder) read_vp8l_dimensions(payload []u8) ! {
	mut br := common.new_bitreader_lsb(payload)
	sig := int(br.read_bits(8)!)
	if sig != vp8l_signature {
		return error('Invalid VP8L signature: 0x${sig:x}')
	}
	d.width = int(br.read_bits(14)!) + 1
	d.height = int(br.read_bits(14)!) + 1
	d.has_alpha = int(br.read_bits(1)!) != 0
	version := int(br.read_bits(3)!)
	if version != 0 {
		return error('Unsupported VP8L version: ${version}')
	}
}

fn (mut d WebpDecoder) parse() ! {
	if d.parsed {
		return
	}
	d.parse_container()!
	for c in d.chunks {
		if fc_eq(c.fourcc, chunk_vp8x) {
			d.parse_vp8x(c.payload)!
			break
		}
	}
	mut has_anmf := false
	for c in d.chunks {
		if fc_eq(c.fourcc, chunk_anmf) {
			has_anmf = true
			break
		}
	}
	if has_anmf || (d.vp8x_flags & vp8x_flag_anim) != 0 {
		d.parse_animation()!
		d.parse_metadata()
		d.parsed = true
		return
	}
	d.detect_layout()!
	d.parse_metadata()
	if d.width == 0 {
		for c in d.chunks {
			if fc_eq(c.fourcc, chunk_vp8l) {
				d.read_vp8l_dimensions(c.payload)!
				break
			}
			if fc_eq(c.fourcc, chunk_vp8) {
				fh := d.parse_vp8_frame_header(c.payload)!
				d.width = fh.width
				d.height = fh.height
				break
			}
		}
	}
	d.parsed = true
}

// ============================================================================
// VP8 lossy: frame header
// ============================================================================

struct Vp8FrameHeader {
mut:
	key_frame        bool
	profile          int
	show             int
	partition_length int
	width            int
	xscale           int
	height           int
	yscale           int
	mb_w             int
	mb_h             int
	data_offset      int
}

fn (mut d WebpDecoder) parse_vp8_frame_header(payload []u8) !Vp8FrameHeader {
	if payload.len < 4 {
		return error('Truncated VP8 header.')
	}
	bits := int(payload[0]) | (int(payload[1]) << 8) | (int(payload[2]) << 16)
	key_frame := (bits & 1) == 0
	profile := (bits >> 1) & 7
	show := (bits >> 4) & 1
	partition_length := bits >> 5
	if profile > 3 {
		return error('Incorrect keyframe parameters.')
	}
	if show == 0 {
		return error('Frame not displayable.')
	}
	mut pos := vp8_frame_header_size
	mut buf_size := payload.len - vp8_frame_header_size
	mut width := 0
	mut xscale := 0
	mut height := 0
	mut yscale := 0
	mut mb_w := 0
	mut mb_h := 0
	if key_frame {
		if buf_size < vp8_picture_header_size {
			return error('cannot parse picture header')
		}
		if payload[pos] != 0x9d || payload[pos + 1] != 0x01 || payload[pos + 2] != 0x2a {
			return error('Bad code word')
		}
		width = ((int(payload[pos + 4]) << 8) | int(payload[pos + 3])) & 0x3fff
		xscale = int(payload[pos + 4]) >> 6
		height = ((int(payload[pos + 6]) << 8) | int(payload[pos + 5])) & 0x3fff
		yscale = int(payload[pos + 6]) >> 6
		pos += vp8_picture_header_size
		buf_size -= vp8_picture_header_size
		mb_w = (width + 15) >> 4
		mb_h = (height + 15) >> 4
	}
	if partition_length > buf_size {
		return error('bad partition length')
	}
	return Vp8FrameHeader{
		key_frame:        key_frame
		profile:          profile
		show:             show
		partition_length: partition_length
		width:            width
		xscale:           xscale
		height:           height
		yscale:           yscale
		mb_w:             mb_w
		mb_h:             mb_h
		data_offset:      pos
	}
}

// ============================================================================
// VP8 lossy: segment / filter / quant / main / proba headers
// ============================================================================

struct Vp8SegmentHeader {
mut:
	use_segment     int
	update_map      int
	absolute_delta  int
	quantizer       []int
	filter_strength []int
	segment_probs   []int
}

fn (mut d WebpDecoder) parse_vp8_segment_header(mut br BooleanDecoder) Vp8SegmentHeader {
	mut hdr := Vp8SegmentHeader{
		use_segment:     br.get_bit(0x80)
		update_map:      0
		absolute_delta:  1
		quantizer:       []int{len: vp8_num_mb_segments}
		filter_strength: []int{len: vp8_num_mb_segments}
		segment_probs:   filled(vp8_mb_feature_tree_probs, 255)
	}
	d.arena.track(hdr.quantizer)
	d.arena.track(hdr.filter_strength)
	d.arena.track(hdr.segment_probs)
	if hdr.use_segment != 0 {
		hdr.update_map = br.get_bit(0x80)
		if br.get_bit(0x80) != 0 {
			hdr.absolute_delta = br.get_bit(0x80)
			for s in 0 .. vp8_num_mb_segments {
				if br.get_bit(0x80) != 0 {
					hdr.quantizer[s] = br.get_signed_value(7)
				}
			}
			for s in 0 .. vp8_num_mb_segments {
				if br.get_bit(0x80) != 0 {
					hdr.filter_strength[s] = br.get_signed_value(6)
				}
			}
		}
		if hdr.update_map != 0 {
			for s in 0 .. vp8_mb_feature_tree_probs {
				if br.get_bit(0x80) != 0 {
					hdr.segment_probs[s] = br.get_value(8)
				} else {
					hdr.segment_probs[s] = 255
				}
			}
		}
	}
	return hdr
}

struct Vp8FilterHeader {
mut:
	simple        int
	level         int
	sharpness     int
	use_lf_delta  int
	ref_lf_delta  []int
	mode_lf_delta []int
	filter_type   int
}

fn (mut d WebpDecoder) parse_vp8_filter_header(mut br BooleanDecoder) Vp8FilterHeader {
	mut hdr := Vp8FilterHeader{
		simple:        br.get_bit(0x80)
		level:         br.get_value(6)
		sharpness:     br.get_value(3)
		use_lf_delta:  br.get_bit(0x80)
		ref_lf_delta:  []int{len: vp8_num_ref_lf_deltas}
		mode_lf_delta: []int{len: vp8_num_mode_lf_deltas}
		filter_type:   0
	}
	d.arena.track(hdr.ref_lf_delta)
	d.arena.track(hdr.mode_lf_delta)
	if hdr.use_lf_delta != 0 {
		if br.get_bit(0x80) != 0 {
			for i in 0 .. vp8_num_ref_lf_deltas {
				if br.get_bit(0x80) != 0 {
					hdr.ref_lf_delta[i] = br.get_signed_value(6)
				}
			}
			for i in 0 .. vp8_num_mode_lf_deltas {
				if br.get_bit(0x80) != 0 {
					hdr.mode_lf_delta[i] = br.get_signed_value(6)
				}
			}
		}
	}
	if hdr.level == 0 {
		hdr.filter_type = 0
	} else if hdr.simple != 0 {
		hdr.filter_type = 1
	} else {
		hdr.filter_type = 2
	}
	return hdr
}

fn compute_quant_matrix(q int, dqy1_dc int, dqy2_dc int, dqy2_ac int, dquv_dc int, dquv_ac int) QuantMat {
	y1_dc := vp8_dc_table[clipi(q + dqy1_dc, 127)]
	y1_ac := vp8_ac_table[clipi(q, 127)]
	y2_dc := vp8_dc_table[clipi(q + dqy2_dc, 127)] * 2
	mut y2_ac := (vp8_ac_table[clipi(q + dqy2_ac, 127)] * 101581) >> 16
	if y2_ac < 8 {
		y2_ac = 8
	}
	uv_dc := vp8_dc_table[clipi(q + dquv_dc, 117)]
	uv_ac := vp8_ac_table[clipi(q + dquv_ac, 127)]
	return QuantMat{
		y1:       [y1_dc, y1_ac]
		y2:       [y2_dc, y2_ac]
		uv:       [uv_dc, uv_ac]
		uv_quant: q + dquv_ac
	}
}

fn (mut d WebpDecoder) parse_vp8_quant(mut br BooleanDecoder, segment Vp8SegmentHeader) []QuantMat {
	base_q0 := br.get_value(7)
	mut dqy1_dc := 0
	if br.get_bit(0x80) != 0 {
		dqy1_dc = br.get_signed_value(4)
	}
	mut dqy2_dc := 0
	if br.get_bit(0x80) != 0 {
		dqy2_dc = br.get_signed_value(4)
	}
	mut dqy2_ac := 0
	if br.get_bit(0x80) != 0 {
		dqy2_ac = br.get_signed_value(4)
	}
	mut dquv_dc := 0
	if br.get_bit(0x80) != 0 {
		dquv_dc = br.get_signed_value(4)
	}
	mut dquv_ac := 0
	if br.get_bit(0x80) != 0 {
		dquv_ac = br.get_signed_value(4)
	}
	mut dqm := []QuantMat{len: vp8_num_mb_segments}
	d.arena.track(dqm)
	for i in 0 .. vp8_num_mb_segments {
		mut q := base_q0
		if segment.use_segment != 0 {
			q = segment.quantizer[i]
			if segment.absolute_delta == 0 {
				q += base_q0
			}
		}
		qm := compute_quant_matrix(q, dqy1_dc, dqy2_dc, dqy2_ac, dquv_dc, dquv_ac)
		d.arena.track(qm.y1)
		d.arena.track(qm.y2)
		d.arena.track(qm.uv)
		dqm[i] = qm
	}
	return dqm
}

struct Vp8MainHeader {
mut:
	colorspace          int
	clamp_type          int
	segment             Vp8SegmentHeader
	filter              Vp8FilterHeader
	num_parts_minus_one int
	dqm                 []QuantMat
}

fn (mut d WebpDecoder) parse_vp8_main_header(mut br BooleanDecoder) Vp8MainHeader {
	colorspace := br.get_bit(0x80)
	clamp_type := br.get_bit(0x80)
	segment := d.parse_vp8_segment_header(mut br)
	filt := d.parse_vp8_filter_header(mut br)
	num_parts_minus_one := (1 << br.get_value(2)) - 1
	dqm := d.parse_vp8_quant(mut br, segment)
	return Vp8MainHeader{
		colorspace:          colorspace
		clamp_type:          clamp_type
		segment:             segment
		filter:              filt
		num_parts_minus_one: num_parts_minus_one
		dqm:                 dqm
	}
}

fn (mut d WebpDecoder) parse_vp8_proba(mut br BooleanDecoder) !Proba {
	mut bands := [][][][]int{len: vp8_num_types}
	d.arena.track(bands)
	for t in 0 .. vp8_num_types {
		mut type_bands := [][][]int{len: vp8_num_bands}
		d.arena.track(type_bands)
		for b in 0 .. vp8_num_bands {
			mut band := [][]int{len: vp8_num_ctx}
			d.arena.track(band)
			for c in 0 .. vp8_num_ctx {
				mut probas := []int{len: vp8_num_probas}
				d.arena.track(probas)
				for p in 0 .. vp8_num_probas {
					idx := ((t * 8 + b) * 3 + c) * 11 + p
					if br.get_bit(vp8_coeffs_update_proba[idx]) != 0 {
						probas[p] = br.get_value(8)
					} else {
						probas[p] = vp8_coeffs_proba_0[idx]
					}
				}
				band[c] = probas
			}
			type_bands[b] = band
		}
		bands[t] = type_bands
	}
	mut bands_ptr := [][][][]int{len: vp8_num_types}
	d.arena.track(bands_ptr)
	for t in 0 .. vp8_num_types {
		mut bp := [][][]int{len: 17}
		d.arena.track(bp)
		for b in 0 .. 17 {
			// Shares the band backing already tracked above; only `bp`'s own
			// backing is a new allocation here.
			bp[b] = bands[t][vp8_bands[b]]
		}
		bands_ptr[t] = bp
	}
	use_skip_proba := br.get_bit(0x80)
	mut skip_p := 0
	if use_skip_proba != 0 {
		skip_p = br.get_value(8)
	}
	return Proba{
		bands:          bands
		bands_ptr:      bands_ptr
		use_skip_proba: use_skip_proba
		skip_p:         skip_p
	}
}

// ============================================================================
// VP8 lossy: intra modes
// ============================================================================

fn (mut d WebpDecoder) parse_vp8_intra_modes_row(mut br BooleanDecoder, mb_w int, mut intra_t []int, mut intra_l []int, segment &Vp8SegmentHeader, proba &Proba) []MbBlock {
	seg_probs := segment.segment_probs
	mut blocks := []MbBlock{len: mb_w}
	d.arena.track(blocks)
	for mb_x in 0 .. mb_w {
		tb := 4 * mb_x
		mut block := MbBlock{
			segment: 0
			skip:    0
			is_i4x4: 0
			imodes:  []int{len: 16}
			uvmode:  0
		}
		d.arena.track(block.imodes)
		if segment.update_map != 0 {
			if br.get_bit(seg_probs[0]) == 0 {
				block.segment = br.get_bit(seg_probs[1])
			} else {
				block.segment = br.get_bit(seg_probs[2]) + 2
			}
		}
		if proba.use_skip_proba != 0 {
			block.skip = br.get_bit(proba.skip_p)
		}
		block.is_i4x4 = 1 - br.get_bit(145)
		if block.is_i4x4 == 0 {
			mut ymode := 0
			if br.get_bit(156) != 0 {
				if br.get_bit(128) != 0 {
					ymode = vp8_tm_pred
				} else {
					ymode = vp8_h_pred
				}
			} else {
				if br.get_bit(163) != 0 {
					ymode = vp8_v_pred
				} else {
					ymode = vp8_dc_pred
				}
			}
			block.imodes[0] = ymode
			for k in 0 .. 4 {
				intra_t[tb + k] = ymode
				intra_l[k] = ymode
			}
		} else {
			for y in 0 .. 4 {
				mut ymode := intra_l[y]
				for x in 0 .. 4 {
					prob_base := (intra_t[tb + x] * 10 + ymode) * 9
					if br.get_bit(vp8_bmodes_proba[prob_base + 0]) == 0 {
						ymode = vp8_b_dc_pred
					} else if br.get_bit(vp8_bmodes_proba[prob_base + 1]) == 0 {
						ymode = vp8_b_tm_pred
					} else if br.get_bit(vp8_bmodes_proba[prob_base + 2]) == 0 {
						ymode = vp8_b_ve_pred
					} else if br.get_bit(vp8_bmodes_proba[prob_base + 3]) == 0 {
						if br.get_bit(vp8_bmodes_proba[prob_base + 4]) == 0 {
							ymode = vp8_b_he_pred
						} else if br.get_bit(vp8_bmodes_proba[prob_base + 5]) == 0 {
							ymode = vp8_b_rd_pred
						} else {
							ymode = vp8_b_vr_pred
						}
					} else if br.get_bit(vp8_bmodes_proba[prob_base + 6]) == 0 {
						ymode = vp8_b_ld_pred
					} else if br.get_bit(vp8_bmodes_proba[prob_base + 7]) == 0 {
						ymode = vp8_b_vl_pred
					} else if br.get_bit(vp8_bmodes_proba[prob_base + 8]) == 0 {
						ymode = vp8_b_hd_pred
					} else {
						ymode = vp8_b_hu_pred
					}
					intra_t[tb + x] = ymode
				}
				for x in 0 .. 4 {
					block.imodes[y * 4 + x] = intra_t[tb + x]
				}
				intra_l[y] = ymode
			}
		}
		if br.get_bit(142) == 0 {
			block.uvmode = vp8_dc_pred
		} else if br.get_bit(114) == 0 {
			block.uvmode = vp8_v_pred
		} else if br.get_bit(183) != 0 {
			block.uvmode = vp8_tm_pred
		} else {
			block.uvmode = vp8_h_pred
		}
		blocks[mb_x] = block
	}
	return blocks
}

// ============================================================================
// VP8 lossy: residuals
// ============================================================================

fn (mut d WebpDecoder) get_large_value(mut br BooleanDecoder, p []int) int {
	if br.get_bit(p[3]) == 0 {
		if br.get_bit(p[4]) == 0 {
			return 2
		}
		return 3 + br.get_bit(p[5])
	}
	if br.get_bit(p[6]) == 0 {
		if br.get_bit(p[7]) == 0 {
			return 5 + br.get_bit(159)
		}
		mut v := 7 + 2 * br.get_bit(165)
		v += br.get_bit(145)
		return v
	}
	bit1 := br.get_bit(p[8])
	bit0 := br.get_bit(p[9 + bit1])
	cat := 2 * bit1 + bit0
	mut v := 0
	for prob in cat_chain(cat) {
		if prob == 0 {
			break
		}
		v += v + br.get_bit(prob)
	}
	v += 3 + (8 << cat)
	return v
}

fn (mut d WebpDecoder) get_coeffs(mut br BooleanDecoder, prob [][][]int, ctx int, dq []int, n int, mut out []int) int {
	mut nn := n
	mut p := prob[nn][ctx]
	for nn < 16 {
		if br.get_bit(p[0]) == 0 {
			return nn
		}
		for br.get_bit(p[1]) == 0 {
			nn++
			p = prob[nn][0]
			if nn == 16 {
				return 16
			}
		}
		p_ctx := prob[nn + 1]
		mut v := 0
		if br.get_bit(p[2]) == 0 {
			v = 1
			p = p_ctx[1]
		} else {
			v = d.get_large_value(mut br, p)
			p = p_ctx[2]
		}
		mut step := dq[0]
		if nn > 0 {
			step = dq[1]
		}
		out[vp8_zigzag[nn]] = br.get_signed(v) * step
		nn++
	}
	return 16
}

fn (mut d WebpDecoder) decode_vp8_residuals(mut br BooleanDecoder, block &MbBlock, dq &QuantMat, bands_ptr [][][][]int, mut nz NzState) ![]int {
	mut coeffs := d.arena.alloc_scratch(384)!
	mb_x := nz.mb_x
	mut above_nz := []int{}
	mut above_nz_dc := []int{}
	unsafe {
		above_nz = nz.above_nz
		above_nz_dc = nz.above_nz_dc
	}
	mut left_nz := nz.left_nz
	mut left_nz_dc := nz.left_nz_dc
	mut dst := 0
	mut first := 0
	mut nz_dc := 0
	mut ac_proba := [][][]int{}
	if block.is_i4x4 == 0 {
		mut dc := d.arena.alloc_scratch(16)!
		ctx := above_nz_dc[mb_x] + left_nz_dc
		nzr := d.get_coeffs(mut br, bands_ptr[1], ctx, dq.y2, 0, mut dc)
		if nzr > 0 {
			nz_dc = 1
		}
		if nzr > 1 {
			transform_wht(mut d.arena, dc, mut coeffs)!
		} else {
			dc0 := (dc[0] + 3) >> 3
			mut i := 0
			for i < 16 * 16 {
				coeffs[i] = dc0
				i += 16
			}
		}
		first = 1
		unsafe {
			ac_proba = bands_ptr[0]
		}
	} else {
		nz_dc = 0
		first = 0
		unsafe {
			ac_proba = bands_ptr[3]
		}
	}
	mut tnz := above_nz[mb_x] & 0x0f
	mut lnz := left_nz & 0x0f
	for y in 0 .. 4 {
		mut l := lnz & 1
		for x in 0 .. 4 {
			ctx := l + (tnz & 1)
			mut blk := d.arena.alloc_scratch(16)!
			nzr := d.get_coeffs(mut br, ac_proba, ctx, dq.y1, first, mut blk)
			if first != 0 {
				for k in 1 .. 16 {
					coeffs[dst + k] = blk[k]
				}
			} else {
				for k in 0 .. 16 {
					coeffs[dst + k] = blk[k]
				}
			}
			if nzr > first {
				l = 1
			} else {
				l = 0
			}
			tnz = (tnz >> 1) | (l * 128)
			dst += 16
		}
		tnz >>= 4
		lnz = (lnz >> 1) | (l * 128)
	}
	mut out_t_nz := tnz
	mut out_l_nz := lnz >> 4
	for ci in 0 .. 2 {
		ch := ci * 2
		tnz = above_nz[mb_x] >> (4 + ch)
		lnz = left_nz >> (4 + ch)
		for y in 0 .. 2 {
			mut l := lnz & 1
			for x in 0 .. 2 {
				ctx := l + (tnz & 1)
				mut blk := d.arena.alloc_scratch(16)!
				nzr := d.get_coeffs(mut br, bands_ptr[2], ctx, dq.uv, 0, mut blk)
				for k in 0 .. 16 {
					coeffs[dst + k] = blk[k]
				}
				if nzr > 0 {
					l = 1
				} else {
					l = 0
				}
				tnz = (tnz >> 1) | (l * 8)
				dst += 16
			}
			tnz >>= 2
			lnz = (lnz >> 1) | (l * 32)
		}
		out_t_nz |= (tnz * 16) * (1 << ch)
		out_l_nz |= (lnz & 0xf0) * (1 << ch)
	}
	above_nz[mb_x] = out_t_nz
	if block.is_i4x4 == 0 {
		above_nz_dc[mb_x] = nz_dc
		nz.left_nz_dc = nz_dc
	}
	nz.left_nz = out_l_nz
	return coeffs
}

fn check_mode(mb_x int, mb_y int, mode int) int {
	if mode == vp8_b_dc_pred {
		if mb_x == 0 {
			if mb_y == 0 {
				return vp8_b_dc_pred_notopleft
			}
			return vp8_b_dc_pred_noleft
		}
		if mb_y == 0 {
			return vp8_b_dc_pred_notop
		}
	}
	return mode
}

// ============================================================================
// VP8 lossy: transforms (IDCT / WHT)
// ============================================================================

fn store_plane(mut plane []int, stride int, x int, y int, v int) {
	idx := y * stride + x
	plane[idx] = clampi(plane[idx] + (v >> 3))
}

fn transform_one(mut ar Arena, in_ []int, mut plane []int, stride int, ox int, oy int) ! {
	mut c := ar.alloc_scratch(16)!
	for i in 0 .. 4 {
		a := in_[0 + i] + in_[8 + i]
		b := in_[0 + i] - in_[8 + i]
		cc := idct_mul2(in_[4 + i]) - idct_mul1(in_[12 + i])
		dd := idct_mul1(in_[4 + i]) + idct_mul2(in_[12 + i])
		c[4 * i + 0] = a + dd
		c[4 * i + 1] = b + cc
		c[4 * i + 2] = b - cc
		c[4 * i + 3] = a - dd
	}
	for i in 0 .. 4 {
		dc := c[i] + 4
		a := dc + c[8 + i]
		b := dc - c[8 + i]
		cc := idct_mul2(c[4 + i]) - idct_mul1(c[12 + i])
		dd := idct_mul1(c[4 + i]) + idct_mul2(c[12 + i])
		store_plane(mut plane, stride, ox + 0, oy + i, a + dd)
		store_plane(mut plane, stride, ox + 1, oy + i, b + cc)
		store_plane(mut plane, stride, ox + 2, oy + i, b - cc)
		store_plane(mut plane, stride, ox + 3, oy + i, a - dd)
	}
}

fn transform_wht(mut ar Arena, in_ []int, mut out []int) ! {
	mut tmp := ar.alloc_scratch(16)!
	for i in 0 .. 4 {
		a0 := in_[0 + i] + in_[12 + i]
		a1 := in_[4 + i] + in_[8 + i]
		a2 := in_[4 + i] - in_[8 + i]
		a3 := in_[0 + i] - in_[12 + i]
		tmp[0 + i] = a0 + a1
		tmp[8 + i] = a0 - a1
		tmp[4 + i] = a3 + a2
		tmp[12 + i] = a3 - a2
	}
	for i in 0 .. 4 {
		base := i * 64
		dc := tmp[0 + i * 4] + 3
		a0 := dc + tmp[3 + i * 4]
		a1 := tmp[1 + i * 4] + tmp[2 + i * 4]
		a2 := tmp[1 + i * 4] - tmp[2 + i * 4]
		a3 := dc - tmp[3 + i * 4]
		out[base + 0] = (a0 + a1) >> 3
		out[base + 16] = (a3 + a2) >> 3
		out[base + 32] = (a0 - a1) >> 3
		out[base + 48] = (a3 - a2) >> 3
	}
}

// ============================================================================
// VP8 lossy: intra prediction
// ============================================================================

fn avg2(a int, b int) int {
	return (a + b + 1) >> 1
}

fn avg3(a int, b int, c int) int {
	return (a + 2 * b + c + 2) >> 2
}

fn setpx(mut plane []int, stride int, ox int, oy int, x int, y int, v int) {
	plane[(oy + y) * stride + ox + x] = v
}

fn predict4(mut ar Arena, mut plane []int, stride int, ox int, oy int, mode int) ! {
	mut top := ar.alloc_scratch(10)!
	for i in 0 .. 10 {
		top[i] = plane[(oy - 1) * stride + ox + (i - 1)]
	}
	mut left := ar.alloc_scratch(4)!
	for j in 0 .. 4 {
		left[j] = plane[(oy + j) * stride + ox - 1]
	}
	if mode == vp8_b_dc_pred {
		mut dc := 4
		for i in 0 .. 4 {
			dc += top[i + 1] + left[i]
		}
		dc >>= 3
		for j in 0 .. 4 {
			for i in 0 .. 4 {
				setpx(mut plane, stride, ox, oy, i, j, dc)
			}
		}
	} else if mode == vp8_b_tm_pred {
		tl := top[0]
		for j in 0 .. 4 {
			base := left[j] - tl
			for i in 0 .. 4 {
				setpx(mut plane, stride, ox, oy, i, j, clampi(top[i + 1] + base))
			}
		}
	} else if mode == vp8_b_ve_pred {
		for j in 0 .. 4 {
			for i in 0 .. 4 {
				setpx(mut plane, stride, ox, oy, i, j, avg3(top[i], top[i + 1], top[i + 2]))
			}
		}
	} else if mode == vp8_b_he_pred {
		a, b, c, dd, e := left[0], left[1], left[2], left[3], left[3]
		tl := top[0]
		row0 := avg3(tl, a, b)
		row1 := avg3(a, b, c)
		row2 := avg3(b, c, dd)
		row3 := avg3(c, dd, e)
		for i in 0 .. 4 {
			setpx(mut plane, stride, ox, oy, i, 0, row0)
			setpx(mut plane, stride, ox, oy, i, 1, row1)
			setpx(mut plane, stride, ox, oy, i, 2, row2)
			setpx(mut plane, stride, ox, oy, i, 3, row3)
		}
	} else if mode == vp8_b_rd_pred {
		ii, jj, kk, ll := left[0], left[1], left[2], left[3]
		xx := top[0]
		aa, bb, cc, dd := top[1], top[2], top[3], top[4]
		setpx(mut plane, stride, ox, oy, 0, 3, avg3(jj, kk, ll))
		setpx(mut plane, stride, ox, oy, 1, 3, avg3(ii, jj, kk))
		setpx(mut plane, stride, ox, oy, 0, 2, avg3(ii, jj, kk))
		setpx(mut plane, stride, ox, oy, 2, 3, avg3(xx, ii, jj))
		setpx(mut plane, stride, ox, oy, 1, 2, avg3(xx, ii, jj))
		setpx(mut plane, stride, ox, oy, 0, 1, avg3(xx, ii, jj))
		setpx(mut plane, stride, ox, oy, 3, 3, avg3(aa, xx, ii))
		setpx(mut plane, stride, ox, oy, 2, 2, avg3(aa, xx, ii))
		setpx(mut plane, stride, ox, oy, 1, 1, avg3(aa, xx, ii))
		setpx(mut plane, stride, ox, oy, 0, 0, avg3(aa, xx, ii))
		setpx(mut plane, stride, ox, oy, 3, 2, avg3(bb, aa, xx))
		setpx(mut plane, stride, ox, oy, 2, 1, avg3(bb, aa, xx))
		setpx(mut plane, stride, ox, oy, 1, 0, avg3(bb, aa, xx))
		setpx(mut plane, stride, ox, oy, 3, 1, avg3(cc, bb, aa))
		setpx(mut plane, stride, ox, oy, 2, 0, avg3(cc, bb, aa))
		setpx(mut plane, stride, ox, oy, 3, 0, avg3(dd, cc, bb))
	} else if mode == vp8_b_ld_pred {
		aa, bb, cc, dd := top[1], top[2], top[3], top[4]
		ee, ff, gg, hh := top[5], top[6], top[7], top[8]
		setpx(mut plane, stride, ox, oy, 0, 0, avg3(aa, bb, cc))
		setpx(mut plane, stride, ox, oy, 1, 0, avg3(bb, cc, dd))
		setpx(mut plane, stride, ox, oy, 0, 1, avg3(bb, cc, dd))
		setpx(mut plane, stride, ox, oy, 2, 0, avg3(cc, dd, ee))
		setpx(mut plane, stride, ox, oy, 1, 1, avg3(cc, dd, ee))
		setpx(mut plane, stride, ox, oy, 0, 2, avg3(cc, dd, ee))
		setpx(mut plane, stride, ox, oy, 3, 0, avg3(dd, ee, ff))
		setpx(mut plane, stride, ox, oy, 2, 1, avg3(dd, ee, ff))
		setpx(mut plane, stride, ox, oy, 1, 2, avg3(dd, ee, ff))
		setpx(mut plane, stride, ox, oy, 0, 3, avg3(dd, ee, ff))
		setpx(mut plane, stride, ox, oy, 3, 1, avg3(ee, ff, gg))
		setpx(mut plane, stride, ox, oy, 2, 2, avg3(ee, ff, gg))
		setpx(mut plane, stride, ox, oy, 1, 3, avg3(ee, ff, gg))
		setpx(mut plane, stride, ox, oy, 3, 2, avg3(ff, gg, hh))
		setpx(mut plane, stride, ox, oy, 2, 3, avg3(ff, gg, hh))
		setpx(mut plane, stride, ox, oy, 3, 3, avg3(gg, hh, hh))
	} else if mode == vp8_b_vr_pred {
		ii, jj, kk := left[0], left[1], left[2]
		xx := top[0]
		aa, bb, cc, dd := top[1], top[2], top[3], top[4]
		setpx(mut plane, stride, ox, oy, 0, 0, avg2(xx, aa))
		setpx(mut plane, stride, ox, oy, 1, 2, avg2(xx, aa))
		setpx(mut plane, stride, ox, oy, 1, 0, avg2(aa, bb))
		setpx(mut plane, stride, ox, oy, 2, 2, avg2(aa, bb))
		setpx(mut plane, stride, ox, oy, 2, 0, avg2(bb, cc))
		setpx(mut plane, stride, ox, oy, 3, 2, avg2(bb, cc))
		setpx(mut plane, stride, ox, oy, 3, 0, avg2(cc, dd))
		setpx(mut plane, stride, ox, oy, 0, 3, avg3(kk, jj, ii))
		setpx(mut plane, stride, ox, oy, 0, 2, avg3(jj, ii, xx))
		setpx(mut plane, stride, ox, oy, 0, 1, avg3(ii, xx, aa))
		setpx(mut plane, stride, ox, oy, 1, 3, avg3(ii, xx, aa))
		setpx(mut plane, stride, ox, oy, 1, 1, avg3(xx, aa, bb))
		setpx(mut plane, stride, ox, oy, 2, 3, avg3(xx, aa, bb))
		setpx(mut plane, stride, ox, oy, 2, 1, avg3(aa, bb, cc))
		setpx(mut plane, stride, ox, oy, 3, 3, avg3(aa, bb, cc))
		setpx(mut plane, stride, ox, oy, 3, 1, avg3(bb, cc, dd))
	} else if mode == vp8_b_vl_pred {
		aa, bb, cc, dd := top[1], top[2], top[3], top[4]
		ee, ff, gg, hh := top[5], top[6], top[7], top[8]
		setpx(mut plane, stride, ox, oy, 0, 0, avg2(aa, bb))
		setpx(mut plane, stride, ox, oy, 1, 0, avg2(bb, cc))
		setpx(mut plane, stride, ox, oy, 0, 2, avg2(bb, cc))
		setpx(mut plane, stride, ox, oy, 2, 0, avg2(cc, dd))
		setpx(mut plane, stride, ox, oy, 1, 2, avg2(cc, dd))
		setpx(mut plane, stride, ox, oy, 3, 0, avg2(dd, ee))
		setpx(mut plane, stride, ox, oy, 2, 2, avg2(dd, ee))
		setpx(mut plane, stride, ox, oy, 0, 1, avg3(aa, bb, cc))
		setpx(mut plane, stride, ox, oy, 1, 1, avg3(bb, cc, dd))
		setpx(mut plane, stride, ox, oy, 0, 3, avg3(bb, cc, dd))
		setpx(mut plane, stride, ox, oy, 2, 1, avg3(cc, dd, ee))
		setpx(mut plane, stride, ox, oy, 1, 3, avg3(cc, dd, ee))
		setpx(mut plane, stride, ox, oy, 3, 1, avg3(dd, ee, ff))
		setpx(mut plane, stride, ox, oy, 2, 3, avg3(dd, ee, ff))
		setpx(mut plane, stride, ox, oy, 3, 2, avg3(ee, ff, gg))
		setpx(mut plane, stride, ox, oy, 3, 3, avg3(ff, gg, hh))
	} else if mode == vp8_b_hd_pred {
		ii, jj, kk, ll := left[0], left[1], left[2], left[3]
		xx := top[0]
		aa, bb, cc := top[1], top[2], top[3]
		setpx(mut plane, stride, ox, oy, 0, 0, avg2(ii, xx))
		setpx(mut plane, stride, ox, oy, 2, 1, avg2(ii, xx))
		setpx(mut plane, stride, ox, oy, 0, 1, avg2(jj, ii))
		setpx(mut plane, stride, ox, oy, 2, 2, avg2(jj, ii))
		setpx(mut plane, stride, ox, oy, 0, 2, avg2(kk, jj))
		setpx(mut plane, stride, ox, oy, 2, 3, avg2(kk, jj))
		setpx(mut plane, stride, ox, oy, 0, 3, avg2(ll, kk))
		setpx(mut plane, stride, ox, oy, 3, 0, avg3(aa, bb, cc))
		setpx(mut plane, stride, ox, oy, 2, 0, avg3(xx, aa, bb))
		setpx(mut plane, stride, ox, oy, 1, 0, avg3(ii, xx, aa))
		setpx(mut plane, stride, ox, oy, 3, 1, avg3(ii, xx, aa))
		setpx(mut plane, stride, ox, oy, 1, 1, avg3(jj, ii, xx))
		setpx(mut plane, stride, ox, oy, 3, 2, avg3(jj, ii, xx))
		setpx(mut plane, stride, ox, oy, 1, 2, avg3(kk, jj, ii))
		setpx(mut plane, stride, ox, oy, 3, 3, avg3(kk, jj, ii))
		setpx(mut plane, stride, ox, oy, 1, 3, avg3(ll, kk, jj))
	} else if mode == vp8_b_hu_pred {
		ii, jj, kk, ll := left[0], left[1], left[2], left[3]
		setpx(mut plane, stride, ox, oy, 0, 0, avg2(ii, jj))
		setpx(mut plane, stride, ox, oy, 2, 0, avg2(jj, kk))
		setpx(mut plane, stride, ox, oy, 0, 1, avg2(jj, kk))
		setpx(mut plane, stride, ox, oy, 2, 1, avg2(kk, ll))
		setpx(mut plane, stride, ox, oy, 0, 2, avg2(kk, ll))
		setpx(mut plane, stride, ox, oy, 1, 0, avg3(ii, jj, kk))
		setpx(mut plane, stride, ox, oy, 3, 0, avg3(jj, kk, ll))
		setpx(mut plane, stride, ox, oy, 1, 1, avg3(jj, kk, ll))
		setpx(mut plane, stride, ox, oy, 3, 1, avg3(kk, ll, ll))
		setpx(mut plane, stride, ox, oy, 1, 2, avg3(kk, ll, ll))
		setpx(mut plane, stride, ox, oy, 3, 2, ll)
		setpx(mut plane, stride, ox, oy, 2, 2, ll)
		setpx(mut plane, stride, ox, oy, 0, 3, ll)
		setpx(mut plane, stride, ox, oy, 1, 3, ll)
		setpx(mut plane, stride, ox, oy, 2, 3, ll)
		setpx(mut plane, stride, ox, oy, 3, 3, ll)
	} else {
		return error('Invalid 4x4 intra mode: ${mode}')
	}
}

fn true_motion(mut plane []int, stride int, ox int, oy int, size int) {
	top_left := plane[(oy - 1) * stride + ox - 1]
	for j in 0 .. size {
		base := plane[(oy + j) * stride + ox - 1] - top_left
		row := (oy + j) * stride + ox
		for i in 0 .. size {
			plane[row + i] = clampi(plane[(oy - 1) * stride + ox + i] + base)
		}
	}
}

fn fill_plane(mut plane []int, stride int, ox int, oy int, size int, value int) {
	for j in 0 .. size {
		dst := (oy + j) * stride + ox
		for i in 0 .. size {
			plane[dst + i] = value
		}
	}
}

fn predict16(mut plane []int, stride int, ox int, oy int, mode int) ! {
	if mode == vp8_dc_pred {
		mut dc := 16
		for j in 0 .. 16 {
			dc += plane[(oy + j) * stride + ox - 1]
			dc += plane[(oy - 1) * stride + ox + j]
		}
		dc >>= 5
		fill_plane(mut plane, stride, ox, oy, 16, dc)
	} else if mode == vp8_v_pred {
		for j in 0 .. 16 {
			src := (oy - 1) * stride + ox
			dst := (oy + j) * stride + ox
			for i in 0 .. 16 {
				plane[dst + i] = plane[src + i]
			}
		}
	} else if mode == vp8_h_pred {
		for j in 0 .. 16 {
			v := plane[(oy + j) * stride + ox - 1]
			dst := (oy + j) * stride + ox
			for i in 0 .. 16 {
				plane[dst + i] = v
			}
		}
	} else if mode == vp8_tm_pred {
		true_motion(mut plane, stride, ox, oy, 16)
	} else if mode == 4 {
		mut dc := 8
		for j in 0 .. 16 {
			dc += plane[(oy + j) * stride + ox - 1]
		}
		fill_plane(mut plane, stride, ox, oy, 16, dc >> 4)
	} else if mode == 5 {
		mut dc := 8
		for i in 0 .. 16 {
			dc += plane[(oy - 1) * stride + ox + i]
		}
		fill_plane(mut plane, stride, ox, oy, 16, dc >> 4)
	} else if mode == 6 {
		fill_plane(mut plane, stride, ox, oy, 16, 0x80)
	} else {
		return error('Invalid 16x16 intra mode: ${mode}')
	}
}

fn predict8(mut plane []int, stride int, ox int, oy int, mode int) ! {
	if mode == vp8_dc_pred {
		mut dc := 8
		for i in 0 .. 8 {
			dc += plane[(oy - 1) * stride + ox + i]
			dc += plane[(oy + i) * stride + ox - 1]
		}
		fill_plane(mut plane, stride, ox, oy, 8, dc >> 4)
	} else if mode == vp8_v_pred {
		for j in 0 .. 8 {
			src := (oy - 1) * stride + ox
			dst := (oy + j) * stride + ox
			for i in 0 .. 8 {
				plane[dst + i] = plane[src + i]
			}
		}
	} else if mode == vp8_h_pred {
		for j in 0 .. 8 {
			v := plane[(oy + j) * stride + ox - 1]
			dst := (oy + j) * stride + ox
			for i in 0 .. 8 {
				plane[dst + i] = v
			}
		}
	} else if mode == vp8_tm_pred {
		true_motion(mut plane, stride, ox, oy, 8)
	} else if mode == 4 {
		mut dc := 4
		for j in 0 .. 8 {
			dc += plane[(oy + j) * stride + ox - 1]
		}
		fill_plane(mut plane, stride, ox, oy, 8, dc >> 3)
	} else if mode == 5 {
		mut dc := 4
		for i in 0 .. 8 {
			dc += plane[(oy - 1) * stride + ox + i]
		}
		fill_plane(mut plane, stride, ox, oy, 8, dc >> 3)
	} else if mode == 6 {
		fill_plane(mut plane, stride, ox, oy, 8, 0x80)
	} else {
		return error('Invalid 8x8 chroma intra mode: ${mode}')
	}
}

// ============================================================================
// VP8 lossy: loop filter
// ============================================================================

fn do_filter2(mut plane []int, idx int, step int) {
	p1 := plane[idx - 2 * step]
	p0 := plane[idx - step]
	q0 := plane[idx]
	q1 := plane[idx + step]
	a := 3 * (q0 - p0) + sclip1(p1 - q1)
	a1 := sclip2((a + 4) >> 3)
	a2 := sclip2((a + 3) >> 3)
	plane[idx - step] = clampi(p0 + a2)
	plane[idx] = clampi(q0 - a1)
}

fn do_filter4(mut plane []int, idx int, step int) {
	p1 := plane[idx - 2 * step]
	p0 := plane[idx - step]
	q0 := plane[idx]
	q1 := plane[idx + step]
	a := 3 * (q0 - p0)
	a1 := sclip2((a + 4) >> 3)
	a2 := sclip2((a + 3) >> 3)
	a3 := (a1 + 1) >> 1
	plane[idx - 2 * step] = clampi(p1 + a3)
	plane[idx - step] = clampi(p0 + a2)
	plane[idx] = clampi(q0 - a1)
	plane[idx + step] = clampi(q1 - a3)
}

fn do_filter6(mut plane []int, idx int, step int) {
	p2 := plane[idx - 3 * step]
	p1 := plane[idx - 2 * step]
	p0 := plane[idx - step]
	q0 := plane[idx]
	q1 := plane[idx + step]
	q2 := plane[idx + 2 * step]
	a := sclip1(3 * (q0 - p0) + sclip1(p1 - q1))
	a1 := (27 * a + 63) >> 7
	a2 := (18 * a + 63) >> 7
	a3 := (9 * a + 63) >> 7
	plane[idx - 3 * step] = clampi(p2 + a3)
	plane[idx - 2 * step] = clampi(p1 + a2)
	plane[idx - step] = clampi(p0 + a1)
	plane[idx] = clampi(q0 - a1)
	plane[idx + step] = clampi(q1 - a2)
	plane[idx + 2 * step] = clampi(q2 - a3)
}

fn hev(plane []int, idx int, step int, thresh int) bool {
	p1 := plane[idx - 2 * step]
	p0 := plane[idx - step]
	q0 := plane[idx]
	q1 := plane[idx + step]
	return absi(p1 - p0) > thresh || absi(q1 - q0) > thresh
}

fn needs_filter(plane []int, idx int, step int, t int) bool {
	p1 := plane[idx - 2 * step]
	p0 := plane[idx - step]
	q0 := plane[idx]
	q1 := plane[idx + step]
	return (4 * absi(p0 - q0) + absi(p1 - q1)) <= t
}

fn needs_filter2(plane []int, idx int, step int, t int, it int) bool {
	p3 := plane[idx - 4 * step]
	p2 := plane[idx - 3 * step]
	p1 := plane[idx - 2 * step]
	p0 := plane[idx - step]
	q0 := plane[idx]
	q1 := plane[idx + step]
	q2 := plane[idx + 2 * step]
	q3 := plane[idx + 3 * step]
	if (4 * absi(p0 - q0) + absi(p1 - q1)) > t {
		return false
	}
	return absi(p3 - p2) <= it && absi(p2 - p1) <= it && absi(p1 - p0) <= it && absi(q3 - q2) <= it && absi(q2 - q1) <= it && absi(q1 - q0) <= it
}

fn filter_loop_26(mut plane []int, idx int, hstride int, vstride int, size int, thresh int, ithresh int, hev_thresh int) {
	thresh2 := 2 * thresh + 1
	mut i := idx
	for _ in 0 .. size {
		if needs_filter2(plane, i, hstride, thresh2, ithresh) {
			if hev(plane, i, hstride, hev_thresh) {
				do_filter2(mut plane, i, hstride)
			} else {
				do_filter6(mut plane, i, hstride)
			}
		}
		i += vstride
	}
}

fn filter_loop_24(mut plane []int, idx int, hstride int, vstride int, size int, thresh int, ithresh int, hev_thresh int) {
	thresh2 := 2 * thresh + 1
	mut i := idx
	for _ in 0 .. size {
		if needs_filter2(plane, i, hstride, thresh2, ithresh) {
			if hev(plane, i, hstride, hev_thresh) {
				do_filter2(mut plane, i, hstride)
			} else {
				do_filter4(mut plane, i, hstride)
			}
		}
		i += vstride
	}
}

fn simple_filter_line(mut plane []int, start int, line_step int, filter_step int, count int, thresh int) {
	thresh2 := 2 * thresh + 1
	mut idx := start
	for _ in 0 .. count {
		if needs_filter(plane, idx, filter_step, thresh2) {
			do_filter2(mut plane, idx, filter_step)
		}
		idx += line_step
	}
}

fn (mut d WebpDecoder) precompute_filter_strengths(filter_hdr &Vp8FilterHeader, segment_hdr &Vp8SegmentHeader) [][]FilterInfo {
	mut fstrengths := [][]FilterInfo{len: vp8_num_mb_segments}
	d.arena.track(fstrengths)
	for s in 0 .. vp8_num_mb_segments {
		mut row := []FilterInfo{len: 2}
		d.arena.track(row)
		fstrengths[s] = row
	}
	if filter_hdr.filter_type <= 0 {
		return fstrengths
	}
	for s in 0 .. vp8_num_mb_segments {
		mut base_level := filter_hdr.level
		if segment_hdr.use_segment != 0 {
			base_level = segment_hdr.filter_strength[s]
			if segment_hdr.absolute_delta == 0 {
				base_level += filter_hdr.level
			}
		}
		for i4x4 in 0 .. 2 {
			mut level := base_level
			if filter_hdr.use_lf_delta != 0 {
				level += filter_hdr.ref_lf_delta[0]
				if i4x4 != 0 {
					level += filter_hdr.mode_lf_delta[0]
				}
			}
			level = clipi(level, 63)
			mut info := FilterInfo{ f_ilevel: 0, f_limit: 0, hev_thresh: 0, f_inner: i4x4 }
			if level > 0 {
				mut ilevel := level
				if filter_hdr.sharpness > 0 {
					if filter_hdr.sharpness > 4 {
						ilevel >>= 2
					} else {
						ilevel >>= 1
					}
					if ilevel > 9 - filter_hdr.sharpness {
						ilevel = 9 - filter_hdr.sharpness
					}
				}
				if ilevel < 1 {
					ilevel = 1
				}
				mut hev_thresh := 0
				if level >= 40 {
					hev_thresh = 2
				} else if level >= 15 {
					hev_thresh = 1
				}
				info = FilterInfo{ f_ilevel: ilevel, f_limit: 2 * level + ilevel, hev_thresh: hev_thresh, f_inner: i4x4 }
			}
			fstrengths[s][i4x4] = info
		}
	}
	return fstrengths
}

fn (mut d WebpDecoder) filter_mb(mut y_plane []int, mut u_plane []int, mut v_plane []int, y_stride int, uv_stride int, ox int, oy int, cux int, cuy int, mb_x int, mb_y int, f_info FilterInfo, filter_type int) {
	limit := f_info.f_limit
	if limit == 0 {
		return
	}
	ilevel := f_info.f_ilevel
	hev_thr := f_info.hev_thresh
	f_inner := f_info.f_inner
	yb := oy * y_stride + ox
	if filter_type == 1 {
		if mb_x > 0 {
			simple_filter_line(mut y_plane, yb, y_stride, 1, 16, limit + 4)
		}
		if f_inner != 0 {
			for k in 1 .. 4 {
				simple_filter_line(mut y_plane, yb + 4 * k, y_stride, 1, 16, limit)
			}
		}
		if mb_y > 0 {
			simple_filter_line(mut y_plane, yb, 1, y_stride, 16, limit + 4)
		}
		if f_inner != 0 {
			for k in 1 .. 4 {
				simple_filter_line(mut y_plane, yb + 4 * k * y_stride, 1, y_stride, 16, limit)
			}
		}
	} else {
		ub := cuy * uv_stride + cux
		vb := ub
		if mb_x > 0 {
			filter_loop_26(mut y_plane, yb, 1, y_stride, 16, limit + 4, ilevel, hev_thr)
			filter_loop_26(mut u_plane, ub, 1, uv_stride, 8, limit + 4, ilevel, hev_thr)
			filter_loop_26(mut v_plane, vb, 1, uv_stride, 8, limit + 4, ilevel, hev_thr)
		}
		if f_inner != 0 {
			for k in 1 .. 4 {
				filter_loop_24(mut y_plane, yb + 4 * k, 1, y_stride, 16, limit, ilevel, hev_thr)
			}
			filter_loop_24(mut u_plane, ub + 4, 1, uv_stride, 8, limit, ilevel, hev_thr)
			filter_loop_24(mut v_plane, vb + 4, 1, uv_stride, 8, limit, ilevel, hev_thr)
		}
		if mb_y > 0 {
			filter_loop_26(mut y_plane, yb, y_stride, 1, 16, limit + 4, ilevel, hev_thr)
			filter_loop_26(mut u_plane, ub, uv_stride, 1, 8, limit + 4, ilevel, hev_thr)
			filter_loop_26(mut v_plane, vb, uv_stride, 1, 8, limit + 4, ilevel, hev_thr)
		}
		if f_inner != 0 {
			for k in 1 .. 4 {
				filter_loop_24(mut y_plane, yb + 4 * k * y_stride, y_stride, 1, 16, limit, ilevel, hev_thr)
			}
			filter_loop_24(mut u_plane, ub + 4 * uv_stride, uv_stride, 1, 8, limit, ilevel, hev_thr)
			filter_loop_24(mut v_plane, vb + 4 * uv_stride, uv_stride, 1, 8, limit, ilevel, hev_thr)
		}
	}
}

// ============================================================================
// VP8 lossy: reconstruct + main decode
// ============================================================================

fn (mut d WebpDecoder) reconstruct_mb(block &MbBlock, coeffs []int, mut y_plane []int, mut u_plane []int, mut v_plane []int, y_stride int, uv_stride int, ox int, oy int, cux int, cuy int, mb_x int, mb_y int, mb_w int, mut top_y []int, mut top_u []int, mut top_v []int) ! {
	// Bring the unfiltered top samples into the work plane before predicting.
	y_top := (oy - 1) * y_stride + ox
	t_base := mb_x * 16
	for i in 0 .. 16 {
		y_plane[y_top + i] = top_y[t_base + i]
	}
	if block.is_i4x4 == 0 {
		ymode := check_mode(mb_x, mb_y, block.imodes[0])
		predict16(mut y_plane, y_stride, ox, oy, ymode)!
		for k in 0 .. 16 {
			bx := ox + (k % 4) * 4
			by := oy + (k / 4) * 4
			mut slice := d.arena.alloc_scratch(16)!
			for j in 0 .. 16 {
				slice[j] = coeffs[k * 16 + j]
			}
			transform_one(mut d.arena, slice, mut y_plane, y_stride, bx, by)!
		}
	} else {
		mut tr := d.arena.alloc_scratch(4)!
		if mb_x >= mb_w - 1 {
			v := top_y[t_base + 15]
			for i in 0 .. 4 {
				tr[i] = v
			}
		} else {
			next_base := (mb_x + 1) * 16
			for i in 0 .. 4 {
				tr[i] = top_y[next_base + i]
			}
		}
		mut dy := -1
		for _ in 0 .. 4 {
			row := (oy + dy) * y_stride + ox + 16
			for i in 0 .. 4 {
				y_plane[row + i] = tr[i]
			}
			dy += 4
		}
		for k in 0 .. 16 {
			bx := ox + (k % 4) * 4
			by := oy + (k / 4) * 4
			predict4(mut d.arena, mut y_plane, y_stride, bx, by, block.imodes[k])!
			mut slice := d.arena.alloc_scratch(16)!
			for j in 0 .. 16 {
				slice[j] = coeffs[k * 16 + j]
			}
			transform_one(mut d.arena, slice, mut y_plane, y_stride, bx, by)!
		}
	}
	uvmode := check_mode(mb_x, mb_y, block.uvmode)
	c_base := mb_x * 8
	u_top := (cuy - 1) * uv_stride + cux
	v_top := u_top
	for i in 0 .. 8 {
		u_plane[u_top + i] = top_u[c_base + i]
		v_plane[v_top + i] = top_v[c_base + i]
	}
	predict8(mut u_plane, uv_stride, cux, cuy, uvmode)!
	predict8(mut v_plane, uv_stride, cux, cuy, uvmode)!
	for k in 0 .. 4 {
		bx := cux + (k % 2) * 4
		by := cuy + (k / 2) * 4
		mut us := d.arena.alloc_scratch(16)!
		mut vs := d.arena.alloc_scratch(16)!
		for j in 0 .. 16 {
			us[j] = coeffs[256 + k * 16 + j]
			vs[j] = coeffs[320 + k * 16 + j]
		}
		transform_one(mut d.arena, us, mut u_plane, uv_stride, bx, by)!
		transform_one(mut d.arena, vs, mut v_plane, uv_stride, bx, by)!
	}
	// Stash the unfiltered bottom row for the next macroblock row.
	y_bot := (oy + 15) * y_stride + ox
	for i in 0 .. 16 {
		top_y[t_base + i] = y_plane[y_bot + i]
	}
	c_bot := (cuy + 7) * uv_stride + cux
	for i in 0 .. 8 {
		top_u[c_base + i] = u_plane[c_bot + i]
		top_v[c_base + i] = v_plane[c_bot + i]
	}
}

fn (mut d WebpDecoder) setup_token_partitions(after []u8, num_parts_minus_one int) []BooleanDecoder {
	last := num_parts_minus_one
	mut parts := []BooleanDecoder{len: last + 1}
	d.arena.track(parts)
	mut pos := last * 3
	mut size_left := after.len - last * 3
	for p in 0 .. last {
		mut psize := int(after[3 * p]) | (int(after[3 * p + 1]) << 8) | (int(after[3 * p + 2]) << 16)
		if psize > size_left {
			psize = size_left
		}
		parts[p] = new_boolean_decoder(after[pos..pos + psize], 0, -1)
		pos += psize
		size_left -= psize
	}
	parts[last] = new_boolean_decoder(after[pos..], 0, -1)
	return parts
}

fn (mut d WebpDecoder) decode_vp8(payload []u8) ![]int {
	fh := d.parse_vp8_frame_header(payload)!
	check_rgba_dimensions(fh.width, fh.height)!
	if !fh.key_frame {
		return error('Only keyframe VP8 decoding is supported')
	}
	mb_w := fh.mb_w
	mb_h := fh.mb_h
	part0_start := fh.data_offset
	part0_len := fh.partition_length
	part0 := payload[part0_start..part0_start + part0_len]
	mut dec_br := new_boolean_decoder(part0, 0, -1)
	main := d.parse_vp8_main_header(mut dec_br)
	num_parts_minus_one := main.num_parts_minus_one
	after := payload[part0_start + part0_len..]
	mut token_br := d.setup_token_partitions(after, num_parts_minus_one)
	dec_br.get_bit(0x80) // update_proba (ignored)
	proba := d.parse_vp8_proba(mut dec_br)!

	yw := mb_w * 16
	yh := mb_h * 16
	uw := mb_w * 8
	uh := mb_h * 8
	y_stride := yw + 5
	uv_stride := uw + 1
	mut y_plane := filled(y_stride * (yh + 1), 129)
	mut u_plane := filled(uv_stride * (uh + 1), 129)
	mut v_plane := filled(uv_stride * (uh + 1), 129)
	d.arena.track(y_plane)
	d.arena.track(u_plane)
	d.arena.track(v_plane)
	for i in 0 .. y_stride {
		y_plane[i] = 127
	}
	for i in 0 .. uv_stride {
		u_plane[i] = 127
		v_plane[i] = 127
	}

	filter_type := main.filter.filter_type
	fstrengths := d.precompute_filter_strengths(&main.filter, &main.segment)
	dqm := main.dqm
	bands_ptr := proba.bands_ptr
	use_skip := proba.use_skip_proba

	mut intra_t := filled(4 * mb_w + 4, vp8_dc_pred)
	mut intra_l := filled(4, vp8_dc_pred)
	d.arena.track(intra_t)
	d.arena.track(intra_l)
	mut nz := NzState{
		mb_x:        0
		above_nz:    []int{len: mb_w}
		above_nz_dc: []int{len: mb_w}
		left_nz:     0
		left_nz_dc:  0
	}
	d.arena.track(nz.above_nz)
	d.arena.track(nz.above_nz_dc)
	// Unfiltered top-sample stash (libwebp's ``yuv_t``).
	mut top_y := filled(16 * mb_w, 127)
	mut top_u := filled(8 * mb_w, 127)
	mut top_v := filled(8 * mb_w, 127)
	d.arena.track(top_y)
	d.arena.track(top_u)
	d.arena.track(top_v)

	// Final (filtered) planes. The work planes above are a scratch cache
	// whose top row is overwritten with the unfiltered stash; the filter
	// runs on the final planes so its top neighbors are already filtered.
	mut y_final := filled(y_stride * (yh + 1), 129)
	mut u_final := filled(uv_stride * (uh + 1), 129)
	mut v_final := filled(uv_stride * (uh + 1), 129)
	d.arena.track(y_final)
	d.arena.track(u_final)
	d.arena.track(v_final)
	for i in 0 .. y_stride {
		y_final[i] = 127
	}
	for i in 0 .. uv_stride {
		u_final[i] = 127
		v_final[i] = 127
	}

	// Per-macroblock scratch: the coefficient block plus the IDCT/predict
	// temporaries carved out of the arena and reused between macroblocks.
	d.arena.init_scratch(4096)

	for mb_y in 0 .. mb_h {
		for k in 0 .. 4 {
			intra_l[k] = vp8_dc_pred
		}
		modes := d.parse_vp8_intra_modes_row(mut dec_br, mb_w, mut intra_t, mut intra_l, &main.segment, &proba)
		mut token_br_y := token_br[mb_y & num_parts_minus_one]
		nz.left_nz = 0
		nz.left_nz_dc = 0
		mut row_finfo := []FilterInfo{len: mb_w}
		d.arena.track(row_finfo)
		for mb_x in 0 .. mb_w {
			nz.mb_x = mb_x
			d.arena.reset_scratch()
			block := modes[mb_x]
			mut skip := 0
			if use_skip != 0 {
				skip = block.skip
			}
			mut coeffs := d.arena.alloc_scratch(384)!
			if skip != 0 {
				nz.above_nz[mb_x] = 0
				if block.is_i4x4 == 0 {
					nz.above_nz_dc[mb_x] = 0
					nz.left_nz_dc = 0
				}
				nz.left_nz = 0
			} else {
				dq := dqm[block.segment]
				coeffs = d.decode_vp8_residuals(mut token_br_y, &block, &dq, bands_ptr, mut nz)!
			}
			mut f_info := FilterInfo{}
			if filter_type > 0 {
				mut fi := fstrengths[block.segment][block.is_i4x4]
				if skip == 0 {
					fi.f_inner |= 1
				}
				f_info = fi
			}
			row_finfo[mb_x] = f_info
			ox := 1 + mb_x * 16
			oy := 1 + mb_y * 16
			cux := 1 + mb_x * 8
			cuy := 1 + mb_y * 8
			d.reconstruct_mb(&block, coeffs, mut y_plane, mut u_plane, mut v_plane, y_stride, uv_stride, ox, oy, cux, cuy, mb_x, mb_y, mb_w, mut top_y, mut top_u, mut top_v)!
		}
		token_br[mb_y & num_parts_minus_one] = token_br_y
		// Copy the reconstructed row into the final planes, then run the
		// in-loop filter on the final planes.
		oy := 1 + mb_y * 16
		for j in 0 .. 16 {
			src := (oy + j) * y_stride
			for i in 0 .. y_stride {
				y_final[src + i] = y_plane[src + i]
			}
		}
		cuy := 1 + mb_y * 8
		for j in 0 .. 8 {
			src := (cuy + j) * uv_stride
			for i in 0 .. uv_stride {
				u_final[src + i] = u_plane[src + i]
				v_final[src + i] = v_plane[src + i]
			}
		}
		if filter_type > 0 {
			for mb_x in 0 .. mb_w {
				d.filter_mb(mut y_final, mut u_final, mut v_final, y_stride, uv_stride, 1 + mb_x * 16, 1 + mb_y * 16, 1 + mb_x * 8, 1 + mb_y * 8, mb_x, mb_y, row_finfo[mb_x], filter_type)
			}
		}
	}

	width := fh.width
	height := fh.height
	mut y_out := []int{len: width * height}
	d.arena.track(y_out)
	mut yo := 0
	for j in 0 .. height {
		base := (j + 1) * y_stride + 1
		for i in 0 .. width {
			y_out[yo] = y_final[base + i]
			yo++
		}
	}
	uw_img := (width + 1) / 2
	uh_img := (height + 1) / 2
	mut u_out := []int{len: uw_img * uh_img}
	mut v_out := []int{len: uw_img * uh_img}
	d.arena.track(u_out)
	d.arena.track(v_out)
	mut uo := 0
	for j in 0 .. uh_img {
		base := (j + 1) * uv_stride + 1
		for i in 0 .. uw_img {
			u_out[uo] = u_final[base + i]
			v_out[uo] = v_final[base + i]
			uo++
		}
	}
	return planes_to_rgba(y_out, u_out, v_out, width, uw_img, width, height)
}

// ============================================================================
// YUV -> RGBA assembly (fancy 4:2:0 upsampling)
// ============================================================================

fn emit_rgba(mut out []int, off int, yval int, uv int) {
	yuv_to_rgb(mut out, off, yval, uv & 0xff, (uv >> 16) & 0xff)
}

fn row_slice(plane []int, stride int, j int, w int) []int {
	base := j * stride
	return plane[base..base + w]
}

fn line_pair(mut out []int, top_y []int, bot_y []int, has_bot bool, top_u []int, top_v []int, cur_u []int, cur_v []int, top_off int, bot_off int, width int) {
	last_pixel_pair := (width - 1) >> 1
	mut tl_uv := top_u[0] | (top_v[0] << 16)
	mut l_uv := cur_u[0] | (cur_v[0] << 16)
	mut uv0 := (3 * tl_uv + l_uv + 0x00020002) >> 2
	emit_rgba(mut out, top_off, top_y[0], uv0)
	if has_bot {
		uv0 = (3 * l_uv + tl_uv + 0x00020002) >> 2
		emit_rgba(mut out, bot_off, bot_y[0], uv0)
	}
	for x in 1 .. last_pixel_pair + 1 {
		t_uv := top_u[x] | (top_v[x] << 16)
		uv := cur_u[x] | (cur_v[x] << 16)
		avg := tl_uv + t_uv + l_uv + uv + 0x00080008
		diag_12 := (avg + 2 * (t_uv + l_uv)) >> 3
		diag_03 := (avg + 2 * (tl_uv + uv)) >> 3
		mut u0 := (diag_12 + tl_uv) >> 1
		mut u1 := (diag_03 + t_uv) >> 1
		emit_rgba(mut out, top_off + (2 * x - 1) * 4, top_y[2 * x - 1], u0)
		emit_rgba(mut out, top_off + (2 * x) * 4, top_y[2 * x], u1)
		if has_bot {
			u0 = (diag_03 + l_uv) >> 1
			u1 = (diag_12 + uv) >> 1
			emit_rgba(mut out, bot_off + (2 * x - 1) * 4, bot_y[2 * x - 1], u0)
			emit_rgba(mut out, bot_off + (2 * x) * 4, bot_y[2 * x], u1)
		}
		tl_uv = t_uv
		l_uv = uv
	}
	if (width & 1) == 0 {
		uv0 = (3 * tl_uv + l_uv + 0x00020002) >> 2
		emit_rgba(mut out, top_off + (width - 1) * 4, top_y[width - 1], uv0)
		if has_bot {
			uv0 = (3 * l_uv + tl_uv + 0x00020002) >> 2
			emit_rgba(mut out, bot_off + (width - 1) * 4, bot_y[width - 1], uv0)
		}
	}
}

fn planes_to_rgba(y_plane []int, u_plane []int, v_plane []int, y_stride int, uv_stride int, width int, height int) []int {
	mut out := []int{len: width * height * 4}
	uv_w := (width + 1) >> 1
	if height <= 0 {
		return out
	}
	line_pair(mut out, row_slice(y_plane, y_stride, 0, width), []int{}, false, row_slice(u_plane, uv_stride, 0, uv_w), row_slice(v_plane, uv_stride, 0, uv_w), row_slice(u_plane, uv_stride, 0, uv_w), row_slice(v_plane, uv_stride, 0, uv_w), 0, 0, width)
	mut p := 1
	for 2 * p < height {
		line_pair(mut out, row_slice(y_plane, y_stride, 2 * p - 1, width), row_slice(y_plane, y_stride, 2 * p, width), true, row_slice(u_plane, uv_stride, p - 1, uv_w), row_slice(v_plane, uv_stride, p - 1, uv_w), row_slice(u_plane, uv_stride, p, uv_w), row_slice(v_plane, uv_stride, p, uv_w), (2 * p - 1) * width * 4, (2 * p) * width * 4, width)
		p++
	}
	if height > 1 && height % 2 == 0 {
		c := height / 2 - 1
		line_pair(mut out, row_slice(y_plane, y_stride, height - 1, width), []int{}, false, row_slice(u_plane, uv_stride, c, uv_w), row_slice(v_plane, uv_stride, c, uv_w), row_slice(u_plane, uv_stride, c, uv_w), row_slice(v_plane, uv_stride, c, uv_w), (height - 1) * width * 4, 0, width)
	}
	return out
}

// ============================================================================
// Public API
// ============================================================================

struct WebpDecoder {
mut:
	data        []u8
	parsed      bool
	width       int
	height      int
	max_sample  int
	has_alpha   bool
	layout      string
	vp8x_flags  int
	chunks      []Chunk
	icc_profile []u8
	exif        []u8
	xmp         []u8
	arena       Arena
	animated    bool
	loop_count  int
	bgcolor     u32
	anim_frames []AnimFrameSpec
}

fn (mut d WebpDecoder) find_alph() []u8 {
	for c in d.chunks {
		if fc_eq(c.fourcc, chunk_alph) {
			return c.payload
		}
	}
	return []u8{}
}

fn (mut d WebpDecoder) load_webp() ![]int {
	d.parse()!
	if d.layout == layout_animated {
		// Animated files expose frame 0 as the static poster image.
		canvases := d.composite_frames()!
		mut out := []int{}
		unsafe { out = canvases[0] } // shallow header move; block is arena-tracked
		return out
	}
	mut is_lossless := false
	mut is_lossy := false
	mut payload := []u8{}
	for c in d.chunks {
		if fc_eq(c.fourcc, chunk_vp8l) {
			is_lossless = true
			payload = c.payload
			break
		}
		if fc_eq(c.fourcc, chunk_vp8) {
			is_lossy = true
			payload = c.payload
			break
		}
	}
	if !is_lossless && !is_lossy {
		return error('WEBP file contains no image chunk')
	}
	if is_lossless {
		argb := d.decode_vp8l(payload)!
		mut rgba := []int{len: argb.len * 4}
		d.arena.track(rgba)
		for i, p in argb {
			rgba[i * 4] = (p >> 16) & 0xff
			rgba[i * 4 + 1] = (p >> 8) & 0xff
			rgba[i * 4 + 2] = p & 0xff
			rgba[i * 4 + 3] = (p >> 24) & 0xff
		}
		return rgba
	}
	mut rgba := d.decode_vp8(payload)!
	d.arena.track(rgba)
	alph := d.find_alph()
	if alph.len > 0 {
		alpha := d.decode_alph(alph, d.width, d.height)!
		for i in 0 .. d.width * d.height {
			rgba[i * 4 + 3] = alpha[i]
		}
	}
	return rgba
}

// ---------------------------------------------------------------------------
// Output geometry: crop + scale.
//
// Port of libwebp's integer rescaler (src/utils/rescaler_utils.c
// + src/dsp/rescaler.c, RFIX=32 fixed-point). C's `rescaler_t` is uint32, so
// scratch values are u32 and every multiply intermediate is u64 before the
// >>32 shift; V unsigned arithmetic wraps exactly like C.
// ---------------------------------------------------------------------------

const rfix = 32
const rescaler_one = u64(1) << 32
const rescaler_rounder = rescaler_one >> 1
const rescaler_u32_max = u64(0xFFFFFFFF)

// rescaler_frac mirrors WEBP_RESCALER_FRAC: (x << 32) / y truncated to u32.
// The truncation is observable when y == 1 (the result wraps to 0), matching
// libwebp.
fn rescaler_frac(x int, y int) u32 {
	return u32((u64(x) << rfix) / u64(y))
}

// mult_fix mirrors the MULT_FIX macro: (x * y + ROUNDER) >> 32.
fn mult_fix(x u64, y u64) u32 {
	return u32((x * y + rescaler_rounder) >> rfix)
}

// mult_fix_floor mirrors MULT_FIX_FLOOR: (x * y) >> 32.
fn mult_fix_floor(x u64, y u64) u32 {
	return u32((x * y) >> rfix)
}

// rescaler_clamp clamps a rescaler output sample the way libwebp does; only
// the upper bound is reachable for valid RGBA input.
fn rescaler_clamp(v u32) int {
	return if v > 255 { 255 } else { int(v) }
}

// Rescaler mirrors WebPRescaler. irow/frow are the two u32 rows inside
// `work`; C swaps the row pointers on every import when y_expand, but here
// the buffer is fixed and the ibase/fbase offsets toggle instead (no slice
// reassignment, hence no hidden clone under -gc none).
struct Rescaler {
mut:
	x_expand     bool
	y_expand     bool
	num_channels int
	fx_scale     u32
	fy_scale     u64 // may be rescaler_one (identity), so u64 not u32
	fxy_scale    u32
	y_accum      int
	y_add        int
	y_sub        int
	x_add        int
	x_sub        int
	src_width    int
	src_height   int
	dst_width    int
	dst_height   int
	work         []u32
	ibase        int
	fbase        int
}

// import_row mirrors WebPRescalerImportRow (shrink/expand dispatch).
fn (mut r Rescaler) import_row(src []int, row_off int) {
	nc := r.num_channels
	n := nc * r.dst_width
	if r.y_expand {
		if r.ibase == 0 {
			r.ibase = n
			r.fbase = 0
		} else {
			r.ibase = 0
			r.fbase = n
		}
	}
	if !r.x_expand {
		for c in 0 .. nc {
			mut x_in := c
			mut x_out := c
			mut total := u32(0)
			mut accum := 0
			mut base := u32(0)
			for x_out < n {
				accum += r.x_add
				for accum > 0 {
					accum -= r.x_sub
					base = u32(src[row_off + x_in])
					total += base
					x_in += nc
				}
				// Emit next horizontal pixel.
				frac := base * u32(-accum)
				r.work[r.fbase + x_out] = total * u32(r.x_sub) - frac
				// Fresh fractional start for the next pixel.
				total = mult_fix(u64(frac), u64(r.fx_scale))
				x_out += nc
			}
		}
	} else {
		for c in 0 .. nc {
			mut x_in := c
			mut x_out := c
			mut accum := r.x_add
			mut left := u32(src[row_off + x_in])
			mut right := left
			if r.src_width > 1 {
				right = u32(src[row_off + x_in + nc])
			}
			x_in += nc
			for {
				r.work[r.fbase + x_out] = right * u32(r.x_add) + (left - right) * u32(accum)
				x_out += nc
				if x_out >= n {
					break
				}
				accum -= r.x_sub
				if accum < 0 {
					left = right
					x_in += nc
					right = u32(src[row_off + x_in])
					accum += r.x_add
				}
			}
		}
	}
	if !r.y_expand {
		// Accumulate the contribution of the new row.
		for x in 0 .. n {
			r.work[r.ibase + x] += r.work[r.fbase + x]
		}
	}
	r.y_accum -= r.y_sub
}

// export_row mirrors WebPRescalerExportRow (expand/shrink/special dispatch).
fn (mut r Rescaler) export_row(mut out []int, dst_off int) {
	n := r.num_channels * r.dst_width
	if r.y_expand {
		if r.y_accum == 0 {
			for x in 0 .. n {
				out[dst_off + x] = rescaler_clamp(mult_fix(u64(r.work[r.fbase + x]), r.fy_scale))
			}
		} else {
			b := rescaler_frac(-r.y_accum, r.y_sub)
			a := u32(rescaler_one - u64(b))
			for x in 0 .. n {
				i_val := u64(a) * u64(r.work[r.fbase + x]) + u64(b) * u64(r.work[r.ibase + x])
				j_val := (i_val + rescaler_rounder) >> rfix
				out[dst_off + x] = rescaler_clamp(mult_fix(j_val, r.fy_scale))
			}
		}
	} else if r.fxy_scale != 0 {
		yscale := u32(r.fy_scale) * u32(-r.y_accum)
		if yscale != 0 {
			for x in 0 .. n {
				frac := mult_fix_floor(u64(r.work[r.fbase + x]), u64(yscale))
				out[dst_off + x] = rescaler_clamp(mult_fix(u64(r.work[r.ibase + x] - frac), u64(r.fxy_scale)))
				r.work[r.ibase + x] = frac // new fractional start
			}
		} else {
			for x in 0 .. n {
				out[dst_off + x] = rescaler_clamp(mult_fix(u64(r.work[r.ibase + x]), u64(r.fxy_scale)))
				r.work[r.ibase + x] = 0
			}
		}
	} else {
		// Special case: no scaling at all (src_w == 1, dst_w <= 2,
		// src_h == dst_h); pass the accumulated row through.
		for x in 0 .. n {
			out[dst_off + x] = int(r.work[r.ibase + x])
			r.work[r.ibase + x] = 0
		}
	}
	r.y_accum += r.y_add
}

// crop_rgba extracts a sub-rectangle from a top-down RGBA plane.
fn (mut d WebpDecoder) crop_rgba(rgba []int, src_w int, src_h int, left int, top int, crop_w int, crop_h int) ![]int {
	if left < 0 || top < 0 {
		return error('WEBP crop offset must not be negative')
	}
	if crop_w <= 0 || crop_h <= 0 {
		return error('WEBP crop size must be positive')
	}
	if left > src_w || top > src_h || crop_w > src_w - left || crop_h > src_h - top {
		return error('WEBP crop rectangle ${left},${top},${crop_w}x${crop_h} exceeds image ${src_w}x${src_h}')
	}
	mut out := []int{len: crop_w * crop_h * 4}
	d.arena.track(out)
	mut o := 0
	for y in top .. top + crop_h {
		start := (y * src_w + left) * 4
		for x in 0 .. crop_w * 4 {
			out[o] = rgba[start + x]
			o++
		}
	}
	return out
}

// resolve_scale fills a zero scale component from the source aspect ratio
// (WebPRescalerGetScaledDimensions).
fn resolve_scale(src_w int, src_h int, w int, h int) !(int, int) {
	mut ww := w
	mut hh := h
	if ww == 0 && src_h > 0 {
		if hh < 0 || src_w <= 0 || hh > (max_int - src_h + 1) / src_w {
			return error('WEBP scale exceeds the supported buffer size')
		}
		ww = (src_w * hh + src_h - 1) / src_h
	}
	if hh == 0 && src_w > 0 {
		if ww < 0 || src_h <= 0 || ww > (max_int - src_w + 1) / src_h {
			return error('WEBP scale exceeds the supported buffer size')
		}
		hh = (src_h * ww + src_w - 1) / src_w
	}
	if ww <= 0 || hh <= 0 {
		return error('WEBP scale resolves to invalid size ${ww}x${hh}')
	}
	return ww, hh
}

// scale_rgba rescales a top-down RGBA plane with the libwebp integer
// rescaler (WebPRescalerInit plus the row import/export drivers).
fn (mut d WebpDecoder) scale_rgba(rgba []int, src_w int, src_h int, dst_w int, dst_h int) ![]int {
	if src_w <= 0 || src_h <= 0 {
		return error('WEBP rescaler: invalid source dimensions')
	}
	if dst_w <= 0 || dst_h <= 0 {
		return error('WEBP rescaler: invalid destination dimensions')
	}
	check_rgba_dimensions(dst_w, dst_h)!
	if dst_w > max_int / int(sizeof(u32)) / 8 {
		return error('WEBP scale scanline exceeds the supported buffer size')
	}
	if src_w == dst_w && src_h == dst_h {
		mut copied := []int{len: rgba.len}
		for i, v in rgba {
			copied[i] = v
		}
		d.arena.track(copied)
		return copied
	}
	x_expand := src_w < dst_w
	y_expand := src_h < dst_h
	nc := 4
	n := nc * dst_w
	mut r := Rescaler{
		x_expand:     x_expand
		y_expand:     y_expand
		num_channels: nc
		src_width:    src_w
		src_height:   src_h
		dst_width:    dst_w
		dst_height:   dst_h
	}
	if x_expand {
		// Expansion uses bilinear interpolation over dst_w - 1 source
		// intervals.
		r.x_add = dst_w - 1
		r.x_sub = src_w - 1
	} else {
		r.x_add = src_w
		r.x_sub = dst_w
		r.fx_scale = rescaler_frac(1, dst_w)
	}
	if y_expand {
		r.y_add = src_h - 1
		r.y_sub = dst_h - 1
		r.y_accum = r.y_sub
		// libwebp computes FRAC(1, x_add) as uint32, which wraps to 0
		// when x_add == 1 (dst_w == 2) and zeroes the output. Use
		// RESCALER_ONE as the exact identity ratio instead (the same
		// documented deviation from libwebp).
		if r.x_add == 1 {
			r.fy_scale = rescaler_one
		} else {
			r.fy_scale = u64(rescaler_frac(1, r.x_add))
		}
	} else {
		r.y_add = src_h
		r.y_sub = dst_h
		r.y_accum = r.y_add
		r.fy_scale = u64(rescaler_frac(1, dst_h))
		// WEBP_RESCALER_FRAC(dst_h, x_add * y_add) with the uint32
		// overflow special case libwebp encodes as fxy_scale = 0.
		ratio := (u64(dst_h) * rescaler_one) / (u64(r.x_add) * u64(r.y_add))
		if ratio > rescaler_u32_max {
			r.fxy_scale = 0
		} else {
			r.fxy_scale = u32(ratio)
		}
	}
	work := []u32{len: 2 * n}
	d.arena.track(work)
	unsafe { r.work = work } // shallow header move; the block is arena-tracked
	r.ibase = 0
	r.fbase = n
	mut out := []int{len: n * dst_h}
	d.arena.track(out)
	mut src_y := 0
	mut dst_y := 0
	for dst_y < dst_h {
		if src_y < src_h {
			r.import_row(rgba, src_y * src_w * nc)
			src_y++
		}
		if r.y_accum <= 0 {
			r.export_row(mut out, dst_y * n)
			dst_y++
		} else if src_y >= src_h {
			return error('WEBP rescaler: incomplete output rows')
		}
	}
	return out
}

// cleanup releases every arena-owned allocation. Called on both the success and
// the error path of the public decode entry point. The returned image's []u8 is
// allocated outside the arena and owned by the caller, so it survives.
fn (mut d WebpDecoder) cleanup() {
	d.arena.free_all()
}

// Options controls the output geometry applied after decoding.
pub struct Options {
pub:
	scale_w int // 0 = preserve aspect ratio
	scale_h int // 0 = preserve aspect ratio
	crop    bool
	crop_x  int
	crop_y  int
	crop_w  int
	crop_h  int
}

fn (mut d WebpDecoder) decode_image_with(opts Options) !common.Image {
	mut rgba := d.load_webp()!
	mut w := d.width
	mut h := d.height
	if opts.crop {
		cropped := d.crop_rgba(rgba, w, h, opts.crop_x, opts.crop_y, opts.crop_w, opts.crop_h)!
		unsafe { rgba = cropped } // shallow header move; both blocks are arena-tracked
		w = opts.crop_w
		h = opts.crop_h
	}
	if opts.scale_w != 0 || opts.scale_h != 0 {
		sw, sh := resolve_scale(w, h, opts.scale_w, opts.scale_h)!
		scaled := d.scale_rgba(rgba, w, h, sw, sh)!
		unsafe { rgba = scaled }
		w = sw
		h = sh
	}
	mut out := []u8{len: rgba.len}
	for i, v in rgba {
		out[i] = u8(v & 0xff)
	}
	return common.Image{
		width:  w
		height: h
		rgba:   out
	}
}

fn (mut d WebpDecoder) decode_image() !common.Image {
	return d.decode_image_with(Options{})
}

fn (mut d WebpDecoder) decode_animation_inner() !WebPAnimation {
	d.parse()!
	if d.layout != layout_animated {
		return error('WEBP file is not animated')
	}
	canvases := d.composite_frames()!
	specs := d.anim_frames
	mut frames := []WebPFrame{}
	for i in 0 .. canvases.len {
		canvas := canvases[i]
		spec := specs[i]
		mut out := []u8{len: canvas.len}
		for j, v in canvas {
			out[j] = u8(v & 0xff)
		}
		frames << WebPFrame{
			rgba:         out
			width:        d.width
			height:       d.height
			duration_ms:  spec.duration
			dispose_bg:   spec.dispose_bg
			blend:        spec.blend
			x:            spec.x
			y:            spec.y
			frame_width:  spec.w
			frame_height: spec.h
		}
	}
	return WebPAnimation{
		frames:     frames
		loop_count: d.loop_count
		width:      d.width
		height:     d.height
		bg_r:       int((d.bgcolor >> 16) & 0xff)
		bg_g:       int((d.bgcolor >> 8) & 0xff)
		bg_b:       int(d.bgcolor & 0xff)
		bg_a:       int(d.bgcolor >> 24)
	}
}

// decode_animation decodes an animated WEBP into fully composited frames.
// The caller owns each frame's rgba buffer and the frames array and must free
// them; the decoder's arena is released before returning.
pub fn decode_animation(data []u8) !WebPAnimation {
	mut d := WebpDecoder{
		data:       data
		max_sample: 255
		bgcolor:    0xFFFFFFFF
	}
	res := d.decode_animation_inner() or {
		d.cleanup()
		return err
	}
	d.cleanup()
	return res
}

// WebpPlayer steps through a fully composited animated WEBP one frame at
// a time, mirroring the viewer-facing API of gif.GifPlayer. Unlike the
// GIF player (which composites incrementally), WEBP frames are already
// composited by decode_animation, so the player only owns the frame
// buffers and a cursor.
pub struct WebpPlayer {
mut:
	anim  WebPAnimation
	index int
}

// new_webp_player decodes and composites every frame up front and rewinds
// the cursor before frame 0.
pub fn new_webp_player(data []u8) !&WebpPlayer {
	mut anim := decode_animation(data)!
	if anim.frames.len == 0 {
		unsafe { anim.frames.free() }
		return error('WEBP animation contains no frames')
	}
	return &WebpPlayer{
		anim:  anim
		index: -1
	}
}

// frame_count returns the number of frames in the animation.
pub fn (p &WebpPlayer) frame_count() int {
	return p.anim.frames.len
}

// loop_count returns the ANIM loop count (0 = forever, >=1 finite).
pub fn (p &WebpPlayer) loop_count() int {
	return p.anim.loop_count
}

// delay_ms returns the display duration of frame `i` in milliseconds.
pub fn (p &WebpPlayer) delay_ms(i int) int {
	return p.anim.frames[i].duration_ms
}

// canvas_width returns the animation canvas width.
pub fn (p &WebpPlayer) canvas_width() int {
	return p.anim.width
}

// canvas_height returns the animation canvas height.
pub fn (p &WebpPlayer) canvas_height() int {
	return p.anim.height
}

// reset rewinds the cursor so the next step() renders frame 0. The
// frames are pre-composited, so there is no canvas to clear.
pub fn (mut p WebpPlayer) reset() {
	p.index = -1
}

// step returns the fully composited RGBA of the next frame in sequence.
// The buffer is owned by the player and stays valid until free(); the
// caller must copy it (e.g. upload to a texture) if it needs to outlive
// the player.
pub fn (mut p WebpPlayer) step() ![]u8 {
	if p.index + 1 >= p.anim.frames.len {
		return error('WebpPlayer: stepped past the last frame (call reset() to loop)')
	}
	p.index++
	return p.anim.frames[p.index].rgba
}

// free releases every frame buffer and the frames array, mirroring
// gif.GifPlayer.free (the player struct itself is freed by the caller).
pub fn (mut p WebpPlayer) free() {
	for f in p.anim.frames {
		unsafe { f.rgba.free() }
	}
	unsafe {
		p.anim.frames.free()
	}
}

// decode decodes a WEBP file into a common.Image (RGBA).
pub fn decode(data []u8) !common.Image {
	return decode_with(data, Options{})
}

// decode_with decodes a WEBP file and applies the output geometry in opts
// (crop first, then scale) before producing the RGBA image.
pub fn decode_with(data []u8, opts Options) !common.Image {
	mut d := WebpDecoder{
		data:       data
		max_sample: 255
	}
	res := d.decode_image_with(opts) or {
		d.cleanup()
		return err
	}
	d.cleanup()
	return res
}
