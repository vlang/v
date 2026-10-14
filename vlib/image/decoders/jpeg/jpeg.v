/**********************************************************************
*
* JPEG image decoder (baseline, progressive, arithmetic, 8/12-bit)
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module jpeg

import image.decoders.common
import os
import runtime
import sync.stdatomic

// ============================================================================
// ZIGZAG DE-SCRAMBLING TABLE
// ============================================================================

const zigzag_order = [
	0,
	1,
	8,
	16,
	9,
	2,
	3,
	10,
	17,
	24,
	32,
	25,
	18,
	11,
	4,
	5,
	12,
	19,
	26,
	33,
	40,
	48,
	41,
	34,
	27,
	20,
	13,
	6,
	7,
	14,
	21,
	28,
	35,
	42,
	49,
	56,
	57,
	50,
	43,
	36,
	29,
	22,
	15,
	23,
	30,
	37,
	44,
	51,
	58,
	59,
	52,
	45,
	38,
	31,
	39,
	46,
	53,
	60,
	61,
	54,
	47,
	55,
	62,
	63,
]

// ============================================================================
// ARITHMETIC CODING PROBABILITY TABLE (ITU-T T.81 Annex H, Table H.1)
// Packed as (qe << 16) | (mps << 8) | (switch << 7) | lps.
// ============================================================================

const arith_tab = [
	u32(0x5a1d0181),
	u32(0x2586020e),
	u32(0x11140310),
	u32(0x080b0412),
	u32(0x03d80514),
	u32(0x01da0617),
	u32(0x00e50719),
	u32(0x006f081c),
	u32(0x0036091e),
	u32(0x001a0a21),
	u32(0x000d0b23),
	u32(0x00060c09),
	u32(0x00030d0a),
	u32(0x00010d0c),
	u32(0x5a7f0f8f),
	u32(0x3f251024),
	u32(0x2cf21126),
	u32(0x207c1227),
	u32(0x17b91328),
	u32(0x1182142a),
	u32(0x0cef152b),
	u32(0x09a1162d),
	u32(0x072f172e),
	u32(0x055c1830),
	u32(0x04061931),
	u32(0x03031a33),
	u32(0x02401b34),
	u32(0x01b11c36),
	u32(0x01441d38),
	u32(0x00f51e39),
	u32(0x00b71f3b),
	u32(0x008a203c),
	u32(0x0068213e),
	u32(0x004e223f),
	u32(0x003b2320),
	u32(0x002c0921),
	u32(0x5ae125a5),
	u32(0x484c2640),
	u32(0x3a0d2741),
	u32(0x2ef12843),
	u32(0x261f2944),
	u32(0x1f332a45),
	u32(0x19a82b46),
	u32(0x15182c48),
	u32(0x11772d49),
	u32(0x0e742e4a),
	u32(0x0bfb2f4b),
	u32(0x09f8304d),
	u32(0x0861314e),
	u32(0x0706324f),
	u32(0x05cd3330),
	u32(0x04de3432),
	u32(0x040f3532),
	u32(0x03633633),
	u32(0x02d43734),
	u32(0x025c3835),
	u32(0x01f83936),
	u32(0x01a43a37),
	u32(0x01603b38),
	u32(0x01253c39),
	u32(0x00f63d3a),
	u32(0x00cb3e3b),
	u32(0x00ab3f3d),
	u32(0x008f203d),
	u32(0x5b1241c1),
	u32(0x4d044250),
	u32(0x412c4351),
	u32(0x37d84452),
	u32(0x2fe84553),
	u32(0x293c4654),
	u32(0x23794756),
	u32(0x1edf4857),
	u32(0x1aa94957),
	u32(0x174e4a48),
	u32(0x14244b48),
	u32(0x119c4c4a),
	u32(0x0f6b4d4a),
	u32(0x0d514e4b),
	u32(0x0bb64f4d),
	u32(0x0a40304d),
	u32(0x583251d0),
	u32(0x4d1c5258),
	u32(0x438e5359),
	u32(0x3bdd545a),
	u32(0x34ee555b),
	u32(0x2eae565c),
	u32(0x299a575d),
	u32(0x25164756),
	u32(0x557059d8),
	u32(0x4ca95a5f),
	u32(0x44d95b60),
	u32(0x3e225c61),
	u32(0x38245d63),
	u32(0x32b45e63),
	u32(0x2e17565d),
	u32(0x56a860df),
	u32(0x4f466165),
	u32(0x47e56266),
	u32(0x41cf6367),
	u32(0x3c3d6468),
	u32(0x375e5d63),
	u32(0x52316669),
	u32(0x4c0f676a),
	u32(0x4639686b),
	u32(0x415e6367),
	u32(0x56276ae9),
	u32(0x50e76b6c),
	u32(0x4b85676d),
	u32(0x55976d6e),
	u32(0x504f6b6f),
	u32(0x5a106fee),
	u32(0x55226d70),
	u32(0x59eb6ff0),
	u32(0x5a1d7171),
]

const num_arith_tbls = 16
const dc_stat_bins = 64
const ac_stat_bins = 256

// ============================================================================
// MARKERS
// ============================================================================

const marker_soi = 0xD8
const marker_eoi = 0xD9
const marker_sof0 = 0xC0
const marker_sof1 = 0xC1
const marker_sof2 = 0xC2
const marker_sof9 = 0xC9
const marker_sof10 = 0xCA
const marker_dht = 0xC4
const marker_dqt = 0xDB
const marker_dac = 0xCC
const marker_dri = 0xDD
const marker_sos = 0xDA
const marker_app14 = 0xEE
const marker_rst0 = 0xD0
const marker_rst7 = 0xD7

// ============================================================================
// INTEGER IDCT CONSTANTS (libjpeg jidctint.c ISLOW)
// ============================================================================

const idct_const_bits = 13
const idct_fix_0_298631336 = 2446
const idct_fix_0_390180644 = 3196
const idct_fix_0_541196100 = 4433
const idct_fix_0_765366865 = 6270
const idct_fix_0_899976223 = 7373
const idct_fix_1_175875602 = 9633
const idct_fix_1_501321110 = 12299
const idct_fix_1_847759065 = 15137
const idct_fix_1_961570560 = 16069
const idct_fix_2_053119869 = 16819
const idct_fix_2_562915447 = 20995
const idct_fix_3_072711026 = 25172

// ============================================================================
// YCbCr -> RGB FIXED POINT (libjpeg jdcolor.c)
// ============================================================================

const ycc_scalebits = 16
const ycc_one_half = 1 << (ycc_scalebits - 1)
const ycc_fix_r = 91881
const ycc_fix_b = 116130
const ycc_fix_g1 = 22554
const ycc_fix_g2 = 46802

// ============================================================================
// STANDARD HUFFMAN TABLES (JPEG Spec Annex K)
// ============================================================================

const std_dc_luma_bits = [0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0]
const std_dc_luma_vals = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

const std_ac_luma_bits = [0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 0x7D]
const std_ac_luma_vals = [
	0x01,
	0x02,
	0x03,
	0x00,
	0x04,
	0x11,
	0x05,
	0x12,
	0x21,
	0x31,
	0x41,
	0x06,
	0x13,
	0x51,
	0x61,
	0x07,
	0x22,
	0x71,
	0x14,
	0x32,
	0x81,
	0x91,
	0xa1,
	0x08,
	0x23,
	0x42,
	0xb1,
	0xc1,
	0x15,
	0x52,
	0xd1,
	0xf0,
	0x24,
	0x33,
	0x62,
	0x72,
	0x82,
	0x09,
	0x0a,
	0x16,
	0x17,
	0x18,
	0x19,
	0x1a,
	0x25,
	0x26,
	0x27,
	0x28,
	0x29,
	0x2a,
	0x34,
	0x35,
	0x36,
	0x37,
	0x38,
	0x39,
	0x3a,
	0x43,
	0x44,
	0x45,
	0x46,
	0x47,
	0x48,
	0x49,
	0x4a,
	0x53,
	0x54,
	0x55,
	0x56,
	0x57,
	0x58,
	0x59,
	0x5a,
	0x63,
	0x64,
	0x65,
	0x66,
	0x67,
	0x68,
	0x69,
	0x6a,
	0x73,
	0x74,
	0x75,
	0x76,
	0x77,
	0x78,
	0x79,
	0x7a,
	0x83,
	0x84,
	0x85,
	0x86,
	0x87,
	0x88,
	0x89,
	0x8a,
	0x92,
	0x93,
	0x94,
	0x95,
	0x96,
	0x97,
	0x98,
	0x99,
	0x9a,
	0xa2,
	0xa3,
	0xa4,
	0xa5,
	0xa6,
	0xa7,
	0xa8,
	0xa9,
	0xaa,
	0xb2,
	0xb3,
	0xb4,
	0xb5,
	0xb6,
	0xb7,
	0xb8,
	0xb9,
	0xba,
	0xc2,
	0xc3,
	0xc4,
	0xc5,
	0xc6,
	0xc7,
	0xc8,
	0xc9,
	0xca,
	0xd2,
	0xd3,
	0xd4,
	0xd5,
	0xd6,
	0xd7,
	0xd8,
	0xd9,
	0xda,
	0xe1,
	0xe2,
	0xe3,
	0xe4,
	0xe5,
	0xe6,
	0xe7,
	0xe8,
	0xe9,
	0xea,
	0xf1,
	0xf2,
	0xf3,
	0xf4,
	0xf5,
	0xf6,
	0xf7,
	0xf8,
	0xf9,
	0xfa,
]

const std_dc_chroma_bits = [0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0]
const std_dc_chroma_vals = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

const std_ac_chroma_bits = [0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 0x77]
const std_ac_chroma_vals = [
	0x00,
	0x01,
	0x02,
	0x03,
	0x11,
	0x04,
	0x05,
	0x21,
	0x31,
	0x06,
	0x12,
	0x41,
	0x51,
	0x07,
	0x61,
	0x71,
	0x13,
	0x22,
	0x32,
	0x81,
	0x08,
	0x14,
	0x42,
	0x91,
	0xa1,
	0xb1,
	0xc1,
	0x09,
	0x23,
	0x33,
	0x52,
	0xf0,
	0x15,
	0x62,
	0x72,
	0xd1,
	0x0a,
	0x16,
	0x24,
	0x34,
	0xe1,
	0x25,
	0xf1,
	0x17,
	0x18,
	0x19,
	0x1a,
	0x26,
	0x27,
	0x28,
	0x29,
	0x2a,
	0x35,
	0x36,
	0x37,
	0x38,
	0x39,
	0x3a,
	0x43,
	0x44,
	0x45,
	0x46,
	0x47,
	0x48,
	0x49,
	0x4a,
	0x53,
	0x54,
	0x55,
	0x56,
	0x57,
	0x58,
	0x59,
	0x5a,
	0x63,
	0x64,
	0x65,
	0x66,
	0x67,
	0x68,
	0x69,
	0x6a,
	0x73,
	0x74,
	0x75,
	0x76,
	0x77,
	0x78,
	0x79,
	0x7a,
	0x82,
	0x83,
	0x84,
	0x85,
	0x86,
	0x87,
	0x88,
	0x89,
	0x8a,
	0x92,
	0x93,
	0x94,
	0x95,
	0x96,
	0x97,
	0x98,
	0x99,
	0x9a,
	0xa2,
	0xa3,
	0xa4,
	0xa5,
	0xa6,
	0xa7,
	0xa8,
	0xa9,
	0xaa,
	0xb2,
	0xb3,
	0xb4,
	0xb5,
	0xb6,
	0xb7,
	0xb8,
	0xb9,
	0xba,
	0xc2,
	0xc3,
	0xc4,
	0xc5,
	0xc6,
	0xc7,
	0xc8,
	0xc9,
	0xca,
	0xd2,
	0xd3,
	0xd4,
	0xd5,
	0xd6,
	0xd7,
	0xd8,
	0xd9,
	0xda,
	0xe2,
	0xe3,
	0xe4,
	0xe5,
	0xe6,
	0xe7,
	0xe8,
	0xe9,
	0xea,
	0xf2,
	0xf3,
	0xf4,
	0xf5,
	0xf6,
	0xf7,
	0xf8,
	0xf9,
	0xfa,
]

// ============================================================================
// DATA STRUCTURES
// ============================================================================

struct QuantTable {
mut:
	set    bool
	values [64]int
}

struct HuffTable {
mut:
	set      bool
	vals     []int
	max_code [16]int
	min_code [16]int
	val_ptr  [16]int
	// P9: first-level decode table, 512 entries indexed by the next 9
	// bits. Entry 0 means "longer than 9 bits / invalid" (fallback loop);
	// otherwise (code_length << 8) | symbol.
	lookup []int
}

struct Component {
mut:
	id        int
	h_samp    int
	v_samp    int
	quant_idx int
	dc_table  int
	ac_table  int
	pred      int
	eob_run   int
	// P3: coefficients are a single flat arena-backed []i32 instead of the
	// old nested [][][64]int. Block (bx, by) starts at
	// (by * blocks_w + bx) * 64. blocks_w / blocks_h record the grid so
	// readers never have to chase nested slice headers.
	blocks_w int
	blocks_h int
	coeff    []i32
}

// ============================================================================
// ARENA (P1)
//
// A single owner for every scratch allocation the decoder makes, so that
// one free_all() at the end of decode() guarantees zero leaks. The
// returned RGBA buffer is allocated OUTSIDE the arena and owned by the
// caller.
//
// Three typed backing buffers with independent bump cursors:
//   small  - lives for the whole decode. Holds Huffman `vals` arrays,
//            which are created before the first SOF (std tables) and
//            must survive any later frame-buffer realloc.
//   frame  - (re)allocated at every SOF. Holds the YCC tables, the
//            flattened coefficients and all render scratch. A later SOF
//            frees the previous frame: once a new frame header arrives
//            the prior frame's pixels are dead.
//   bytes  - lives for the whole decode. Arithmetic-coder statistics,
//            the fixed binary symbol and (from P5) the 8-bit scale LUT.
//
// alloc_* bump the cursor and hand back a shared sub-slice (no copy).
// ============================================================================

struct Arena {
mut:
	small     []int
	small_pos int
	frame     []int
	frame_pos int
	coeff     []i32
	coeff_pos int
	i16       []i16
	i16_pos   int
	bytes     []u8
	bytes_pos int
}

// alloc_small returns a shared zero-initialised sub-slice of the
// whole-decode int region (Huffman vals).
fn (mut a Arena) alloc_small(n int) ![]int {
	if n <= 0 {
		return []int{}
	}
	if a.small_pos + n > a.small.len {
		return error('arena: small region exhausted (${a.small_pos + n} > ${a.small.len})')
	}
	res := unsafe { a.small[a.small_pos..a.small_pos + n] }
	a.small_pos += n
	return res
}

// alloc_frame returns a shared zero-initialised sub-slice of the current
// frame int region (YCC tables / render scratch).
fn (mut a Arena) alloc_frame(n int) ![]int {
	if n <= 0 {
		return []int{}
	}
	if a.frame_pos + n > a.frame.len {
		return error('arena: frame region exhausted (${a.frame_pos + n} > ${a.frame.len})')
	}
	res := unsafe { a.frame[a.frame_pos..a.frame_pos + n] }
	a.frame_pos += n
	return res
}

// alloc_coeff returns a shared zero-initialised sub-slice of the
// per-frame i32 coefficient region (P3: 4-byte coeffs halve the memory
// of the old []int coeff).
fn (mut a Arena) alloc_coeff(n int) ![]i32 {
	if n <= 0 {
		return []i32{}
	}
	if a.coeff_pos + n > a.coeff.len {
		return error('arena: coeff region exhausted (${a.coeff_pos + n} > ${a.coeff.len})')
	}
	res := unsafe { a.coeff[a.coeff_pos..a.coeff_pos + n] }
	a.coeff_pos += n
	return res
}

// alloc_bytes returns a shared zero-initialised sub-slice of the
// whole-decode byte region (arith stats / LUT).
fn (mut a Arena) alloc_bytes(n int) ![]u8 {
	if n <= 0 {
		return []u8{}
	}
	if a.bytes_pos + n > a.bytes.len {
		return error('arena: byte region exhausted (${a.bytes_pos + n} > ${a.bytes.len})')
	}
	res := unsafe { a.bytes[a.bytes_pos..a.bytes_pos + n] }
	a.bytes_pos += n
	return res
}

// alloc_i16 returns a shared zero-initialised sub-slice of the
// per-frame i16 region (P5: sample planes / upsample scratch / cs).
// Sample values fit 0..4095 (12-bit) so i16 storage is safe; all
// intermediate arithmetic is done in int and cast back on store.
fn (mut a Arena) alloc_i16(n int) ![]i16 {
	if n <= 0 {
		return []i16{}
	}
	if a.i16_pos + n > a.i16.len {
		return error('arena: i16 region exhausted (${a.i16_pos + n} > ${a.i16.len})')
	}
	res := unsafe { a.i16[a.i16_pos..a.i16_pos + n] }
	a.i16_pos += n
	return res
}

// reset_frame drops the previous frame + coeff + i16 backings and starts
// fresh ones of the given capacities. Called at every SOF; the prior
// frame is dead.
fn (mut a Arena) reset_frame(int_cap int, coeff_cap int, i16_cap int) {
	unsafe {
		a.frame.free()
		a.coeff.free()
		a.i16.free()
	}
	a.frame = []int{len: int_cap}
	a.frame_pos = 0
	a.coeff = []i32{len: coeff_cap}
	a.coeff_pos = 0
	a.i16 = []i16{len: i16_cap}
	a.i16_pos = 0
}

// free_all releases every backing buffer exactly once. Called on both the
// success and the error path of decode().
fn (mut a Arena) free_all() {
	unsafe {
		a.small.free()
		a.frame.free()
		a.coeff.free()
		a.i16.free()
		a.bytes.free()
	}
	a.small = []int{}
	a.frame = []int{}
	a.coeff = []i32{}
	a.i16 = []i16{}
	a.bytes = []u8{}
	a.small_pos = 0
	a.frame_pos = 0
	a.coeff_pos = 0
	a.i16_pos = 0
	a.bytes_pos = 0
}

struct JpegDecoder {
mut:
	data             []u8
	pos              int
	bit_buff         u64
	bit_count        int
	width            int
	height           int
	ch_count         int
	components       []Component
	q_tables         [4]QuantTable
	dc_huff          [4]HuffTable
	ac_huff          [4]HuffTable
	max_h_samp       int
	max_v_samp       int
	mcu_width        int
	mcu_height       int
	is_prog          bool
	arithmetic       bool
	restart_interval int
	precision        int
	max_sample       int
	center           int
	idct_pass1_bits  int
	adobe_transform  int
	cr_r_tab         []int
	cb_b_tab         []int
	cr_g_tab         []int
	cb_g_tab         []int
	// P5: 4096-entry scale LUT (v -> 8-bit) replacing the float scale8.
	scale_lut []u8
	// P1: every scratch buffer above is carved out of this arena.
	arena Arena
	// Arithmetic (Annex H) state
	arith_c          u32
	arith_a          u32
	arith_ct         int
	arith_unread     int
	arith_fixed_bin  []u8
	arith_dc_stats   [16][]u8
	arith_ac_stats   [16][]u8
	arith_last_dc    [4]int
	arith_dc_context [4]int
	arith_mode       string
	arith_scan_ss    int
	arith_scan_ah    int
	arith_dc_l       [16]int
	arith_dc_u       [16]int
	arith_ac_k       [16]int
}

// ============================================================================
// HUFFMAN TABLE BUILD
// ============================================================================

// build populates the table from a 16-entry code-length histogram and its
// symbol list. `vals` is a caller-allocated (arena) slice of length
// symbols.len; the old local `sizes`/`codes` scratch arrays are gone:
// min/max code and the value pointer are derived directly from `counts`,
// since the codes for a length are a contiguous run starting at the
// running `code`.
fn (mut t HuffTable) build(counts []int, symbols []int, mut vals []int, mut lut []int) {
	t.set = true
	for i, s in symbols {
		vals[i] = s
	}
	t.vals = vals
	t.lookup = lut
	mut code := 0
	mut j := 0
	for i in 0 .. 16 {
		if counts[i] == 0 {
			t.max_code[i] = -1
			t.min_code[i] = 0
			t.val_ptr[i] = j
		} else {
			t.val_ptr[i] = j
			t.min_code[i] = code
			t.max_code[i] = code + counts[i] - 1
			j += counts[i]
		}
		code = (code + counts[i]) << 1
	}
	// P9: first-level LUT for all codes up to 9 bits. Each code is
	// replicated over the 9-length low-bit suffixes so any 9-bit peek
	// that starts with the code hits the entry.
	for i in 0 .. 9 {
		if counts[i] == 0 {
			continue
		}
		length := i + 1
		bits := 9 - length
		rep := 1 << bits
		for c in t.min_code[i] .. t.max_code[i] + 1 {
			sym := t.vals[t.val_ptr[i] + (c - t.min_code[i])] & 0xFF
			entry := (length << 8) | sym
			base := c << bits
			for r in 0 .. rep {
				lut[base + r] = entry
			}
		}
	}
}

// ============================================================================
// BIT READER
// ============================================================================

fn (mut d JpegDecoder) read_byte() !int {
	if d.pos >= d.data.len {
		return error('Unexpected EOF in read_byte')
	}
	b := d.data[d.pos]
	d.pos++
	return int(b)
}

fn (mut d JpegDecoder) read_word() !int {
	hi := d.read_byte()!
	lo := d.read_byte()!
	return ((hi << 8) & 0xFFFF) | lo
}

fn (mut d JpegDecoder) skip_bytes(count int) {
	d.pos += count
}

fn (mut d JpegDecoder) align() {
	d.bit_buff = 0
	d.bit_count = 0
}

// fill tops the bit buffer up toward 64 bits. The buffer is MSB-aligned:
// valid bits sit at the top (bit 63 = next to read), free space at the
// bottom. New bytes are OR-ed into the free space just below the valid
// bits (never shifted in from the left, which would push buffered bits
// out of the u64). FF00 is consumed as a literal 0xFF data byte; a
// marker byte (FF followed by a non-zero byte) is NOT consumed - d.pos
// is left on the FF so the post-scan main loop still sees EOI/RST.
fn (mut d JpegDecoder) fill() ! {
	for d.bit_count <= 56 {
		if d.pos >= d.data.len {
			return
		}
		b := d.data[d.pos]
		if b == 0xFF {
			if d.pos + 1 >= d.data.len {
				return
			}
			nb := d.data[d.pos + 1]
			if nb != 0x00 {
				return // marker: leave d.pos on the FF
			}
			d.pos += 2
			d.bit_buff |= u64(0xFF) << (56 - d.bit_count)
			d.bit_count += 8
		} else {
			d.pos += 1
			d.bit_buff |= u64(b) << (56 - d.bit_count)
			d.bit_count += 8
		}
	}
}

// ensure guarantees at least n bits are buffered. If a marker or EOF
// stops fill short of n bits, the entropy stream is desynced and we
// report it (matching the old per-bit error behaviour).
fn (mut d JpegDecoder) ensure(n int) ! {
	if d.bit_count < n {
		d.fill()!
		if d.bit_count < n {
			if d.pos < d.data.len && d.data[d.pos] == 0xFF {
				nb := if d.pos + 1 < d.data.len { d.data[d.pos + 1] } else { u8(0) }
				return error('Unexpected marker FF${nb:02X} in entropy stream')
			}
			return error('Unexpected EOF in entropy stream')
		}
	}
}

// peek_bits returns the next n bits (MSB-first) without consuming them.
// The caller must have ensured n bits are available.
fn (d &JpegDecoder) peek_bits(n int) int {
	return int((d.bit_buff >> (64 - n)) & ((u64(1) << n) - 1))
}

// drop_bits consumes the next n bits (shifts the MSB-aligned buffer).
fn (mut d JpegDecoder) drop_bits(n int) {
	d.bit_buff <<= n
	d.bit_count -= n
}

fn (mut d JpegDecoder) get_bit() !int {
	d.ensure(1)!
	bit := (d.bit_buff >> 63) & 1
	d.drop_bits(1)
	return int(bit)
}

fn (mut d JpegDecoder) read_bits(n int) !int {
	if n <= 0 {
		return 0
	}
	d.ensure(n)!
	val := int((d.bit_buff >> (64 - n)) & ((u64(1) << n) - 1))
	d.drop_bits(n)
	return val
}

// ============================================================================
// TABLE INIT
// ============================================================================

fn (mut d JpegDecoder) install_std_huff_tables() ! {
	mut t0 := d.dc_huff[0]
	mut v0 := d.arena.alloc_small(std_dc_luma_vals.len)!
	mut l0 := d.arena.alloc_small(512)!
	t0.build(std_dc_luma_bits, std_dc_luma_vals, mut v0, mut l0)
	d.dc_huff[0] = t0
	mut t1 := d.ac_huff[0]
	mut v1 := d.arena.alloc_small(std_ac_luma_vals.len)!
	mut l1 := d.arena.alloc_small(512)!
	t1.build(std_ac_luma_bits, std_ac_luma_vals, mut v1, mut l1)
	d.ac_huff[0] = t1
	mut t2 := d.dc_huff[1]
	mut v2 := d.arena.alloc_small(std_dc_chroma_vals.len)!
	mut l2 := d.arena.alloc_small(512)!
	t2.build(std_dc_chroma_bits, std_dc_chroma_vals, mut v2, mut l2)
	d.dc_huff[1] = t2
	mut t3 := d.ac_huff[1]
	mut v3 := d.arena.alloc_small(std_ac_chroma_vals.len)!
	mut l3 := d.arena.alloc_small(512)!
	t3.build(std_ac_chroma_bits, std_ac_chroma_vals, mut v3, mut l3)
	d.ac_huff[1] = t3
}

fn (mut d JpegDecoder) build_ycc_tables(max_sample int) ! {
	center := (max_sample + 1) / 2
	size := max_sample + 1
	mut cr_r := d.arena.alloc_frame(size)!
	mut cb_b := d.arena.alloc_frame(size)!
	mut cr_g := d.arena.alloc_frame(size)!
	mut cb_g := d.arena.alloc_frame(size)!
	for i in 0 .. size {
		x := i - center
		cr_r[i] = (ycc_fix_r * x + ycc_one_half) >> ycc_scalebits
		cb_b[i] = (ycc_fix_b * x + ycc_one_half) >> ycc_scalebits
		cr_g[i] = (-ycc_fix_g2) * x
		cb_g[i] = (-ycc_fix_g1) * x + ycc_one_half
	}
	d.cr_r_tab = cr_r
	d.cb_b_tab = cb_b
	d.cr_g_tab = cr_g
	d.cb_g_tab = cb_g
}

fn (mut d JpegDecoder) init_arith_tables() ! {
	mut fb := d.arena.alloc_bytes(1)!
	fb[0] = 113
	d.arith_fixed_bin = fb
	mut dcs := [16][]u8{}
	for i in 0 .. num_arith_tbls {
		dcs[i] = d.arena.alloc_bytes(dc_stat_bins)!
	}
	d.arith_dc_stats = dcs
	mut acs := [16][]u8{}
	for i in 0 .. num_arith_tbls {
		acs[i] = d.arena.alloc_bytes(ac_stat_bins)!
	}
	d.arith_ac_stats = acs
	mut dl := [16]int{}
	mut du := [16]int{}
	mut ak := [16]int{}
	for i in 0 .. num_arith_tbls {
		dl[i] = 0
		du[i] = 1
		ak[i] = 5
	}
	d.arith_dc_l = dl
	d.arith_dc_u = du
	d.arith_ac_k = ak
}

// ============================================================================
// SEGMENT PARSERS
// ============================================================================

fn (mut d JpegDecoder) parse_sof(marker int) ! {
	d.is_prog = marker == marker_sof2 || marker == marker_sof10
	d.arithmetic = marker == marker_sof9 || marker == marker_sof10
	d.skip_bytes(2)
	precision := d.read_byte()!
	if precision != 8 && precision != 12 {
		return error('Unsupported JPEG precision: ${precision}-bit')
	}
	d.precision = precision
	d.max_sample = (1 << precision) - 1
	d.center = 1 << (precision - 1)
	if precision == 8 {
		d.idct_pass1_bits = 2
	} else {
		d.idct_pass1_bits = 1
	}
	d.height = d.read_word()!
	d.width = d.read_word()!
	if d.width <= 0 || d.height <= 0 || d.width > max_int / d.height / 4 {
		return error('JPEG dimensions exceed the supported buffer size')
	}
	d.ch_count = d.read_byte()!
	if d.ch_count < 1 || d.ch_count > 4 {
		return error('Unsupported component count: ${d.ch_count}')
	}
	mut comps := []Component{len: d.ch_count}
	for i in 0 .. d.ch_count {
		comp_id := d.read_byte()!
		sampling := d.read_byte()!
		quant_idx := d.read_byte()!
		mut c := Component{}
		c.id = comp_id
		c.h_samp = (sampling >> 4) & 0x0F
		c.v_samp = sampling & 0x0F
		c.quant_idx = quant_idx
		if c.h_samp < 1 || c.v_samp < 1 {
			return error('Invalid sampling factor for component ${comp_id}: ${c.h_samp}x${c.v_samp}')
		}
		comps[i] = c
	}
	mut max_h := 0
	mut max_v := 0
	for c in comps {
		if c.h_samp > max_h {
			max_h = c.h_samp
		}
		if c.v_samp > max_v {
			max_v = c.v_samp
		}
	}
	d.max_h_samp = max_h
	d.max_v_samp = max_v
	d.mcu_width = max_h * 8
	d.mcu_height = max_v * 8
	mcus_x := (d.width + d.mcu_width - 1) / d.mcu_width
	mcus_y := (d.height + d.mcu_height - 1) / d.mcu_height
	// Size the per-frame arena from the frame geometry (plan 3.1), then
	// drop the previous frame (dead) and lay out: YCC tables, then one
	// flat coeff buffer per component. Render scratch is bump-allocated
	// from the same frame later, after coeff is placed.
	mut coeff_total := i64(0)
	mut render_need := i64(0)
	mut cs_max := 0
	for c in comps {
		bw := mcus_x * c.h_samp
		bh := mcus_y * c.v_samp
		coeff_total += i64(bw) * bh * 64
		aw := (d.width * c.h_samp + max_h - 1) / max_h
		ah := (d.height * c.v_samp + max_v - 1) / max_v
		render_need += i64(bw) * 8 * bh * 8 + i64(aw) * ah
		hf := max_h / c.h_samp
		vf := max_v / c.v_samp
		render_need += i64(aw) * hf * ah * vf
		if aw > cs_max {
			cs_max = aw
		}
	}
	ycc_need := 4 * (d.max_sample + 1)
	// P5: render scratch (planes / crops / upsampled / cs) moves to the
	// i16 region; the int frame region now only holds the YCC tables.
	int_cap := ycc_need + 128
	i16_cap := render_need + cs_max + 128
	if coeff_total > max_int / int(sizeof(i32))
		|| i16_cap > max_int / int(sizeof(i16)) {
		return error('JPEG frame exceeds the supported buffer size')
	}
	d.arena.reset_frame(int_cap, int(coeff_total), int(i16_cap))
	d.build_ycc_tables(d.max_sample)!
	for i in 0 .. d.ch_count {
		mut c := comps[i]
		c.blocks_w = mcus_x * c.h_samp
		c.blocks_h = mcus_y * c.v_samp
		c.coeff = d.arena.alloc_coeff(c.blocks_w * c.blocks_h * 64)!
		comps[i] = c
	}
	d.components = comps
}

fn (mut d JpegDecoder) parse_dqt() ! {
	length := d.read_word()!
	end_pos := d.pos + length - 2
	for d.pos < end_pos {
		info := d.read_byte()!
		table_id := info & 0x0F
		pq := (info >> 4) & 0x0F
		if table_id >= 4 {
			return error('Invalid quantization table ID: ${table_id}')
		}
		if pq != 0 && pq != 1 {
			return error('Invalid quantization table precision: ${pq}')
		}
		mut qt := d.q_tables[table_id]
		qt.set = true
		for i in 0 .. 64 {
			mut value := 0
			if pq == 1 {
				value = d.read_word()!
			} else {
				value = d.read_byte()!
			}
			qt.values[zigzag_order[i]] = value
		}
		d.q_tables[table_id] = qt
	}
}

fn (mut d JpegDecoder) parse_dht() ! {
	length := d.read_word()!
	end_pos := d.pos + length - 2
	for d.pos < end_pos {
		info := d.read_byte()!
		table_class := (info >> 4) & 0x0F
		table_id := info & 0x0F
		if table_class == 2 || table_class == 3 {
			return error('arithmetic DHT class not used; use DAC')
		}
		if table_id >= 4 {
			return error('Invalid Huffman table ID: ${table_id}')
		}
		mut counts := d.arena.alloc_small(16)!
		mut total := 0
		for i in 0 .. 16 {
			counts[i] = d.read_byte()!
			total += counts[i]
		}
		mut symbols := d.arena.alloc_small(total)!
		mut si := 0
		for i in 0 .. 16 {
			for _ in 0 .. counts[i] {
				symbols[si] = d.read_byte()!
				si++
			}
		}
		mut vv := d.arena.alloc_small(total)!
		mut lut := d.arena.alloc_small(512)!
		if table_class == 0 {
			mut t := d.dc_huff[table_id]
			t.build(counts, symbols, mut vv, mut lut)
			d.dc_huff[table_id] = t
		} else {
			mut t := d.ac_huff[table_id]
			t.build(counts, symbols, mut vv, mut lut)
			d.ac_huff[table_id] = t
		}
	}
}

fn (mut d JpegDecoder) parse_dac() ! {
	length := d.read_word()!
	mut len := length - 2
	for len > 0 {
		index := d.read_byte()!
		val := d.read_byte()!
		len -= 2
		if index < 0 || index >= 2 * num_arith_tbls {
			return error('Invalid DAC index: ${index}')
		}
		if index >= num_arith_tbls {
			d.arith_ac_k[index - num_arith_tbls] = val
		} else {
			d.arith_dc_l[index] = val & 0x0F
			d.arith_dc_u[index] = (val >> 4) & 0x0F
			if d.arith_dc_l[index] > d.arith_dc_u[index] {
				return error('Invalid DAC value: L > U (${val})')
			}
		}
	}
}

fn (mut d JpegDecoder) parse_adobe_transform() ! {
	length := d.read_word()!
	if length < 2 {
		return error('Invalid APP14 segment length: ${length}')
	}
	data_len := length - 2
	start := d.pos
	mut end := start + data_len
	if end > d.data.len {
		end = d.data.len
	}
	// Read-only view into the input buffer; no clone (L7). The bytes are
	// consumed within this call, so sharing the backing is safe.
	data := unsafe { d.data[start..end] }
	d.skip_bytes(data_len)
	if data.len >= 12 && data[0] == u8(`A`) && data[1] == u8(`d`) && data[2] == u8(`o`)
		&& data[3] == u8(`b`) && data[4] == u8(`e`) && data[5] == 0 {
		d.adobe_transform = int(data[11])
	} else {
		d.adobe_transform = -1
	}
}

fn (mut d JpegDecoder) parse_sos() ! {
	d.skip_bytes(2)
	comp_count := d.read_byte()!
	mut scan_comps := d.arena.alloc_small(comp_count)!
	mut sc_n := 0
	for _ in 0 .. comp_count {
		comp_id := d.read_byte()!
		tables := d.read_byte()!
		for idx, c in d.components {
			if c.id == comp_id {
				mut cc := d.components[idx]
				cc.dc_table = (tables >> 4) & 0x0F
				cc.ac_table = tables & 0x0F
				d.components[idx] = cc
				scan_comps[sc_n] = idx
				sc_n++
				break
			}
		}
	}
	ss := d.read_byte()!
	se := d.read_byte()!
	approx := d.read_byte()!
	ah := (approx >> 4) & 0x0F
	al := approx & 0x0F
	d.align()
	if d.mcu_width == 0 || d.mcu_height == 0 {
		return error('Invalid MCU dimensions')
	}
	interleaved := scan_comps.len > 1
	mut mcus_x := 0
	mut mcus_y := 0
	if interleaved {
		mcus_x = (d.width + d.mcu_width - 1) / d.mcu_width
		mcus_y = (d.height + d.mcu_height - 1) / d.mcu_height
	} else {
		c := d.components[scan_comps[0]]
		mcus_x = (d.width * c.h_samp + d.max_h_samp * 8 - 1) / (d.max_h_samp * 8)
		mcus_y = (d.height * c.v_samp + d.max_v_samp * 8 - 1) / (d.max_v_samp * 8)
	}
	if d.arithmetic {
		d.decode_scan_arithmetic(scan_comps, ss, se, ah, al, interleaved, mcus_x, mcus_y)
		return
	}
	for i in 0 .. d.components.len {
		mut cc := d.components[i]
		cc.pred = 0
		cc.eob_run = 0
		d.components[i] = cc
	}
	mut mcu_count := 0
	for my in 0 .. mcus_y {
		for mx in 0 .. mcus_x {
			if d.restart_interval > 0 && mcu_count > 0 && mcu_count % d.restart_interval == 0 {
				d.handle_restart(scan_comps)!
			}
			if !interleaved {
				d.decode_block(scan_comps[0], mx, my, ss, se, ah, al)!
			} else {
				for c_idx in scan_comps {
					c := d.components[c_idx]
					for v in 0 .. c.v_samp {
						by := my * c.v_samp + v
						for h in 0 .. c.h_samp {
							bx := mx * c.h_samp + h
							d.decode_block(c_idx, bx, by, ss, se, ah, al)!
						}
					}
				}
			}
			mcu_count++
		}
	}
	d.align()
}

fn (mut d JpegDecoder) handle_restart(scan_comps []int) ! {
	d.align()
	mut marker := d.read_byte()!
	for marker == 0xFF {
		marker = d.read_byte()!
	}
	if !(marker_rst0 <= marker && marker <= marker_rst7) {
		return error('Expected RST marker, found FF${marker:02X}')
	}
	for c_idx in scan_comps {
		mut c := d.components[c_idx]
		c.pred = 0
		c.eob_run = 0
		d.components[c_idx] = c
	}
}

// ============================================================================
// HUFFMAN DECODE
// ============================================================================

fn (mut d JpegDecoder) huff_decode(table &HuffTable) !int {
	if !table.set {
		return error('Huffman table not set')
	}
	// P9: fast path - one 9-bit peek into the first-level table. Only
	// taken when 9 bits are already buffered, so end-of-scan short
	// buffers fall through to the exact per-length loop below.
	if d.bit_count >= 9 {
		e := unsafe { table.lookup[int((d.bit_buff >> 55) & 511)] }
		if e != 0 {
			d.drop_bits(e >> 8)
			return e & 0xFF
		}
	}
	// One fill per symbol: the wide buffer holds >= 57 bits, enough for
	// the 16-bit peek window, so the per-length loop is just shifts and
	// compares (no per-bit byte read / stuffing check / error hop).
	d.fill()!
	mut code := u32(0)
	for length in 0 .. 16 {
		d.ensure(length + 1)!
		code = (code << 1) | u32((d.bit_buff >> (63 - length)) & 1)
		if int(code) <= table.max_code[length] {
			d.drop_bits(length + 1)
			idx := table.val_ptr[length] + (int(code) - table.min_code[length])
			return unsafe { table.vals[idx] }
		}
	}
	return error('Invalid Huffman code - no match found')
}

fn (mut d JpegDecoder) receive_extend(category int) !int {
	if category == 0 {
		return 0
	}
	value := d.read_bits(category)!
	threshold := 1 << (category - 1)
	if value < threshold {
		return value - (1 << category) + 1
	}
	return value
}

fn (mut d JpegDecoder) decode_block(ci int, bx int, by int, ss int, se int, ah int, al int) ! {
	comp := d.components[ci]
	if by >= comp.blocks_h || bx >= comp.blocks_w {
		return
	}
	// P3: write coefficients in place into the flat arena buffer. `coeff`
	// is a slice-header copy that shares the backing, so indexed writes go
	// straight to the component's storage - no [64]int copy in, no
	// 512-byte write-back out. pred/eob are cached in locals and flushed
	// to the component at every exit.
	base := (by * comp.blocks_w + bx) * 64
	mut coeff := unsafe { &comp.coeff[0] }
	mut pred := comp.pred
	mut eob := comp.eob_run
	if ss == 0 {
		if ah == 0 {
			diff_category := d.huff_decode(&d.dc_huff[comp.dc_table])!
			// P10: inlined receive_extend (saves a call + ensure hop per
			// block; semantics identical).
			mut diff := 0
			if diff_category != 0 {
				if d.bit_count < diff_category {
					d.ensure(diff_category)!
				}
				mut v := int((d.bit_buff >> (64 - diff_category)) & ((u64(1) << diff_category) - 1))
				d.drop_bits(diff_category)
				if v < (1 << (diff_category - 1)) {
					v -= (1 << diff_category) - 1
				}
				diff = v
			}
			pred += diff
			unsafe { coeff[base + 0] = pred * (1 << al) }
		} else {
			if d.get_bit()! == 1 {
				unsafe { coeff[base + 0] |= (1 << al) }
			}
		}
		if se == 0 {
			d.components[ci].pred = pred
			d.components[ci].eob_run = eob
			return
		}
	}
	p1 := 1 << al
	m1 := -p1
	if ah == 0 {
		if eob > 0 {
			eob -= 1
			d.components[ci].pred = pred
			d.components[ci].eob_run = eob
			return
		}
		mut k := if ss > 0 { ss } else { 1 }
		for k <= se {
			symbol := d.huff_decode(&d.ac_huff[comp.ac_table])!
			r := symbol >> 4
			s := symbol & 0x0F
			if s != 0 {
				k += r
				if k > 63 {
					break
				}
				if d.bit_count < s {
					d.ensure(s)!
				}
				mut value := int((d.bit_buff >> (64 - s)) & ((u64(1) << s) - 1))
				d.drop_bits(s)
				if value < (1 << (s - 1)) {
					value -= (1 << s) - 1
				}
				unsafe { coeff[base + zigzag_order[k]] = value * (1 << al) }
				k++
			} else if r != 15 {
				eob_run := (1 << r) + d.read_bits(r)!
				eob = eob_run - 1
				break
			} else {
				k += 16
			}
		}
	} else {
		mut k := ss
		if eob == 0 {
			for k <= se {
				symbol := d.huff_decode(&d.ac_huff[comp.ac_table])!
				mut r := symbol >> 4
				mut s := symbol & 0x0F
				if s != 0 {
					if d.get_bit()! == 1 {
						s = p1
					} else {
						s = m1
					}
				} else if r != 15 {
					eob = (1 << r) + d.read_bits(r)!
					break
				}
				for k <= se {
					pos := base + zigzag_order[k]
					coef := unsafe { coeff[pos] }
					if coef != 0 {
						if d.get_bit()! == 1 && (coef & p1) == 0 {
							unsafe { coeff[pos] = coef + (if coef > 0 { p1 } else { m1 }) }
						}
					} else {
						r -= 1
						if r < 0 {
							break
						}
					}
					k++
				}
				if s != 0 && k <= se {
					unsafe { coeff[base + zigzag_order[k]] = s }
				}
				k++
			}
		}
		if eob > 0 {
			for k <= se {
				pos := base + zigzag_order[k]
				coef := unsafe { coeff[pos] }
				if coef != 0 {
					if d.get_bit()! == 1 && (coef & p1) == 0 {
						unsafe { coeff[pos] = coef + (if coef > 0 { p1 } else { m1 }) }
					}
				}
				k++
			}
			eob -= 1
		}
	}
	d.components[ci].pred = pred
	d.components[ci].eob_run = eob
}

// ============================================================================
// ANNEX H ARITHMETIC DECODING
// ============================================================================

fn (mut d JpegDecoder) arith_get_byte() int {
	if d.pos >= d.data.len {
		return 0xD9
	}
	b := d.data[d.pos]
	d.pos++
	return int(b)
}

fn (mut d JpegDecoder) arith_decode(mut stats []u8, idx int) int {
	for d.arith_a < 0x8000 {
		d.arith_ct -= 1
		if d.arith_ct < 0 {
			mut data := 0
			if d.arith_unread != 0 {
				data = 0
			} else {
				data = d.arith_get_byte()
				if data == 0xFF {
					for {
						data = d.arith_get_byte()
						if data != 0xFF {
							break
						}
					}
					if data == 0 {
						data = 0xFF
					} else {
						d.arith_unread = data
						data = 0
					}
				}
			}
			d.arith_c = ((d.arith_c << 8) | u32(data)) & 0xFFFFFFFF
			d.arith_ct += 8
			if d.arith_ct < 0 {
				d.arith_ct += 1
				if d.arith_ct == 0 {
					d.arith_a = 0x8000
				}
			}
		}
		d.arith_a <<= 1
	}
	mut sv := stats[idx]
	mut qe := arith_tab[int(sv & 0x7f)]
	nl := qe & 0xFF
	qe >>= 8
	nm := qe & 0xFF
	qe >>= 8
	mut temp := d.arith_a - qe
	d.arith_a = temp
	temp <<= d.arith_ct
	if d.arith_c >= temp {
		d.arith_c -= temp
		if d.arith_a < qe {
			d.arith_a = qe
			stats[idx] = u8((sv & 0x80) ^ u8(nm))
		} else {
			d.arith_a = qe
			stats[idx] = u8((sv & 0x80) ^ u8(nl))
			sv ^= 0x80
		}
	} else if d.arith_a < 0x8000 {
		if d.arith_a < qe {
			stats[idx] = u8((sv & 0x80) ^ u8(nl))
			sv ^= 0x80
		} else {
			stats[idx] = u8((sv & 0x80) ^ u8(nm))
		}
	}
	return int(sv >> 7)
}

// zero_u8 clears a stats slice in place. Used instead of re-allocating a
// fresh []u8 per pass/restart (L6): the arena owns the backing, we just
// reset the bytes.
fn zero_u8(mut s []u8) {
	for i in 0 .. s.len {
		s[i] = 0
	}
}

fn (mut d JpegDecoder) arith_process_restart(scan_comps []int) {
	mut marker := 0
	if d.arith_unread != 0 {
		marker = d.arith_unread
		d.arith_unread = 0
	} else {
		marker = d.read_byte() or { 0 }
		for marker == 0xFF {
			marker = d.read_byte() or { 0 }
		}
	}
	for ci, c_idx in scan_comps {
		comp := d.components[c_idx]
		if !d.is_prog || (d.arith_scan_ss == 0 && d.arith_scan_ah == 0) {
			mut s := d.arith_dc_stats[comp.dc_table]
			zero_u8(mut s)
			d.arith_last_dc[ci] = 0
			d.arith_dc_context[ci] = 0
		}
		if !d.is_prog || d.arith_scan_ss != 0 {
			mut s := d.arith_ac_stats[comp.ac_table]
			zero_u8(mut s)
		}
	}
	d.arith_c = 0
	d.arith_a = 0
	d.arith_ct = -16
}

fn (mut d JpegDecoder) arith_start_pass(scan_comps []int, ss int, _se int, ah int, _al int) ! {
	d.arith_scan_ss = ss
	d.arith_scan_ah = ah
	if d.is_prog {
		if ah == 0 {
			d.arith_mode = if ss == 0 { 'dc_first' } else { 'ac_first' }
		} else {
			d.arith_mode = if ss == 0 { 'dc_refine' } else { 'ac_refine' }
		}
	} else {
		d.arith_mode = 'seq'
	}
	for ci, c_idx in scan_comps {
		comp := d.components[c_idx]
		if !d.is_prog || (ss == 0 && ah == 0) {
			tbl := comp.dc_table
			if tbl < 0 || tbl >= num_arith_tbls {
				return error('Invalid arithmetic DC table: ${tbl}')
			}
			mut s := d.arith_dc_stats[tbl]
			zero_u8(mut s)
		}
		if !d.is_prog || ss != 0 {
			tbl := comp.ac_table
			if tbl < 0 || tbl >= num_arith_tbls {
				return error('Invalid arithmetic AC table: ${tbl}')
			}
			mut s := d.arith_ac_stats[tbl]
			zero_u8(mut s)
		}
	}
	for i in 0 .. 4 {
		d.arith_last_dc[i] = 0
		d.arith_dc_context[i] = 0
	}
	d.arith_c = 0
	d.arith_a = 0
	d.arith_ct = -16
}

fn (mut d JpegDecoder) arith_dc_diff(comp_idx int, ci int) {
	tbl := d.components[comp_idx].dc_table
	mut stats := d.arith_dc_stats[tbl]
	ctx := d.arith_dc_context[ci]
	if d.arith_decode(mut stats, ctx) == 0 {
		d.arith_dc_context[ci] = 0
		return
	}
	sign := d.arith_decode(mut stats, ctx + 1)
	mut off := ctx + 2 + sign
	mut m := d.arith_decode(mut stats, off)
	if m != 0 {
		off = 20
		for d.arith_decode(mut stats, off) != 0 {
			m <<= 1
			if m == 0x8000 {
				d.arith_ct = -1
				return
			}
			off += 1
		}
	}
	if m < ((1 << d.arith_dc_l[tbl]) >> 1) {
		d.arith_dc_context[ci] = 0
	} else if m > ((1 << d.arith_dc_u[tbl]) >> 1) {
		d.arith_dc_context[ci] = 12 + (sign * 4)
	} else {
		d.arith_dc_context[ci] = 4 + (sign * 4)
	}
	mut v := m
	off += 14
	mut mm := m >> 1
	for mm != 0 {
		if d.arith_decode(mut stats, off) != 0 {
			v |= mm
		}
		mm >>= 1
	}
	v += 1
	if sign != 0 {
		v = -v
	}
	d.arith_last_dc[ci] = (d.arith_last_dc[ci] + v) & 0xffff
}

fn (mut d JpegDecoder) arith_decode_seq_block(comp_idx int, ci int, bx int, by int) {
	comp := d.components[comp_idx]
	if by >= comp.blocks_h || bx >= comp.blocks_w {
		return
	}
	// Flat in-place writes (P3): `coeff` shares the component's arena
	// backing, so no per-block copy in/out and no write-back.
	cbase := (by * comp.blocks_w + bx) * 64
	mut coeff := unsafe { comp.coeff }
	d.arith_dc_diff(comp_idx, ci)
	mut dc := d.arith_last_dc[ci]
	if dc >= 0x8000 {
		dc -= 0x10000
	}
	coeff[cbase + 0] = dc
	tbl := comp.ac_table
	mut stats := d.arith_ac_stats[tbl]
	mut k := 1
	for k <= 63 {
		mut base := 3 * (k - 1)
		if d.arith_decode(mut stats, base) != 0 {
			break
		}
		for d.arith_decode(mut stats, base + 1) == 0 {
			base += 3
			k += 1
			if k > 63 {
				d.arith_ct = -1
				return
			}
		}
		mut fb := unsafe { d.arith_fixed_bin }
		sign := d.arith_decode(mut fb, 0)
		mut off := base + 2
		mut m := d.arith_decode(mut stats, off)
		if m != 0 {
			if d.arith_decode(mut stats, off) != 0 {
				m <<= 1
				off = if k <= d.arith_ac_k[tbl] { 189 } else { 217 }
				for d.arith_decode(mut stats, off) != 0 {
					m <<= 1
					if m == 0x8000 {
						d.arith_ct = -1
						return
					}
					off += 1
				}
			}
		}
		mut v := m
		off += 14
		mut mm := m >> 1
		for mm != 0 {
			if d.arith_decode(mut stats, off) != 0 {
				v |= mm
			}
			mm >>= 1
		}
		v += 1
		if sign != 0 {
			v = -v
		}
		coeff[cbase + zigzag_order[k]] = v
		k += 1
	}
}

fn (mut d JpegDecoder) arith_decode_dc_first_block(comp_idx int, ci int, bx int, by int, al int) {
	comp := d.components[comp_idx]
	if by >= comp.blocks_h || bx >= comp.blocks_w {
		return
	}
	cbase := (by * comp.blocks_w + bx) * 64
	mut coeff := unsafe { comp.coeff }
	d.arith_dc_diff(comp_idx, ci)
	mut dc := (d.arith_last_dc[ci] << al) & 0xffff
	if dc >= 0x8000 {
		dc -= 0x10000
	}
	coeff[cbase + 0] = dc
}

fn (mut d JpegDecoder) arith_decode_ac_first_block(comp_idx int, _ci int, bx int, by int, ss int, se int, al int) {
	comp := d.components[comp_idx]
	if by >= comp.blocks_h || bx >= comp.blocks_w {
		return
	}
	cbase := (by * comp.blocks_w + bx) * 64
	mut coeff := unsafe { comp.coeff }
	tbl := comp.ac_table
	mut stats := d.arith_ac_stats[tbl]
	mut k := ss
	for k <= se {
		mut base := 3 * (k - 1)
		if d.arith_decode(mut stats, base) != 0 {
			break
		}
		for d.arith_decode(mut stats, base + 1) == 0 {
			base += 3
			k += 1
			if k > se {
				d.arith_ct = -1
				return
			}
		}
		mut fb := unsafe { d.arith_fixed_bin }
		sign := d.arith_decode(mut fb, 0)
		mut off := base + 2
		mut m := d.arith_decode(mut stats, off)
		if m != 0 {
			if d.arith_decode(mut stats, off) != 0 {
				m <<= 1
				off = if k <= d.arith_ac_k[tbl] { 189 } else { 217 }
				for d.arith_decode(mut stats, off) != 0 {
					m <<= 1
					if m == 0x8000 {
						d.arith_ct = -1
						return
					}
					off += 1
				}
			}
		}
		mut v := m
		off += 14
		mut mm := m >> 1
		for mm != 0 {
			if d.arith_decode(mut stats, off) != 0 {
				v |= mm
			}
			mm >>= 1
		}
		v += 1
		if sign != 0 {
			v = -v
		}
		mut val := (v * (1 << al)) & 0xffff
		if val >= 0x8000 {
			val -= 0x10000
		}
		coeff[cbase + zigzag_order[k]] = val
		k += 1
	}
}

fn (mut d JpegDecoder) arith_decode_dc_refine_block(comp_idx int, _ci int, bx int, by int, al int) {
	comp := d.components[comp_idx]
	if by >= comp.blocks_h || bx >= comp.blocks_w {
		return
	}
	cbase := (by * comp.blocks_w + bx) * 64
	mut coeff := unsafe { comp.coeff }
	p1 := 1 << al
	mut fb := unsafe { d.arith_fixed_bin }
	if d.arith_decode(mut fb, 0) != 0 {
		coeff[cbase + 0] |= p1
	}
}

fn (mut d JpegDecoder) arith_decode_ac_refine_block(comp_idx int, _ci int, bx int, by int, ss int, se int, al int) {
	comp := d.components[comp_idx]
	if by >= comp.blocks_h || bx >= comp.blocks_w {
		return
	}
	cbase := (by * comp.blocks_w + bx) * 64
	mut coeff := unsafe { comp.coeff }
	tbl := comp.ac_table
	mut stats := d.arith_ac_stats[tbl]
	p1 := 1 << al
	m1 := (-1) << al
	mut kex := se
	for kex > 0 {
		if coeff[cbase + zigzag_order[kex]] != 0 {
			break
		}
		kex -= 1
	}
	mut k := ss
	for k <= se {
		mut base := 3 * (k - 1)
		if k > kex {
			if d.arith_decode(mut stats, base) != 0 {
				break
			}
		}
		for {
			pos := cbase + zigzag_order[k]
			coef := coeff[pos]
			if coef != 0 {
				if d.arith_decode(mut stats, base + 2) != 0 {
					if coef < 0 {
						coeff[pos] = coef + m1
					} else {
						coeff[pos] = coef + p1
					}
				}
				break
			}
			if d.arith_decode(mut stats, base + 1) != 0 {
				mut fb := unsafe { d.arith_fixed_bin }
				if d.arith_decode(mut fb, 0) != 0 {
					coeff[pos] = m1
				} else {
					coeff[pos] = p1
				}
				break
			}
			base += 3
			k += 1
			if k > se {
				d.arith_ct = -1
				return
			}
		}
		k += 1
	}
}

fn (mut d JpegDecoder) arith_decode_block(comp_idx int, ci int, bx int, by int, ss int, se int, _ah int, al int) {
	mode := d.arith_mode
	if mode == 'seq' {
		d.arith_decode_seq_block(comp_idx, ci, bx, by)
	} else if mode == 'dc_first' {
		d.arith_decode_dc_first_block(comp_idx, ci, bx, by, al)
	} else if mode == 'ac_first' {
		d.arith_decode_ac_first_block(comp_idx, ci, bx, by, ss, se, al)
	} else if mode == 'dc_refine' {
		d.arith_decode_dc_refine_block(comp_idx, ci, bx, by, al)
	} else if mode == 'ac_refine' {
		d.arith_decode_ac_refine_block(comp_idx, ci, bx, by, ss, se, al)
	}
}

fn (mut d JpegDecoder) decode_scan_arithmetic(scan_comps []int, ss int, se int, ah int, al int, interleaved bool, mcus_x int, mcus_y int) {
	d.arith_start_pass(scan_comps, ss, se, ah, al) or { return }
	mut mcu_count := 0
	for my in 0 .. mcus_y {
		for mx in 0 .. mcus_x {
			if d.restart_interval > 0 && mcu_count > 0 && mcu_count % d.restart_interval == 0 {
				d.arith_process_restart(scan_comps)
			}
			if d.arith_ct == -1 {
				return
			}
			if !interleaved {
				d.arith_decode_block(scan_comps[0], 0, mx, my, ss, se, ah, al)
			} else {
				for ci in 0 .. scan_comps.len {
					c_idx := scan_comps[ci]
					c := d.components[c_idx]
					for v in 0 .. c.v_samp {
						by := my * c.v_samp + v
						for h in 0 .. c.h_samp {
							bx := mx * c.h_samp + h
							d.arith_decode_block(c_idx, ci, bx, by, ss, se, ah, al)
						}
					}
				}
			}
			mcu_count++
		}
	}
	d.align()
}

// ============================================================================
// IDCT (ISLOW)
// ============================================================================

fn idct_descale(x int, n int) int {
	return (x + (1 << (n - 1))) >> n
}

// idct_block_to_plane runs the separable ISLOW IDCT for block (bx, by)
// and writes the level-shifted, clamped samples directly into `pp`
// (P4: no by-value [64]int return, no separate clamp loop in render).
// The +center level shift and the [0, max_sample] clamp are fused into
// the second-pass output stage. M1: `pp` is the raw plane pointer so
// the same routine can run inside a spawned band worker.
fn (d &JpegDecoder) idct_block_to_plane(comp &Component, bx int, by int, pp &i16,
	padded_w int) {
	if by >= comp.blocks_h || bx >= comp.blocks_w {
		return
	}
	center := d.center
	maxs := d.max_sample
	// Flat coeff (P3): block (bx, by) lives at cbase in the component's
	// []i32; every read below is cbase + (row*8 + col).
	cbase := (by * comp.blocks_w + bx) * 64
	coeffs := comp.coeff
	q := d.q_tables[comp.quant_idx].values
	pass1_bits := d.idct_pass1_bits
	// P10: whole-block DC-only shortcut. When every AC coefficient is
	// zero the two per-pass shortcuts below would collapse anyway: each
	// column yields dc = (c0*q0) << pass1_bits and each row descales it
	// once. Compute that value directly and fill the 8x8 region.
	mut dc_only := true
	for k in 1 .. 64 {
		if coeffs[cbase + k] != 0 {
			dc_only = false
			break
		}
	}
	if dc_only {
		v := clamp8(idct_descale((coeffs[cbase] * q[0]) << pass1_bits,
			pass1_bits + 3) + center, maxs)
		unsafe {
			for yy in 0 .. 8 {
				rw := (by * 8 + yy) * padded_w + bx * 8
				for xx in 0 .. 8 {
					pp[rw + xx] = v
				}
			}
		}
		return
	}
	mut ws := [8][8]int{}
	for u in 0 .. 8 {
		if coeffs[cbase + 1 * 8 + u] == 0 && coeffs[cbase + 2 * 8 + u] == 0 && coeffs[cbase + 3 * 8 + u] == 0
			&& coeffs[cbase + 4 * 8 + u] == 0 && coeffs[cbase + 5 * 8 + u] == 0 && coeffs[cbase + 6 * 8 + u] == 0
			&& coeffs[cbase + 7 * 8 + u] == 0 {
			dcval := (coeffs[cbase + 0 * 8 + u] * q[0 * 8 + u]) << pass1_bits
			for y in 0 .. 8 {
				ws[y][u] = dcval
			}
			continue
		}
		mut z2 := coeffs[cbase + 2 * 8 + u] * q[2 * 8 + u]
		mut z3 := coeffs[cbase + 6 * 8 + u] * q[6 * 8 + u]
		mut z1 := (z2 + z3) * idct_fix_0_541196100
		mut tmp2 := z1 + z3 * (-idct_fix_1_847759065)
		mut tmp3 := z1 + z2 * idct_fix_0_765366865
		z2 = coeffs[cbase + 0 * 8 + u] * q[0 * 8 + u]
		z3 = coeffs[cbase + 4 * 8 + u] * q[4 * 8 + u]
		mut tmp0 := (z2 + z3) << idct_const_bits
		mut tmp1 := (z2 - z3) << idct_const_bits
		tmp10 := tmp0 + tmp3
		tmp13 := tmp0 - tmp3
		tmp11 := tmp1 + tmp2
		tmp12 := tmp1 - tmp2
		tmp0 = coeffs[cbase + 7 * 8 + u] * q[7 * 8 + u]
		tmp1 = coeffs[cbase + 5 * 8 + u] * q[5 * 8 + u]
		tmp2 = coeffs[cbase + 3 * 8 + u] * q[3 * 8 + u]
		tmp3 = coeffs[cbase + 1 * 8 + u] * q[1 * 8 + u]
		z1 = tmp0 + tmp3
		z2 = tmp1 + tmp2
		mut z3v := tmp0 + tmp2
		mut z4 := tmp1 + tmp3
		z5 := (z3v + z4) * idct_fix_1_175875602
		tmp0 = tmp0 * idct_fix_0_298631336
		tmp1 = tmp1 * idct_fix_2_053119869
		tmp2 = tmp2 * idct_fix_3_072711026
		tmp3 = tmp3 * idct_fix_1_501321110
		z1 = z1 * (-idct_fix_0_899976223)
		z2 = z2 * (-idct_fix_2_562915447)
		z3v = z3v * (-idct_fix_1_961570560)
		z4 = z4 * (-idct_fix_0_390180644)
		z3v += z5
		z4 += z5
		tmp0 += z1 + z3v
		tmp1 += z2 + z4
		tmp2 += z2 + z3v
		tmp3 += z1 + z4
		shift := idct_const_bits - pass1_bits
		ws[0][u] = idct_descale(tmp10 + tmp3, shift)
		ws[7][u] = idct_descale(tmp10 - tmp3, shift)
		ws[1][u] = idct_descale(tmp11 + tmp2, shift)
		ws[6][u] = idct_descale(tmp11 - tmp2, shift)
		ws[2][u] = idct_descale(tmp12 + tmp1, shift)
		ws[5][u] = idct_descale(tmp12 - tmp1, shift)
		ws[3][u] = idct_descale(tmp13 + tmp0, shift)
		ws[4][u] = idct_descale(tmp13 - tmp0, shift)
	}
	base_x := bx * 8
	base_y := by * 8
	for y in 0 .. 8 {
		wptr := ws[y]
		row := (base_y + y) * padded_w + base_x
		if wptr[1] == 0 && wptr[2] == 0 && wptr[3] == 0 && wptr[4] == 0
			&& wptr[5] == 0 && wptr[6] == 0 && wptr[7] == 0 {
			mut dcval := idct_descale(wptr[0], pass1_bits + 3) + center
			if dcval < 0 {
				dcval = 0
			}
			if dcval > maxs {
				dcval = maxs
			}
			unsafe {
				for x in 0 .. 8 {
					pp[row + x] = i16(dcval)
				}
			}
			continue
		}
		mut z2 := wptr[2]
		mut z3 := wptr[6]
		mut z1 := (z2 + z3) * idct_fix_0_541196100
		mut tmp2 := z1 + z3 * (-idct_fix_1_847759065)
		mut tmp3 := z1 + z2 * idct_fix_0_765366865
		mut tmp0 := (wptr[0] + wptr[4]) << idct_const_bits
		mut tmp1 := (wptr[0] - wptr[4]) << idct_const_bits
		tmp10 := tmp0 + tmp3
		tmp13 := tmp0 - tmp3
		tmp11 := tmp1 + tmp2
		tmp12 := tmp1 - tmp2
		tmp0 = wptr[7]
		tmp1 = wptr[5]
		tmp2 = wptr[3]
		tmp3 = wptr[1]
		z1 = tmp0 + tmp3
		z2 = tmp1 + tmp2
		mut z3v := tmp0 + tmp2
		mut z4 := tmp1 + tmp3
		z5 := (z3v + z4) * idct_fix_1_175875602
		tmp0 = tmp0 * idct_fix_0_298631336
		tmp1 = tmp1 * idct_fix_2_053119869
		tmp2 = tmp2 * idct_fix_3_072711026
		tmp3 = tmp3 * idct_fix_1_501321110
		z1 = z1 * (-idct_fix_0_899976223)
		z2 = z2 * (-idct_fix_2_562915447)
		z3v = z3v * (-idct_fix_1_961570560)
		z4 = z4 * (-idct_fix_0_390180644)
		z3v += z5
		z4 += z5
		tmp0 += z1 + z3v
		tmp1 += z2 + z4
		tmp2 += z2 + z3v
		tmp3 += z1 + z4
		shift := idct_const_bits + pass1_bits + 3
		// P6: row = (by*8+y)*padded_w + bx*8 with bx<blocks_w,
		// by<blocks_h and plane sized padded_w*padded_h, so row+0..7 are
		// always in range; elide the bounds checks.
		unsafe {
			pp[row + 0] = i16(clamp8(idct_descale(tmp10 + tmp3, shift) + center, maxs))
			pp[row + 7] = i16(clamp8(idct_descale(tmp10 - tmp3, shift) + center, maxs))
			pp[row + 1] = i16(clamp8(idct_descale(tmp11 + tmp2, shift) + center, maxs))
			pp[row + 6] = i16(clamp8(idct_descale(tmp11 - tmp2, shift) + center, maxs))
			pp[row + 2] = i16(clamp8(idct_descale(tmp12 + tmp1, shift) + center, maxs))
			pp[row + 5] = i16(clamp8(idct_descale(tmp12 - tmp1, shift) + center, maxs))
			pp[row + 3] = i16(clamp8(idct_descale(tmp13 + tmp0, shift) + center, maxs))
			pp[row + 4] = i16(clamp8(idct_descale(tmp13 - tmp0, shift) + center, maxs))
		}
	}
}

// clamp8 clamps v into [0, maxs].
fn clamp8(v int, maxs int) int {
	if v < 0 {
		return 0
	}
	if v > maxs {
		return maxs
	}
	return v
}

// ============================================================================
// RENDER BANDS (M1)
// ============================================================================

// render_yield parks the current thread briefly while the main thread
// waits for band workers. Same C-interop level the sync vlib itself
// uses for threads/atomics; available on nix and mingw.
$if windows {
	fn C.Sleep(ms u32)

	fn render_yield() {
		C.Sleep(0)
	}
} $else {
	fn C.sched_yield() int

	fn render_yield() {
		C.sched_yield()
	}
}

// barrier_wait blocks until the stage completion counter reaches
// `expect`. Workers bump it atomically (seq_cst, so their plane writes
// are visible to the main thread once the bump is observed). No V
// channel is used: V never frees a chan, which would leak a block per
// render stage. The counter is a raw heap cell because passing
// `&stack_local` into a spawn makes V heap-escape it with no way to
// free; the main thread frees it right after barrier_wait returns.
fn new_barrier_counter() &u64 {
	return &u64(unsafe { vcalloc_noscan(sizeof(u64)) })
}

fn free_barrier_counter(cnt &u64) {
	unsafe { free(cnt) }
}

fn barrier_wait(cnt &u64, expect u64) {
	// Bounded busy-spin first (the common case: bands finish within a
	// few microseconds of each other), then fall back to yielding so a
	// main thread on a busy core never starves a worker.
	for _ in 0 .. 4096 {
		if stdatomic.load_u64(cnt) >= expect {
			return
		}
	}
	for stdatomic.load_u64(cnt) < expect {
		render_yield()
	}
}

// band_range returns the half-open row range [y0, y1) of band k when
// `rows` rows are split into `n` contiguous bands of equal size (last
// band may be short). Bands are disjoint, so they can run in parallel.
fn band_range(rows int, k int, n int) (int, int) {
	mut size := (rows + n - 1) / n
	if size < 1 {
		size = 1
	}
	y0 := k * size
	if y0 >= rows {
		return rows, rows
	}
	mut y1 := y0 + size
	if y1 > rows {
		y1 = rows
	}
	return y0, y1
}

// render_threads returns the worker count for the parallel render
// stages: min(cpus, 4), forced to 1 (serial path) for small images.
// The JPG_THREADS env escape hatch overrides everything: 0 forces the
// serial path, a positive value forces that many workers even on small
// images (used by the tests to exercise the parallel bands on small
// fixtures).
fn render_threads(out_w int, out_h int) int {
	env := os.getenv('JPG_THREADS')
	if env.len > 0 {
		mut v := env.int()
		if v < 1 {
			return 1
		}
		if v > 4 {
			v = 4
		}
		return v
	}
	if out_w * out_h < 128 * 128 {
		return 1
	}
	mut n := runtime.nr_cpus()
	if n > 4 {
		n = 4
	}
	if n < 1 {
		n = 1
	}
	return n
}

// idct_band runs the IDCT for block rows [by0, by1) of one component
// and fuses the matching padding-crop rows (the rows this band just
// wrote). Bands are disjoint in both planes, so bands are race-free and
// the result is identical to the old whole-plane IDCT + whole-plane
// crop pass.
fn idct_band(d &JpegDecoder, comp Component, plane_ptr &i16, crop_ptr &i16,
	padded_w int, actual_w int, actual_h int, by0 int, by1 int) {
	for by in by0 .. by1 {
		for bx in 0 .. comp.blocks_w {
			d.idct_block_to_plane(&comp, bx, by, plane_ptr, padded_w)
		}
		y0 := by * 8
		mut y1 := y0 + 8
		if y1 > actual_h {
			y1 = actual_h
		}
		unsafe {
			for y in y0 .. y1 {
				for x in 0 .. actual_w {
					crop_ptr[y * actual_w + x] = plane_ptr[y * padded_w + x]
				}
			}
		}
	}
}

// idct_worker is the spawned form of idct_band: same work, plus the
// atomic completion bump on the stage barrier counter.
fn idct_worker(d &JpegDecoder, comp Component, plane_ptr &i16, crop_ptr &i16,
	padded_w int, actual_w int, actual_h int, by0 int, by1 int, cnt &u64) {
	idct_band(d, comp, plane_ptr, crop_ptr, padded_w, actual_w, actual_h, by0, by1)
	stdatomic.add_u64(cnt, 1)
}

// ============================================================================
// FANCY UPSAMPLING
// ============================================================================

// upsample_2x2_left / upsample_2x2_right are the horizontal combine for
// the left/right output pixel of source column c in the 2x2 triangle
// filter. Shared by upsample_plane and the fused h2v2 color path (M2)
// so both are bit-identical by construction. [inline] matters: the
// fused color loop calls them once per output pixel.
@[inline]
fn upsample_2x2_left(cs_c int, cs_prev int, first bool) int {
	if first {
		return (cs_c * 4 + 8) >> 4
	}
	return (cs_c * 3 + cs_prev + 8) >> 4
}

@[inline]
fn upsample_2x2_right(cs_c int, cs_next int, last bool) int {
	if last {
		return (cs_c * 4 + 7) >> 4
	}
	return (cs_c * 3 + cs_next + 7) >> 4
}

// upsample_plane writes the scaled plane into the caller-provided
// `result` buffer (arena-backed). The 2x2 path's `cs` scratch row is
// supplied by the caller and reused across rows instead of being
// allocated per row (L3).
fn upsample_plane(plane []i16, src_w int, src_h int, mut result []i16, dst_w int, dst_h int,
	scale_x int, scale_y int, mut cs []i16) {
	// P5: planes are i16 storage, but every intermediate multiply must be
	// done in int (V would otherwise infer the literals as i16 and wrap).
	// Reads are widened with int(); only the final store casts back to i16.
	// P6: all result/cs/plane indices are derived from the consistent
	// src/dst dimensions the caller passes, so they are provably in range;
	// the whole body runs without bounds checks.
	unsafe {
		mut rp := &result[0]
		mut cp := &cs[0]
		if scale_x == 2 && scale_y == 2 {
			for y in 0 .. dst_h {
				cy := y / 2
				mut adj := if y % 2 == 0 { cy - 1 } else { cy + 1 }
				if adj < 0 {
					adj = 0
				}
				if adj > src_h - 1 {
					adj = src_h - 1
				}
				for c in 0 .. src_w {
					cp[c] = i16(3 * int(plane[cy * src_w + c]) + int(plane[adj * src_w + c]))
				}
				base := y * dst_w
				rp[base] = i16(upsample_2x2_left(int(cp[0]), 0, true))
				rp[base + 1] = i16(upsample_2x2_right(int(cp[0]), int(cp[1]), false))
				for c in 1 .. src_w - 1 {
					rp[base + 2 * c] = i16(upsample_2x2_left(int(cp[c]), int(cp[c - 1]), false))
					rp[base + 2 * c + 1] = i16(upsample_2x2_right(int(cp[c]), int(cp[c + 1]),
						c == src_w - 1))
				}
				rp[base + dst_w - 2] = i16(upsample_2x2_left(int(cp[src_w - 1]),
					int(cp[src_w - 2]), false))
				rp[base + dst_w - 1] = i16(upsample_2x2_right(int(cp[src_w - 1]), 0, true))
			}
		} else if scale_x == 2 && scale_y == 1 {
			for y in 0 .. src_h {
				base := y * dst_w
				rp[base] = plane[y * src_w]
				rp[base + 1] = i16((int(plane[y * src_w]) * 3 + int(plane[y * src_w + 1]) + 2) >> 2)
				for c in 1 .. src_w - 1 {
					rp[base + 2 * c] = i16((int(plane[y * src_w + c]) * 3 + int(plane[y * src_w + c - 1]) + 1) >> 2)
					rp[base + 2 * c + 1] = i16((int(plane[y * src_w + c]) * 3 + int(plane[y * src_w + c + 1]) + 2) >> 2)
				}
				rp[base + dst_w - 2] = i16((int(plane[y * src_w + src_w - 1]) * 3 + int(plane[y * src_w + src_w - 2]) + 1) >> 2)
				rp[base + dst_w - 1] = plane[y * src_w + src_w - 1]
			}
		} else if scale_x == 1 && scale_y == 2 {
			for y in 0 .. dst_h {
				cy := y / 2
				even := y % 2 == 0
				mut adj := if even { cy - 1 } else { cy + 1 }
				if adj < 0 {
					adj = 0
				}
				if adj > src_h - 1 {
					adj = src_h - 1
				}
				bias := if even { 1 } else { 2 }
				for x in 0 .. src_w {
					rp[y * dst_w + x] = i16((3 * int(plane[cy * src_w + x]) + int(plane[adj * src_w + x]) + bias) >> 2)
				}
			}
		} else {
			for y in 0 .. dst_h {
				for x in 0 .. dst_w {
					mut sx := x / scale_x
					mut sy := y / scale_y
					if sx > src_w - 1 {
						sx = src_w - 1
					}
					if sy > src_h - 1 {
						sy = src_h - 1
					}
					rp[y * dst_w + x] = plane[sy * src_w + sx]
				}
			}
		}
	}
}

// ============================================================================
// COLOR CONVERSION
// ============================================================================

fn (d &JpegDecoder) ycbcr_to_rgb(y int, cb int, cr int) (int, int, int) {
	mut r := y + d.cr_r_tab[cr]
	mut g := y + ((d.cb_g_tab[cb] + d.cr_g_tab[cr]) >> ycc_scalebits)
	mut b := y + d.cb_b_tab[cb]
	if r < 0 {
		r = 0
	}
	if r > d.max_sample {
		r = d.max_sample
	}
	if g < 0 {
		g = 0
	}
	if g > d.max_sample {
		g = d.max_sample
	}
	if b < 0 {
		b = 0
	}
	if b > d.max_sample {
		b = d.max_sample
	}
	return r, g, b
}

fn (d &JpegDecoder) ycck_to_rgb(y int, cb int, cr int, k int) (int, int, int) {
	x := cb - 128
	z := cr - 128
	cr_r := (ycc_fix_r * z + ycc_one_half) >> ycc_scalebits
	cb_b := (ycc_fix_b * x + ycc_one_half) >> ycc_scalebits
	cr_g := (-ycc_fix_g2) * z
	cb_g := (-ycc_fix_g1) * x + ycc_one_half
	mut c := 255 - (y + cr_r)
	mut m := 255 - (y + ((cb_g + cr_g) >> ycc_scalebits))
	mut yy := 255 - (y + cb_b)
	if c < 0 {
		c = 0
	}
	if c > 255 {
		c = 255
	}
	if m < 0 {
		m = 0
	}
	if m > 255 {
		m = 255
	}
	if yy < 0 {
		yy = 0
	}
	if yy > 255 {
		yy = 255
	}
	r := int(f64(c) * f64(k) / 255.0 + 0.5)
	g := int(f64(m) * f64(k) / 255.0 + 0.5)
	b := int(f64(yy) * f64(k) / 255.0 + 0.5)
	return r, g, b
}

fn scale8(v int, max_sample int) int {
	if max_sample == 255 {
		return v
	}
	mut s := int(f64(v) * 255.0 / f64(max_sample) + 0.5)
	if s < 0 {
		s = 0
	}
	if s > 255 {
		s = 255
	}
	return s
}

// ============================================================================
// COLOR ROW BANDS (M1)
// ============================================================================

// gray_rows writes output rows [y0, y1) of the grayscale path into the
// caller's p32 buffer. Bands are disjoint in p32.
fn gray_rows(d &JpegDecoder, p32 &u32, yp &i16, lp &u8, yw int, out_w int,
	y0 int, y1 int) {
	_ = d
	unsafe {
		for y in y0 .. y1 {
			for x in 0 .. out_w {
				yv := u32(lp[yp[y * yw + x]])
				p32[y * out_w + x] = yv | (yv << 8) | (yv << 16) | 0xFF000000
			}
		}
	}
}

// gray_worker is the spawned form of gray_rows.
fn gray_worker(d &JpegDecoder, p32 &u32, yp &i16, lp &u8, yw int, out_w int,
	y0 int, y1 int, cnt &u64) {
	gray_rows(d, p32, yp, lp, yw, out_w, y0, y1)
	stdatomic.add_u64(cnt, 1)
}

// cmyk_rows writes output rows [y0, y1) of the CMYK/YCCK path.
fn cmyk_rows(d &JpegDecoder, p32 &u32, p0 &i16, p1 &i16, p2 &i16, p3 &i16,
	w0 int, w1 int, w2 int, w3 int, out_w int, y0 int, y1 int) {
	unsafe {
		for y in y0 .. y1 {
			for x in 0 .. out_w {
				s0 := p0[y * w0 + x]
				s1 := p1[y * w1 + x]
				s2 := p2[y * w2 + x]
				s3 := p3[y * w3 + x]
				mut r := 0
				mut g := 0
				mut b := 0
				if d.adobe_transform == 2 {
					r, g, b = d.ycck_to_rgb(s0, s1, s2, s3)
				} else {
					r = int(f64(s0) * f64(s3) / 255.0 + 0.5)
					g = int(f64(s1) * f64(s3) / 255.0 + 0.5)
					b = int(f64(s2) * f64(s3) / 255.0 + 0.5)
				}
				p32[y * out_w + x] = u32(r) | (u32(g) << 8) | (u32(b) << 16) | 0xFF000000
			}
		}
	}
}

// cmyk_worker is the spawned form of cmyk_rows.
fn cmyk_worker(d &JpegDecoder, p32 &u32, p0 &i16, p1 &i16, p2 &i16, p3 &i16,
	w0 int, w1 int, w2 int, w3 int, out_w int, y0 int, y1 int, cnt &u64) {
	cmyk_rows(d, p32, p0, p1, p2, p3, w0, w1, w2, w3, out_w, y0, y1)
	stdatomic.add_u64(cnt, 1)
}

// ycbcr_rows writes output rows [y0, y1) of the 3-component YCbCr path.
// The YCC tables on `d` are read-only here, so bands are race-free.
fn ycbcr_rows(d &JpegDecoder, p32 &u32, yp &i16, cbp &i16, crp &i16, lp &u8,
	yw int, cb_w int, cr_w int, out_w int, ms int, y0 int, y1 int) {
	unsafe {
		cr_r := &d.cr_r_tab[0]
		cb_g := &d.cb_g_tab[0]
		cr_g := &d.cr_g_tab[0]
		cb_b := &d.cb_b_tab[0]
		for y in y0 .. y1 {
			yoff := y * yw
			cboff := y * cb_w
			croff := y * cr_w
			poff := y * out_w
			for x in 0 .. out_w {
				yv := int(yp[yoff + x])
				cbv := int(cbp[cboff + x])
				crv := int(crp[croff + x])
				mut r := yv + cr_r[crv]
				mut g := yv + ((cb_g[cbv] + cr_g[crv]) >> ycc_scalebits)
				mut b := yv + cb_b[cbv]
				if r < 0 {
					r = 0
				} else if r > ms {
					r = ms
				}
				if g < 0 {
					g = 0
				} else if g > ms {
					g = ms
				}
				if b < 0 {
					b = 0
				} else if b > ms {
					b = ms
				}
				p32[poff + x] = u32(lp[r]) | (u32(lp[g]) << 8) | (u32(lp[b]) << 16) | 0xFF000000
			}
		}
	}
}

// ycbcr_worker is the spawned form of ycbcr_rows.
fn ycbcr_worker(d &JpegDecoder, p32 &u32, yp &i16, cbp &i16, crp &i16, lp &u8,
	yw int, cb_w int, cr_w int, out_w int, ms int, y0 int, y1 int, cnt &u64) {
	ycbcr_rows(d, p32, yp, cbp, crp, lp, yw, cb_w, cr_w, out_w, ms, y0, y1)
	stdatomic.add_u64(cnt, 1)
}

// ycc_store converts one (y, cb, cr) triple to clamped RGBA and writes
// it to p32[pi]. [inline]: called once per output pixel by the fused
// h2v2 color loop.
@[inline]
fn ycc_store(p32 &u32, pi int, yv int, cbv int, crv int, lp &u8, ms int,
	cr_r &int, cb_g &int, cr_g &int, cb_b &int) {
	unsafe {
		mut r := yv + cr_r[crv]
		mut g := yv + ((cb_g[cbv] + cr_g[crv]) >> ycc_scalebits)
		mut b := yv + cb_b[cbv]
		if r < 0 {
			r = 0
		} else if r > ms {
			r = ms
		}
		if g < 0 {
			g = 0
		} else if g > ms {
			g = ms
		}
		if b < 0 {
			b = 0
		} else if b > ms {
			b = ms
		}
		p32[pi] = u32(lp[r]) | (u32(lp[g]) << 8) | (u32(lp[b]) << 16) | 0xFF000000
	}
}

// ycbcr_rows_fused converts output rows [y0, y1) for the common h2v2
// (4:2:0) case, computing the 2x2 triangle chroma filter inline per
// row instead of materializing upsampled chroma planes first (M2).
// cb/cr are the cropped source planes (width cw, height ch); the
// per-pair arithmetic goes through upsample_2x2_left/right, so the
// result is bit-identical to upsample_plane + ycbcr_rows. out_w is
// always <= 2*cw (chroma covers the output), so full pairs need no
// bounds guards and only an odd out_w leaves one leftover pixel.
fn ycbcr_rows_fused(d &JpegDecoder, p32 &u32, yp &i16, cb &i16, cr &i16, lp &u8,
	yw int, cw int, ch int, out_w int, ms int, y0 int, y1 int) {
	unsafe {
		cr_r := &d.cr_r_tab[0]
		cb_g := &d.cb_g_tab[0]
		cr_g := &d.cr_g_tab[0]
		cb_b := &d.cb_b_tab[0]
		full := out_w >> 1
		for y in y0 .. y1 {
			cy := y >> 1
			mut adj := if y & 1 == 0 { cy - 1 } else { cy + 1 }
			if adj < 0 {
				adj = 0
			}
			if adj > ch - 1 {
				adj = ch - 1
			}
			crow := cy * cw
			arow := adj * cw
			yoff := y * yw
			poff := y * out_w
			mut cb_p := 0
			mut cr_p := 0
			mut cb_c := 3 * int(cb[crow]) + int(cb[arow])
			mut cr_c := 3 * int(cr[crow]) + int(cr[arow])
			for c in 0 .. full {
				mut cb_n := cb_c
				mut cr_n := cr_c
				if c + 1 < cw {
					cb_n = 3 * int(cb[crow + c + 1]) + int(cb[arow + c + 1])
					cr_n = 3 * int(cr[crow + c + 1]) + int(cr[arow + c + 1])
				}
				x := 2 * c
				cbv := upsample_2x2_left(cb_c, cb_p, c == 0)
				crv := upsample_2x2_left(cr_c, cr_p, c == 0)
				ycc_store(p32, poff + x, int(yp[yoff + x]), cbv, crv, lp, ms, cr_r, cb_g,
					cr_g, cb_b)
				cbv = upsample_2x2_right(cb_c, cb_n, c == cw - 1)
				crv = upsample_2x2_right(cr_c, cr_n, c == cw - 1)
				ycc_store(p32, poff + x + 1, int(yp[yoff + x + 1]), cbv, crv, lp, ms, cr_r,
					cb_g, cr_g, cb_b)
				cb_p = cb_c
				cb_c = cb_n
				cr_p = cr_c
				cr_c = cr_n
			}
			if out_w & 1 == 1 {
				// Odd output width: the last output pixel is the left
				// pixel of chroma pair `full` (full < cw).
				cbv := upsample_2x2_left(cb_c, cb_p, full == 0)
				crv := upsample_2x2_left(cr_c, cr_p, full == 0)
				ycc_store(p32, poff + out_w - 1, int(yp[yoff + out_w - 1]), cbv, crv, lp,
					ms, cr_r, cb_g, cr_g, cb_b)
			}
		}
	}
}

// ycbcr_worker_fused is the spawned form of ycbcr_rows_fused.
fn ycbcr_worker_fused(d &JpegDecoder, p32 &u32, yp &i16, cb &i16, cr &i16, lp &u8,
	yw int, cw int, ch int, out_w int, ms int, y0 int, y1 int, cnt &u64) {
	ycbcr_rows_fused(d, p32, yp, cb, cr, lp, yw, cw, ch, out_w, ms, y0, y1)
	stdatomic.add_u64(cnt, 1)
}

// ============================================================================
// RENDER
// ============================================================================

fn (mut d JpegDecoder) render() !common.Image {
	comps := d.components
	max_h := d.max_h_samp
	max_v := d.max_v_samp
	// P5: build the 4096-entry scale LUT once (identity for 8-bit) so
	// the per-pixel scale8 float is replaced by a lookup.
	mut lut := d.arena.alloc_bytes(4096)!
	ms := d.max_sample
	for v in 0 .. 4096 {
		mut s := scale8(v, ms)
		if s > 255 {
			s = 255
		}
		lut[v] = u8(s)
	}
	d.scale_lut = lut
	out_w := d.width
	out_h := d.height
	// M1: worker count for the two parallel render stages (1 = serial).
	nthreads := render_threads(out_w, out_h)
	dptr := &d
	// Fixed stack arrays for the per-component plane handles / dims, so
	// the containers themselves never hit the heap (L2).
	mut planes := [4][]i16{}
	mut crops := [4][]i16{}
	mut dims := [8]int{}
	mut padded_ws := [4]int{}
	// M1: every buffer a worker may touch is allocated here on the main
	// thread before any spawn; workers only write into these ranges.
	for i in 0 .. d.ch_count {
		comp := comps[i]
		padded_w := comp.blocks_w * 8
		padded_h := comp.blocks_h * 8
		actual_w := (d.width * comp.h_samp + max_h - 1) / max_h
		actual_h := (d.height * comp.v_samp + max_v - 1) / max_v
		planes[i] = d.arena.alloc_i16(padded_w * padded_h)!
		crops[i] = d.arena.alloc_i16(actual_w * actual_h)!
		padded_ws[i] = padded_w
		dims[2 * i] = actual_w
		dims[2 * i + 1] = actual_h
	}
	// Stage A - IDCT: split each component's block rows into disjoint
	// bands; each band runs the IDCT plus its crop-copy rows.
	if nthreads > 1 {
		mut total_bh := 0
		for i in 0 .. d.ch_count {
			total_bh += comps[i].blocks_h
		}
		mut band_rows := (total_bh + nthreads - 1) / nthreads
		if band_rows < 1 {
			band_rows = 1
		}
		done := new_barrier_counter()
		mut tasks := 0
		for i in 0 .. d.ch_count {
			comp := comps[i]
			mut by0 := 0
			for by0 < comp.blocks_h {
				mut by1 := by0 + band_rows
				if by1 > comp.blocks_h {
					by1 = comp.blocks_h
				}
				spawn idct_worker(dptr, comp, unsafe { &planes[i][0] }, unsafe { &crops[i][0] },
					padded_ws[i], dims[2 * i], dims[2 * i + 1], by0, by1, done)
				tasks++
				by0 = by1
			}
		}
		barrier_wait(done, u64(tasks))
		free_barrier_counter(done)
	} else {
		for i in 0 .. d.ch_count {
			comp := comps[i]
			idct_band(dptr, comp, unsafe { &planes[i][0] }, unsafe { &crops[i][0] },
				padded_ws[i], dims[2 * i], dims[2 * i + 1], 0, comp.blocks_h)
		}
	}
	for i in 0 .. d.ch_count {
		planes[i] = crops[i]
	}
	mut rgba := []u8{len: out_w * out_h * 4}
	mut p32 := unsafe { &u32(&rgba[0]) }
	if d.ch_count == 1 {
		yw := dims[0]
		// P6: plane sized to cover out_w*out_h; LUT index in [0,4095].
		yp := unsafe { &planes[0][0] }
		lp := unsafe { &lut[0] }
		if nthreads > 1 {
			done := new_barrier_counter()
			mut tasks := 0
			for k in 0 .. nthreads {
				y0, y1 := band_range(out_h, k, nthreads)
				if y1 > y0 {
					spawn gray_worker(dptr, p32, yp, lp, yw, out_w, y0, y1, done)
					tasks++
				}
			}
			barrier_wait(done, u64(tasks))
			free_barrier_counter(done)
		} else {
			gray_rows(dptr, p32, yp, lp, yw, out_w, 0, out_h)
		}
		return common.Image{ width: out_w, height: out_h, rgba: rgba }
	}
	if d.ch_count == 4 {
		mut cmyk_planes := [4][]i16{}
		mut cmyk_widths := [4]int{}
		cmyk_planes[0] = planes[0]
		cmyk_widths[0] = dims[0]
		for i in 1 .. 4 {
			hf := max_h / comps[i].h_samp
			vf := max_v / comps[i].v_samp
			mut up := d.arena.alloc_i16(dims[2 * i] * hf * (dims[2 * i + 1] * vf))!
			mut cs := d.arena.alloc_i16(dims[2 * i])!
			upsample_plane(planes[i], dims[2 * i], dims[2 * i + 1], mut up, dims[2 * i] * hf,
				dims[2 * i + 1] * vf, hf, vf, mut cs)
			cmyk_planes[i] = up
			cmyk_widths[i] = dims[2 * i] * hf
		}
		// P6: cmyk planes sized to cover out_w*out_h; indices in range.
		mut cmyk_ptrs := unsafe { [&i16(0), &i16(0), &i16(0), &i16(0)] }
		for i in 0 .. 4 {
			cmyk_ptrs[i] = unsafe { &cmyk_planes[i][0] }
		}
		if nthreads > 1 {
			done := new_barrier_counter()
			mut tasks := 0
			for k in 0 .. nthreads {
				y0, y1 := band_range(out_h, k, nthreads)
				if y1 > y0 {
					spawn cmyk_worker(dptr, p32, cmyk_ptrs[0], cmyk_ptrs[1], cmyk_ptrs[2],
						cmyk_ptrs[3], cmyk_widths[0], cmyk_widths[1], cmyk_widths[2],
						cmyk_widths[3], out_w, y0, y1, done)
					tasks++
				}
			}
			barrier_wait(done, u64(tasks))
			free_barrier_counter(done)
		} else {
			cmyk_rows(dptr, p32, cmyk_ptrs[0], cmyk_ptrs[1], cmyk_ptrs[2], cmyk_ptrs[3],
				cmyk_widths[0], cmyk_widths[1], cmyk_widths[2], cmyk_widths[3], out_w, 0, out_h)
		}
		return common.Image{ width: out_w, height: out_h, rgba: rgba }
	}
	yw := dims[0]
	cb_h_factor := max_h / comps[1].h_samp
	cb_v_factor := max_v / comps[1].v_samp
	cr_h_factor := max_h / comps[2].h_samp
	cr_v_factor := max_v / comps[2].v_samp
	// M2: the common 4:2:0 case (both chroma planes h2v2, hence equal
	// source dims) fuses the chroma upsample into the color loop and
	// skips materializing cb_up/cr_up entirely.
	fused_chroma := cb_h_factor == 2 && cb_v_factor == 2 && cr_h_factor == 2
		&& cr_v_factor == 2 && dims[2] == dims[4] && dims[3] == dims[5]
	yp := unsafe { &planes[0][0] }
	lp := unsafe { &lut[0] }
	if fused_chroma {
		cw := dims[2]
		ch := dims[3]
		cbp := unsafe { &planes[1][0] }
		crp := unsafe { &planes[2][0] }
		if nthreads > 1 {
			done := new_barrier_counter()
			mut tasks := 0
			for k in 0 .. nthreads {
				y0, y1 := band_range(out_h, k, nthreads)
				if y1 > y0 {
					spawn ycbcr_worker_fused(dptr, p32, yp, cbp, crp, lp, yw, cw, ch,
						out_w, ms, y0, y1, done)
					tasks++
				}
			}
			barrier_wait(done, u64(tasks))
			free_barrier_counter(done)
		} else {
			ycbcr_rows_fused(dptr, p32, yp, cbp, crp, lp, yw, cw, ch, out_w, ms, 0, out_h)
		}
		return common.Image{ width: out_w, height: out_h, rgba: rgba }
	}
	cb_w := dims[2] * cb_h_factor
	cr_w := dims[4] * cr_h_factor
	mut cb_up := d.arena.alloc_i16(dims[2] * cb_h_factor * (dims[3] * cb_v_factor))!
	mut cb_cs := d.arena.alloc_i16(dims[2])!
	upsample_plane(planes[1], dims[2], dims[3], mut cb_up, dims[2] * cb_h_factor,
		dims[3] * cb_v_factor, cb_h_factor, cb_v_factor, mut cb_cs)
	mut cr_up := d.arena.alloc_i16(dims[4] * cr_h_factor * (dims[5] * cr_v_factor))!
	mut cr_cs := d.arena.alloc_i16(dims[4])!
	upsample_plane(planes[2], dims[4], dims[5], mut cr_up, dims[4] * cr_h_factor,
		dims[5] * cr_v_factor, cr_h_factor, cr_v_factor, mut cr_cs)
	// P6: every index below is provably in range (planes/cb_up/cr_up are
	// sized to cover out_w*out_h after upsample; ycbcr_to_rgb clamps
	// r/g/b to [0, max_sample] so the 4096-entry LUT index is safe), so
	// the per-pixel bounds checks are elided.
	cbp := unsafe { &cb_up[0] }
	crp := unsafe { &cr_up[0] }
	if nthreads > 1 {
		done := new_barrier_counter()
		mut tasks := 0
		for k in 0 .. nthreads {
			y0, y1 := band_range(out_h, k, nthreads)
			if y1 > y0 {
				spawn ycbcr_worker(dptr, p32, yp, cbp, crp, lp, yw, cb_w, cr_w, out_w, ms,
					y0, y1, done)
				tasks++
			}
		}
		barrier_wait(done, u64(tasks))
		free_barrier_counter(done)
	} else {
		ycbcr_rows(dptr, p32, yp, cbp, crp, lp, yw, cb_w, cr_w, out_w, ms, 0, out_h)
	}
	return common.Image{ width: out_w, height: out_h, rgba: rgba }
}

// ============================================================================
// MAIN LOOP
// ============================================================================

fn (mut d JpegDecoder) load_jpeg() !common.Image {
	mut saw_sos := false
	for d.pos < d.data.len {
		mut marker := 0
		if d.arith_unread != 0 {
			marker = d.arith_unread
			d.arith_unread = 0
		} else {
			byte_val := d.read_byte()!
			if byte_val != 0xFF {
				continue
			}
			marker = d.read_byte()!
			for marker == 0xFF {
				marker = d.read_byte()!
			}
		}
		if marker == marker_soi {
			continue
		} else if marker == marker_eoi {
			if saw_sos {
				return d.render()!
			}
			return error('No SOS segment found in JPEG')
		} else if marker == marker_sof0 || marker == marker_sof1 || marker == marker_sof2
			|| marker == marker_sof9 || marker == marker_sof10 {
			d.parse_sof(marker)!
		} else if marker == marker_dht {
			d.parse_dht()!
		} else if marker == marker_dac {
			d.parse_dac()!
		} else if marker == marker_dqt {
			d.parse_dqt()!
		} else if marker == marker_app14 {
			d.parse_adobe_transform()!
		} else if marker == marker_dri {
			d.skip_bytes(2)
			d.restart_interval = d.read_word()!
		} else if marker == marker_sos {
			d.parse_sos()!
			saw_sos = true
		} else if marker_rst0 <= marker && marker <= marker_rst7 {
			continue
		} else {
			length := d.read_word()!
			d.skip_bytes(length - 2)
		}
	}
	if saw_sos {
		return d.render()!
	}
	return error('No SOS segment found in JPEG')
}

// ============================================================================
// PUBLIC API
// ============================================================================

pub fn decode(data []u8) !common.Image {
	if data.len < 2 || data[0] != 0xFF || data[1] != 0xD8 {
		return error('Not a valid JPEG file (missing SOI marker)')
	}
	mut d := JpegDecoder{
		data:            data
		adobe_transform: -1
		precision:       8
		max_sample:      255
		center:          128
	}
	// Whole-decode arena regions. The per-frame region is (re)allocated
	// in parse_sof once the frame geometry is known.
	d.arena.small = []int{len: 32768}
	d.arena.bytes = []u8{len: 16384}
	d.install_std_huff_tables()!
	d.init_arith_tables()!
	img := d.load_jpeg() or {
		unsafe { d.components.free() }
		d.arena.free_all()
		return err
	}
	// The returned rgba is allocated outside the arena; every scratch
	// buffer the decoder used is released here.
	unsafe { d.components.free() }
	d.arena.free_all()
	return img
}
