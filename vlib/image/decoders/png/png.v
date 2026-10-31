/**********************************************************************
*
* PNG image decoder (all color types, Adam7, APNG, gamma correction)
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module png

import hash.crc32
import hash.adler32
import hash.huffman
import math
import image.decoders.common

// ============================================================================
// Constants
// ============================================================================

const png_signature = [u8(0x89), 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]

const chunk_ihdr = 'IHDR'
const chunk_plte = 'PLTE'
const chunk_idat = 'IDAT'
const chunk_iend = 'IEND'
const chunk_trns = 'tRNS'
const chunk_gama = 'gAMA'
const chunk_chrm = 'cHRM'
const chunk_actl = 'acTL'
const chunk_fctl = 'fcTL'
const chunk_fdat = 'fdAT'

const apng_dispose_none = 0
const apng_dispose_background = 1
const apng_dispose_previous = 2

const apng_blend_source = 0
const apng_blend_over = 1

const gama_scale = 100000.0
const chroma_scale = 100000.0

const color_gray = 0
const color_rgb = 2
const color_palette = 3
const color_gray_alpha = 4
const color_rgba = 6

const filter_none = 0
const filter_sub = 1
const filter_up = 2
const filter_average = 3
const filter_paeth = 4

const interlace_none = 0
const interlace_adam7 = 1

struct Pass {
	xstart int
	ystart int
	xstep  int
	ystep  int
}

// adam7_passes returns the seven Adam7 interlace pass descriptors.
fn adam7_passes() []Pass {
	return [
		Pass{ xstart: 0, ystart: 0, xstep: 8, ystep: 8 },
		Pass{ xstart: 4, ystart: 0, xstep: 8, ystep: 8 },
		Pass{ xstart: 0, ystart: 4, xstep: 4, ystep: 8 },
		Pass{ xstart: 2, ystart: 0, xstep: 4, ystep: 4 },
		Pass{ xstart: 0, ystart: 2, xstep: 2, ystep: 4 },
		Pass{ xstart: 1, ystart: 0, xstep: 2, ystep: 2 },
		Pass{ xstart: 0, ystart: 1, xstep: 1, ystep: 2 },
	]
}

// srgb_chrm returns the sRGB cHRM coordinates (white, red, green, blue).
fn srgb_chrm() [8]f64 {
	return [0.3127, 0.3290, 0.6400, 0.3300, 0.3000, 0.6000, 0.1500, 0.0600]
}

fn bradford_mat() [3][3]f64 {
	return [
		[0.8951000, 0.2664000, -0.1614000],
		[-0.7502000, 1.7135000, 0.0367000],
		[0.0389000, -0.0685000, 1.0296000],
	]
}

fn srgb_xyz_to_rgb_mat() [3][3]f64 {
	return [
		[3.2404542, -1.5371385, -0.4985314],
		[-0.9692660, 1.8760108, 0.0415560],
		[0.0556434, -0.2040259, 1.0572252],
	]
}

// ============================================================================
// Small helpers
// ============================================================================

fn be_u32(b []u8, off int) u32 {
	return (u32(b[off]) << 24) | (u32(b[off + 1]) << 16) | (u32(b[off + 2]) << 8) |
		u32(b[off + 3])
}

fn be_u16(b []u8, off int) u32 {
	return (u32(b[off]) << 8) | u32(b[off + 1])
}

fn iabs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

fn fabs(x f64) f64 {
	if x < 0.0 {
		return -x
	}
	return x
}

fn clampf(x f64, lo f64, hi f64) f64 {
	if x < lo {
		return lo
	}
	if x > hi {
		return hi
	}
	return x
}

fn clampi(x int, lo int, hi int) int {
	if x < lo {
		return lo
	}
	if x > hi {
		return hi
	}
	return x
}

// round_half_even rounds half to even (banker's rounding).
fn round_half_even(x f64) f64 {
	f := math.floor(x)
	diff := x - f
	if diff > 0.5 {
		return f + 1.0
	}
	if diff < 0.5 {
		return f
	}
	// Exact tie: round toward even.
	if math.fmod(f, 2.0) == 0.0 {
		return f
	}
	return f + 1.0
}

fn channels_for(color_type int) int {
	match color_type {
		color_gray { return 1 }
		color_rgb { return 3 }
		color_palette { return 1 }
		color_gray_alpha { return 2 }
		color_rgba { return 4 }
		else { return 0 }
	}
}

fn valid_bit_depth(color_type int, bit_depth int) bool {
	match color_type {
		color_gray { return bit_depth in [1, 2, 4, 8, 16] }
		color_rgb { return bit_depth in [8, 16] }
		color_palette { return bit_depth in [1, 2, 4, 8] }
		color_gray_alpha { return bit_depth in [8, 16] }
		color_rgba { return bit_depth in [8, 16] }
		else { return false }
	}
}

// scale_to_8bit resamples a sample from [0, max_sample] to [0, 255].
fn scale_to_8bit(value int, max_sample int) u8 {
	if max_sample == 255 {
		return u8(clampi(value, 0, 255))
	}
	if max_sample > 255 {
		return u8(clampi(value >> 8, 0, 255))
	}
	return u8(int(round_half_even(clampf(f64(value) * 255.0 / f64(max_sample), 0.0, 255.0))))
}

// ============================================================================
// Data structures
// ============================================================================

struct Chunk {
	ctype string
	data  []u8
}

struct PngFrame {
mut:
	sequence_number int
	width           int
	height          int
	x_offset        int
	y_offset        int
	delay_num       int
	delay_den       int
	dispose_op      int
	blend_op        int
	data            []u8
	rgba            []int
}

struct PngDecoder {
mut:
	data               []u8
	pos                int
	chunks             []Chunk
	width              int
	height             int
	bit_depth          int
	color_type         int
	compression_method int
	filter_method      int
	interlace_method   int
	palette            []u8 // flat RGB triples
	trns               []int
	idat               []u8
	gamma              f64
	has_gamma          bool
	chrm               [8]f64
	has_chrm           bool
	animation          bool
	num_frames         int
	num_plays          int
	frames             []PngFrame
	arena              Arena
}

// ============================================================================
// Arena allocator
// ============================================================================
//
// The Windows viewer build is effectively `-gc none` (no Boehm collector: the
// generated C maps v_free to _aligned_free and leaves gc_runtime_init empty),
// so every array the decoder allocates must be released explicitly or it leaks
// for the life of the process. The PNG decode path allocates a lot of
// short-lived scratch (decompressed rows, per-line sample buffers, the native
// RGBA buffer, the APNG canvas). Instead of threading `free()` through every
// error path, all scratch is carved out of one arena and released in a single
// `free_all()` at the end of each public decode call, on both the success and
// the error path.
//
// Regions (bump-allocated, never individually freed):
//   ints         - persistent per decode: the native RGBA output / APNG canvas.
//   bytes        - persistent per decode: the unfilter prev/current row buffers.
//   scratch      - reset between scanlines: sample-unpack + color-convert
//                buffers (samples, pass_rgba).
//   frame_scratch - reset between APNG frames: the per-frame native buffer.
//
// The returned 8-bit image ([]u8 produced by native_to_image) is allocated
// OUTSIDE the arena and owned by the caller.
struct Arena {
mut:
	ints              []int
	ints_pos          int
	bytes             []u8
	bytes_pos         int
	scratch           []int
	scratch_pos       int
	frame_scratch     []int
	frame_scratch_pos int
}

// alloc_ints returns a shared zero-initialised sub-slice of the persistent
// int region (native RGBA / APNG canvas).
fn (mut a Arena) alloc_ints(n int) ![]int {
	if n <= 0 {
		return []int{}
	}
	if a.ints_pos + n > a.ints.len {
		return error('arena: int region exhausted (${a.ints_pos + n} > ${a.ints.len})')
	}
	res := unsafe { a.ints[a.ints_pos..a.ints_pos + n] }
	a.ints_pos += n
	return res
}

// alloc_bytes returns a shared zero-initialised sub-slice of the persistent
// byte region (unfilter prev/current rows).
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

// alloc_scratch returns a shared zero-initialised sub-slice of the per-line
// scratch region (samples / pass_rgba).
fn (mut a Arena) alloc_scratch(n int) ![]int {
	if n <= 0 {
		return []int{}
	}
	if a.scratch_pos + n > a.scratch.len {
		return error('arena: scratch region exhausted (${a.scratch_pos + n} > ${a.scratch.len})')
	}
	res := unsafe { a.scratch[a.scratch_pos..a.scratch_pos + n] }
	a.scratch_pos += n
	return res
}

// alloc_frame_scratch returns a shared zero-initialised sub-slice of the
// per-frame scratch region (APNG per-frame native buffer).
fn (mut a Arena) alloc_frame_scratch(n int) ![]int {
	if n <= 0 {
		return []int{}
	}
	if a.frame_scratch_pos + n > a.frame_scratch.len {
		return error('arena: frame_scratch region exhausted (${a.frame_scratch_pos + n} > ${a.frame_scratch.len})')
	}
	res := unsafe { a.frame_scratch[a.frame_scratch_pos..a.frame_scratch_pos + n] }
	a.frame_scratch_pos += n
	return res
}

// reset_scratch rewinds the per-line scratch cursor. Called at the top of
// every scanline so the samples/pass_rgba buffers are reused.
fn (mut a Arena) reset_scratch() {
	a.scratch_pos = 0
}

// reset_bytes rewinds the persistent byte cursor. Called at the top of every
// interlace pass so the prev/current row buffers are reused across passes
// (only prev/cur live in the byte region, and they are dead between passes).
fn (mut a Arena) reset_bytes() {
	a.bytes_pos = 0
}

// reset_frame_scratch rewinds the per-frame scratch cursor. Called at the
// top of every APNG frame so the per-frame native buffer is reused.
fn (mut a Arena) reset_frame_scratch() {
	a.frame_scratch_pos = 0
}

// free_all releases every backing buffer exactly once. Called on both the
// success and the error path of each public decode function.
fn (mut a Arena) free_all() {
	unsafe {
		a.ints.free()
		a.bytes.free()
		a.scratch.free()
		a.frame_scratch.free()
	}
	a.ints = []int{}
	a.bytes = []u8{}
	a.scratch = []int{}
	a.frame_scratch = []int{}
	a.ints_pos = 0
	a.bytes_pos = 0
	a.scratch_pos = 0
	a.frame_scratch_pos = 0
}

// init_arena sizes the decode scratch regions from the frame geometry. It is
// called once after IHDR is parsed (so width/height/channels/bit_depth are
// known) and before any scratch is allocated. The persistent `ints` region
// holds the full-canvas native buffer; `bytes` holds two scanline rows;
// `scratch` holds one line's sample + rgba buffers; `frame_scratch` holds
// one APNG frame's native buffer (only sized when the image is animated).
fn (mut d PngDecoder) init_arena() ! {
	// Check products and backing-buffer byte counts before performing arithmetic.
	// PNG dimensions can be valid individually while their RGBA product overflows.
	limit := max_int / int(sizeof(int)) - 16
	if d.width <= 0 || d.height <= 0 || d.width > limit / d.height / 4 {
		return error('PNG dimensions exceed the supported buffer size')
	}
	if d.width > (max_int - 7) / d.channels() / d.bit_depth
		|| d.width > limit / (d.channels() + 4) {
		return error('PNG scanline exceeds the supported buffer size')
	}
	native := d.width * d.height * 4
	max_scan := d.scan_bytes_for_width(d.width)
	if max_scan > (max_int - 16) / 2 {
		return error('PNG scanline exceeds the supported buffer size')
	}
	mut frame_cap := 0
	if d.animation {
		frame_cap = native + 16
	}
	d.arena = Arena{
		ints:          []int{len: native}
		bytes:         []u8{len: 2 * max_scan + 16}
		scratch:       []int{len: d.width * (d.channels() + 4) + 16}
		frame_scratch: []int{len: frame_cap}
	}
}

// free_owned releases the heap arrays the decoder allocated for itself during
// parse: the chunk index (and its owned ctype strings), the palette, the tRNS
// table, the concatenated IDAT stream, and the APNG frame list (and each
// frame's concatenated data). `d.data` is caller-owned and never freed here;
// chunk/frame `data` fields are slices into `d.data` and are not freed either.
fn (mut d PngDecoder) free_owned() {
	unsafe {
		for c in d.chunks {
			c.ctype.free()
		}
		d.chunks.free()
		d.palette.free()
		d.trns.free()
		d.idat.free()
		for f in d.frames {
			f.data.free()
		}
		d.frames.free()
	}
	d.chunks = []Chunk{}
	d.palette = []u8{}
	d.trns = []int{}
	d.idat = []u8{}
	d.frames = []PngFrame{}
}

// ============================================================================
// Container parsing
// ============================================================================

fn (mut d PngDecoder) parse_signature() ! {
	if d.data.len < 8 {
		return error('File too short for PNG signature')
	}
	for i in 0 .. 8 {
		if d.data[i] != png_signature[i] {
			return error('Invalid PNG signature')
		}
	}
}

// iend_tag is the big-endian u32 of the ASCII bytes "IEND", used to detect
// the trailer chunk without allocating a string during the counting pass.
const iend_tag = u32(0x49454e44)

// png_crc is a cached IEEE CRC-32 instance used for streaming the chunk CRC
// over (ctype bytes + chunk data) without building a combined scratch array.
// Allocated once at program init (a single 256-word table), never per decode.
const png_crc = crc32.new(crc32.ieee)

fn (mut d PngDecoder) parse_chunks() ! {
	// First pass: count the chunks (headers only) so the []Chunk array can
	// be pre-sized. Under `-gc none` V's array__ensure_cap does NOT free the
	// old backing block when the element type contains slices (it skips the
	// free for slice-bearing buffers), so `d.chunks << Chunk{}` would leak a
	// block per growth step. Pre-sizing to the exact count avoids all growth.
	mut count := 0
	mut p := 8
	for p + 8 <= d.data.len {
		length := int(be_u32(d.data, p))
		if p + 12 + length > d.data.len {
			ctype := d.data[p + 4..p + 8].bytestr()
			return error('Truncated PNG chunk ${ctype}: declared ${length} bytes')
		}
		count++
		p += 12 + length
		if be_u32(d.data, p - 8) == iend_tag {
			break
		}
	}
	mut chunks := []Chunk{len: count}
	mut idx := 0
	d.pos = 8
	for d.pos + 8 <= d.data.len {
		length := int(be_u32(d.data, d.pos))
		ctype := d.data[d.pos + 4..d.pos + 8].bytestr()
		if d.pos + 12 + length > d.data.len {
			return error('Truncated PNG chunk ${ctype}: declared ${length} bytes')
		}
		// chunk_data aliases the caller-owned `d.data` (no copy); it is read
		// only for the CRC check and by the per-chunk parsers, and is never
		// freed here.
		chunk_data := unsafe { d.data[d.pos + 8..d.pos + 8 + length] }
		crc := be_u32(d.data, d.pos + 8 + length)
		// Stream the CRC over the ctype bytes and the chunk data directly from
		// d.data (no combined scratch array, so nothing to free).
		ctype_bytes := unsafe { d.data[d.pos + 4..d.pos + 8] }
		mut calc := png_crc.update(0, ctype_bytes)
		calc = png_crc.update(calc, chunk_data)
		if crc != calc {
			return error('CRC mismatch in PNG chunk ${ctype}: expected ${crc:08X}, got ${calc:08X}')
		}
		chunks[idx] = Chunk{
			ctype: ctype
			data:  chunk_data
		}
		idx++
		d.pos += 12 + length
		if ctype == chunk_iend {
			break
		}
	}
	d.chunks = chunks
	mut has_iend := false
	for c in d.chunks {
		if c.ctype == chunk_iend {
			has_iend = true
		}
	}
	if !has_iend {
		return error('Missing IEND chunk')
	}
}

fn (d &PngDecoder) find_chunk(ctype string) ?[]u8 {
	for c in d.chunks {
		if c.ctype == ctype {
			return c.data
		}
	}
	return none
}

fn (mut d PngDecoder) parse_ihdr(data []u8) ! {
	if data.len != 13 {
		return error('IHDR must be 13 bytes, got ${data.len}')
	}
	d.width = int(be_u32(data, 0))
	d.height = int(be_u32(data, 4))
	d.bit_depth = int(data[8])
	d.color_type = int(data[9])
	d.compression_method = int(data[10])
	d.filter_method = int(data[11])
	d.interlace_method = int(data[12])

	if d.width <= 0 || d.height <= 0 {
		return error('Invalid PNG dimensions: ${d.width}x${d.height}')
	}
	if d.compression_method != 0 {
		return error('Unsupported PNG compression method: ${d.compression_method}')
	}
	if d.filter_method != 0 {
		return error('Unsupported PNG filter method: ${d.filter_method}')
	}
	if d.interlace_method != interlace_none && d.interlace_method != interlace_adam7 {
		return error('Unsupported PNG interlace method: ${d.interlace_method}')
	}
	if channels_for(d.color_type) == 0 {
		return error('Unknown PNG color type: ${d.color_type}')
	}
	if !valid_bit_depth(d.color_type, d.bit_depth) {
		return error('Bit depth ${d.bit_depth} is invalid for color type ${d.color_type}')
	}
}

fn (mut d PngDecoder) parse_plte(data []u8) ! {
	if d.color_type != color_palette {
		return error('PLTE chunk is only valid for palette color type')
	}
	if data.len % 3 != 0 {
		return error('PLTE chunk length must be a multiple of 3')
	}
	if data.len < 6 || data.len > 768 {
		return error('PLTE chunk has invalid size: ${data.len}')
	}
	d.palette = data.clone()
}

fn (mut d PngDecoder) parse_trns(data []u8) ! {
	if d.color_type == color_palette {
		if data.len > 256 {
			return error('tRNS for palette must be at most 256 bytes')
		}
		d.trns = []int{len: data.len}
		for i, v in data {
			d.trns[i] = int(v)
		}
	} else if d.color_type == color_gray {
		if data.len != 2 {
			return error('tRNS for grayscale must be 2 bytes')
		}
		d.trns = [int(be_u16(data, 0))]
	} else if d.color_type == color_rgb {
		if data.len != 6 {
			return error('tRNS for RGB must be 6 bytes')
		}
		d.trns = [int(be_u16(data, 0)), int(be_u16(data, 2)), int(be_u16(data, 4))]
	} else {
		return error('tRNS chunk is not valid for color type ${d.color_type}')
	}
}

fn (mut d PngDecoder) parse_gama(data []u8) ! {
	if data.len != 4 {
		return error('gAMA chunk must be 4 bytes')
	}
	raw := be_u32(data, 0)
	if raw == 0 {
		return error('gAMA value must be nonzero')
	}
	d.gamma = gama_scale / f64(raw)
	d.has_gamma = true
}

fn (mut d PngDecoder) parse_chrm(data []u8) ! {
	if data.len != 32 {
		return error('cHRM chunk must be 32 bytes')
	}
	mut coords := [8]f64{}
	for i in 0 .. 8 {
		coords[i] = f64(be_u32(data, i * 4)) / chroma_scale
	}
	mut i := 0
	for i < 8 {
		x := coords[i]
		y := coords[i + 1]
		if y <= 0.0 {
			return error('cHRM y coordinate must be positive')
		}
		if x + y >= 1.0 {
			return error('cHRM x + y must be less than 1')
		}
		i += 2
	}
	d.chrm = coords
	d.has_chrm = true
}

fn (mut d PngDecoder) parse_actl(data []u8) ! {
	if data.len != 8 {
		return error('acTL chunk must be 8 bytes')
	}
	num_frames := int(be_u32(data, 0))
	num_plays := int(be_u32(data, 4))
	if num_frames <= 0 {
		return error('acTL num_frames must be positive')
	}
	d.num_frames = num_frames
	d.num_plays = num_plays
	d.animation = true
}

fn (mut d PngDecoder) parse_fctl(data []u8) !PngFrame {
	if data.len != 26 {
		return error('fcTL chunk must be 26 bytes')
	}
	frame := PngFrame{
		sequence_number: int(be_u32(data, 0))
		width:           int(be_u32(data, 4))
		height:          int(be_u32(data, 8))
		x_offset:        int(be_u32(data, 12))
		y_offset:        int(be_u32(data, 16))
		delay_num:       int(be_u16(data, 20))
		delay_den:       int(be_u16(data, 22))
		dispose_op:      int(data[24])
		blend_op:        int(data[25])
	}
	if frame.width <= 0 {
		return error('APNG frame width must be positive')
	}
	if frame.height <= 0 {
		return error('APNG frame height must be positive')
	}
	if frame.x_offset + frame.width > d.width || frame.y_offset + frame.height > d.height {
		return error('APNG frame offset is outside the canvas')
	}
	if frame.dispose_op != apng_dispose_none && frame.dispose_op != apng_dispose_background && frame.dispose_op != apng_dispose_previous {
		return error('Unknown APNG dispose_op: ${frame.dispose_op}')
	}
	if frame.blend_op != apng_blend_source && frame.blend_op != apng_blend_over {
		return error('Unknown APNG blend_op: ${frame.blend_op}')
	}
	return frame
}

fn (mut d PngDecoder) parse_animation() ! {
	d.animation = false
	d.frames = []
	d.num_frames = 0
	d.num_plays = 0

	mut actl_index := -1
	mut actl_count := 0
	for i, c in d.chunks {
		if c.ctype == chunk_actl {
			if actl_index < 0 {
				actl_index = i
			}
			actl_count++
		}
	}
	if actl_index < 0 {
		return
	}
	if actl_count > 1 {
		return error('Multiple acTL chunks')
	}
	d.parse_actl(d.chunks[actl_index].data)!

	mut fctl_indices := []int{cap: d.chunks.len}
	mut fdat_indices := []int{cap: d.chunks.len}
	mut idat_indices := []int{cap: d.chunks.len}
	defer {
		unsafe {
			fctl_indices.free()
			fdat_indices.free()
			idat_indices.free()
		}
	}
	for i, c in d.chunks {
		if c.ctype == chunk_fctl {
			fctl_indices << i
		} else if c.ctype == chunk_fdat {
			fdat_indices << i
		} else if c.ctype == chunk_idat {
			idat_indices << i
		}
	}

	if idat_indices.len == 0 {
		return error('Missing IDAT chunk')
	}
	for idx in fctl_indices {
		if idx < actl_index {
			return error('APNG frame chunks before acTL')
		}
	}
	for idx in fdat_indices {
		if idx < actl_index {
			return error('APNG frame chunks before acTL')
		}
	}
	for idx in idat_indices {
		if idx < actl_index {
			return error('acTL must appear before IDAT')
		}
	}
	if fctl_indices.len != d.num_frames {
		return error('acTL num_frames ${d.num_frames} does not match fcTL count ${fctl_indices.len}')
	}

	// Pre-size the frame list to num_frames so `[]PngFrame` (a slice-bearing
	// element type) never grows; under `-gc none` growth of a slice-bearing
	// array leaks the old backing block. `seen` tracks how many frames have
	// been filled so far (the old code used d.frames.len for this).
	d.frames = []PngFrame{len: d.num_frames}
	// Pre-size each frame's data buffer the same way (fdAT/IDAT payloads are
	// known from the index lists collected above), then fill by index.
	mut frame_data_len := []int{len: d.num_frames}
	mut fpos := []int{len: d.num_frames}
	defer {
		unsafe {
			frame_data_len.free()
			fpos.free()
		}
	}
	mut cur_frame := 0
	for i in actl_index + 1 .. d.chunks.len {
		ctype := d.chunks[i].ctype
		if ctype == chunk_fctl {
			cur_frame++
		} else if ctype == chunk_fdat {
			if cur_frame >= 1 && cur_frame <= d.num_frames && d.chunks[i].data.len >= 4 {
				frame_data_len[cur_frame - 1] += d.chunks[i].data.len - 4
			}
		} else if ctype == chunk_idat {
			frame_data_len[0] += d.chunks[i].data.len
		}
	}
	for i in 0 .. d.num_frames {
		d.frames[i].data = []u8{len: frame_data_len[i]}
	}
	mut seen := 0

	mut expected_seq := 0
	mut current_frame_idx := -1
	mut idat_seen := false
	mut fdat_seen := false

	for i, c in d.chunks {
		if i <= actl_index {
			continue
		}
		if c.ctype == chunk_fctl {
			frame := d.parse_fctl(c.data)!
			if frame.sequence_number != expected_seq {
				return error('APNG sequence error: expected ${expected_seq}, got ${frame.sequence_number}')
			}
			expected_seq++
			pre_data := d.frames[seen].data
			d.frames[seen] = frame
			d.frames[seen].data = pre_data
			seen++
			current_frame_idx = seen - 1
		} else if c.ctype == chunk_fdat {
			if c.data.len < 4 {
				return error('fdAT chunk too short')
			}
			seq := int(be_u32(c.data, 0))
			payload := c.data[4..]
			if seq != expected_seq {
				return error('APNG sequence error: expected ${expected_seq}, got ${seq}')
			}
			expected_seq++
			if current_frame_idx < 0 {
				return error('fdAT chunk before fcTL')
			}
			if seen == 1 {
				return error('fdAT is not valid for the first APNG frame')
			}
			if !idat_seen {
				return error('fdAT before IDAT in APNG')
			}
			p := fpos[current_frame_idx]
			copy(mut d.frames[current_frame_idx].data[p..p + payload.len], payload)
			fpos[current_frame_idx] = p + payload.len
			fdat_seen = true
		} else if c.ctype == chunk_idat {
			if seen == 0 {
				return error('IDAT before fcTL in APNG')
			}
			if fdat_seen {
				return error('IDAT after fdAT in APNG')
			}
			if seen > 1 {
				return error('IDAT after second fcTL in APNG')
			}
			idat_seen = true
			p := fpos[0]
			copy(mut d.frames[0].data[p..p + c.data.len], c.data)
			fpos[0] = p + c.data.len
		}
	}

	if !idat_seen {
		return error('Missing IDAT chunk')
	}
	if seen != d.num_frames {
		return error('acTL num_frames ${d.num_frames} does not match fcTL count ${seen}')
	}
	first := d.frames[0]
	if first.data.len == 0 {
		return error('First APNG frame missing IDAT data')
	}
	if first.width != d.width || first.height != d.height || first.x_offset != 0 || first.y_offset != 0 {
		return error('First APNG frame must cover the full canvas')
	}
	for fi in 1 .. d.frames.len {
		if d.frames[fi].data.len == 0 {
			return error('APNG frame ${fi} missing fdAT data')
		}
	}
}

fn (mut d PngDecoder) parse() ! {
	d.parse_signature()!
	d.parse_chunks()!

	ihdr := d.find_chunk(chunk_ihdr) or { return error('Missing IHDR chunk') }
	d.parse_ihdr(ihdr)!

	if plte := d.find_chunk(chunk_plte) {
		d.parse_plte(plte)!
	}
	if trns := d.find_chunk(chunk_trns) {
		d.parse_trns(trns)!
	}
	if gama := d.find_chunk(chunk_gama) {
		d.parse_gama(gama)!
	}
	if chrm := d.find_chunk(chunk_chrm) {
		d.parse_chrm(chrm)!
	}

	// Pre-size the IDAT concatenation: `<<` growth leaks the old backing
	// block under -gc none (see png_improve_plan.md P0 leak #1).
	mut idat_total := 0
	for c in d.chunks {
		if c.ctype == chunk_idat {
			idat_total += c.data.len
		}
	}
	mut idat := []u8{len: idat_total}
	mut ipos := 0
	for c in d.chunks {
		if c.ctype == chunk_idat {
			copy(mut idat[ipos..ipos + c.data.len], c.data)
			ipos += c.data.len
		}
	}
	d.idat = idat
	if d.idat.len == 0 {
		return error('Missing IDAT chunk')
	}
	d.parse_animation()!
}

// ============================================================================
// Geometry helpers
// ============================================================================

fn (d &PngDecoder) channels() int {
	return channels_for(d.color_type)
}

fn (d &PngDecoder) max_sample() int {
	if d.color_type == color_palette {
		return 255
	}
	return (1 << d.bit_depth) - 1
}

fn (d &PngDecoder) bytes_per_pixel() int {
	bpp := (d.channels() * d.bit_depth) / 8
	if bpp < 1 {
		return 1
	}
	return bpp
}

fn (d &PngDecoder) scan_bytes_for_width(width int) int {
	return (width * d.channels() * d.bit_depth + 7) / 8
}

fn pass_dimensions_for(width int, height int, xstart int, ystart int, xstep int, ystep int) (int, int) {
	if width <= xstart || height <= ystart {
		return 0, 0
	}
	pw := (width - xstart + xstep - 1) / xstep
	ph := (height - ystart + ystep - 1) / ystep
	return pw, ph
}

fn (d &PngDecoder) expected_raw_size_for(width int, height int) int {
	if d.interlace_method == interlace_none {
		return height * (d.scan_bytes_for_width(width) + 1)
	}
	mut total := 0
	passes := adam7_passes()
	for p in passes {
		pw, ph := pass_dimensions_for(width, height, p.xstart, p.ystart, p.xstep, p.ystep)
		if pw == 0 || ph == 0 {
			continue
		}
		scan_bytes := d.scan_bytes_for_width(pw)
		total += ph * (scan_bytes + 1)
	}
	unsafe { passes.free() }
	return total
}

// ============================================================================
// Decompression
// ============================================================================

// ============================================================================
// Vendored inflate (RFC 1951) with pre-sized output
// ============================================================================
// Ported from v/vlib/compress/deflate/deflate_inflate.v (V 0.5.2) with one
// structural change: output is written by index into a caller-sized buffer
// instead of a `<<`-grown array. Under -gc none V's array growth never frees
// the old backing block, so the stdlib inflate leaked every doubling copy
// (~1.4 MB/decode) and paid ~19% of decode instructions in growth memcpy
// (see png_improve_plan.md P0). The fixed trees are built per call (two
// small tables, negligible next to a full decode) to keep everything
// explicitly freed.

const inf_length_bases = [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51,
	59, 67, 83, 99, 115, 131, 163, 195, 227, 258]
const inf_length_extra_bits = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4,
	4, 4, 5, 5, 5, 5, 0]
const inf_dist_bases = [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513,
	769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577]
const inf_dist_extra_bits = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9,
	10, 10, 11, 11, 12, 12, 13, 13]
// code-length alphabet order (RFC 1951)
const inf_cl_order = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

// InfHuffTree is a flat LSB-first Huffman lookup table for DEFLATE.
struct InfHuffTree {
	table    []u32 // entry: (symbol << 5) | code_length; 0xFFFF_FFFF = invalid
	max_bits int
}

fn inf_build_tree(lengths []int) !InfHuffTree {
	mut max_bits := 0
	for l in lengths {
		if l > max_bits {
			max_bits = l
		}
	}
	if max_bits == 0 {
		return InfHuffTree{ table: [u32(0)], max_bits: 0 }
	}
	table := huffman.flat_table(lengths: lengths, max_bits: max_bits, bit_order: .lsb_first)!
	return InfHuffTree{ table: table, max_bits: max_bits }
}

fn (mut t InfHuffTree) free() {
	unsafe { t.table.free() }
}

fn inf_fixed_lengths(mut lens []int) {
	for i in 0 .. 144 {
		lens[i] = 8
	}
	for i in 144 .. 256 {
		lens[i] = 9
	}
	for i in 256 .. 280 {
		lens[i] = 7
	}
	for i in 280 .. 288 {
		lens[i] = 8
	}
}

struct InfBitReader {
	buf []u8
mut:
	pos   int
	bits  u32
	nbits int
}

@[direct_array_access; inline]
fn (mut r InfBitReader) read_bits(n int) !u32 {
	for r.nbits < n {
		if r.pos >= r.buf.len {
			return error('inflate: unexpected end of stream')
		}
		r.bits |= u32(r.buf[r.pos]) << r.nbits
		r.pos++
		r.nbits += 8
	}
	val := r.bits & ((u32(1) << n) - 1)
	r.bits >>= u32(n)
	r.nbits -= n
	return val
}

@[inline]
fn (mut r InfBitReader) align_byte() {
	r.bits = 0
	r.nbits = 0
}

@[direct_array_access; inline]
fn (mut r InfBitReader) read_byte_raw() !u8 {
	if r.pos >= r.buf.len {
		return error('inflate: unexpected end of stream')
	}
	b := r.buf[r.pos]
	r.pos++
	return b
}

@[direct_array_access; inline]
fn (mut r InfBitReader) huff_decode(t InfHuffTree) !u32 {
	for r.nbits < t.max_bits {
		if r.pos >= r.buf.len {
			break
		}
		r.bits |= u32(r.buf[r.pos]) << r.nbits
		r.pos++
		r.nbits += 8
	}
	idx := int(r.bits & ((u32(1) << t.max_bits) - 1))
	entry := t.table[idx]
	if entry == 0xffff_ffff {
		return error('inflate: invalid Huffman code')
	}
	len_ := int(entry & 0x1f)
	if r.nbits < len_ {
		return error('inflate: unexpected end of stream')
	}
	sym := entry >> 5
	r.bits >>= u32(len_)
	r.nbits -= len_
	return sym
}

// inf_block decodes one Huffman block into out starting at olen and returns
// the new write index. Every write is bounds-checked against out.len (the
// caller pre-sized it to the expected decompressed size).
@[direct_array_access]
fn inf_block(mut r InfBitReader, mut out []u8, olen0 int, ll InfHuffTree, dist InfHuffTree) !int {
	mut olen := olen0
	for {
		sym := r.huff_decode(ll)!
		if sym == 256 {
			break
		}
		if sym < 256 {
			if olen >= out.len {
				return error('inflate: output overflow')
			}
			out[olen] = u8(sym)
			olen++
		} else {
			li := int(sym) - 257
			if li < 0 || li >= inf_length_bases.len {
				return error('inflate: invalid length symbol ${sym}')
			}
			length := inf_length_bases[li] + int(r.read_bits(inf_length_extra_bits[li])!)
			dsym := r.huff_decode(dist)!
			di := int(dsym)
			if di >= inf_dist_bases.len {
				return error('inflate: invalid distance symbol ${dsym}')
			}
			distance := inf_dist_bases[di] + int(r.read_bits(inf_dist_extra_bits[di])!)
			if distance > olen {
				return error('inflate: distance past output start')
			}
			if olen + length > out.len {
				return error('inflate: output overflow')
			}
			base := olen - distance
			// Byte-by-byte: distance may be smaller than length (overlapping
			// copy), same semantics as the stdlib `out << out[base + i]`.
			for i in 0 .. length {
				out[olen] = out[base + i]
				olen++
			}
		}
	}
	return olen
}

// inf_read_lengths decodes the dynamic-block code lengths directly into the
// two pre-sized length arrays (positions < hlit go to ll_lens, the rest to
// d_lens). Overshoot past hlit+hdist from repeat codes is discarded, which
// matches the stdlib's `all_lens[..hlit]` / `[hlit..]` slicing.
@[direct_array_access]
fn inf_read_lengths(mut r InfBitReader, cl_tree InfHuffTree, hlit int, hdist int, mut ll_lens []int,
	mut d_lens []int) ! {
	mut n := 0
	mut last := 0
	for n < hlit + hdist {
		sym := r.huff_decode(cl_tree)!
		if sym <= 15 {
			if n < hlit {
				ll_lens[n] = int(sym)
			} else {
				d_lens[n - hlit] = int(sym)
			}
			last = int(sym)
			n++
		} else if sym == 16 {
			if n == 0 {
				return error('inflate: repeat with empty history')
			}
			rep := int(r.read_bits(2)!) + 3
			for _ in 0 .. rep {
				if n < hlit {
					ll_lens[n] = last
				} else if n - hlit < hdist {
					d_lens[n - hlit] = last
				}
				n++
			}
		} else if sym == 17 {
			rep := int(r.read_bits(3)!) + 3
			for _ in 0 .. rep {
				if n < hlit {
					ll_lens[n] = 0
				} else if n - hlit < hdist {
					d_lens[n - hlit] = 0
				}
				n++
			}
		} else if sym == 18 {
			rep := int(r.read_bits(7)!) + 11
			for _ in 0 .. rep {
				if n < hlit {
					ll_lens[n] = 0
				} else if n - hlit < hdist {
					d_lens[n - hlit] = 0
				}
				n++
			}
		} else {
			return error('inflate: bad code length symbol')
		}
	}
}

@[direct_array_access]
fn inf_dynamic_block(mut r InfBitReader, mut out []u8, olen0 int) !int {
	mut olen := olen0
	hlit := int(r.read_bits(5)!) + 257
	hdist := int(r.read_bits(5)!) + 1
	hclen := int(r.read_bits(4)!) + 4
	mut cl_lens := []int{len: 19}
	defer {
		unsafe { cl_lens.free() }
	}
	for i in 0 .. hclen {
		cl_lens[inf_cl_order[i]] = int(r.read_bits(3)!)
	}
	mut cl_tree := inf_build_tree(cl_lens)!
	defer {
		cl_tree.free()
	}
	mut ll_lens := []int{len: hlit}
	defer {
		unsafe { ll_lens.free() }
	}
	mut d_lens := []int{len: hdist}
	defer {
		unsafe { d_lens.free() }
	}
	inf_read_lengths(mut r, cl_tree, hlit, hdist, mut ll_lens, mut d_lens)!
	mut ll_tree := inf_build_tree(ll_lens)!
	defer {
		ll_tree.free()
	}
	mut d_tree := inf_build_tree(d_lens)!
	defer {
		d_tree.free()
	}
	return inf_block(mut r, mut out, olen, ll_tree, d_tree)!
}

// inflate_zlib inflates a RFC 1950 zlib stream into a pre-allocated buffer
// of exactly `expected` bytes and validates the adler32 trailer. Replaces
// compress.zlib.decompress (see the section header for why).
fn inflate_zlib(data []u8, expected int) ![]u8 {
	if data.len < 6 {
		return error('invalid zlib stream: too short')
	}
	if data[0] & 0x0f != 8 {
		return error('invalid zlib stream: unsupported compression method')
	}
	if (u32(data[0]) * 256 + u32(data[1])) % 31 != 0 {
		return error('invalid zlib stream: bad header checksum')
	}
	if data[1] & 0x20 != 0 {
		return error('invalid zlib stream: preset dictionary not supported')
	}
	payload := data[2..data.len - 4]
	expected_adler := be_u32(data, data.len - 4)
	mut out := []u8{len: expected}
	mut keep_out := false
	defer {
		if !keep_out {
			unsafe { out.free() }
		}
	}
	mut r := InfBitReader{ buf: payload }
	mut ll_lens := []int{len: 288}
	defer {
		unsafe { ll_lens.free() }
	}
	inf_fixed_lengths(mut ll_lens)
	mut fixed_ll := inf_build_tree(ll_lens)!
	defer {
		fixed_ll.free()
	}
	mut d_lens := []int{len: 32, init: 5}
	defer {
		unsafe { d_lens.free() }
	}
	mut fixed_d := inf_build_tree(d_lens)!
	defer {
		fixed_d.free()
	}
	mut olen := 0
	for {
		bfinal := r.read_bits(1)!
		btype := r.read_bits(2)!
		match btype {
			0 {
				r.align_byte()
				len_ := int(r.read_byte_raw()!) | (int(u32(r.read_byte_raw()!) << 8))
				nlen := int(r.read_byte_raw()!) | (int(u32(r.read_byte_raw()!) << 8))
				if len_ & 0xffff != (~nlen) & 0xffff {
					return error('inflate: bad stored block length')
				}
				if olen + len_ > out.len {
					return error('inflate: output overflow')
				}
				for i in 0 .. len_ {
					out[olen + i] = r.read_byte_raw()!
				}
				olen += len_
			}
			1 {
				olen = inf_block(mut r, mut out, olen, fixed_ll, fixed_d)!
			}
			2 {
				olen = inf_dynamic_block(mut r, mut out, olen)!
			}
			else {
				return error('inflate: reserved block type')
			}
		}
		if bfinal == 1 {
			break
		}
	}
	consumed := r.pos - (r.nbits >> 3)
	if consumed != payload.len {
		return error('invalid zlib stream: trailing data before adler32')
	}
	if olen != expected {
		return error('inflate: decompressed to ${olen} bytes, expected ${expected}')
	}
	if adler32.sum(out) != expected_adler {
		return error('invalid zlib stream: adler32 mismatch')
	}
	keep_out = true
	return out
}

fn decompress_bounded(data []u8, expected int, label string) ![]u8 {
	raw := inflate_zlib(data, expected) or {
		return error('Failed to decompress ${label}: ${err}')
	}
	return raw
}

// ============================================================================
// Filtering
// ============================================================================

// apply_filter reconstructs one scanline in place. `line` already holds the
// raw filtered bytes for the current row; `prev` holds the previous
// reconstructed row (zeroed for the first row). This mirrors the per-row
// branch of the old unfilter_scanlines but writes into caller-owned arena
// buffers instead of allocating a clone per row.
@[direct_array_access]
fn apply_filter(filter_type int, mut line []u8, prev []u8, bpp int, scan_bytes int) ! {
	if filter_type == filter_none {
		return
	} else if filter_type == filter_sub {
		for i in bpp .. scan_bytes {
			line[i] = u8((int(line[i]) + int(line[i - bpp])) & 0xff)
		}
	} else if filter_type == filter_up {
		for i in 0 .. scan_bytes {
			line[i] = u8((int(line[i]) + int(prev[i])) & 0xff)
		}
	} else if filter_type == filter_average {
		for i in 0 .. scan_bytes {
			a := if i >= bpp { int(line[i - bpp]) } else { 0 }
			line[i] = u8((int(line[i]) + ((a + int(prev[i])) >> 1)) & 0xff)
		}
	} else if filter_type == filter_paeth {
		for i in 0 .. scan_bytes {
			a := if i >= bpp { int(line[i - bpp]) } else { 0 }
			b := int(prev[i])
			c := if i >= bpp { int(prev[i - bpp]) } else { 0 }
			p := a + b - c
			pa := iabs(p - a)
			pb := iabs(p - b)
			pc := iabs(p - c)
			mut pred := 0
			if pa <= pb && pa <= pc {
				pred = a
			} else if pb <= pc {
				pred = b
			} else {
				pred = c
			}
			line[i] = u8((int(line[i]) + pred) & 0xff)
		}
	} else {
		return error('Unknown PNG filter type: ${filter_type}')
	}
}

// ============================================================================
// Sample unpacking + color conversion
// ============================================================================

// unpack_into decodes one scanline's packed samples into the caller-provided
// `out` buffer (which must have len == width * channels). Mirrors the old
// unpack_scanline but writes into an arena buffer instead of allocating.
@[direct_array_access]
fn unpack_into(line []u8, width int, channels int, bit_depth int, mut out []int) ! {
	total := width * channels
	if out.len != total {
		return error('unpack_into: out.len ${out.len} != width*channels ${total}')
	}
	if bit_depth == 8 {
		if line.len < total {
			return error('Scanline too short for 8-bit samples')
		}
		for i in 0 .. total {
			out[i] = int(line[i])
		}
		return
	}
	if bit_depth == 16 {
		exp := total * 2
		if line.len < exp {
			return error('Scanline too short for 16-bit samples')
		}
		for i in 0 .. total {
			out[i] = (int(line[i * 2]) << 8) | int(line[i * 2 + 1])
		}
		return
	}
	mask := (1 << bit_depth) - 1
	mut bitpos := 0
	for i in 0 .. total {
		byte_index := bitpos >> 3
		if byte_index >= line.len {
			return error('Scanline too short for packed samples')
		}
		shift := 8 - bit_depth - (bitpos & 7)
		out[i] = (int(line[byte_index]) >> shift) & mask
		bitpos += bit_depth
	}
}

// samples_to_rgba_into expands one scanline of samples into RGBA written into
// the caller-provided `out` buffer (len == (samples.len / channels) * 4).
// Mirrors the old samples_to_rgba but writes into an arena buffer instead of
// growing a fresh array with `<<`.
@[direct_array_access]
fn (d &PngDecoder) samples_to_rgba_into(samples []int, mut out []int, dst_off int) ! {
	channels := d.channels()
	pixels := samples.len / channels
	if dst_off < 0 || dst_off + pixels * 4 > out.len {
		return error('samples_to_rgba_into: dst_off ${dst_off} + pixels*4 ${pixels * 4} > out.len ${out.len}')
	}
	if d.color_type == color_gray {
		mut transparent := -1
		mut has_tr := false
		if d.trns.len > 0 {
			transparent = d.trns[0]
			has_tr = true
		}
		ms := d.max_sample()
		for i in 0 .. pixels {
			value := samples[i]
			alpha := if has_tr && value == transparent { 0 } else { ms }
			out[dst_off + i * 4] = value
			out[dst_off + i * 4 + 1] = value
			out[dst_off + i * 4 + 2] = value
			out[dst_off + i * 4 + 3] = alpha
		}
	} else if d.color_type == color_rgb {
		mut has_tr := d.trns.len == 3
		ms := d.max_sample()
		for i in 0 .. pixels {
			r := samples[i * 3]
			g := samples[i * 3 + 1]
			b := samples[i * 3 + 2]
			alpha := if has_tr && r == d.trns[0] && g == d.trns[1] && b == d.trns[2] {
				0
			} else {
				ms
			}
			out[dst_off + i * 4] = r
			out[dst_off + i * 4 + 1] = g
			out[dst_off + i * 4 + 2] = b
			out[dst_off + i * 4 + 3] = alpha
		}
	} else if d.color_type == color_palette {
		if d.palette.len == 0 {
			return error('Palette image missing PLTE chunk')
		}
		npal := d.palette.len / 3
		for i in 0 .. pixels {
			index := samples[i]
			if index >= npal {
				return error('Palette index ${index} out of range')
			}
			r := int(d.palette[index * 3])
			g := int(d.palette[index * 3 + 1])
			b := int(d.palette[index * 3 + 2])
			alpha := if index < d.trns.len { d.trns[index] } else { 255 }
			out[dst_off + i * 4] = r
			out[dst_off + i * 4 + 1] = g
			out[dst_off + i * 4 + 2] = b
			out[dst_off + i * 4 + 3] = alpha
		}
	} else if d.color_type == color_gray_alpha {
		for i in 0 .. pixels {
			gray := samples[i * 2]
			alpha := samples[i * 2 + 1]
			out[dst_off + i * 4] = gray
			out[dst_off + i * 4 + 1] = gray
			out[dst_off + i * 4 + 2] = gray
			out[dst_off + i * 4 + 3] = alpha
		}
	} else if d.color_type == color_rgba {
		for i in 0 .. pixels {
			out[dst_off + i * 4] = samples[i * 4]
			out[dst_off + i * 4 + 1] = samples[i * 4 + 1]
			out[dst_off + i * 4 + 2] = samples[i * 4 + 2]
			out[dst_off + i * 4 + 3] = samples[i * 4 + 3]
		}
	} else {
		return error('Unsupported PNG color type: ${d.color_type}')
	}
}

// ============================================================================
// Pass decoding
// ============================================================================

// decode_rgba_from_data reconstructs the native RGBA buffer for one image
// (static or a single APNG frame) from the decompressed `raw` stream. The
// caller supplies `rgba` (len == width*height*4) carved from the arena: the
// persistent `ints` region for a static image, or `frame_scratch` for an
// APNG frame. All per-row scratch comes from the arena too, so nothing here
// allocates on the heap. The unfilter runs in place inside `raw` (a fresh
// inflate buffer the caller owns): the previous reconstructed row is simply
// the previous row's region of `raw`, so no per-row copy or ping-pong
// buffers are needed (P1).
@[direct_array_access]
fn (mut d PngDecoder) decode_rgba_from_data(mut raw []u8, width int, height int, passes []Pass,
	mut rgba []int) ! {
	if rgba.len != width * height * 4 {
		return error('decode_rgba_from_data: rgba.len ${rgba.len} != width*height*4 ${width * height * 4}')
	}
	mut pos := 0
	for p in passes {
		pw, ph := pass_dimensions_for(width, height, p.xstart, p.ystart, p.xstep, p.ystep)
		if pw == 0 || ph == 0 {
			continue
		}
		scan_bytes := d.scan_bytes_for_width(pw)
		expected := ph * (scan_bytes + 1)
		if pos + expected > raw.len {
			return error('IDAT data too short: expected ${expected} bytes for pass at (${p.xstart}, ${p.ystart})')
		}
		pass_raw := unsafe { raw[pos..pos + expected] }
		pos += expected

		bpp := d.bytes_per_pixel()
		channels := d.channels()
		contiguous := p.xstep == 1 && p.ystep == 1
		// The first reconstructed row of a pass filters against a zeroed
		// previous row; later rows filter against the previous row's region
		// of pass_raw (already reconstructed in place).
		d.arena.reset_bytes()
		mut zeros := d.arena.alloc_bytes(scan_bytes)!
		for i in 0 .. scan_bytes {
			zeros[i] = 0
		}
		mut ppos := 0
		mut prev_pos := -1
		for py in 0 .. ph {
			filter_type := int(pass_raw[ppos])
			ppos++
			mut cur := unsafe { pass_raw[ppos..ppos + scan_bytes] }
			prev := if prev_pos < 0 { zeros } else { pass_raw[prev_pos..prev_pos + scan_bytes] }
			ppos += scan_bytes
			apply_filter(filter_type, mut cur, prev, bpp, scan_bytes)!
			prev_pos = ppos - scan_bytes

			// Per-line scratch: samples (and pass_rgba for interlaced
			// passes), reused every row.
			d.arena.reset_scratch()
			mut samples := d.arena.alloc_scratch(pw * channels)!
			unpack_into(cur, pw, channels, d.bit_depth, mut samples)!

			y := p.ystart + py * p.ystep
			if contiguous {
				// Non-interlaced: expand samples straight into the
				// destination row of `rgba`, skipping the pass_rgba
				// scratch and the scatter pass entirely.
				d.samples_to_rgba_into(samples, mut rgba, y * width * 4)!
			} else {
				mut pass_rgba := d.arena.alloc_scratch(pw * 4)!
				d.samples_to_rgba_into(samples, mut pass_rgba, 0)!
				for px in 0 .. pw {
					x := p.xstart + px * p.xstep
					dst := (y * width + x) * 4
					src := px * 4
					rgba[dst] = pass_rgba[src]
					rgba[dst + 1] = pass_rgba[src + 1]
					rgba[dst + 2] = pass_rgba[src + 2]
					rgba[dst + 3] = pass_rgba[src + 3]
				}
			}
		}
	}
	if pos != raw.len {
		return error('IDAT data has ${raw.len - pos} extra bytes')
	}
}

fn (d &PngDecoder) passes() []Pass {
	if d.interlace_method == interlace_none {
		return [Pass{ xstart: 0, ystart: 0, xstep: 1, ystep: 1 }]
	}
	return adam7_passes()
}

fn (mut d PngDecoder) decode_static_rgba() ![]int {
	expected := d.expected_raw_size_for(d.width, d.height)
	mut raw := decompress_bounded(d.idat, expected, 'IDAT')!
	mut rgba := d.arena.alloc_ints(d.width * d.height * 4)!
	passes := d.passes()
	d.decode_rgba_from_data(mut raw, d.width, d.height, passes, mut rgba) or {
		unsafe { passes.free() }
		unsafe { raw.free() }
		return err
	}
	unsafe { passes.free() }
	unsafe { raw.free() }
	return rgba
}

fn (mut d PngDecoder) decode_frame_rgba(frame &PngFrame) ![]int {
	expected := d.expected_raw_size_for(frame.width, frame.height)
	mut raw := decompress_bounded(frame.data, expected, 'frame data')!
	d.arena.reset_frame_scratch()
	mut rgba := d.arena.alloc_frame_scratch(frame.width * frame.height * 4)!
	passes := d.passes()
	d.decode_rgba_from_data(mut raw, frame.width, frame.height, passes, mut rgba) or {
		unsafe { passes.free() }
		unsafe { raw.free() }
		return err
	}
	unsafe { passes.free() }
	unsafe { raw.free() }
	return rgba
}

// ============================================================================
// Color correction
// ============================================================================

fn mat3_mul(a [3][3]f64, b [3][3]f64) [3][3]f64 {
	mut out := [3][3]f64{}
	for i in 0 .. 3 {
		for j in 0 .. 3 {
			out[i][j] = a[i][0] * b[0][j] + a[i][1] * b[1][j] + a[i][2] * b[2][j]
		}
	}
	return out
}

fn mat3_vec_mul(m [3][3]f64, v [3]f64) [3]f64 {
	mut out := [3]f64{}
	for i in 0 .. 3 {
		out[i] = m[i][0] * v[0] + m[i][1] * v[1] + m[i][2] * v[2]
	}
	return out
}

fn mat3_inverse(m [3][3]f64) ![3][3]f64 {
	a := m[0][0]
	b := m[0][1]
	c := m[0][2]
	dd := m[1][0]
	e := m[1][1]
	f := m[1][2]
	g := m[2][0]
	h := m[2][1]
	i := m[2][2]
	det := a * (e * i - f * h) - b * (dd * i - f * g) + c * (dd * h - e * g)
	if fabs(det) < 1e-12 {
		return error('Cannot invert singular 3x3 matrix')
	}
	inv := 1.0 / det
	return [
		[(e * i - f * h) * inv, (c * h - b * i) * inv, (b * f - c * e) * inv],
		[(f * g - dd * i) * inv, (a * i - c * g) * inv, (c * dd - a * f) * inv],
		[(dd * h - e * g) * inv, (b * g - a * h) * inv, (a * e - b * dd) * inv],
	]
}

fn white_xyz(coords [8]f64) [3]f64 {
	x := coords[0]
	y := coords[1]
	return [x / y, 1.0, (1.0 - x - y) / y]
}

fn rgb_to_xyz_matrix(coords [8]f64) ![3][3]f64 {
	rx := coords[2]
	ry := coords[3]
	gx := coords[4]
	gy := coords[5]
	bx := coords[6]
	by := coords[7]
	rxv := rx / ry
	gxv := gx / gy
	bxv := bx / by
	mut unscaled := [3][3]f64{}
	unscaled[0][0] = rxv
	unscaled[0][1] = gxv
	unscaled[0][2] = bxv
	unscaled[1][0] = 1.0
	unscaled[1][1] = 1.0
	unscaled[1][2] = 1.0
	unscaled[2][0] = (1.0 - rx - ry) / ry
	unscaled[2][1] = (1.0 - gx - gy) / gy
	unscaled[2][2] = (1.0 - bx - by) / by
	y_values := mat3_vec_mul(mat3_inverse(unscaled)!, white_xyz(coords))
	mut out := [3][3]f64{}
	for i in 0 .. 3 {
		for j in 0 .. 3 {
			out[i][j] = unscaled[i][j] * y_values[j]
		}
	}
	return out
}

fn chromatic_adaptation(src_white [3]f64, dst_white [3]f64) ![3][3]f64 {
	bmat := bradford_mat()
	src_cone := mat3_vec_mul(bmat, src_white)
	dst_cone := mat3_vec_mul(bmat, dst_white)
	for v in src_cone {
		if fabs(v) < 1e-12 {
			return error('Cannot adapt from degenerate white point')
		}
	}
	scale := [dst_cone[0] / src_cone[0], dst_cone[1] / src_cone[1], dst_cone[2] / src_cone[2]]
	mut diag := [3][3]f64{}
	diag[0][0] = scale[0]
	diag[1][1] = scale[1]
	diag[2][2] = scale[2]
	return mat3_mul(mat3_mul(mat3_inverse(bmat)!, diag), bmat)
}

fn coords_are_close(a [8]f64, b [8]f64) bool {
	for i in 0 .. 8 {
		if fabs(a[i] - b[i]) > 1e-4 {
			return false
		}
	}
	return true
}

fn source_to_srgb_matrix(coords [8]f64) ![3][3]f64 {
	if coords_are_close(coords, srgb_chrm()) {
		return [
			[1.0, 0.0, 0.0],
			[0.0, 1.0, 0.0],
			[0.0, 0.0, 1.0],
		]
	}
	src_rgb_to_xyz := rgb_to_xyz_matrix(coords)!
	adapt := chromatic_adaptation(white_xyz(coords), white_xyz(srgb_chrm()))!
	return mat3_mul(mat3_mul(srgb_xyz_to_rgb_mat(), adapt), src_rgb_to_xyz)
}

fn (d &PngDecoder) apply_gamma_correction(rgba []int, target_gamma f64) ![]int {
	if target_gamma <= 0.0 {
		return error('target_gamma must be positive')
	}
	if !d.has_gamma {
		return rgba.clone()
	}
	mut out := rgba.clone()
	ms := f64(d.max_sample())
	if ms <= 0.0 {
		return out
	}
	g := d.gamma
	mut i := 0
	for i < out.len {
		for k in 0 .. 3 {
			value := f64(out[i + k]) / ms
			linear := value ** g
			corrected := (linear ** (1.0 / target_gamma)) * ms
			out[i + k] = int(round_half_even(clampf(corrected, 0.0, ms)))
		}
		i += 4
	}
	return out
}

fn (d &PngDecoder) apply_color_management(rgba []int, target_gamma f64) ![]int {
	if target_gamma <= 0.0 {
		return error('target_gamma must be positive')
	}
	if !d.has_chrm {
		return d.apply_gamma_correction(rgba, target_gamma)!
	}
	if !d.has_gamma {
		return error('cHRM color management requires gAMA chunk')
	}
	matrix := source_to_srgb_matrix(d.chrm)!
	mut out := rgba.clone()
	ms := f64(d.max_sample())
	if ms <= 0.0 {
		return out
	}
	g := d.gamma
	mut i := 0
	for i < out.len {
		mut linear := [3]f64{}
		for k in 0 .. 3 {
			linear[k] = (f64(out[i + k]) / ms) ** g
		}
		srgb_linear := mat3_vec_mul(matrix, linear)
		for k in 0 .. 3 {
			corrected := (clampf(srgb_linear[k], 0.0, 1.0) ** (1.0 / target_gamma)) * ms
			out[i + k] = int(round_half_even(clampf(corrected, 0.0, ms)))
		}
		i += 4
	}
	return out
}

fn (d &PngDecoder) apply_color_correction(rgba []int, target_gamma f64) ![]int {
	if d.has_chrm {
		return d.apply_color_management(rgba, target_gamma)!
	}
	return d.apply_gamma_correction(rgba, target_gamma)!
}

// ============================================================================
// APNG compositing
// ============================================================================

fn (mut d PngDecoder) composite_frame(mut canvas []int, frame_rgba []int, frame &PngFrame) ! {
	ms := f64(d.max_sample())
	if ms <= 0.0 {
		return
	}
	mut use_linear := false
	mut inv_gamma := 1.0
	if d.has_gamma {
		g := d.gamma
		if fabs(g - 1.0) > 1e-12 {
			use_linear = true
			inv_gamma = 1.0 / g
		}
	}
	g := d.gamma
	for fy in 0 .. frame.height {
		y := frame.y_offset + fy
		src_row := fy * frame.width * 4
		for fx in 0 .. frame.width {
			x := frame.x_offset + fx
			dst := (y * d.width + x) * 4
			src := src_row + fx * 4
			if frame.blend_op == apng_blend_source {
				canvas[dst] = frame_rgba[src]
				canvas[dst + 1] = frame_rgba[src + 1]
				canvas[dst + 2] = frame_rgba[src + 2]
				canvas[dst + 3] = frame_rgba[src + 3]
			} else if frame.blend_op == apng_blend_over {
				sr := frame_rgba[src]
				sg := frame_rgba[src + 1]
				sb := frame_rgba[src + 2]
				sa := frame_rgba[src + 3]
				dr := canvas[dst]
				dg := canvas[dst + 1]
				db := canvas[dst + 2]
				da := f64(canvas[dst + 3]) / ms
				fa := f64(sa) / ms
				out_a := fa + da * (1.0 - fa)
				if out_a <= 0.0 {
					canvas[dst] = 0
					canvas[dst + 1] = 0
					canvas[dst + 2] = 0
					canvas[dst + 3] = 0
				} else {
					mut out_r := 0.0
					mut out_g := 0.0
					mut out_b := 0.0
					if use_linear {
						sr_l := (f64(sr) / ms) ** g
						sg_l := (f64(sg) / ms) ** g
						sb_l := (f64(sb) / ms) ** g
						dr_l := (f64(dr) / ms) ** g
						dg_l := (f64(dg) / ms) ** g
						db_l := (f64(db) / ms) ** g
						out_r = ((sr_l * fa + dr_l * da * (1.0 - fa)) / out_a) ** inv_gamma * ms
						out_g = ((sg_l * fa + dg_l * da * (1.0 - fa)) / out_a) ** inv_gamma * ms
						out_b = ((sb_l * fa + db_l * da * (1.0 - fa)) / out_a) ** inv_gamma * ms
					} else {
						out_r = (f64(sr) * fa + f64(dr) * da * (1.0 - fa)) / out_a
						out_g = (f64(sg) * fa + f64(dg) * da * (1.0 - fa)) / out_a
						out_b = (f64(sb) * fa + f64(db) * da * (1.0 - fa)) / out_a
					}
					canvas[dst] = int(round_half_even(clampf(out_r, 0.0, ms)))
					canvas[dst + 1] = int(round_half_even(clampf(out_g, 0.0, ms)))
					canvas[dst + 2] = int(round_half_even(clampf(out_b, 0.0, ms)))
					canvas[dst + 3] = int(round_half_even(clampf(out_a * ms, 0.0, ms)))
				}
			} else {
				return error('Unknown APNG blend_op: ${frame.blend_op}')
			}
		}
	}
}

fn clear_region(mut canvas []int, frame &PngFrame, canvas_width int) {
	for fy in 0 .. frame.height {
		y := frame.y_offset + fy
		for fx in 0 .. frame.width {
			x := frame.x_offset + fx
			dst := (y * canvas_width + x) * 4
			canvas[dst] = 0
			canvas[dst + 1] = 0
			canvas[dst + 2] = 0
			canvas[dst + 3] = 0
		}
	}
}

// decode_animation composites the APNG (or the single frame of a static
// image) into a list of 8-bit RGBA frames. Each frame's native buffer is
// converted to an owned []u8 image inside the loop and the native is freed,
// so per-frame natives never accumulate. The persistent compositing canvas
// lives in the arena's `ints` region; the per-frame native lives in
// `frame_scratch` (reset each frame); the per-frame display snapshot is a
// short-lived heap clone freed right after conversion.
fn (mut d PngDecoder) decode_animation(apply_cc bool, target_gamma f64) ![]common.Frame {
	ms := d.max_sample()
	if !d.animation {
		mut display := d.decode_static_rgba()!
		mut display_heap := false
		if apply_cc {
			display = d.apply_color_correction(display, target_gamma)!
			display_heap = true
		}
		img := native_to_image(display, d.width, d.height, ms)
		if display_heap {
			unsafe { display.free() }
		}
		return [common.Frame{
			image:    img
			delay_ms: 10
			disposal: apng_dispose_none
		}]
	}

	mut canvas := d.arena.alloc_ints(d.width * d.height * 4)!
	// Pre-size the output frame list so `[]common.Frame` (whose element type
	// contains a slice) never grows under `-gc none`.
	mut out := []common.Frame{len: d.frames.len}
	for i, frame in d.frames {
		mut saved := []int{}
		if frame.dispose_op == apng_dispose_previous {
			saved = canvas.clone()
		}
		frame_rgba := d.decode_frame_rgba(frame)!
		d.composite_frame(mut canvas, frame_rgba, frame)!
		mut display := canvas.clone()
		if apply_cc {
			mut cc := d.apply_color_correction(canvas, target_gamma)!
			unsafe { display.free() }
			unsafe { display = cc }
		}
		img := native_to_image(display, d.width, d.height, ms)
		unsafe { display.free() }
		mut delay := 0
		if frame.delay_den != 0 {
			delay = int(round_half_even(f64(frame.delay_num) * 1000.0 / f64(frame.delay_den)))
		}
		out[i] = common.Frame{
			image:    img
			delay_ms: delay
			disposal: frame.dispose_op
		}
		if frame.dispose_op == apng_dispose_background {
			clear_region(mut canvas, frame, d.width)
		} else if frame.dispose_op == apng_dispose_previous {
			copy(mut canvas, saved)
			unsafe { saved.free() }
		}
	}
	return out
}

// ============================================================================
// Public API
// ============================================================================

@[direct_array_access]
fn native_to_image(native []int, width int, height int, max_sample int) common.Image {
	mut rgba := []u8{len: native.len}
	if max_sample == 255 {
		// 8-bit source: scale_to_8bit is the identity on in-range values,
		// so skip the per-pixel clamp/branch (P1 fast path).
		for i, v in native {
			rgba[i] = u8(v)
		}
	} else {
		for i, v in native {
			rgba[i] = scale_to_8bit(v, max_sample)
		}
	}
	return common.Image{
		width:  width
		height: height
		rgba:   rgba
	}
}

// cleanup releases every heap allocation the decoder made: the arena scratch
// and the parse-time owned arrays. It never touches `d.data` (caller-owned)
// or any image returned to the caller. Called on both the success and the
// error path of each public entry point.
fn (mut d PngDecoder) cleanup() {
	d.arena.free_all()
	d.free_owned()
}

// decode_first decodes and returns a single 8-bit RGBA image (the first
// frame for an APNG), freeing any extra frames it produced.
fn (mut d PngDecoder) decode_first(apply_cc bool, target_gamma f64) !common.Image {
	d.init_arena()!
	mut frames := d.decode_animation(apply_cc, target_gamma)!
	if frames.len == 0 {
		return error('decode produced no frames')
	}
	img := frames[0].image
	for i in 1 .. frames.len {
		unsafe { frames[i].image.rgba.free() }
	}
	unsafe { frames.free() }
	return img
}

// decode decodes a PNG (or the first frame of an APNG) into an 8-bit RGBA image.
pub fn decode(data []u8) !common.Image {
	mut d := PngDecoder{
		data: data
	}
	res := d.parse_and_decode(false, 2.2) or {
		d.cleanup()
		return err
	}
	d.cleanup()
	return res
}

// decode_color_corrected decodes with gAMA/cHRM color correction applied.
pub fn decode_color_corrected(data []u8, target_gamma f64) !common.Image {
	mut d := PngDecoder{
		data: data
	}
	res := d.parse_and_decode(true, target_gamma) or {
		d.cleanup()
		return err
	}
	d.cleanup()
	return res
}

// parse_and_decode runs the full parse + first-frame decode pipeline.
fn (mut d PngDecoder) parse_and_decode(apply_cc bool, target_gamma f64) !common.Image {
	d.parse()!
	return d.decode_first(apply_cc, target_gamma)!
}

// decode_animation decodes an APNG into a common.Animation of 8-bit frames.
pub fn decode_animation(data []u8) !common.Animation {
	mut d := PngDecoder{
		data: data
	}
	res := d.parse_and_decode_all() or {
		d.cleanup()
		return err
	}
	d.cleanup()
	return res
}

// parse_and_decode_all runs parse + full animation decode.
fn (mut d PngDecoder) parse_and_decode_all() !common.Animation {
	d.parse()!
	d.init_arena()!
	frames := d.decode_animation(false, 2.2)!
	return common.Animation{
		frames:     frames
		loop_count: d.num_plays
	}
}
