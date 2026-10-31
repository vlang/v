/**********************************************************************
*
* BMP image decoder
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module bmp

import image.decoders.common

// BMP constants
const bmp_signature = [u8(0x42), u8(0x4D)] // 'BM'
const bmp_file_header_size = 14
const bmp_core_header_size = 12
const bmp_info_header_size = 40
const bmp_v2_header_size = 52
const bmp_v3_header_size = 56
const bmp_v4_header_size = 108
const bmp_v5_header_size = 124

const bi_rgb = u32(0)
const bi_rle8 = u32(1)
const bi_rle4 = u32(2)
const bi_bitfields = u32(3)

// Arena allocator
// ============================================================================
//
// The Windows viewer build is effectively `-gc none` (no Boehm collector), so
// every array the decoder allocates must be released explicitly or it leaks for
// the life of the process. The RLE decode path allocates short-lived scratch (the
// row buffer and the per-run pixel buffer). Instead of threading `free()` through
// every error path, all scratch is carved out of one arena and released in a
// single `free_all()` at the end of each public decode call, on both the success
// and the error path. The returned 8-bit image ([]u8) is allocated OUTSIDE the
// arena and owned by the caller.
struct Arena {
mut:
	ints        []int
	ints_pos    int
	scratch     []int
	scratch_pos int
}

// alloc_ints returns a sub-slice of the persistent int region (the RLE row
// buffer).
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

// alloc_scratch returns a sub-slice of the per-run scratch region (pixels).
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

// reset_scratch rewinds the per-run scratch cursor so the pixels buffer is
// reused between RLE runs.
fn (mut a Arena) reset_scratch() {
	a.scratch_pos = 0
}

// free_all releases every backing buffer exactly once.
fn (mut a Arena) free_all() {
	unsafe {
		a.ints.free()
		a.scratch.free()
	}
	a.ints = []int{}
	a.scratch = []int{}
	a.ints_pos = 0
	a.scratch_pos = 0
}

// BmpDecoder parses a BMP file and decodes it to RGBA.
struct BmpDecoder {
mut:
	data              []u8
	pos               int
	parsed            bool
	file_size         u32
	reserved          u32
	pixel_data_offset int
	dib_header_size   int
	width             int
	height            int
	planes            int
	bit_count         int
	compression       u32
	image_size        u32
	colors_used       u32
	red_mask          u32
	green_mask        u32
	blue_mask         u32
	alpha_mask        u32
	palette           []u8 // flat, 4 bytes per entry
	top_down          bool
	arena             Arena
}

// decode decodes BMP bytes into a common.Image.
pub fn decode(data []u8) !common.Image {
	mut d := BmpDecoder{
		data: data
	}
	result := d.decode_image() or {
		d.cleanup()
		return err
	}
	d.cleanup()
	return result
}

// decode_image parses and decodes into a common.Image. The returned image's
// rgba buffer is caller-owned; all decoder scratch is released by cleanup().
fn (mut d BmpDecoder) decode_image() !common.Image {
	d.parse()!
	rgba := d.decode_rgba()!
	return common.Image{
		width:  d.width
		height: d.height
		rgba:   rgba
	}
}

// init_arena sizes the RLE scratch regions from the frame geometry. Called at
// the top of decode_rle (the uncompressed path needs no arena).
fn (mut d BmpDecoder) init_arena() !void {
	rows_len := d.width * d.height
	scratch_len := (if d.width > 256 { d.width } else { 256 }) + 2
	d.arena.ints = []int{len: rows_len}
	d.arena.scratch = []int{len: scratch_len}
}

// free_owned releases the parse-time owned arrays (the palette). The caller's
// `data` is never freed.
fn (mut d BmpDecoder) free_owned() {
	unsafe {
		d.palette.free()
	}
	d.palette = []u8{}
}

// cleanup releases the arena and all owned arrays. Called on both the success
// and the error path of decode().
fn (mut d BmpDecoder) cleanup() {
	d.arena.free_all()
	d.free_owned()
}

// --- little-endian read helpers (non-advancing) ---

fn read_u16_at(data []u8, off int) u16 {
	return u16(data[off]) | (u16(data[off + 1]) << 8)
}

fn read_u32_at(data []u8, off int) u32 {
	return u32(data[off]) | (u32(data[off + 1]) << 8) |
		(u32(data[off + 2]) << 16) | (u32(data[off + 3]) << 24)
}

// --- bit utilities ---

fn bit_length(x u32) int {
	mut n := 0
	mut v := x
	for v > 0 {
		v >>= 1
		n++
	}
	return n
}

fn clamp_i(v int, lo int, hi int) int {
	if v < lo {
		return lo
	}
	if v > hi {
		return hi
	}
	return v
}

// round_half_even rounds half to even (banker's rounding).
fn round_half_even(x f64) int {
	f := int(math_floor(x))
	diff := x - f64(f)
	if diff > 0.5 {
		return f + 1
	}
	if diff < 0.5 {
		return f
	}
	if f % 2 == 0 {
		return f
	}
	return f + 1
}

fn math_floor(x f64) f64 {
	// minimal floor without importing math in this module
	i := int(x)
	if f64(i) > x {
		return f64(i - 1)
	}
	return f64(i)
}

// extract_component extracts and scales one masked color component to 0..255.
fn extract_component(value u32, mask u32) int {
	if mask == 0 {
		return 0
	}
	lowbit := mask & (u32(0) - mask)
	shift := bit_length(lowbit) - 1
	width := bit_length(mask) - shift
	raw := (value & mask) >> shift
	mut result := 0
	if width == 8 {
		result = int(raw)
	} else if width == 5 {
		result = int((raw * 255 + 15) / 31)
	} else if width == 6 {
		result = int((raw * 255 + 31) / 63)
	} else {
		max_raw := (u32(1) << width) - 1
		if max_raw != 0 {
			result = round_half_even(f64(raw) * 255.0 / f64(max_raw))
		}
	}
	return clamp_i(result, 0, 255)
}

// --- reading with bounds checks ---

fn (mut d BmpDecoder) require(n int) !void {
	if d.pos + n > d.data.len {
		return error('Unexpected EOF in BMP data at offset ${d.pos}')
	}
}

fn (mut d BmpDecoder) read_u8() !u8 {
	d.require(1)!
	v := d.data[d.pos]
	d.pos += 1
	return v
}

fn (mut d BmpDecoder) read_u16() !u16 {
	d.require(2)!
	v := read_u16_at(d.data, d.pos)
	d.pos += 2
	return v
}

fn (mut d BmpDecoder) read_i16() !int {
	return int(i16(d.read_u16()!))
}

fn (mut d BmpDecoder) read_u32() !u32 {
	d.require(4)!
	v := read_u32_at(d.data, d.pos)
	d.pos += 4
	return v
}

fn (mut d BmpDecoder) read_i32() !int {
	return int(i32(d.read_u32()!))
}

// --- parsing ---

fn (mut d BmpDecoder) parse_signature() !void {
	d.require(2)!
	if !(d.data[d.pos] == 0x42 && d.data[d.pos + 1] == 0x4D) {
		return error('Invalid BMP signature')
	}
	d.pos += 2
}

fn (mut d BmpDecoder) parse_file_header() !void {
	d.file_size = d.read_u32()!
	d.reserved = d.read_u32()!
	d.pixel_data_offset = int(d.read_u32()!)
	if d.file_size < u32(bmp_file_header_size) {
		return error('BMP file size ${d.file_size} is too small')
	}
	if d.pixel_data_offset < bmp_file_header_size {
		return error('BMP pixel data offset ${d.pixel_data_offset} is too small')
	}
	if d.pixel_data_offset > d.data.len {
		return error('BMP pixel data offset ${d.pixel_data_offset} is beyond EOF')
	}
}

fn (mut d BmpDecoder) parse_dib_header() !void {
	dib_start := d.pos
	d.dib_header_size = int(d.read_u32()!)
	if d.dib_header_size == bmp_core_header_size {
		d.width = d.read_i16()!
		d.height = d.read_i16()!
		d.planes = int(d.read_u16()!)
		d.bit_count = int(d.read_u16()!)
		d.compression = bi_rgb
		d.colors_used = 0
	} else if d.dib_header_size >= bmp_info_header_size {
		d.width = d.read_i32()!
		d.height = d.read_i32()!
		d.planes = int(d.read_u16()!)
		d.bit_count = int(d.read_u16()!)
		d.compression = d.read_u32()!
		d.image_size = d.read_u32()!
		d.read_i32()! // x_ppm
		d.read_i32()! // y_ppm
		d.colors_used = d.read_u32()!
		d.read_u32()! // important_colors
		if d.dib_header_size >= bmp_v2_header_size {
			d.red_mask = d.read_u32()!
			d.green_mask = d.read_u32()!
			d.blue_mask = d.read_u32()!
		}
		if d.dib_header_size >= bmp_v3_header_size {
			d.alpha_mask = d.read_u32()!
		}
		if d.dib_header_size >= bmp_v4_header_size {
			d.read_u32()! // color_space
			d.require(36)!
			d.pos += 36
			d.read_u32()! // gamma_red
			d.read_u32()! // gamma_green
			d.read_u32()! // gamma_blue
		}
		if d.dib_header_size >= bmp_v5_header_size {
			d.read_u32()! // intent
			d.read_u32()! // profile_offset
			d.read_u32()! // profile_size
			d.read_u32()! // reserved2
		}
	} else {
		return error('Unsupported BMP DIB header size: ${d.dib_header_size}')
	}
	dib_end := dib_start + d.dib_header_size
	if d.pos > dib_end {
		return error('BMP DIB header fields exceed declared header size')
	}
	d.pos = dib_end
	d.top_down = d.height < 0
	if d.height < 0 {
		d.height = -d.height
	}
}

fn (mut d BmpDecoder) parse_palette() !void {
	if d.bit_count > 8 {
		d.palette = []u8{}
		return
	}
	palette_start := bmp_file_header_size + d.dib_header_size
	max_palette := 1 << d.bit_count
	mut palette_count := int(d.colors_used)
	if palette_count == 0 {
		palette_count = max_palette
	}
	if palette_count > max_palette {
		return error('BMP palette count ${palette_count} exceeds bit depth ${max_palette}')
	}
	entry_size := if d.dib_header_size == bmp_core_header_size { 3 } else { 4 }
	palette_end := palette_start + palette_count * entry_size
	if palette_end > d.pixel_data_offset {
		return error('BMP palette ends at ${palette_end}, past pixel data offset ${d.pixel_data_offset}')
	}
	mut pal := []u8{len: palette_count * 4}
	for i in 0 .. palette_count {
		off := palette_start + i * entry_size
		b := d.data[off]
		g := d.data[off + 1]
		r := d.data[off + 2]
		j := i * 4
		pal[j] = r
		pal[j + 1] = g
		pal[j + 2] = b
		pal[j + 3] = u8(255)
	}
	d.palette = pal
}

fn (mut d BmpDecoder) validate() !void {
	if d.width <= 0 {
		return error('Invalid BMP width: ${d.width}')
	}
	if d.height <= 0 {
		return error('Invalid BMP height: ${d.height}')
	}
	// Bound both the RGBA bytes and the integer scratch used by RLE.
	limit := max_int / int(sizeof(int)) - 2
	if d.width > limit / d.height || d.width > max_int / d.height / 4 {
		return error('BMP dimensions exceed the supported buffer size')
	}
	if d.planes < 1 {
		return error('Invalid BMP planes: ${d.planes}')
	}
	if d.bit_count != 1 && d.bit_count != 4 && d.bit_count != 8 && d.bit_count != 16
		&& d.bit_count != 24 && d.bit_count != 32 {
		return error('Unsupported BMP bit count: ${d.bit_count}')
	}
	if d.width > (max_int - 31) / d.bit_count {
		return error('BMP scanline exceeds the supported buffer size')
	}
	if d.compression != bi_rgb && d.compression != bi_rle8 && d.compression != bi_rle4
		&& d.compression != bi_bitfields {
		return error('Unsupported BMP compression method: ${d.compression}')
	}
	if d.compression == bi_bitfields {
		if d.bit_count != 16 && d.bit_count != 32 {
			return error('BI_BITFIELDS requires 16 or 32 bits, got ${d.bit_count}')
		}
		if d.red_mask == 0 || d.green_mask == 0 || d.blue_mask == 0 {
			return error('BI_BITFIELDS requires red, green, and blue masks')
		}
	}
	if d.compression == bi_rle8 && d.bit_count != 8 {
		return error('BI_RLE8 requires 8-bit pixels')
	}
	if d.compression == bi_rle4 && d.bit_count != 4 {
		return error('BI_RLE4 requires 4-bit pixels')
	}
	if d.bit_count <= 8 && d.palette.len == 0 {
		return error('Indexed BMP requires a palette')
	}
	mut used := u32(0)
	for mask in [4]u32[d.red_mask, d.green_mask, d.blue_mask, d.alpha_mask] {
		if mask & used != 0 {
			return error('Overlapping BMP color masks')
		}
		used |= mask
	}
	if d.bit_count < 32 && (used >> d.bit_count) != 0 {
		return error('BMP color mask exceeds bit depth')
	}
}

fn (mut d BmpDecoder) parse() !void {
	if d.parsed {
		return
	}
	d.parse_signature()!
	d.parse_file_header()!
	d.parse_dib_header()!
	d.parse_palette()!
	d.validate()!
	d.parsed = true
}

fn (d BmpDecoder) row_stride() int {
	bits_per_row := d.width * d.bit_count
	return ((bits_per_row + 31) / 32) * 4
}

// --- decoding ---

fn (mut d BmpDecoder) decode_rgba() ![]u8 {
	d.parse()!
	if d.compression == bi_rgb || d.compression == bi_bitfields {
		return d.decode_uncompressed()
	}
	if d.compression == bi_rle8 {
		return d.decode_rle(8)
	}
	if d.compression == bi_rle4 {
		return d.decode_rle(4)
	}
	return error('Unsupported BMP compression method: ${d.compression}')
}

fn (mut d BmpDecoder) decode_uncompressed() ![]u8 {
	stride := d.row_stride()
	if stride > (d.data.len - d.pixel_data_offset) / d.height {
		return error('BMP pixel data is truncated')
	}
	mut rgba := []u8{len: d.width * d.height * 4}
	mut oi := 0
	for out_y in 0 .. d.height {
		src_y := if d.top_down { out_y } else { d.height - 1 - out_y }
		row_start := d.pixel_data_offset + src_y * stride
		for x in 0 .. d.width {
			px := d.pixel_uncompressed(row_start, x)!
			rgba[oi] = px[0]
			rgba[oi + 1] = px[1]
			rgba[oi + 2] = px[2]
			rgba[oi + 3] = px[3]
			oi += 4
		}
	}
	return rgba
}

fn (mut d BmpDecoder) pixel_uncompressed(row_start int, x int) ![4]u8 {
	if d.bit_count == 24 {
		off := row_start + x * 3
		return [d.data[off + 2], d.data[off + 1], d.data[off], u8(255)]
	}
	if d.bit_count == 32 {
		off := row_start + x * 4
		if d.red_mask != 0 || d.green_mask != 0 || d.blue_mask != 0 || d.alpha_mask != 0 {
			value := read_u32_at(d.data, off)
			a := if d.alpha_mask != 0 { extract_component(value, d.alpha_mask) } else { 255 }
			return [u8(extract_component(value, d.red_mask)),
				u8(extract_component(value, d.green_mask)), u8(extract_component(value, d.blue_mask)),
				u8(a)]
		}
		return [d.data[off + 2], d.data[off + 1], d.data[off], u8(255)]
	}
	if d.bit_count == 16 {
		off := row_start + x * 2
		value := u32(read_u16_at(d.data, off))
		if d.red_mask != 0 || d.green_mask != 0 || d.blue_mask != 0 {
			a := if d.alpha_mask != 0 { extract_component(value, d.alpha_mask) } else { 255 }
			return [u8(extract_component(value, d.red_mask)),
				u8(extract_component(value, d.green_mask)), u8(extract_component(value, d.blue_mask)),
				u8(a)]
		}
		return [u8(extract_component(value, 0x7C00)), u8(extract_component(value, 0x03E0)),
			u8(extract_component(value, 0x001F)), u8(255)]
	}
	mut index := 0
	if d.bit_count == 8 {
		index = int(d.data[row_start + x])
	} else if d.bit_count == 4 {
		byte := d.data[row_start + (x >> 1)]
		index = if x % 2 == 0 { int(byte >> 4) } else { int(byte & 0x0F) }
	} else if d.bit_count == 1 {
		byte := d.data[row_start + (x >> 3)]
		index = int((byte >> (7 - (x % 8))) & 1)
	} else {
		return error('Unsupported BMP bit count: ${d.bit_count}')
	}
	if index * 4 >= d.palette.len {
		return error('BMP palette index ${index} is out of range')
	}
	p := index * 4
	return [d.palette[p], d.palette[p + 1], d.palette[p + 2], d.palette[p + 3]]
}

fn (mut d BmpDecoder) rle_fill(mut rows []int, y int, x int, pixels []int) !int {
	mut cx := x
	for pixel in pixels {
		if y >= d.height {
			return error('BMP RLE run exceeds image height')
		}
		if cx >= d.width {
			return error('BMP RLE run exceeds image width')
		}
		if pixel * 4 >= d.palette.len {
			return error('BMP RLE palette index ${pixel} is out of range')
		}
		rows[y * d.width + cx] = pixel
		cx += 1
	}
	return cx
}

fn (mut d BmpDecoder) decode_rle(bits int) ![]u8 {
	d.init_arena()!
	data := d.data
	mut pos := d.pixel_data_offset
	rows_len := d.width * d.height
	mut rows := d.arena.alloc_ints(rows_len)!
	for i in 0 .. rows_len {
		rows[i] = -1
	}
	mut x := 0
	mut y := 0
	for {
		if pos + 1 >= data.len {
			return error('Truncated BMP RLE data')
		}
		count := int(data[pos])
		value := int(data[pos + 1])
		pos += 2
		if count > 0 {
			d.arena.reset_scratch()
			mut pixels := d.arena.alloc_scratch(count)!
			if bits == 8 {
				for i in 0 .. count {
					pixels[i] = value
				}
			} else {
				for i in 0 .. count {
					if i % 2 == 0 {
						pixels[i] = (value >> 4) & 0x0F
					} else {
						pixels[i] = value & 0x0F
					}
				}
			}
			x = d.rle_fill(mut rows, y, x, pixels)!
			continue
		}
		if value == 0x00 {
			break
		}
		if value == 0x01 {
			x = 0
			y += 1
			if y > d.height {
				return error('BMP RLE end-of-line exceeds image height')
			}
			continue
		}
		if value == 0x02 {
			if pos + 1 >= data.len {
				return error('Truncated BMP RLE delta')
			}
			dx := int(i8(data[pos]))
			dy := int(i8(data[pos + 1]))
			pos += 2
			x += dx
			y += dy
			if x < 0 || x > d.width || y < 0 || y > d.height {
				return error('BMP RLE delta moves outside image')
			}
			continue
		}
		abs_count := value
		d.arena.reset_scratch()
		mut pixels := d.arena.alloc_scratch(abs_count)!
		if bits == 8 {
			literal_bytes := abs_count
			pad := literal_bytes % 2
			if pos + literal_bytes + pad > data.len {
				return error('Truncated BMP RLE8 absolute data')
			}
			for i in 0 .. literal_bytes {
				pixels[i] = int(data[pos + i])
			}
			pos += literal_bytes + pad
		} else {
			literal_bytes := (abs_count + 1) / 2
			pad := literal_bytes % 2
			if pos + literal_bytes + pad > data.len {
				return error('Truncated BMP RLE4 absolute data')
			}
			mut n := 0
			for i in 0 .. literal_bytes {
				b := data[pos + i]
				pixels[n] = int((b >> 4) & 0x0F)
				n++
				if n < abs_count {
					pixels[n] = int(b & 0x0F)
					n++
				}
			}
			pixels = pixels[0..n]
			pos += literal_bytes + pad
		}
		x = d.rle_fill(mut rows, y, x, pixels)!
	}
	// Write RGBA with a row-order mapping so the bottom-up reversal needs no
	// second array: for !top_down the decoded row 0 is the image bottom.
	mut rgba := []u8{len: d.width * d.height * 4}
	mut oi := 0
	for oy in 0 .. d.height {
		sy := if !d.top_down { d.height - 1 - oy } else { oy }
		base := sy * d.width
		for xx in 0 .. d.width {
			idx := rows[base + xx]
			if idx < 0 {
				return error('BMP RLE data did not fill the image')
			}
			if idx * 4 >= d.palette.len {
				return error('BMP RLE palette index out of range')
			}
			p := idx * 4
			rgba[oi] = d.palette[p]
			rgba[oi + 1] = d.palette[p + 1]
			rgba[oi + 2] = d.palette[p + 2]
			rgba[oi + 3] = d.palette[p + 3]
			oi += 4
		}
	}
	return rgba
}
