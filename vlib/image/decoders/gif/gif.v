/**********************************************************************
*
* GIF image and animation decoder
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module gif

import image.decoders.common

// ============================================================================
// Constants
// ============================================================================

const block_extension = 0x21
const block_image = 0x2c
const block_trailer = 0x3b

const ext_graphic_control = 0xf9
const ext_comment = 0xfe
const ext_application = 0xff

const gce_transparency_flag = 0x01
const gce_disposal_shift = 4
const gce_disposal_mask = 0x07

const image_local_color_table_flag = 0x80
const image_interlace_flag = 0x40

const lsd_global_color_table_flag = 0x80

const max_lzw_code_size = 12
const max_lzw_code = 1 << max_lzw_code_size

const dispose_unspecified = 0
const dispose_keep = 1
const dispose_background = 2
const dispose_previous = 3

// GIF interlace passes: (first row, row step).
fn interlace_start(pass int) int {
	match pass {
		0 { return 0 }
		1 { return 4 }
		2 { return 2 }
		else { return 1 }
	}
}

fn interlace_step(pass int) int {
	match pass {
		0 { return 8 }
		1 { return 8 }
		2 { return 4 }
		else { return 2 }
	}
}

// ============================================================================
// Helpers
// ============================================================================

fn imax(a int, b int) int {
	if a > b {
		return a
	}
	return b
}

fn imin(a int, b int) int {
	if a < b {
		return a
	}
	return b
}

// ============================================================================
// Arena
//
// A tiny bump allocator that owns all per-frame decode scratch (the copied
// LZW stream, the decoded index buffer, the interlace row order). The
// scratch is reset to a fixed base between frames, so the backing is
// allocated once (sized for the largest frame) and reused; `free_all()`
// releases it exactly once at the end of a decode. The caller-owned
// result (the returned RGBA) is allocated OUTSIDE the arena.
// ============================================================================

struct Arena {
mut:
	bytes []u8
	bpos  int
	ints  []int
	ipos  int
	// base offsets that reset() rewinds to (the tables live below base_i).
	base_b int
	base_i int
}

fn (mut a Arena) alloc_bytes(n int) ![]u8 {
	if n < 0 || a.bpos + n > a.bytes.len {
		return error('gif arena: bytes exhausted (${a.bpos}+${n} > ${a.bytes.len})')
	}
	unsafe {
		s := a.bytes[a.bpos..a.bpos + n]
		a.bpos += n
		return s
	}
}

fn (mut a Arena) alloc_ints(n int) ![]int {
	if n < 0 || a.ipos + n > a.ints.len {
		return error('gif arena: ints exhausted (${a.ipos}+${n} > ${a.ints.len})')
	}
	unsafe {
		s := a.ints[a.ipos..a.ipos + n]
		a.ipos += n
		return s
	}
}

// reset rewinds the bump cursors to the base, discarding the previous
// frame's scratch without touching the tables (which live below base_i).
fn (mut a Arena) reset() {
	a.bpos = a.base_b
	a.ipos = a.base_i
}

fn (mut a Arena) free_all() {
	unsafe {
		a.bytes.free()
		a.ints.free()
	}
}

// ============================================================================
// LZW decoder (GIF, LSB-first)
//
// The dictionary is stored as four flat int arrays instead of a [][]u8 of
// per-entry heap slices. A code `c` denotes a string whose last byte is
// suffix[c], whose previous entry is prefix[c] (-1 for a literal), whose
// length is length[c], and whose FIRST byte is first[c]. The first byte
// is cached because every new entry starts with its prefix's first byte
// (new = prev_string + one byte), so first[next] = first[prev] in O(1).
//
// Output is written straight into the caller's `out` buffer by walking
// the prefix chain backwards, so there is no per-entry allocation and no
// concat_u8 churn.
// ============================================================================

fn lzw_init(mut prefix []int, mut suffix []int, mut length []int, mut first []int, clear_code int) {
	for i in 0 .. clear_code {
		prefix[i] = -1
		suffix[i] = i
		length[i] = 1
		first[i] = i
	}
}

fn lzw_decode_into(data []u8, min_code_size int, expected_count int, mut prefix []int,
	mut suffix []int, mut length []int, mut first []int, mut out []u8) ! {
	if min_code_size < 2 || min_code_size > max_lzw_code_size {
		return error('Invalid GIF LZW minimum code size: ${min_code_size}')
	}
	clear_code := 1 << min_code_size
	eoi_code := clear_code + 1
	lzw_init(mut prefix, mut suffix, mut length, mut first, clear_code)

	mut br := common.new_bitreader_lsb(data)
	mut next_code := eoi_code + 1
	mut code_size := min_code_size + 1
	mut prev := -1
	mut w := 0

	for {
		code := int(br.read_bits(code_size)!)

		if code == clear_code {
			// The initial codes are unchanged; just rewind the cursor.
			next_code = eoi_code + 1
			code_size = min_code_size + 1
			prev = -1
			continue
		}
		if code == eoi_code {
			break
		}

		// Resolve the entry to emit as (base_code [+ one extra byte]).
		mut base_code := 0
		mut extra := -1
		if code < next_code {
			base_code = code
		} else if code == next_code && prev >= 0 {
			// KwKwK: the entry being defined right now = prev_string + first(prev).
			base_code = prev
			extra = first[prev]
		} else {
			return error('Invalid GIF LZW code ${code} (next_code ${next_code})')
		}

		// Guard against a malformed stream overrunning the output buffer.
		mut emit_len := length[base_code]
		if extra >= 0 {
			emit_len += 1
		}
		if w + emit_len > expected_count {
			return error('GIF LZW overflow: ${w}+${emit_len} > ${expected_count}')
		}

		// Define the new dictionary entry (when there is room).
		if prev >= 0 && next_code < max_lzw_code {
			ns := if extra >= 0 { extra } else { first[code] }
			prefix[next_code] = prev
			suffix[next_code] = ns
			length[next_code] = length[prev] + 1
			first[next_code] = first[prev]
			next_code++
			if next_code == (1 << code_size) && code_size < max_lzw_code_size {
				code_size++
			}
		}

		// Emit base_code's string by walking the prefix chain backwards.
		mut l := length[base_code]
		mut i := w + l - 1
		mut cc := base_code
		for l > 0 {
			out[i] = u8(suffix[cc])
			i--
			cc = prefix[cc]
			l--
		}
		w += length[base_code]
		if extra >= 0 {
			out[w] = u8(extra)
			w++
		}
		prev = code
	}

	if w != expected_count {
		return error('GIF LZW decoded ${w} pixels, expected ${expected_count}')
	}
}

// ============================================================================
// Data structures
// ============================================================================

struct Gce {
mut:
	present           bool
	transparency      bool
	transparent_index int
	delay             int
	disposal          int
}

// Rect is a clipped canvas rectangle (x0,y0 inclusive; x1,y1 exclusive).
struct Rect {
	x0 int
	y0 int
	x1 int
	y1 int
}

// Rgba is a solid fill color.
struct Rgba {
	r u8
	g u8
	b u8
	a u8
}

struct GifFrameRec {
	left              int
	top               int
	width             int
	height            int
	interlaced        bool
	lct_start         int // offset into the input of the local color table (0 if none)
	lct_len           int // byte length of the local color table (0 if none)
	min_code_size     int
	lzw_start         int // offset in the input of the LZW sub-block stream
	lzw_len           int // total decoded LZW byte count
	has_transparency  bool
	transparent_index int
	delay_ms          int
	disposal          int
}

struct GifDecoder {
mut:
	data               []u8
	pos                int
	parsed             bool
	version            string
	width              int
	height             int
	gct                []u8 // view into the input buffer (flat RGB triplets)
	bg_color_index     int
	pixel_aspect_ratio int
	frame_count        int
	frames             []GifFrameRec
	pending_gce        Gce
	loop_count         int
	// first-frame preview attributes
	frame_left        int
	frame_top         int
	frame_width       int
	frame_height      int
	interlaced        bool
	local_color_table []u8
	min_code_size     int
	has_transparency  bool
	transparent_index int
	// decode scratch (arena-backed)
	scratch    Arena
	lzw_prefix []int
	lzw_suffix []int
	lzw_length []int
	lzw_first  []int
}

// ============================================================================
// Byte readers
// ============================================================================

fn (d &GifDecoder) require(n int) ! {
	if d.pos + n > d.data.len {
		return error('Unexpected EOF in GIF data at offset ${d.pos}')
	}
}

fn (mut d GifDecoder) read_byte() !u8 {
	d.require(1)!
	v := d.data[d.pos]
	d.pos++
	return v
}

fn (mut d GifDecoder) read_uint16() !int {
	d.require(2)!
	v := int(d.data[d.pos]) | (int(d.data[d.pos + 1]) << 8)
	d.pos += 2
	return v
}

// ============================================================================
// Header / Logical Screen Descriptor / color tables
// ============================================================================

fn (mut d GifDecoder) parse_header() ! {
	d.require(6)!
	unsafe {
		magic := d.data[d.pos..d.pos + 6]
		if magic[0] == 0x47 && magic[1] == 0x49 && magic[2] == 0x46 && magic[3] == 0x38 {
			if magic[4] == 0x37 && magic[5] == 0x61 {
				d.version = '87a'
			} else if magic[4] == 0x39 && magic[5] == 0x61 {
				d.version = '89a'
			} else {
				return error('Invalid GIF version signature')
			}
		} else {
			return error('Invalid GIF signature')
		}
	}
	d.pos += 6
}

fn (mut d GifDecoder) parse_logical_screen_descriptor() ! {
	d.width = d.read_uint16()!
	d.height = d.read_uint16()!
	packed := int(d.read_byte()!)
	has_gct := (packed & lsd_global_color_table_flag) != 0
	gct_size := packed & 0x07
	gct_entries := 1 << (gct_size + 1)
	d.bg_color_index = int(d.read_byte()!)
	d.pixel_aspect_ratio = int(d.read_byte()!)

	if d.width <= 0 {
		return error('Invalid GIF width: ${d.width}')
	}
	if d.height <= 0 {
		return error('Invalid GIF height: ${d.height}')
	}
	if d.width > max_int / d.height / 4 {
		return error('GIF dimensions exceed the supported buffer size')
	}
	if has_gct {
		d.gct = d.color_table_view(gct_entries)!
	}
}

// color_table_view returns a zero-copy view of `count` RGB triplets.
fn (mut d GifDecoder) color_table_view(count int) ![]u8 {
	d.require(count * 3)!
	unsafe {
		v := d.data[d.pos..d.pos + count * 3]
		d.pos += count * 3
		return v
	}
}

// ============================================================================
// Block stream
// ============================================================================

// read_sub_blocks concatenates a length-prefixed sub-block stream into a
// fresh heap buffer (used for extension payloads, which are small and
// short-lived; the caller frees it).
fn (mut d GifDecoder) read_sub_blocks() ![]u8 {
	mut out := []u8{len: 0}
	for {
		count := int(d.read_byte()!)
		if count == 0 {
			break
		}
		d.require(count)!
		unsafe {
			out << d.data[d.pos..d.pos + count]
		}
		d.pos += count
	}
	return out
}

fn parse_graphic_control(payload []u8) !Gce {
	if payload.len < 4 {
		return error('GIF Graphic Control Extension too short')
	}
	packed := int(payload[0])
	delay := int(payload[1]) | (int(payload[2]) << 8)
	return Gce{
		present:           true
		transparency:      (packed & gce_transparency_flag) != 0
		transparent_index: int(payload[3])
		delay:             delay
		disposal:          (packed >> gce_disposal_shift) & gce_disposal_mask
	}
}

fn (mut d GifDecoder) parse_extension() ! {
	label := int(d.read_byte()!)
	mut payload := d.read_sub_blocks()!
	if label == ext_graphic_control {
		d.pending_gce = parse_graphic_control(payload)!
	} else if label == ext_application {
		d.parse_application(payload)
	}
	unsafe { payload.free() }
	// Comment and Plain Text extensions are parsed and skipped.
}

fn (mut d GifDecoder) parse_application(payload []u8) {
	if payload.len < 11 {
		return
	}
	mut is_netscape := true
	mut is_animext := true
	ns := 'NETSCAPE2.0'
	ae := 'ANIMEXTS1.0'
	for i in 0 .. 11 {
		if payload[i] != ns[i] {
			is_netscape = false
		}
		if payload[i] != ae[i] {
			is_animext = false
		}
	}
	if !is_netscape && !is_animext {
		return
	}
	if payload.len >= 14 && payload[11] == 0x01 {
		d.loop_count = int(payload[12]) | (int(payload[13]) << 8)
	}
}

fn (mut d GifDecoder) parse_image_descriptor() ! {
	left := d.read_uint16()!
	top := d.read_uint16()!
	width := d.read_uint16()!
	height := d.read_uint16()!
	packed := int(d.read_byte()!)
	lct_flag := (packed & image_local_color_table_flag) != 0
	interlace := (packed & image_interlace_flag) != 0
	lct_size := packed & 0x07
	mut lct_start := 0
	mut lct_len := 0
	if lct_flag {
		n := 1 << (lct_size + 1)
		d.require(n * 3)!
		lct_start = d.pos
		lct_len = n * 3
		d.pos += n * 3
	}
	min_code_size := int(d.read_byte()!)

	// Record the LZW sub-block stream as an offset + length into the input
	// instead of copying it now; the contiguous copy happens in the scratch
	// arena at composite time. This walk validates framing and advances pos.
	lzw_start := d.pos
	mut lzw_len := 0
	for {
		d.require(1)!
		count := int(d.data[d.pos])
		d.pos++
		if count == 0 {
			break
		}
		d.require(count)!
		lzw_len += count
		d.pos += count
	}

	gce := d.pending_gce
	has_transparency := gce.present && gce.transparency
	mut transparent_index := 0
	mut delay_ms := 0
	mut disposal := dispose_unspecified
	if gce.present {
		transparent_index = gce.transparent_index
		delay_ms = gce.delay * 10
		disposal = gce.disposal
	}

	frame := GifFrameRec{
		left:              left
		top:               top
		width:             width
		height:            height
		interlaced:        interlace
		lct_start:         lct_start
		lct_len:           lct_len
		min_code_size:     min_code_size
		lzw_start:         lzw_start
		lzw_len:           lzw_len
		has_transparency:  has_transparency
		transparent_index: transparent_index
		delay_ms:          delay_ms
		disposal:          disposal
	}
	d.frames << frame

	d.frame_count++
	if d.frame_count == 1 {
		d.frame_left = left
		d.frame_top = top
		d.frame_width = width
		d.frame_height = height
		d.interlaced = interlace
		d.local_color_table = unsafe { d.data[lct_start..lct_start + lct_len] }
		d.min_code_size = min_code_size
		d.has_transparency = has_transparency
		d.transparent_index = transparent_index
	}

	d.pending_gce = Gce{}
}

// count_image_descriptors walks the block stream from `start` counting
// image descriptors (0x2C), skipping extension blocks and sub-blocks.
// It mirrors walk_blocks' framing so the result is an exact upper bound
// on d.frames.len, letting parse() preallocate the array and avoid
// V's array-growth leak under `-gc none`. It stops (without error) on
// malformed data; the real walk_blocks reports the error.
fn count_image_descriptors(data []u8, start int) int {
	mut p := start
	mut n := 0
	for p < data.len {
		intro := int(data[p])
		p++
		if intro == block_trailer {
			break
		} else if intro == block_extension {
			p++ // extension label
			for p < data.len {
				c := int(data[p])
				p++
				if c == 0 {
					break
				}
				p += c
			}
		} else if intro == block_image {
			n++
			p += 9 // 4x uint16 + packed byte
			if p > data.len {
				break
			}
			packed := int(data[p - 1])
			if (packed & image_local_color_table_flag) != 0 {
				p += (1 << ((packed & 0x07) + 1)) * 3
			}
			p++ // min code size
			for p < data.len {
				c := int(data[p])
				p++
				if c == 0 {
					break
				}
				p += c
			}
		} else {
			break
		}
	}
	return n
}

fn (mut d GifDecoder) walk_blocks() ! {
	for d.pos < d.data.len {
		introducer := int(d.read_byte()!)
		if introducer == block_trailer {
			break
		} else if introducer == block_extension {
			d.parse_extension()!
		} else if introducer == block_image {
			d.parse_image_descriptor()!
		} else {
			return error('Unknown GIF block introducer: 0x${introducer:02X}')
		}
	}
}

fn (mut d GifDecoder) parse() ! {
	if d.parsed {
		return
	}
	d.parse_header()!
	d.parse_logical_screen_descriptor()!
	// Preallocate the frames array to the exact descriptor count so the
	// pushes in walk_blocks never grow it (avoids the -gc none growth leak).
	d.frames = []GifFrameRec{len: 0, cap: count_image_descriptors(d.data, d.pos)}
	d.walk_blocks()!
	if d.frame_count == 0 {
		return error('GIF contains no image descriptor')
	}
	d.parsed = true
}

// prepare_scratch sizes the arena for the largest frame and lays the four
// LZW tables at the bottom of the int region (below base_i, so they
// survive the per-frame reset).
fn (mut d GifDecoder) prepare_scratch() ! {
	mut max_idx := 0
	mut max_lzw := 0
	mut max_fh := 0
	for f in d.frames {
		if f.width <= 0 || f.height <= 0 || f.lzw_len > max_int - 1024
			|| f.width > (max_int - f.lzw_len - 1024) / f.height {
			return error('GIF frame exceeds the supported buffer size')
		}
		if f.width * f.height > max_idx {
			max_idx = f.width * f.height
		}
		if f.lzw_len > max_lzw {
			max_lzw = f.lzw_len
		}
		if f.height > max_fh {
			max_fh = f.height
		}
	}
	if max_idx > max_int - max_lzw - 1024 {
		return error('GIF frame exceeds the supported buffer size')
	}
	bytes_cap := max_idx + max_lzw + 1024
	ints_cap := 4 * max_lzw_code + max_fh + 1024
	d.scratch = Arena{
		bytes: []u8{len: bytes_cap}
		ints:  []int{len: ints_cap}
	}
	d.lzw_prefix = unsafe { d.scratch.ints[0..max_lzw_code] }
	d.lzw_suffix = unsafe { d.scratch.ints[max_lzw_code..2 * max_lzw_code] }
	d.lzw_length = unsafe { d.scratch.ints[2 * max_lzw_code..3 * max_lzw_code] }
	d.lzw_first = unsafe { d.scratch.ints[3 * max_lzw_code..4 * max_lzw_code] }
	d.scratch.base_b = 0
	d.scratch.base_i = 4 * max_lzw_code
}

// free_scratch releases the arena and the frames array. The caller-owned
// result (returned RGBA) lives outside these and is untouched.
fn (mut d GifDecoder) free_scratch() {
	d.scratch.free_all()
	unsafe {
		d.frames.free()
	}
}

// ============================================================================
// Compositing
// ============================================================================

// copy_sub_blocks copies the length-prefixed sub-block stream at `start`
// into `dst` (which was sized to the precomputed total).
fn copy_sub_blocks(data []u8, start int, mut dst []u8) {
	unsafe {
		mut sp := start
		mut dp := 0
		for {
			count := int(data[sp])
			sp++
			if count == 0 {
				break
			}
			for k in 0 .. count {
				dst[dp] = data[sp]
				dp++
				sp++
			}
		}
	}
}

// fill_interlace_order writes the fh stream-row -> canvas-row mapping.
fn fill_interlace_order(fh int, interlaced bool, mut order []int) {
	if interlaced {
		mut n := 0
		for pass in 0 .. 4 {
			mut y := interlace_start(pass)
			step := interlace_step(pass)
			for y < fh {
				order[n] = y
				n++
				y += step
			}
		}
	} else {
		for y in 0 .. fh {
			order[y] = y
		}
	}
}

fn frame_rect(frame &GifFrameRec, width int, height int) Rect {
	return Rect{
		x0: imax(0, frame.left)
		y0: imax(0, frame.top)
		x1: imin(width, frame.left + frame.width)
		y1: imin(height, frame.top + frame.height)
	}
}

fn copy_rect_into(canvas []u8, width int, rect Rect, mut dst []u8) int {
	rw := rect.x1 - rect.x0
	mut dp := 0
	unsafe {
		for y in rect.y0 .. rect.y1 {
			start := (y * width + rect.x0) * 4
			for k in 0 .. rw * 4 {
				dst[dp] = canvas[start + k]
				dp++
			}
		}
	}
	return dp
}

fn restore_rect(mut canvas []u8, width int, rect Rect, snap []u8) {
	rw := rect.x1 - rect.x0
	mut i := 0
	for y in rect.y0 .. rect.y1 {
		start := (y * width + rect.x0) * 4
		for k in 0 .. rw * 4 {
			canvas[start + k] = snap[i + k]
		}
		i += rw * 4
	}
}

fn fill_rect(mut canvas []u8, width int, rect Rect, c Rgba) {
	for y in rect.y0 .. rect.y1 {
		for x in rect.x0 .. rect.x1 {
			off := (y * width + x) * 4
			canvas[off] = c.r
			canvas[off + 1] = c.g
			canvas[off + 2] = c.b
			canvas[off + 3] = c.a
		}
	}
}

fn (d &GifDecoder) bg_rgba(frame &GifFrameRec) Rgba {
	bg := d.bg_color_index
	if frame.has_transparency && frame.transparent_index == bg {
		return Rgba{}
	}
	if bg * 3 + 2 < d.gct.len {
		return Rgba{
			r: d.gct[bg * 3]
			g: d.gct[bg * 3 + 1]
			b: d.gct[bg * 3 + 2]
			a: 255
		}
	}
	return Rgba{}
}

fn (mut d GifDecoder) composite_frame(mut canvas []u8, width int, height int, frame &GifFrameRec) ! {
	// Palette is a zero-copy view: the local table if present, else global.
	mut palette := unsafe { d.gct }
	if frame.lct_len > 0 {
		palette = unsafe { d.data[frame.lct_start..frame.lct_start + frame.lct_len] }
	}
	if palette.len == 0 {
		return error('GIF image has no color table')
	}
	fw := frame.width
	fh := frame.height

	// Reset the per-frame scratch and carve the LZW copy, index buffer,
	// and interlace order out of it.
	d.scratch.reset()
	mut lzwbuf := d.scratch.alloc_bytes(frame.lzw_len)!
	copy_sub_blocks(d.data, frame.lzw_start, mut lzwbuf)
	mut indices := d.scratch.alloc_bytes(fw * fh)!
	lzw_decode_into(lzwbuf, frame.min_code_size, fw * fh, mut d.lzw_prefix, mut d.lzw_suffix,
		mut d.lzw_length, mut d.lzw_first, mut indices)!
	mut order := d.scratch.alloc_ints(fh)!
	fill_interlace_order(fh, frame.interlaced, mut order)

	has_t := frame.has_transparency
	t_idx := frame.transparent_index
	left := frame.left
	top := frame.top
	ncolors := palette.len / 3

	for r in 0 .. fh {
		fy := order[r]
		cy := top + fy
		if cy < 0 || cy >= height {
			continue
		}
		row_start := r * fw
		for fx in 0 .. fw {
			cx := left + fx
			if cx < 0 || cx >= width {
				continue
			}
			index := int(indices[row_start + fx])
			if has_t && index == t_idx {
				continue
			}
			if index >= ncolors {
				return error('GIF palette index ${index} is out of range')
			}
			off := (cy * width + cx) * 4
			canvas[off] = palette[index * 3]
			canvas[off + 1] = palette[index * 3 + 1]
			canvas[off + 2] = palette[index * 3 + 2]
			canvas[off + 3] = 255
		}
	}
}

fn (mut d GifDecoder) coalesce() ![]common.Frame {
	d.parse()!
	d.prepare_scratch()!
	width := d.width
	height := d.height
	mut canvas := []u8{len: width * height * 4}

	// One reusable snapshot buffer sized to the largest frame rect, so the
	// dispose_previous path never allocates per-frame.
	mut max_snap := 0
	for fi in 0 .. d.frames.len {
		r := frame_rect(unsafe { &d.frames[fi] }, width, height)
		area := (r.x1 - r.x0) * (r.y1 - r.y0) * 4
		if area > max_snap {
			max_snap = area
		}
	}
	mut snapbuf := []u8{len: max_snap}

	mut has_pending := false
	mut pmethod := 0
	mut prect := Rect{}

	mut out := []common.Frame{len: 0, cap: d.frames.len}
	for fi in 0 .. d.frames.len {
		frame := unsafe { &d.frames[fi] }
		if has_pending {
			if pmethod == dispose_background {
				fill_rect(mut canvas, width, prect, d.bg_rgba(frame))
			} else if pmethod == dispose_previous {
				// Pass the whole buffer (no slice): a `snapbuf[0..n]`
				// expression would clone. The rect bounds the read.
				restore_rect(mut canvas, width, prect, snapbuf)
			}
		}
		rect := frame_rect(frame, width, height)
		// Snapshot before compositing only when this frame disposes to
		// previous; the next frame restores from the reusable buffer.
		if frame.disposal == dispose_previous {
			copy_rect_into(canvas, width, rect, mut snapbuf)
		}
		d.composite_frame(mut canvas, width, height, frame)!

		out << common.Frame{
			image:    common.Image{ width: width, height: height, rgba: canvas.clone() }
			delay_ms: frame.delay_ms
			disposal: frame.disposal
		}

		if frame.disposal == dispose_background {
			has_pending = true
			pmethod = dispose_background
			prect = rect
		} else if frame.disposal == dispose_previous {
			has_pending = true
			pmethod = dispose_previous
			prect = rect
		} else {
			has_pending = false
		}
	}
	// Release the reusable snapshot buffer and the working canvas; the
	// per-frame clones in `out` are the result.
	unsafe {
		snapbuf.free()
		canvas.free()
	}
	return out
}

// ============================================================================
// Public API
// ============================================================================

// decode decodes the first GIF frame into a top-down RGBA image.
pub fn decode(data []u8) !common.Image {
	mut d := GifDecoder{ data: data, loop_count: 1 }
	d.parse()!
	d.prepare_scratch()!
	width := d.width
	height := d.height
	mut canvas := []u8{len: width * height * 4}
	d.composite_frame(mut canvas, width, height, &d.frames[0])!
	d.free_scratch()
	return common.Image{ width: width, height: height, rgba: canvas }
}

// decode_animation coalesces every GIF frame into a viewer-ready animation.
pub fn decode_animation(data []u8) !common.Animation {
	mut d := GifDecoder{ data: data, loop_count: 1 }
	frames := d.coalesce()!
	loop_count := d.loop_count
	d.free_scratch()
	mut anim := common.Animation{ loop_count: loop_count }
	// Move the frames array in without a deep clone (V would otherwise
	// clone_to_depth the nested []common.Frame on the struct literal).
	unsafe {
		anim.frames = frames
	}
	return anim
}

// ============================================================================
// Incremental player
// ============================================================================

// GifPlayer decodes and composites GIF frames one at a time onto a
// persistent canvas, instead of coalescing every frame up front. This
// keeps GIF loading O(1) per navigation (parse + one frame) so a large
// animated GIF never blocks the UI thread, and it avoids holding every
// frame's RGBA in memory at once.
//
// Frames must be stepped in order (0, 1, 2, ...). When a loop restarts,
// call reset() to clear the canvas before stepping frame 0 again. The
// disposal bookkeeping mirrors coalesce() exactly.
pub struct GifPlayer {
pub mut:
	width  int
	height int
mut:
	d           GifDecoder
	canvas      []u8
	snapbuf     []u8
	index       int
	has_pending bool
	pmethod     int
	prect       Rect
}

// new_gif_player parses the GIF header/block stream (fast) and prepares
// an empty canvas. No frame pixels are decoded until step() is called.
pub fn new_gif_player(data []u8) !&GifPlayer {
	mut d := GifDecoder{ data: data, loop_count: 1 }
	d.parse()!
	if d.frames.len == 0 {
		return error('GIF contains no image descriptor')
	}
	d.prepare_scratch()!
	mut max_snap := 0
	for fi in 0 .. d.frames.len {
		r := frame_rect(unsafe { &d.frames[fi] }, d.width, d.height)
		area := (r.x1 - r.x0) * (r.y1 - r.y0) * 4
		if area > max_snap {
			max_snap = area
		}
	}
	return &GifPlayer{
		d:       d
		width:   d.width
		height:  d.height
		canvas:  []u8{len: d.width * d.height * 4}
		snapbuf: []u8{len: max_snap}
		index:   -1
	}
}

// frame_count returns the number of frames in the GIF.
pub fn (p &GifPlayer) frame_count() int {
	return p.d.frames.len
}

// canvas_width returns the GIF canvas width (interface parity with the
// viewer's AnimPlayer).
pub fn (p &GifPlayer) canvas_width() int {
	return p.width
}

// canvas_height returns the GIF canvas height (interface parity with the
// viewer's AnimPlayer).
pub fn (p &GifPlayer) canvas_height() int {
	return p.height
}

// loop_count returns the NETSCAPE loop count (0 = forever, >=1 finite).
pub fn (p &GifPlayer) loop_count() int {
	return p.d.loop_count
}

// delay_ms returns the display duration of frame `i` in milliseconds.
pub fn (p &GifPlayer) delay_ms(i int) int {
	return p.d.frames[i].delay_ms
}

// reset clears the canvas to transparent and rewinds so the next step()
// renders frame 0. Used when a loop restarts.
pub fn (mut p GifPlayer) reset() {
	// Zero the existing canvas in place (it's already the right size)
	// rather than reallocating, which would leak the old backing.
	for i in 0 .. p.canvas.len {
		p.canvas[i] = 0
	}
	p.index = -1
	p.has_pending = false
	p.pmethod = 0
	p.prect = Rect{}
}

// step composites the next frame in sequence onto the persistent canvas
// and returns the full-canvas RGBA. The returned slice is the live
// canvas; the caller must copy it (e.g. upload to a texture) before the
// next step(), which mutates it in place.
pub fn (mut p GifPlayer) step() ![]u8 {
	if p.index + 1 >= p.d.frames.len {
		return error('GifPlayer: stepped past the last frame (call reset() to loop)')
	}
	if p.has_pending {
		if p.pmethod == dispose_background {
			fill_rect(mut p.canvas, p.width, p.prect, p.d.bg_rgba(&p.d.frames[p.index + 1]))
		} else if p.pmethod == dispose_previous {
			restore_rect(mut p.canvas, p.width, p.prect, p.snapbuf)
		}
	}
	p.index++
	frame := unsafe { &p.d.frames[p.index] }
	rect := frame_rect(frame, p.width, p.height)
	// Snapshot before compositing only when this frame disposes to
	// previous; the next frame restores from the reusable buffer.
	if frame.disposal == dispose_previous {
		copy_rect_into(p.canvas, p.width, rect, mut p.snapbuf)
	}
	p.d.composite_frame(mut p.canvas, p.width, p.height, frame)!
	if frame.disposal == dispose_background {
		p.has_pending = true
		p.pmethod = dispose_background
		p.prect = rect
	} else if frame.disposal == dispose_previous {
		p.has_pending = true
		p.pmethod = dispose_previous
		p.prect = rect
	} else {
		p.has_pending = false
	}
	return p.canvas
}

// free releases the player's persistent canvas and the decoder's scratch.
// The viewer must call this when it drops a player to avoid leaking the
// long-lived canvas and arena.
pub fn (mut p GifPlayer) free() {
	unsafe {
		p.canvas.free()
		p.snapbuf.free()
	}
	p.d.free_scratch()
}
