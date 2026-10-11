/**********************************************************************
*
* TIFF image decoder (classic TIFF 6.0)
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module tiff

import math
import image.decoders.common

// ============================================================================
// Constants
// ============================================================================

const tiff_magic = 42
const bigtiff_magic = 43

const type_byte = 1
const type_ascii = 2
const type_short = 3
const type_long = 4
const type_rational = 5
const type_undefined = 7
const type_slong = 9
const type_srational = 10

const tag_image_width = 256
const tag_image_length = 257
const tag_bits_per_sample = 258
const tag_compression = 259
const tag_photometric = 262
const tag_fill_order = 266
const tag_strip_offsets = 273
const tag_samples_per_pixel = 277
const tag_rows_per_strip = 278
const tag_strip_byte_counts = 279
const tag_planar_config = 284
const tag_color_map = 320
const tag_tile_width = 322
const tag_tile_length = 323
const tag_tile_offsets = 324
const tag_tile_byte_counts = 325
const tag_extra_samples = 338

const photometric_white_is_zero = 0
const photometric_black_is_zero = 1
const photometric_rgb = 2
const photometric_palette = 3

const compression_none = 1
const compression_lzw = 5
const compression_packbits = 32773

const planar_chunky = 1
const planar_separate = 2

const lzw_clear_code = 256
const lzw_eoi_code = 257
const lzw_max_code_size = 12

// ============================================================================
// Helpers
// ============================================================================

fn type_size(ftype int) int {
	match ftype {
		type_byte, type_ascii, type_undefined { return 1 }
		type_short { return 2 }
		type_long, type_slong { return 4 }
		type_rational, type_srational { return 8 }
		else { return 0 }
	}
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
	if math.fmod(f, 2.0) == 0.0 {
		return f
	}
	return f + 1.0
}

// scale_to_8bit resamples a sample from [0, max_sample] to [0, 255].
fn scale_to_8bit(value int, max_sample int) u8 {
	if max_sample == 255 {
		return u8(clampi(value, 0, 255))
	}
	if max_sample > 255 {
		return u8(clampi(value >> 8, 0, 255))
	}
	return u8(int(round_half_even(f64(value) * 255.0 / f64(max_sample))))
}

fn concat_u8(a []u8, b []u8) []u8 {
	mut c := []u8{len: a.len + b.len}
	for i in 0 .. a.len {
		c[i] = a[i]
	}
	for i in 0 .. b.len {
		c[a.len + i] = b[i]
	}
	return c
}

fn first_byte(s []u8) []u8 {
	if s.len > 0 {
		return s[0..1]
	}
	return []u8{}
}

// ============================================================================
// Arena allocator
// ============================================================================
//
// The Windows viewer build is effectively `-gc none` (no Boehm collector), so
// every array the decoder allocates must be released explicitly or it leaks for
// the life of the process. The TIFF decode path allocates a lot of short-lived
// scratch (per-strip decompressed bytes, unpacked sample values, the per-plane
// sample buffers, and the native RGBA buffer). All of it is carved out of one
// arena and released in a single `free_all()` at the end of each public decode
// call, on both the success and the error path. The returned 8-bit image
// ([]u8 produced by native_to_image) is allocated OUTSIDE the arena and owned
// by the caller.
//
// Regions (bump-allocated, never individually freed):
//   ints    - persistent per page: the flat sample planes + the native RGBA.
//   bytes   - reset per strip/tile: the decompressed block.
//   scratch - reset per strip/tile: the unpacked sample values.
struct Arena {
mut:
	ints        []int
	ints_pos    int
	bytes       []u8
	bytes_pos   int
	scratch     []int
	scratch_pos int
}

// alloc_ints returns a sub-slice of the persistent int region (planes/native).
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

// alloc_bytes returns a sub-slice of the per-strip byte region (decompressed).
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

// alloc_scratch returns a sub-slice of the per-strip scratch region (values).
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

// reset_strip rewinds the per-strip byte and scratch cursors so the decompressed
// block and unpacked values buffers are reused between strips/tiles.
fn (mut a Arena) reset_strip() {
	a.bytes_pos = 0
	a.scratch_pos = 0
}

// free_all releases every backing buffer exactly once.
fn (mut a Arena) free_all() {
	unsafe {
		a.ints.free()
		a.bytes.free()
		a.scratch.free()
	}
	a.ints = []int{}
	a.bytes = []u8{}
	a.scratch = []int{}
	a.ints_pos = 0
	a.bytes_pos = 0
	a.scratch_pos = 0
}

// ============================================================================
// Data structures
// ============================================================================

struct IfdEntry {
	tag    int
	ftype  int
	count  int
	values []int
}

// A page is a flat list of IFD entries. A flat list (not a map) is used because
// V map values copied into an array cannot be freed under `-gc none` (the
// ownership flag is not carried by the copy), whereas slices free cleanly.
struct TiffDecoder {
mut:
	data       []u8
	little     bool
	first_ifd  int
	pages      [][]IfdEntry
	width      int
	height     int
	max_sample int
	arena      Arena
}

// ============================================================================
// Byte-order aware readers
// ============================================================================

fn (d &TiffDecoder) require(offset int, n int) ! {
	if offset < 0 || offset + n > d.data.len {
		return error('Unexpected EOF in TIFF data at offset ${offset} (need ${n}, have ${d.data.len})')
	}
}

fn (d &TiffDecoder) read_u16(offset int) !int {
	d.require(offset, 2)!
	if d.little {
		return int(d.data[offset]) | (int(d.data[offset + 1]) << 8)
	}
	return (int(d.data[offset]) << 8) | int(d.data[offset + 1])
}

fn (d &TiffDecoder) read_i16(offset int) !int {
	v := d.read_u16(offset)!
	if v >= 0x8000 {
		return v - 0x10000
	}
	return v
}

fn (d &TiffDecoder) read_u32(offset int) !int {
	d.require(offset, 4)!
	if d.little {
		return int(d.data[offset]) | (int(d.data[offset + 1]) << 8) | (int(d.data[offset + 2]) << 16) | (int(d.data[offset + 3]) << 24)
	}
	return (int(d.data[offset]) << 24) | (int(d.data[offset + 1]) << 16) | (int(d.data[offset + 2]) << 8) | int(d.data[offset + 3])
}

fn (d &TiffDecoder) read_i32(offset int) !int {
	v := d.read_u32(offset)!
	if v >= 0x80000000 {
		return v - 0x100000000
	}
	return v
}

fn (d &TiffDecoder) read_values_at(offset int, ftype int, count int) ![]int {
	ints_per := if ftype == type_rational || ftype == type_srational { 2 } else { 1 }
	mut values := []int{len: count * ints_per}
	mut vi := 0
	mut pos := offset
	for _ in 0 .. count {
		if ftype == type_byte || ftype == type_undefined || ftype == type_ascii {
			d.require(pos, 1)!
			values[vi] = int(d.data[pos])
			vi++
			pos += 1
		} else if ftype == type_short {
			values[vi] = d.read_u16(pos)!
			vi++
			pos += 2
		} else if ftype == type_long {
			values[vi] = d.read_u32(pos)!
			vi++
			pos += 4
		} else if ftype == type_slong {
			values[vi] = d.read_i32(pos)!
			vi++
			pos += 4
		} else if ftype == type_rational {
			num := d.read_u32(pos)!
			den := d.read_u32(pos + 4)!
			values[vi] = num
			values[vi + 1] = den
			vi += 2
			pos += 8
		} else if ftype == type_srational {
			num := d.read_i32(pos)!
			den := d.read_i32(pos + 4)!
			values[vi] = num
			values[vi + 1] = den
			vi += 2
			pos += 8
		} else {
			return error('Unknown TIFF field type: ${ftype}')
		}
	}
	return values
}

// ============================================================================
// Header + IFD parsing
// ============================================================================

fn (mut d TiffDecoder) parse_header() ! {
	d.require(0, 8)!
	bom0 := d.data[0]
	bom1 := d.data[1]
	if bom0 == 0x49 && bom1 == 0x49 { // 'II'
		d.little = true
	} else if bom0 == 0x4D && bom1 == 0x4D { // 'MM'
		d.little = false
	} else {
		return error('Invalid TIFF byte order')
	}
	magic := d.read_u16(2)!
	if magic == bigtiff_magic {
		return error('BigTIFF is not supported')
	}
	if magic != tiff_magic {
		return error('Invalid TIFF magic number: ${magic}')
	}
	d.first_ifd = d.read_u32(4)!
	if d.first_ifd < 8 || d.first_ifd >= d.data.len {
		return error('TIFF first IFD offset ${d.first_ifd} is out of range')
	}
}

fn (mut d TiffDecoder) parse_ifd(offset int) !([]IfdEntry, int) {
	count := d.read_u16(offset)!
	mut entries := []IfdEntry{len: count}
	mut pos := offset + 2
	for i in 0 .. count {
		d.require(pos, 12)!
		tag := d.read_u16(pos)!
		ftype := d.read_u16(pos + 2)!
		value_count := d.read_u32(pos + 4)!
		if type_size(ftype) == 0 {
			return error('Unknown TIFF field type ${ftype} for tag ${tag}')
		}
		if value_count < 0 {
			return error('Negative count ${value_count} for tag ${tag}')
		}
		total := type_size(ftype) * value_count
		mut values := []int{}
		if total <= 4 {
			values = d.read_values_at(pos + 8, ftype, value_count)!
		} else {
			value_offset := d.read_u32(pos + 8)!
			d.require(value_offset, total)!
			values = d.read_values_at(value_offset, ftype, value_count)!
		}
		entries[i] = IfdEntry{
			tag:    tag
			ftype:  ftype
			count:  value_count
			values: values
		}
		pos += 12
	}
	next_ifd := d.read_u32(pos)!
	return entries, next_ifd
}

fn (mut d TiffDecoder) parse() ! {
	d.parse_header()!
	// Count the IFD chain first so the pages array can be pre-sized: V's
	// array__ensure_cap does not free the old backing block when the element
	// type contains pointers/slices (a map does), so `<<` growth would leak.
	mut seen := map[int]bool{}
	mut count := 0
	mut off := d.first_ifd
	for off != 0 {
		if off in seen {
			unsafe { seen.free() }
			return error('TIFF IFD chain contains a loop')
		}
		seen[off] = true
		count++
		n := d.read_u16(off)!
		off = d.read_u32(off + 2 + n * 12)!
	}
	unsafe { seen.free() }
	if count == 0 {
		return error('TIFF file contains no IFD pages')
	}
	d.pages = [][]IfdEntry{len: count}
	mut idx := 0
	off = d.first_ifd
	for off != 0 {
		entries, next_ifd := d.parse_ifd(off)!
		d.pages[idx] = entries
		idx++
		off = next_ifd
	}
}

// ============================================================================
// Tag access helpers
// ============================================================================

fn find_entry(page []IfdEntry, tag int) ?int {
	for i, e in page {
		if e.tag == tag {
			return i
		}
	}
	return none
}

fn get_tag(page []IfdEntry, tag int) ?[]int {
	idx := find_entry(page, tag) or { return none }
	return page[idx].values
}

fn get_short(page []IfdEntry, tag int, default int) int {
	idx := find_entry(page, tag) or { return default }
	v := page[idx].values
	if v.len > 0 {
		return v[0]
	}
	return default
}

fn has_tag(page []IfdEntry, tag int) bool {
	return find_entry(page, tag) != none
}

// ============================================================================
// Validation
// ============================================================================

fn supported_bit_depth(depth int) bool {
	return depth == 1 || depth == 2 || depth == 4 || depth == 8 || depth == 16
}

fn (d &TiffDecoder) validate_page(page []IfdEntry) ! {
	width := get_short(page, tag_image_width, -1)
	height := get_short(page, tag_image_length, -1)
	if width == -1 || height == -1 {
		return error('TIFF page is missing ImageWidth or ImageLength')
	}
	if width <= 0 || height <= 0 {
		return error('Invalid TIFF dimensions: ${width}x${height}')
	}
	spp := get_short(page, tag_samples_per_pixel, 1)
	if spp < 1 || spp > 4 {
		return error('Unsupported TIFF SamplesPerPixel: ${spp}')
	}
	// Check the byte size of both planar samples and the native RGBA arena.
	limit := max_int / int(sizeof(int)) - 16
	if width > limit / height / (spp + 4) {
		return error('TIFF dimensions exceed the supported buffer size')
	}
	bps := get_tag(page, tag_bits_per_sample) or { []int{} }
	eff_len := if bps.len == 0 { 1 } else { bps.len }
	if !(eff_len == 1 || eff_len == spp) {
		return error('TIFF BitsPerSample count ${bps.len} != SamplesPerPixel ${spp}')
	}
	first_bps := if bps.len == 0 { 8 } else { bps[0] }
	for depth in bps {
		if !supported_bit_depth(depth) {
			return error('Unsupported TIFF BitsPerSample: ${depth}')
		}
		if depth != first_bps {
			return error('Mixed BitsPerSample per channel is not supported')
		}
	}
	photometric := get_short(page, tag_photometric, -1)
	if photometric == -1 {
		return error('TIFF page is missing PhotometricInterpretation')
	}
	if !(photometric == photometric_white_is_zero || photometric == photometric_black_is_zero || photometric == photometric_rgb || photometric == photometric_palette) {
		return error('Unsupported TIFF PhotometricInterpretation: ${photometric}')
	}
	compression := get_short(page, tag_compression, compression_none)
	if !(compression == compression_none || compression == compression_lzw || compression == compression_packbits) {
		return error('Unsupported TIFF compression: ${compression}')
	}
	planar := get_short(page, tag_planar_config, planar_chunky)
	if planar != planar_chunky && planar != planar_separate {
		return error('Unsupported TIFF PlanarConfiguration: ${planar}')
	}
	fill_order := get_short(page, tag_fill_order, 1)
	if fill_order != 1 {
		return error('Unsupported TIFF FillOrder: ${fill_order}')
	}
	if photometric == photometric_palette {
		if spp != 1 {
			return error('Palette TIFF requires SamplesPerPixel 1')
		}
		cmap := get_tag(page, tag_color_map) or { [] }
		expected := 3 * (1 << first_bps)
		if cmap.len < expected {
			return error('TIFF ColorMap has ${cmap.len} entries, need ${expected}')
		}
	}
	if photometric == photometric_rgb && spp < 3 {
		return error('RGB TIFF requires at least 3 samples per pixel')
	}

	planes := if planar == planar_separate { spp } else { 1 }
	if has_tag(page, tag_tile_width) {
		tile_w := get_short(page, tag_tile_width, 0)
		tile_h := get_short(page, tag_tile_length, 0)
		if tile_w <= 0 || tile_h <= 0 {
			return error('Invalid TIFF tile size: ${tile_w}x${tile_h}')
		}
		if tile_w > limit / tile_h / spp {
			return error('TIFF tile exceeds the supported buffer size')
		}
		across := (width - 1) / tile_w + 1
		down := (height - 1) / tile_h + 1
		offsets := get_tag(page, tag_tile_offsets) or { return error('TIFF tile page is missing offsets') }
		counts := get_tag(page, tag_tile_byte_counts) or { return error('TIFF tile page is missing byte counts') }
		if offsets.len != counts.len || offsets.len != across * down * planes {
			return error('TIFF tile offset and byte count arrays do not match the page layout')
		}
	} else {
		rows := get_short(page, tag_rows_per_strip, height)
		rows_per_strip := if rows > 0 { rows } else { height }
		strips := (height - 1) / rows_per_strip + 1
		offsets := get_tag(page, tag_strip_offsets) or { return error('TIFF strip page is missing offsets') }
		counts := get_tag(page, tag_strip_byte_counts) or { return error('TIFF strip page is missing byte counts') }
		if offsets.len != counts.len || offsets.len != strips * planes {
			return error('TIFF strip offset and byte count arrays do not match the page layout')
		}
	}
}

// ============================================================================
// Sample unpacking
// ============================================================================

fn (mut d TiffDecoder) unpack_samples(raw []u8, bits int, samples_per_row int, rows int) ![]int {
	total := samples_per_row * rows
	if bits == 8 {
		if raw.len < total {
			return error('TIFF sample data is truncated')
		}
		mut out := d.arena.alloc_scratch(total)!
		for i in 0 .. total {
			out[i] = int(raw[i])
		}
		return out
	}
	if bits == 16 {
		if raw.len < total * 2 {
			return error('TIFF 16-bit sample data is truncated')
		}
		mut out := d.arena.alloc_scratch(total)!
		for i in 0 .. total {
			out[i] = d.read_u16_at(raw, i * 2)
		}
		return out
	}
	if !(bits == 1 || bits == 2 || bits == 4) {
		return error('Unsupported TIFF BitsPerSample: ${bits}')
	}
	mask := (1 << bits) - 1
	bytes_per_row := (samples_per_row * bits + 7) / 8
	if raw.len < bytes_per_row * rows {
		return error('TIFF sub-byte sample data is truncated')
	}
	mut values := d.arena.alloc_scratch(total)!
	mut vi := 0
	for r in 0 .. rows {
		row_off := r * bytes_per_row
		mut bitpos := 0
		for _ in 0 .. samples_per_row {
			byte := int(raw[row_off + (bitpos >> 3)])
			shift := 8 - bits - (bitpos & 7)
			values[vi] = (byte >> shift) & mask
			vi++
			bitpos += bits
		}
	}
	return values
}

fn (d &TiffDecoder) read_u16_at(buf []u8, off int) int {
	if d.little {
		return int(buf[off]) | (int(buf[off + 1]) << 8)
	}
	return (int(buf[off]) << 8) | int(buf[off + 1])
}

// ============================================================================
// Compression
// ============================================================================

fn (mut d TiffDecoder) decompress(raw []u8, compression int, expected int) ![]u8 {
	if compression == compression_none {
		return raw
	}
	if compression == compression_packbits {
		mut out := d.arena.alloc_bytes(expected)!
		n := packbits_into(raw, mut out)!
		return out[0..n]
	}
	if compression == compression_lzw {
		mut out := d.arena.alloc_bytes(expected)!
		n := lzw_into(raw, mut out)!
		return out[0..n]
	}
	return error('Unsupported TIFF compression: ${compression}')
}

// packbits_into decodes PackBits data into the pre-sized `out` buffer and
// returns the number of bytes written.
fn packbits_into(raw []u8, mut out []u8) !int {
	mut oi := 0
	mut i := 0
	n := raw.len
	for i < n {
		ctrl := int(raw[i])
		i++
		if ctrl <= 127 {
			cnt := ctrl + 1
			if i + cnt > n {
				return error('Truncated TIFF PackBits literal run')
			}
			if oi + cnt > out.len {
				return error('TIFF PackBits output overflow')
			}
			for k in 0 .. cnt {
				out[oi + k] = raw[i + k]
			}
			oi += cnt
			i += cnt
		} else if ctrl == 128 {
			continue
		} else {
			cnt := 257 - ctrl
			if i >= n {
				return error('Truncated TIFF PackBits repeat run')
			}
			if oi + cnt > out.len {
				return error('TIFF PackBits output overflow')
			}
			val := raw[i]
			for k in 0 .. cnt {
				out[oi + k] = val
			}
			oi += cnt
			i++
		}
	}
	return oi
}

fn lzw_read_bits(raw []u8, bitpos int, nbits int) int {
	mut value := 0
	for i in 0 .. nbits {
		byte := int(raw[(bitpos + i) >> 3])
		bit := (byte >> (7 - ((bitpos + i) & 7))) & 1
		value = (value * 2) | bit
	}
	return value
}

// lzw_into decodes TIFF LZW data into the pre-sized `out` buffer and returns
// the number of bytes written. The string table is heap-allocated but freed on
// every exit path (success, clear-code reset, and error) so nothing leaks under
// `-gc none`.
fn lzw_into(raw []u8, mut out []u8) !int {
	mut oi := 0
	mut bitpos := 0
	mut nbits := 9
	total_bits := raw.len * 8
	mut table := lzw_init_table()
	mut next_code := 258
	mut max_code := (1 << nbits) - 1
	mut prev := []u8{}

	for true {
		if bitpos + nbits > total_bits {
			break
		}
		code := lzw_read_bits(raw, bitpos, nbits)
		bitpos += nbits

		if code == lzw_eoi_code {
			break
		}
		if code == lzw_clear_code {
			free_lzw_table(mut table)
			nbits = 9
			table = lzw_init_table()
			next_code = 258
			max_code = (1 << nbits) - 1
			prev = []u8{}
			continue
		}

		mut entry := []u8{}
		mut fresh := false
		if code < next_code && table[code].len > 0 {
			entry = table[code]
		} else if code == next_code {
			entry = concat_u8(prev, first_byte(prev))
			fresh = true
		} else {
			free_lzw_table(mut table)
			return error('Invalid TIFF LZW code: ${code}')
		}

		if oi + entry.len > out.len {
			if fresh {
				unsafe { entry.free() }
			}
			free_lzw_table(mut table)
			return error('TIFF LZW output overflow')
		}
		for k in 0 .. entry.len {
			out[oi + k] = entry[k]
		}
		oi += entry.len

		if prev.len > 0 && next_code < 4096 {
			// Index assignment (not `<<`): V's `<<` deep-clones a slice element,
			// which would leak the freshly built array. Storing by index shares the
			// data with the table, which owns and frees it.
			if fresh {
				table[next_code] = entry
			} else {
				table[next_code] = concat_u8(prev, first_byte(entry))
			}
			next_code++
			if next_code >= max_code && nbits < lzw_max_code_size {
				nbits++
				max_code = (1 << nbits) - 1
			}
			unsafe {
				prev = entry
			}
		} else if fresh {
			// A fresh (KwKwK) entry that cannot be added to the table only occurs
			// when prev is empty, so `entry` is an empty array; free it and keep
			// prev empty.
			unsafe {
				entry.free()
			}
			prev = []u8{}
		} else {
			unsafe {
				prev = entry
			}
		}
	}
	free_lzw_table(mut table)
	return oi
}

// lzw_init_table builds a fixed 4096-slot table (slots 0..255 are the single
// byte codes; the rest start empty). Entries are written by index, so no `<<`
// growth (which would leak the old backing block for slice-bearing element
// types under `-gc none`) ever happens.
fn lzw_init_table() [][]u8 {
	mut table := [][]u8{len: 4096}
	for i in 0 .. 256 {
		table[i] = [u8(i)]
	}
	return table
}

// free_lzw_table releases every entry and the table array itself.
fn free_lzw_table(mut table [][]u8) {
	for i, _ in table {
		mut e := table[i]
		unsafe {
			e.free()
		}
	}
	unsafe {
		table.free()
	}
}

// ============================================================================
// Layout decoding
// ============================================================================

fn (d &TiffDecoder) read_block(offset int, size int) ![]u8 {
	d.require(offset, size)!
	return d.data[offset..offset + size]
}

fn (mut d TiffDecoder) decode_strips(page []IfdEntry, width int, height int, spp int, bits int, planar int, compression int) ![]int {
	offsets := get_tag(page, tag_strip_offsets) or { return error('TIFF strip page is missing offsets or byte counts') }
	byte_counts := get_tag(page, tag_strip_byte_counts) or { return error('TIFF strip page is missing offsets or byte counts') }

	mut rows_per_strip := get_short(page, tag_rows_per_strip, height)
	if rows_per_strip <= 0 {
		rows_per_strip = height
	}

	plane_size := width * height
	mut planes := d.arena.alloc_ints(spp * plane_size)!

	if planar == planar_chunky {
		samples_per_row := width * spp
		for s in 0 .. offsets.len {
			row_start := s * rows_per_strip
			if row_start >= height {
				break
			}
			mut rows_in_strip := rows_per_strip
			if height - row_start < rows_in_strip {
				rows_in_strip = height - row_start
			}
			d.arena.reset_strip()
			expected := (samples_per_row * rows_in_strip * bits + 7) / 8
			mut raw := d.read_block(offsets[s], byte_counts[s])!
			raw = d.decompress(raw, compression, expected)!
			vals := d.unpack_samples(raw, bits, samples_per_row, rows_in_strip)!
			need := rows_in_strip * samples_per_row
			for idx in 0 .. need {
				row_local := idx / samples_per_row
				col := idx % samples_per_row
				channel := col % spp
				x := col / spp
				pixel := (row_start + row_local) * width + x
				planes[channel * plane_size + pixel] = vals[idx]
			}
		}
	} else {
		strips_per_plane := (height + rows_per_strip - 1) / rows_per_strip
		for plane in 0 .. spp {
			samples_per_row := width
			for s in 0 .. strips_per_plane {
				arr_idx := plane * strips_per_plane + s
				if arr_idx >= offsets.len {
					return error('TIFF separate-plane strip array is short')
				}
				row_start := s * rows_per_strip
				if row_start >= height {
					break
				}
				mut rows_in_strip := rows_per_strip
				if height - row_start < rows_in_strip {
					rows_in_strip = height - row_start
				}
				d.arena.reset_strip()
				expected := (samples_per_row * rows_in_strip * bits + 7) / 8
				mut raw := d.read_block(offsets[arr_idx], byte_counts[arr_idx])!
				raw = d.decompress(raw, compression, expected)!
				vals := d.unpack_samples(raw, bits, samples_per_row, rows_in_strip)!
				base := row_start * width
				for j in 0 .. rows_in_strip * samples_per_row {
					planes[plane * plane_size + base + j] = vals[j]
				}
			}
		}
	}
	return planes
}

fn (mut d TiffDecoder) decode_tiles(page []IfdEntry, width int, height int, spp int, bits int, planar int, compression int) ![]int {
	tile_w := get_short(page, tag_tile_width, -1)
	tile_h := get_short(page, tag_tile_length, -1)
	offsets := get_tag(page, tag_tile_offsets) or { return error('TIFF tile page is missing tile geometry or offsets') }
	byte_counts := get_tag(page, tag_tile_byte_counts) or { return error('TIFF tile page is missing tile geometry or offsets') }
	if tile_w <= 0 || tile_h <= 0 {
		return error('Invalid TIFF tile size: ${tile_w}x${tile_h}')
	}

	tiles_across := (width + tile_w - 1) / tile_w
	tiles_down := (height + tile_h - 1) / tile_h
	tiles_total := tiles_across * tiles_down

	plane_size := width * height
	mut planes := d.arena.alloc_ints(spp * plane_size)!

	if planar == planar_chunky {
		samples_per_row := tile_w * spp
		expected := (samples_per_row * tile_h * bits + 7) / 8
		for ty in 0 .. tiles_down {
			for tx in 0 .. tiles_across {
				tidx := ty * tiles_across + tx
				d.arena.reset_strip()
				mut raw := d.read_block(offsets[tidx], byte_counts[tidx])!
				raw = d.decompress(raw, compression, expected)!
				vals := d.unpack_samples(raw, bits, samples_per_row, tile_h)!
				place_tile(mut planes, vals, width, height, spp, tx, ty, tile_w, tile_h, true, 0)
			}
		}
	} else {
		samples_per_row := tile_w
		expected := (samples_per_row * tile_h * bits + 7) / 8
		for plane in 0 .. spp {
			for ty in 0 .. tiles_down {
				for tx in 0 .. tiles_across {
					arr_idx := plane * tiles_total + ty * tiles_across + tx
					if arr_idx >= offsets.len {
						return error('TIFF separate-plane tile array is short')
					}
					d.arena.reset_strip()
					mut raw := d.read_block(offsets[arr_idx], byte_counts[arr_idx])!
					raw = d.decompress(raw, compression, expected)!
					vals := d.unpack_samples(raw, bits, samples_per_row, tile_h)!
					place_tile(mut planes, vals, width, height, spp, tx, ty, tile_w, tile_h, false, plane)
				}
			}
		}
	}
	return planes
}

fn place_tile(mut planes []int, vals []int, width int, height int, spp int, tx int, ty int, tile_w int, tile_h int, chunky bool, plane int) {
	plane_size := width * height
	for py in 0 .. tile_h {
		gy := ty * tile_h + py
		if gy >= height {
			continue
		}
		for px in 0 .. tile_w {
			gx := tx * tile_w + px
			if gx >= width {
				continue
			}
			if chunky {
				base := (py * tile_w + px) * spp
				for c in 0 .. spp {
					planes[c * plane_size + gy * width + gx] = vals[base + c]
				}
			} else {
				planes[plane * plane_size + gy * width + gx] = vals[py * tile_w + px]
			}
		}
	}
}

// ============================================================================
// Photometric conversion
// ============================================================================

fn (mut d TiffDecoder) photometric_to_rgba(planes []int, page []IfdEntry, width int, height int, spp int, bits int, photometric int) ![]int {
	n := width * height
	plane_size := width * height
	mut rgba := d.arena.alloc_ints(n * 4)!

	if photometric == photometric_palette {
		cmap := get_tag(page, tag_color_map) or { return error('Palette TIFF is missing ColorMap') }
		d.max_sample = 65535
		opaque := 65535
		plane := 1 << bits
		if cmap.len < 3 * plane {
			return error('TIFF ColorMap has ${cmap.len} entries, need ${3 * plane}')
		}
		for i in 0 .. n {
			idx := planes[i]
			if idx >= plane {
				return error('TIFF palette index ${idx} out of range')
			}
			rgba[i * 4] = cmap[idx]
			rgba[i * 4 + 1] = cmap[plane + idx]
			rgba[i * 4 + 2] = cmap[2 * plane + idx]
			rgba[i * 4 + 3] = opaque
		}
		return rgba
	}

	max_sample := (1 << bits) - 1
	d.max_sample = max_sample

	if photometric == photometric_white_is_zero {
		for i in 0 .. n {
			gray := max_sample - planes[i]
			rgba[i * 4] = gray
			rgba[i * 4 + 1] = gray
			rgba[i * 4 + 2] = gray
			rgba[i * 4 + 3] = max_sample
		}
	} else if photometric == photometric_black_is_zero {
		for i in 0 .. n {
			gray := planes[i]
			rgba[i * 4] = gray
			rgba[i * 4 + 1] = gray
			rgba[i * 4 + 2] = gray
			rgba[i * 4 + 3] = max_sample
		}
	} else if photometric == photometric_rgb {
		extra := get_tag(page, tag_extra_samples) or { [] }
		has_alpha := spp >= 4 && extra.len >= 1
		for i in 0 .. n {
			rgba[i * 4] = planes[i]
			rgba[i * 4 + 1] = planes[plane_size + i]
			rgba[i * 4 + 2] = planes[2 * plane_size + i]
			rgba[i * 4 + 3] = if has_alpha { planes[3 * plane_size + i] } else { max_sample }
		}
	} else {
		return error('Unsupported TIFF PhotometricInterpretation: ${photometric}')
	}
	return rgba
}

// ============================================================================
// Page decode
// ============================================================================

fn (mut d TiffDecoder) decode_page(page []IfdEntry) ![]int {
	d.validate_page(page)!
	width := get_short(page, tag_image_width, 0)
	height := get_short(page, tag_image_length, 0)
	spp := get_short(page, tag_samples_per_pixel, 1)
	bps := get_tag(page, tag_bits_per_sample) or { []int{} }
	bits := if bps.len == 0 { 8 } else { bps[0] }
	photometric := get_short(page, tag_photometric, 0)
	compression := get_short(page, tag_compression, compression_none)
	planar := get_short(page, tag_planar_config, planar_chunky)

	d.init_arena(width, height, spp, bits, max_block_samples(width, height, spp, page))!
	mut planes := []int{}
	if has_tag(page, tag_tile_width) {
		planes = d.decode_tiles(page, width, height, spp, bits, planar, compression)!
	} else {
		planes = d.decode_strips(page, width, height, spp, bits, planar, compression)!
	}
	return d.photometric_to_rgba(planes, page, width, height, spp, bits, photometric)!
}

// max_block_samples returns the largest single strip/tile sample count for the
// page, so the per-strip scratch/byte regions are sized to cover an overhanging
// edge tile (whose full-tile sample count can exceed the image area).
fn max_block_samples(width int, height int, spp int, page []IfdEntry) int {
	mut m := spp * width * height
	if has_tag(page, tag_tile_width) {
		tile_w := get_short(page, tag_tile_width, 0)
		tile_h := get_short(page, tag_tile_length, 0)
		tile_samples := tile_w * spp * tile_h
		if tile_samples > m {
			m = tile_samples
		}
	}
	return m
}

// init_arena sizes the decode scratch regions from the page geometry. It frees
// any previous page's arena first so multi-page decode reuses one arena.
fn (mut d TiffDecoder) init_arena(width int, height int, spp int, bits int, max_block_samples int) !void {
	d.arena.free_all()
	plane_total := spp * width * height
	native_total := width * height * 4
	d.arena.ints = []int{len: plane_total + native_total + 16}
	d.arena.bytes = []u8{len: (max_block_samples * bits + 7) / 8 + 16}
	d.arena.scratch = []int{len: max_block_samples + 16}
}

// free_owned releases the parse-time owned arrays: every page's entry value
// arrays, the page maps, and the pages array. The caller's `data` is never
// freed.
fn (mut d TiffDecoder) free_owned() {
	for page in d.pages {
		for e in page {
			unsafe {
				e.values.free()
			}
		}
		unsafe {
			page.free()
		}
	}
	unsafe {
		d.pages.free()
	}
	d.pages = []
}

// cleanup releases the arena and all owned arrays. Called on both the success
// and the error path of each public decode function.
fn (mut d TiffDecoder) cleanup() {
	d.arena.free_all()
	d.free_owned()
}

// ============================================================================
// Public API
// ============================================================================

fn native_to_image(native []int, width int, height int, max_sample int) common.Image {
	mut rgba := []u8{len: native.len}
	for i, v in native {
		rgba[i] = scale_to_8bit(v, max_sample)
	}
	return common.Image{
		width:  width
		height: height
		rgba:   rgba
	}
}

// decode decodes the first page of a TIFF into an 8-bit RGBA image.
pub fn decode(data []u8) !common.Image {
	mut d := TiffDecoder{
		data: data
	}
	res := d.decode_first() or {
		d.cleanup()
		return err
	}
	d.cleanup()
	return res
}

fn (mut d TiffDecoder) decode_first() !common.Image {
	d.parse()!
	page := d.pages[0]
	d.width = get_short(page, tag_image_width, 0)
	d.height = get_short(page, tag_image_length, 0)
	native := d.decode_page(page)!
	return native_to_image(native, d.width, d.height, d.max_sample)
}

// decode_pages decodes every page of a TIFF into a common.Animation.
pub fn decode_pages(data []u8) !common.Animation {
	mut d := TiffDecoder{
		data: data
	}
	res := d.decode_all_pages() or {
		d.cleanup()
		return err
	}
	d.cleanup()
	return res
}

fn (mut d TiffDecoder) decode_all_pages() !common.Animation {
	d.parse()!
	mut frames := []common.Frame{len: d.pages.len}
	for i, page in d.pages {
		w := get_short(page, tag_image_width, 0)
		h := get_short(page, tag_image_length, 0)
		native := d.decode_page(page)!
		img := native_to_image(native, w, h, d.max_sample)
		frames[i] = common.Frame{
			image:    img
			delay_ms: 0
			disposal: 0
		}
	}
	return common.Animation{
		frames:     frames
		loop_count: 1
	}
}
