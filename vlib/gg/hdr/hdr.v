// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module hdr

import gg
import math
import os

// ColorHDR is a linear-light RGBA color. Unlike gg.Color, components are f32
// and may exceed 1, so highlights survive until the tone-mapping stage.
pub struct ColorHDR {
pub mut:
	r f32
	g f32
	b f32
	a f32 = 1
}

// ToneMap selects the curve used to compress HDR values into display range.
pub enum ToneMap {
	none
	reinhard
	aces
}

// black returns an opaque black HDR color.
pub fn black() ColorHDR {
	return ColorHDR{}
}

// white returns an opaque unit HDR color.
pub fn white() ColorHDR {
	return ColorHDR{1, 1, 1, 1}
}

// from_rgb builds an HDR color from linear-light RGB components.
pub fn from_rgb(r f32, g f32, b f32) ColorHDR {
	return ColorHDR{r, g, b, 1}
}

// from_rgba builds an HDR color from linear-light RGBA components.
pub fn from_rgba(r f32, g f32, b f32, a f32) ColorHDR {
	return ColorHDR{r, g, b, a}
}

fn srgb_to_linear(c f32) f32 {
	if c <= 0.04045 {
		return c / 12.92
	}
	return f32(math.pow(f64((c + 0.055) / 1.055), 2.4))
}

fn linear_to_srgb(c f32) f32 {
	if c <= 0.0031308 {
		return c * 12.92
	}
	return 1.055 * f32(math.pow(f64(c), 1.0 / 2.4)) - 0.055
}

// from_ldr_color converts an sRGB gg.Color to a linear HDR color.
pub fn from_ldr_color(c gg.Color) ColorHDR {
	return ColorHDR{srgb_to_linear(f32(c.r) / 255), srgb_to_linear(f32(c.g) / 255), srgb_to_linear(f32(c.b) / 255), f32(c.a) / 255}
}

// to_ldr_color converts to sRGB, clamping components to [0, 255]. Tone map
// first with tone_map if the color may exceed 1.
pub fn (c &ColorHDR) to_ldr_color() gg.Color {
	return gg.Color{
		r: u8(math.clip(linear_to_srgb(c.r) * 255, 0, 255))
		g: u8(math.clip(linear_to_srgb(c.g) * 255, 0, 255))
		b: u8(math.clip(linear_to_srgb(c.b) * 255, 0, 255))
		a: u8(math.clip(c.a * 255, 0, 255))
	}
}

// luminance returns the perceptual luminance of an HDR color.
pub fn (c &ColorHDR) luminance() f32 {
	return 0.2126 * c.r + 0.7152 * c.g + 0.0722 * c.b
}

// scale multiplies the RGB components, keeping alpha unchanged.
pub fn (c &ColorHDR) scale(factor f32) ColorHDR {
	return ColorHDR{c.r * factor, c.g * factor, c.b * factor, c.a}
}

// add returns the component-wise sum of two HDR colors.
pub fn (c &ColorHDR) add(b ColorHDR) ColorHDR {
	return ColorHDR{c.r + b.r, c.g + b.g, c.b + b.b, c.a + b.a}
}

// tone_map compresses one component into display range.
pub fn tone_map_channel(x f32, tone ToneMap) f32 {
	if x <= 0 {
		return 0
	}
	match tone {
		.none {
			return x
		}
		.reinhard {
			return x / (1 + x)
		}
		.aces {
			return math.clip(x * (2.51 * x + 0.03) / (x * (2.43 * x + 0.59) + 0.14), 0, 1)
		}
	}
}

// tone_map applies exposure and a curve to an HDR color, then converts to sRGB.
pub fn (c &ColorHDR) tone_map(exposure f32, tone ToneMap) gg.Color {
	return gg.Color{
		r: u8(math.clip(linear_to_srgb(tone_map_channel(c.r * exposure, tone)) * 255, 0, 255))
		g: u8(math.clip(linear_to_srgb(tone_map_channel(c.g * exposure, tone)) * 255, 0, 255))
		b: u8(math.clip(linear_to_srgb(tone_map_channel(c.b * exposure, tone)) * 255, 0, 255))
		a: u8(math.clip(c.a * 255, 0, 255))
	}
}

// HDRImage is a width by height RGBA buffer of linear-light f32 pixels.
pub struct HDRImage {
pub mut:
	width  int
	height int
	pixels []f32
}

// new_image returns a zeroed HDR image of the given size.
pub fn new_image(width int, height int) !HDRImage {
	if width <= 0 || height <= 0 {
		return error('hdr: invalid image size ${width}x${height}')
	}
	return HDRImage{
		width:  width
		height: height
		pixels: []f32{len: width * height * 4}
	}
}

// from_ldr expands an interleaved RGBA8 buffer into linear-light floats.
pub fn from_ldr(pixels []u8) []f32 {
	mut out := []f32{len: pixels.len}
	mut at := 0
	for at + 3 < pixels.len {
		out[at] = srgb_to_linear(f32(pixels[at]) / 255)
		out[at + 1] = srgb_to_linear(f32(pixels[at + 1]) / 255)
		out[at + 2] = srgb_to_linear(f32(pixels[at + 2]) / 255)
		out[at + 3] = f32(pixels[at + 3]) / 255
		at += 4
	}
	return out
}

// to_ldr_into tone maps src into a caller-owned RGBA8 buffer. dst is not
// resized; bytes beyond min(src.len, dst.len) are left untouched.
pub fn to_ldr_into(mut dst []u8, src []f32, exposure f32, tone ToneMap) {
	limit := if src.len < dst.len { src.len } else { dst.len }
	mut at := 0
	for at + 3 < limit {
		for c in 0 .. 3 {
			mapped := tone_map_channel(src[at + c] * exposure, tone)
			dst[at + c] = u8(math.clip(linear_to_srgb(mapped) * 255, 0, 255))
		}
		dst[at + 3] = u8(math.clip(src[at + 3] * 255, 0, 255))
		at += 4
	}
}

// apply_exposure multiplies the RGB channels of an HDR buffer by a factor.
pub fn apply_exposure(mut pixels []f32, exposure f32) {
	mut at := 0
	for at + 3 < pixels.len {
		pixels[at] *= exposure
		pixels[at + 1] *= exposure
		pixels[at + 2] *= exposure
		at += 4
	}
}

fn box_blur_into(mut dst []f32, src []f32, width int, height int, radius int) {
	if radius <= 0 {
		n := if src.len < dst.len { src.len } else { dst.len }
		for i in 0 .. n {
			dst[i] = src[i]
		}
		return
	}
	for y in 0 .. height {
		for x in 0 .. width {
			at := (y * width + x) * 4
			if at + 3 >= src.len || at + 3 >= dst.len {
				continue
			}
			mut sum := [f32(0), 0, 0]
			mut count := f32(0)
			for by in y - radius .. y + radius + 1 {
				sy := if by < 0 {
					0
				} else if by >= height {
					height - 1
				} else {
					by
				}
				for bx in x - radius .. x + radius + 1 {
					sx := if bx < 0 {
						0
					} else if bx >= width {
						width - 1
					} else {
						bx
					}
					sat := (sy * width + sx) * 4
					if sat + 2 >= src.len {
						continue
					}
					for c in 0 .. 3 {
						sum[c] += src[sat + c]
					}
					count++
				}
			}
			if count > 0 {
				for c in 0 .. 3 {
					dst[at + c] = sum[c] / count
				}
			}
			dst[at + 3] = src[at + 3]
		}
	}
}

// bloom_into adds a blurred bright pass back into an HDR buffer. threshold and
// strength are linear-light values; radius is the box-blur radius in pixels.
pub fn bloom_into(mut dst []f32, src []f32, width int, height int, threshold f32, strength f32,
	radius int) {
	mut bright := []f32{len: src.len}
	mut at := 0
	for at + 3 < src.len {
		l := 0.2126 * src[at] + 0.7152 * src[at + 1] + 0.0722 * src[at + 2]
		if l > threshold {
			bright[at] = src[at]
			bright[at + 1] = src[at + 1]
			bright[at + 2] = src[at + 2]
		}
		bright[at + 3] = src[at + 3]
		at += 4
	}
	mut blurred := []f32{len: src.len}
	box_blur_into(mut blurred, bright, width, height, radius)
	n := if src.len < dst.len { src.len } else { dst.len }
	for i in 0 .. n {
		dst[i] = src[i] + blurred[i] * strength
	}
}

// float_targets_available reports whether native float render targets are
// available on the current backend. The CPU API works on every backend.
pub fn float_targets_available() bool {
	$if js {
		return false
	} $else {
		return true
	}
}

fn rgbe_to_color(r u8, g u8, b u8, e u8) ColorHDR {
	if e == 0 {
		return ColorHDR{}
	}
	scale := f32(math.ldexp(1.0, int(e) - 136))
	return ColorHDR{f32(r) * scale, f32(g) * scale, f32(b) * scale, 1}
}

fn color_to_rgbe(c ColorHDR) [4]u8 {
	mut v := c.r
	if c.g > v {
		v = c.g
	}
	if c.b > v {
		v = c.b
	}
	if v < 1e-32 {
		return [u8(0), 0, 0, 0]!
	}
	mantissa, exponent := math.frexp(f64(v))
	scale := f32(mantissa) * 256 / v
	return [
		u8(math.clip(c.r * scale, 0, 255))
		u8(math.clip(c.g * scale, 0, 255))
		u8(math.clip(c.b * scale, 0, 255))
		u8(exponent + 128),
	]!
}

fn read_rgbe_channel(data []u8, start int, mut out []u8) !int {
	mut pos := start
	mut i := 0
	for i < out.len {
		if pos >= data.len {
			return error('hdr: truncated RGBE run')
		}
		count := int(data[pos])
		pos++
		if count > 128 {
			n := count - 128
			if n == 0 || pos >= data.len {
				return error('hdr: invalid RGBE run')
			}
			value := data[pos]
			pos++
			for _ in 0 .. n {
				if i < out.len {
					out[i] = value
					i++
				}
			}
		} else {
			if count == 0 || pos + count > data.len {
				return error('hdr: invalid RGBE literal')
			}
			for _ in 0 .. count {
				if i < out.len {
					out[i] = data[pos]
					i++
				}
				pos++
			}
		}
	}
	return pos
}

// decode_rgbe parses a Radiance RGBE image (RLE or flat scanlines) into an
// HDR image. Only the standard -Y height +X width orientation is required;
// flipped +Y files are handled by reversing rows.
pub fn decode_rgbe(data []u8) !HDRImage {
	mut pos := 0
	mut width := 0
	mut height := 0
	mut flipped := false
	for pos < data.len {
		mut end := pos
		for end < data.len && data[end] != `\n` {
			end++
		}
		line := data[pos..end].bytestr().trim_space()
		pos = if end < data.len { end + 1 } else { end }
		if line.starts_with('-') || line.starts_with('+') {
			parts := line.fields()
			if parts.len == 4 && (parts[0] == '-Y' || parts[0] == '+Y')
				&& (parts[2] == '+X' || parts[2] == '-X') {
				width = parts[3].int()
				height = parts[1].int()
				flipped = parts[0] == '+Y'
				break
			}
		}
	}
	if width <= 0 || height <= 0 {
		return error('hdr: missing or invalid RGBE resolution line')
	}
	// The dimensions come from an untrusted header and drive an allocation.
	// Compute the pixel count in i64 and reject anything unreasonable before
	// calling new_image, so a crafted header cannot overflow or exhaust memory.
	pixels := i64(width) * i64(height)
	if pixels <= 0 || pixels > i64(1) << 28 {
		return error('hdr: RGBE image dimensions too large')
	}
	mut img := new_image(width, height)!
	for y in 0 .. height {
		if pos + 4 <= data.len && data[pos] == 2 && data[pos + 1] == 2
			&& (int(data[pos + 2]) << 8 | int(data[pos + 3])) == width {
			pos += 4
			mut ch0 := []u8{len: width}
			mut ch1 := []u8{len: width}
			mut ch2 := []u8{len: width}
			mut ch3 := []u8{len: width}
			pos = read_rgbe_channel(data, pos, mut ch0)!
			pos = read_rgbe_channel(data, pos, mut ch1)!
			pos = read_rgbe_channel(data, pos, mut ch2)!
			pos = read_rgbe_channel(data, pos, mut ch3)!
			channels := [ch0, ch1, ch2, ch3]
			for x in 0 .. width {
				dst_y := if flipped { height - 1 - y } else { y }
				at := (dst_y * width + x) * 4
				img.pixels[at] = f32(channels[0][x])
				img.pixels[at + 1] = f32(channels[1][x])
				img.pixels[at + 2] = f32(channels[2][x])
				img.pixels[at + 3] = 1
			}
		} else {
			for x in 0 .. width {
				if pos + 4 > data.len {
					return error('hdr: truncated RGBE image data')
				}
				c := rgbe_to_color(data[pos], data[pos + 1], data[pos + 2], data[pos + 3])
				pos += 4
				dst_y := if flipped { height - 1 - y } else { y }
				at := (dst_y * width + x) * 4
				img.pixels[at] = c.r
				img.pixels[at + 1] = c.g
				img.pixels[at + 2] = c.b
				img.pixels[at + 3] = 1
			}
		}
	}
	return img
}

// load_rgbe loads a Radiance .hdr file from disk.
pub fn load_rgbe(path string) !HDRImage {
	data := os.read_file(path) or { return error('hdr: cannot read "${path}": ${err}') }
	return decode_rgbe(data.bytes())
}

// encode_rgbe writes an uncompressed Radiance RGBE image. The flat encoding
// is valid RGBE and keeps the writer backend-independent.
pub fn (img &HDRImage) encode_rgbe() []u8 {
	mut out := []u8{}
	out << '#?RADIANCE\n'.bytes()
	out << 'FORMAT=32-bit_rle_rgbe\n\n'.bytes()
	out << '-Y ${img.height} +X ${img.width}\n'.bytes()
	for i in 0 .. img.pixels.len / 4 {
		c := color_to_rgbe(ColorHDR{img.pixels[i * 4], img.pixels[i * 4 + 1], img.pixels[i * 4 + 2], 1})
		out << c[0]
		out << c[1]
		out << c[2]
		out << c[3]
	}
	return out
}

// save_rgbe writes an HDR image as a Radiance .hdr file.
pub fn (img &HDRImage) save_rgbe(path string) ! {
	os.write_file(path, img.encode_rgbe().bytestr())!
}
