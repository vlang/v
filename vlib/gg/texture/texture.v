// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module texture

import gg

// TextureFormat identifies a block-compressed or uncompressed pixel format.
pub enum TextureFormat {
	unknown
	rgba8
	dxt1
	dxt3
	dxt5
	etc1
	etc2
	astc
	bptc
}

// Ktx2Header is the fixed header of a KTX2 container.
pub struct Ktx2Header {
pub:
	vk_format        u32
	type_size        u32
	pixel_width      u32
	pixel_height     u32
	level_count      u32
	supercompression u32
}

fn u32_at(bytes []u8, at int) !u32 {
	if at < 0 || at + 4 > bytes.len {
		return error('texture: truncated KTX2 header')
	}
	return u32(bytes[at]) | (u32(bytes[at + 1]) << 8) | (u32(bytes[at + 2]) << 16) |
		(u32(bytes[at + 3]) << 24)
}

// parse_ktx2 validates and reads a KTX2 header from bytes.
pub fn parse_ktx2(bytes []u8) !Ktx2Header {
	signature := [u8(0xAB), 0x4B, 0x54, 0x58, 0x20, 0x32, 0x30, 0xBB, 0x0D, 0x0A, 0x1A, 0x0A]
	if bytes.len < 80 {
		return error('texture: KTX2 file too short')
	}
	for i, expected in signature {
		if bytes[i] != expected {
			return error('texture: bad KTX2 signature')
		}
	}
	return Ktx2Header{
		vk_format:        u32_at(bytes, 12)!
		type_size:        u32_at(bytes, 16)!
		pixel_width:      u32_at(bytes, 20)!
		pixel_height:     u32_at(bytes, 24)!
		level_count:      u32_at(bytes, 40)!
		supercompression: u32_at(bytes, 44)!
	}
}

// format_from_vk maps a Vulkan format id to a friendly texture format.
pub fn format_from_vk(vk u32) TextureFormat {
	return match vk {
		37 { .rgba8 } // VK_FORMAT_R8G8B8A8_UNORM
		0x59, 0x5A, 0x5B { .dxt1 } // BC1_RGB / BC1_RGBA / BC1_RGBA_SRGB
		0x5C, 0x5D { .dxt3 }
		0x5E, 0x5F { .dxt5 }
		0x6B, 0x6C { .etc2 }
		0x71, 0x72 { .astc }
		0x80, 0x81 { .bptc }
		else { .unknown }
	}
}

fn rgb565(color u16) gg.Color {
	r := (color >> 11) & 0x1F
	g := (color >> 5) & 0x3F
	b := color & 0x1F
	return gg.Color{
		r: u8(f32(r) / 31 * 255)
		g: u8(f32(g) / 63 * 255)
		b: u8(f32(b) / 31 * 255)
		a: 255
	}
}

fn mix_color(a gg.Color, b gg.Color) gg.Color {
	return gg.Color{
		r: u8((f32(a.r) * 2 + f32(b.r)) / 3)
		g: u8((f32(a.g) * 2 + f32(b.g)) / 3)
		b: u8((f32(a.b) * 2 + f32(b.b)) / 3)
		a: 255
	}
}

// decode_dxt1 expands one 8-byte DXT1 block into 16 RGBA pixels in row-major
// order. Four-color mode is used when color0 > color1.
pub fn decode_dxt1(block []u8) []gg.Color {
	mut out := []gg.Color{len: 16}
	if block.len < 8 {
		return out
	}
	c0 := u16(block[0]) | (u16(block[1]) << 8)
	c1 := u16(block[2]) | (u16(block[3]) << 8)
	a := rgb565(c0)
	b := rgb565(c1)
	mut c := gg.Color{}
	mut d := gg.Color{}
	if c0 > c1 {
		c = mix_color(a, b)
		d = mix_color(b, a)
	} else {
		c = mix_color(a, b)
		// 3-color DXT1 mode: the fourth color is transparent black.
		d = gg.Color{ a: 0 }
	}
	colors := [a, b, c, d]
	for i in 0 .. 16 {
		byte := block[4 + i / 4]
		shift := u8((i % 4) * 2)
		idx := (byte >> shift) & 0x3
		out[i] = colors[idx]
	}
	return out
}

// block_dimensions returns the number of 4x4 blocks covering a texture.
pub fn block_dimensions(width int, height int) (int, int) {
	return (width + 3) / 4, (height + 3) / 4
}
