module hdr

import gg
import math
import os
import time

fn test_color_conversions() {
	c := from_ldr_color(gg.rgb(255, 255, 255))
	assert c.r > 0.99 && c.g > 0.99 && c.b > 0.99
	back := c.to_ldr_color()
	assert back.r >= 254 && back.g >= 254 && back.b >= 254

	mut bright := from_rgb(4, 2, 1)
	assert bright.luminance() > 2.3
	scaled := bright.scale(0.5)
	assert scaled.r == 2
	sum := bright.add(from_rgb(1, 1, 1))
	assert sum.r == 5
}

fn test_tone_mapping() {
	assert tone_map_channel(-1, .aces) == 0
	assert tone_map_channel(1, .none) == 1
	assert tone_map_channel(4, .reinhard) < 1
	assert tone_map_channel(4, .aces) < 1 && tone_map_channel(4, .aces) > 0.9

	src := [f32(4), 4, 4, 1]
	mut plain := [u8(0), 0, 0, 255]
	mut aces := [u8(0), 0, 0, 255]
	to_ldr_into(mut plain, src, 1, .none)
	to_ldr_into(mut aces, src, 1, .aces)
	assert plain[0] == 255
	assert aces[0] > 230 && aces[0] < 255
	assert aces[3] == 255
}

fn test_exposure_and_bloom() {
	mut px := []f32{len: 16, init: 0.5}
	px[0] = 4
	apply_exposure(mut px, 2)
	assert px[1] == 1

	mut base := []f32{len: 16, init: 0.1}
	base[5] = 3
	mut out := []f32{len: 16, init: 0}
	bloom_into(mut out, base, 4, 4, 1.0, 0.5, 1)
	assert out[5] >= base[5]
	assert out[0] > base[0]
}

fn test_ldr_roundtrip() {
	ldr := [u8(255), 128, 0, 255]
	linear := from_ldr(ldr)
	assert linear[0] > 0.99
	assert linear[1] > 0.2 && linear[1] < 0.3
	assert linear[2] == 0
	mut out := [u8(0), 0, 0, 0]
	to_ldr_into(mut out, linear, 1, .none)
	assert math.abs(f32(out[1]) - 128) <= 1
	assert out[3] == 255
}

fn test_rgbe_roundtrip() {
	mut img := new_image(2, 2)!
	img.pixels[0] = 4
	img.pixels[1] = 2
	img.pixels[2] = 1
	img.pixels[3] = 1
	img.pixels[7] = 8
	data := img.encode_rgbe()
	decoded := decode_rgbe(data)!
	assert decoded.width == 2 && decoded.height == 2
	assert math.abs(decoded.pixels[0] - 4) < 0.05
	assert math.abs(decoded.pixels[1] - 2) < 0.05
	assert math.abs(decoded.pixels[2] - 1) < 0.05
}

fn test_rgbe_rle_decode() {
	mut data := []u8{}
	data << '#?RADIANCE\n'.bytes()
	data << 'FORMAT=32-bit_rle_rgbe\n\n'.bytes()
	data << '-Y 1 +X 8\n'.bytes()
	data << [u8(2), 2, 0, 8]
	data << [u8(136), 200]
	data << [u8(136), 150]
	data << [u8(136), 100]
	data << [u8(136), 136]
	img := decode_rgbe(data)!
	assert img.width == 8 && img.height == 1
	assert img.pixels[0] > 199 && img.pixels[0] < 201
	assert img.pixels[2] > 99 && img.pixels[2] < 101
	assert img.pixels[3] == 1
}

fn test_rgbe_oversized_header_is_rejected() {
	mut data := []u8{}
	data << '#?RADIANCE\n'.bytes()
	data << 'FORMAT=32-bit_rle_rgbe\n\n'.bytes()
	data << '-Y 65536 +X 65536\n'.bytes()
	data << [u8(0), 0, 0, 0]
	img := decode_rgbe(data) or {
		assert err.msg().contains('dimensions too large')
		return
	}
	assert false, 'decode_rgbe accepted oversized dimensions ${img.width}x${img.height}'
}

fn test_rgbe_file_io() {
	mut img := new_image(1, 1)!
	img.pixels[0] = 3
	img.pixels[1] = 1
	img.pixels[2] = 0.5
	path := os.join_path(os.temp_dir(), 'gg_hdr_${time.now().unix_nano()}.hdr')
	defer { os.rm(path) or {} }
	img.save_rgbe(path)!
	loaded := load_rgbe(path)!
	assert loaded.width == 1 && loaded.height == 1
	assert math.abs(loaded.pixels[0] - 3) < 0.05
}

fn test_capability() {
	assert float_targets_available() == true
}
