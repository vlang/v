module main

// HDR starter: linear-light colors, exposure, tone mapping, bloom and Radiance
// .hdr file I/O. Run with: v run examples/gg/hdr_base.v
import gg.hdr
import os

fn main() {
	// Values above 1 stay intact until the tone-mapping stage.
	sun := hdr.from_rgb(8, 6, 3)
	display := sun.tone_map(0.5, .aces)
	println('HDR sun -> display: r=${display.r} g=${display.g} b=${display.b}')

	// Expand an LDR image to linear light, adjust exposure, apply bloom, then
	// compress it back to an RGBA8 buffer.
	ldr := [u8(255), 180, 90, 255, 60, 120, 200, 255, 230, 230, 230, 255, 20, 30, 40, 255]
	mut linear := hdr.from_ldr(ldr)
	hdr.apply_exposure(mut linear, 1.5)
	mut bloomed := []f32{len: linear.len}
	hdr.bloom_into(mut bloomed, linear, 2, 2, 1.0, 0.3, 1)
	mut out := []u8{len: ldr.len}
	hdr.to_ldr_into(mut out, bloomed, 1, .aces)
	println('tone mapped pixel: ${out[0]},${out[1]},${out[2]},${out[3]}')

	// Save and reload a small HDR image.
	mut img := hdr.new_image(2, 2) or { panic('image failed') }
	img.pixels[0] = 4
	img.pixels[1] = 2
	img.pixels[2] = 1
	path := os.join_path(os.temp_dir(), 'gg_hdr_example.hdr')
	img.save_rgbe(path) or { panic('save failed') }
	loaded := hdr.load_rgbe(path) or { panic('load failed') }
	println('round trip: ${loaded.width}x${loaded.height} r=${loaded.pixels[0]:.2f}')
	os.rm(path) or {}

	// Native float render targets are opt-in and need a live sokol context.
	if hdr.float_targets_available() {
		println('float render targets available (rgba16f/rgba32f)')
	} else {
		println('using the CPU HDR path on this backend')
	}
}
