module main

// CPU path tracer starter: build a small scene, trace it into an HDR buffer,
// tone map it and save both Radiance .hdr and PPM previews.
// Run with: v run examples/gg/raytrace_base.v
import gg
import gg.hdr
import gg.raytrace as rt
import os

fn main() {
	w := 160
	h := 100
	mut scene := rt.new_scene()
	floor := scene.add_material(rt.diffuse_material(hdr.from_rgb(0.75, 0.75, 0.75)))
	red := scene.add_material(rt.diffuse_material(hdr.from_rgb(0.9, 0.15, 0.12)))
	metal := scene.add_material(rt.metal_material(hdr.from_rgb(0.9, 0.9, 0.95), 0.08))
	light := scene.add_material(rt.emissive_material(hdr.from_rgb(6, 5, 4)))
	scene.add_sphere(gg.Vec3{0, 0, -3}, 1.2, red)
	scene.add_sphere(gg.Vec3{2, 0, -4}, 1, metal)
	scene.add_sphere(gg.Vec3{-2, 1, -4}, 0.8, light)
	scene.add_triangle(gg.Vec3{-6, -1.2, -8}, gg.Vec3{6, -1.2, -8}, gg.Vec3{-6, -1.2, 4}, floor)
	scene.add_triangle(gg.Vec3{6, -1.2, -8}, gg.Vec3{6, -1.2, 4}, gg.Vec3{-6, -1.2, 4}, floor)
	scene.build()

	mut cam := gg.Camera3D{}
	cam.position = gg.Vec3{0, 0, 4}
	cam.target = gg.Vec3{0, 0, -2}
	cfg := rt.RenderConfig{
		width:     w
		height:    h
		samples:   4
		max_depth: 4
	}
	mut renderer := rt.new_renderer(scene, cam, cfg)
	img := renderer.render() or { panic('render failed') }

	hdr_path := os.join_path(os.temp_dir(), 'gg_raytrace_example.hdr')
	img.save_rgbe(hdr_path) or { panic('save failed') }
	println('HDR saved to ${hdr_path}')

	mut ldr := []u8{len: w * h * 4}
	hdr.to_ldr_into(mut ldr, img.pixels, 1, .aces)
	mut ppm := []u8{}
	ppm << 'P6\n${w} ${h}\n255\n'.bytes()
	for i in 0 .. w * h {
		at := i * 4
		ppm << ldr[at]
		ppm << ldr[at + 1]
		ppm << ldr[at + 2]
	}
	ppm_path := os.join_path(os.temp_dir(), 'gg_raytrace_example.ppm')
	os.write_file(ppm_path, ppm.bytestr()) or { panic('ppm save failed') }
	println('PPM saved to ${ppm_path}')
}
