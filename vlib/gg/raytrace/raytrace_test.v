module raytrace

import gg
import gg.hdr
import math

fn test_ray_sphere_hit_and_miss() {
	s := new_scene()
	mat := s.add_material(diffuse_material(hdr.white()))
	s.add_sphere(gg.Vec3{0, 0, 5}, 1, mat)
	s.build()
	hit := s.intersect(Ray{gg.Vec3{}, gg.Vec3{0, 0, 1}}) or { panic('sphere was missed') }
	assert hit.hit && hit.kind == .sphere
	assert math.abs(hit.t - 4) < 0.01
	assert hit.front
	assert math.abs(hit.normal.z + 1) < 0.01
	assert s.intersect(Ray{gg.Vec3{}, gg.Vec3{1, 0, 0}}) == none
}

fn test_bvh_returns_nearest() {
	s := new_scene()
	mat := s.add_material(diffuse_material(hdr.white()))
	for z in 3 .. 9 {
		s.add_sphere(gg.Vec3{0, 0, f32(z)}, 1, mat)
	}
	s.build()
	assert s.nodes.len > 1
	hit := s.intersect(Ray{gg.Vec3{}, gg.Vec3{0, 0, 1}}) or { panic('miss') }
	assert hit.t < 2.1
}

fn test_ray_triangle() {
	s := new_scene()
	mat := s.add_material(diffuse_material(hdr.white()))
	s.add_triangle(gg.Vec3{-1, -1, 5}, gg.Vec3{1, -1, 5}, gg.Vec3{0, 1, 5}, mat)
	s.build()
	hit := s.intersect(Ray{gg.Vec3{}, gg.Vec3{0, 0, 1}}) or { panic('triangle missed') }
	assert hit.kind == .triangle
	assert hit.uv.x > 0 && hit.uv.y > 0
}

fn test_material_lookup() {
	s := new_scene()
	idx := s.add_material(metal_material(hdr.from_rgb(1, 0, 0), 0.2))
	s.add_sphere(gg.Vec3{0, 0, 3}, 1, idx)
	s.build()
	hit := s.intersect(Ray{gg.Vec3{}, gg.Vec3{0, 0, 1}}) or { panic('miss') }
	mat := s.material_for(hit)
	assert mat.kind == .metal
	assert mat.albedo.r == 1
	assert s.material_for(Hit{}).kind == .diffuse
}

fn test_render_emissive_scene() {
	s := new_scene()
	em := s.add_material(emissive_material(hdr.from_rgb(4, 2, 1)))
	s.add_sphere(gg.Vec3{0, 0, 5}, 3, em)
	s.build()
	mut cam := gg.Camera3D{}
	cam.position = gg.Vec3{0, 0, 0}
	cam.target = gg.Vec3{0, 0, 1}
	cfg := RenderConfig{ width: 2, height: 2, samples: 1, max_depth: 1 }
	mut r := new_renderer(s, cam, cfg)
	img := r.render() or { panic('render failed') }
	assert img.pixels.len == 16
	assert img.pixels[0] > 3
	assert img.pixels[1] > 1
	assert img.pixels[3] == 1
}

fn test_render_environment() {
	mut s := new_scene()
	s.environment = hdr.from_rgb(1, 2, 3)
	s.build()
	mut cam := gg.Camera3D{}
	cam.position = gg.Vec3{0, 0, 0}
	cam.target = gg.Vec3{0, 0, 1}
	mut r := new_renderer(s, cam, RenderConfig{ width: 2, height: 2, samples: 1, max_depth: 1 })
	img := r.render() or { panic('render failed') }
	assert img.pixels[0] > 0.9 && img.pixels[0] < 1.1
	assert img.pixels[2] > 2.9
}

fn test_render_to_ldr() {
	s := new_scene()
	em := s.add_material(emissive_material(hdr.white().scale(4)))
	s.add_sphere(gg.Vec3{0, 0, 5}, 3, em)
	s.build()
	mut cam := gg.Camera3D{}
	cam.position = gg.Vec3{0, 0, 0}
	cam.target = gg.Vec3{0, 0, 1}
	mut r := new_renderer(s, cam, RenderConfig{ width: 2, height: 2, samples: 1, max_depth: 1 })
	mut pixels := []u8{len: 16, init: 0}
	r.render_to_ldr(mut pixels, 1, .aces) or { panic('render failed') }
	assert pixels[3] == 255
	assert pixels[0] > 0
}
