module raytrace

import gg
import gg.hdr
import math

// RenderConfig controls resolution, sampling and recursion depth.
pub struct RenderConfig {
pub mut:
	width     int = 320
	height    int = 200
	samples   int = 4
	max_depth int = 4
	seed      u32 = 0x9e3779b9
}

// Renderer path traces a Scene through a gg.Camera3D into HDR buffers.
pub struct Renderer {
pub mut:
	scene  &Scene
	camera gg.Camera3D
	config RenderConfig
	rng    u32
}

// new_renderer returns a renderer for a scene and camera.
pub fn new_renderer(scene &Scene, cam gg.Camera3D, cfg RenderConfig) Renderer {
	seed := if cfg.seed == 0 { u32(0x9e3779b9) } else { cfg.seed }
	return Renderer{ scene: scene, camera: cam, config: cfg, rng: seed }
}

fn (mut r Renderer) next() f32 {
	if r.rng == 0 {
		r.rng = 0x9e3779b9
	}
	r.rng ^= r.rng << 13
	r.rng ^= r.rng >> 17
	r.rng ^= r.rng << 5
	return f32(r.rng & 0x00ffffff) / f32(0x00ffffff)
}

fn reflect(d gg.Vec3, n gg.Vec3) gg.Vec3 {
	return d - n.scale(2 * d.dot(n))
}

fn refract(d gg.Vec3, n gg.Vec3, eta f32) ?gg.Vec3 {
	cosine := -n.dot(d)
	sin2 := eta * eta * (1 - cosine * cosine)
	if sin2 > 1 {
		return none
	}
	cost := f32(math.sqrt(f64(1 - sin2)))
	return d.scale(eta) + n.scale(eta * cosine - cost)
}

fn make_basis(n gg.Vec3) (gg.Vec3, gg.Vec3) {
	mut a := gg.Vec3{1, 0, 0}
	if math.abs(f64(n.x)) > 0.9 {
		a = gg.Vec3{0, 1, 0}
	}
	u := a.cross(n).normalized()
	return u, n.cross(u)
}

fn (mut r Renderer) cosine_dir(n gg.Vec3) gg.Vec3 {
	u, v := make_basis(n)
	phi := 2 * math.pi * f64(r.next())
	mut r2 := f32(0)
	for r2 <= 0 || r2 > 1 {
		r2 = r.next()
	}
	rad := f32(math.sqrt(f64(r2)))
	height := f32(math.sqrt(f64(1 - r2)))
	return u.scale(f32(math.cos(phi)) * rad) + v.scale(f32(math.sin(phi)) * rad) + n.scale(height)
}

fn (mut r Renderer) random_unit() gg.Vec3 {
	for {
		d := gg.Vec3{r.next() * 2 - 1, r.next() * 2 - 1, r.next() * 2 - 1}
		length2 := d.dot(d)
		if length2 > 0 && length2 <= 1 {
			return d.scale(1 / f32(math.sqrt(f64(length2))))
		}
	}
}

fn schlick(cosine f32, ior f32) f32 {
	mut r0 := (1 - ior) / (1 + ior)
	r0 = r0 * r0
	return r0 + (1 - r0) * f32(math.pow(f64(1 - cosine), 5))
}

// trace returns the radiance travelling along ray, including all bounces up
// to the configured depth limit.
pub fn (mut r Renderer) trace(ray Ray, depth int) hdr.ColorHDR {
	if depth >= r.config.max_depth {
		return hdr.ColorHDR{}
	}
	hit := r.scene.intersect(ray) or { return r.scene.environment }
	mat := r.scene.material_for(hit)
	if mat.kind == .emissive {
		return mat.emission
	}
	facing := hit.normal.scale(if hit.front { 1 } else { -1 })
	origin := hit.point + facing.scale(epsilon)
	match mat.kind {
		.diffuse {
			next := Ray{origin, r.cosine_dir(facing)}
			return mat.albedo.scale(0.95).add(r.trace(next, depth + 1))
		}
		.metal {
			dir := (reflect(ray.direction, facing) + r.random_unit().scale(mat.roughness)).normalized()
			next := Ray{origin, dir}
			return mat.albedo.scale(0.95).add(r.trace(next, depth + 1))
		}
		.dielectric {
			eta := if hit.front { 1 / mat.ior } else { mat.ior }
			cosine := f32(math.abs(f64(-facing.dot(ray.direction))))
			prob := schlick(cosine, mat.ior)
			if refracted := refract(ray.direction, facing, eta) {
				if r.next() >= prob {
					return r.trace(Ray{origin, refracted}, depth + 1)
				}
			}
			return r.trace(Ray{origin, reflect(ray.direction, facing)}, depth + 1)
		}
		.emissive {
			return mat.emission
		}
	}
}

// render accumulates the scene into a linear-light HDR image. The returned
// pixels can be passed to gg.hdr tone mapping or saved as Radiance RGBE.
pub fn (mut r Renderer) render() !hdr.HDRImage {
	if r.config.width <= 0 || r.config.height <= 0 || r.config.samples <= 0 {
		return error('raytrace: invalid render configuration')
	}
	if !r.scene.built {
		r.scene.build()
	}
	mut img := hdr.new_image(r.config.width, r.config.height)!
	w := f32(r.config.width)
	h := f32(r.config.height)
	for py in 0 .. r.config.height {
		for px in 0 .. r.config.width {
			at := (py * r.config.width + px) * 4
			mut accum := hdr.ColorHDR{}
			for _ in 0 .. r.config.samples {
				mut ray := gg.screen_to_world_ray(f32(px) + r.next(), f32(py) + r.next(), r.camera, w, h)
				ray.direction = ray.direction.normalized()
				accum = accum.add(r.trace(Ray{ray.origin, ray.direction}, 0))
			}
			inv := 1 / f32(r.config.samples)
			img.pixels[at] = accum.r * inv
			img.pixels[at + 1] = accum.g * inv
			img.pixels[at + 2] = accum.b * inv
			img.pixels[at + 3] = 1
		}
	}
	return img
}

// render_to_ldr renders directly into a caller-owned RGBA8 buffer.
pub fn (mut r Renderer) render_to_ldr(mut dst []u8, exposure f32, tone hdr.ToneMap) ! {
	img := r.render()!
	hdr.to_ldr_into(mut dst, img.pixels, exposure, tone)
}
