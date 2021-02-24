/**********************************************************************
* path tracing demo
*
* Copyright (c) 2019-2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* This file contains a path tracer example in less of 500 line of codes
* 3 demo scenes included
*
* This code is inspired by:
* - "Realistic Ray Tracing" by Peter Shirley 2000 ISBN-13: 978-1568814612
* - https://www.kevinbeason.com/smallpt/
*
* Known limitations:
* - there are some approximation errors in the calculations
* - to speed-up the code a cos/sin table is used
* - the full precision code is present but commented, can be restored very easily
* - an higher number of samples ( > 60) can block the program on higher resolutions
*   without a stack size increase
* - as a recursive program this code depend on the stack size,
*   for higher number of samples increase the stack size
*   in linux: ulimit -s byte_size_of_the_stack
*   example: ulimit -s 16000000
* - No OpenMP support
**********************************************************************/
import os
import math
import rand
import time

const (
	inf = 1e+10
	eps = 1e-4
	f_0 = 0.0
)

//**************************** 3D Vector utility struct *********************
struct Vec {
mut:
	x f64 = 0.0
	y f64 = 0.0
	z f64 = 0.0
}

[inline]
fn (v Vec) + (b Vec) Vec {
	return Vec{v.x + b.x, v.y + b.y, v.z + b.z}
}

[inline]
fn (v Vec) - (b Vec) Vec {
	return Vec{v.x - b.x, v.y - b.y, v.z - b.z}
}

[inline]
fn (v Vec) * (b Vec) Vec {
	return Vec{v.x * b.x, v.y * b.y, v.z * b.z}
}

[inline]
fn (v Vec) dot(b Vec) f64 {
	return v.x * b.x + v.y * b.y + v.z * b.z
}

[inline]
fn (v Vec) mult_s(b f64) Vec {
	return Vec{v.x * b, v.y * b, v.z * b}
}

[inline]
fn (v Vec) cross(b Vec) Vec {
	return Vec{v.y * b.z - v.z * b.y, v.z * b.x - v.x * b.z, v.x * b.y - v.y * b.x}
}

[inline]
fn (v Vec) norm() Vec {
	tmp_norm := 1.0 / math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
	return Vec{v.x * tmp_norm, v.y * tmp_norm, v.z * tmp_norm}
}

//********************************Image**************************************
struct Image {
	width  int
	height int
	data   &Vec
}

fn new_image(w int, h int) Image {
	vecsize := int(sizeof(Vec))
	return Image{
		width: w
		height: h
		data: &Vec(vcalloc(vecsize * w * h))
	}
}

// write out a .ppm file
fn (image Image) save_as_ppm(file_name string) {
	npixels := image.width * image.height
	mut f_out := os.create(file_name) or { panic(err) }
	f_out.writeln('P3') or { panic(err) }
	f_out.writeln('$image.width $image.height') or { panic(err) }
	f_out.writeln('255') or { panic(err) }
	for i in 0 .. npixels {
		c_r := to_int(unsafe { image.data[i] }.x)
		c_g := to_int(unsafe { image.data[i] }.y)
		c_b := to_int(unsafe { image.data[i] }.z)
		f_out.write_str('$c_r $c_g $c_b ') or { panic(err) }
	}
	f_out.close()
}

//********************************** Ray ************************************
struct Ray {
	o Vec
	d Vec
}

// material types, used in radiance()
enum Refl_t {
	diff
	spec
	refr
}

//******************************** Sphere ***********************************
struct Sphere {
	rad  f64 = 0.0 // radius
	p    Vec    // position
	e    Vec    // emission
	c    Vec    // color
	refl Refl_t // reflection type => [diffuse, specular, refractive]
}

fn (sp Sphere) intersect(r Ray) f64 {
	op := sp.p - r.o // Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
	b := op.dot(r.d)
	mut det := b * b - op.dot(op) + sp.rad * sp.rad

	if det < 0 {
		return 0
	}

	det = math.sqrt(det)

	mut t := b - det
	if t > eps {
		return t
	}

	t = b + det
	if t > eps {
		return t
	}
	return 0
}

/*********************************** Scenes **********************************
* 0) Cornell Box with 2 spheres
* 1) Sunset
* 2) Psychedelic
* The sphere fileds are: Sphere{radius, position, emission, color, material}
******************************************************************************/
const (
	cen     = Vec{50, 40.8, -860} // used by scene 1
	spheres = [
		[/* scene 0 cornnel box */ Sphere{
			rad: 1e+5
			p: Vec{1e+5 + 1, 40.8, 81.6}
			e: Vec{}
			c: Vec{.75, .25, .25}
			refl: .diff
		}, /* Left */ Sphere{
			rad: 1e+5
			p: Vec{-1e+5 + 99, 40.8, 81.6}
			e: Vec{}
			c: Vec{.25, .25, .75}
			refl: .diff
		}, /* Rght */ Sphere{
			rad: 1e+5
			p: Vec{50, 40.8, 1e+5}
			e: Vec{}
			c: Vec{.75, .75, .75}
			refl: .diff
		}, /* Back */ Sphere{
			rad: 1e+5
			p: Vec{50, 40.8, -1e+5 + 170}
			e: Vec{}
			c: Vec{}
			refl: .diff
		}, /* Frnt */ Sphere{
			rad: 1e+5
			p: Vec{50, 1e+5, 81.6}
			e: Vec{}
			c: Vec{.75, .75, .75}
			refl: .diff
		}, /* Botm */ Sphere{
			rad: 1e+5
			p: Vec{50, -1e+5 + 81.6, 81.6}
			e: Vec{}
			c: Vec{.75, .75, .75}
			refl: .diff
		}, /* Top */ Sphere{
			rad: 16.5
			p: Vec{27, 16.5, 47}
			e: Vec{}
			c: Vec{1, 1, 1}.mult_s(.999)
			refl: .spec
		}, /* Mirr */ Sphere{
			rad: 16.5
			p: Vec{73, 16.5, 78}
			e: Vec{}
			c: Vec{1, 1, 1}.mult_s(.999)
			refl: .refr
		}, /* Glas */ Sphere{
			rad: 600
			p: Vec{50, 681.6 - .27, 81.6}
			e: Vec{12, 12, 12}
			c: Vec{}
			refl: .diff
		} /* Lite */],
		[/* scene 1 sunset */ Sphere{
			rad: 1600
			p: Vec{1.0, 0.0, 2.0}.mult_s(3000)
			e: Vec{1.0, .9, .8}.mult_s(1.2e+1 * 1.56 * 2)
			c: Vec{}
			refl: .diff
		}, /* sun */ Sphere{
			rad: 1560
			p: Vec{1, 0, 2}.mult_s(3500)
			e: Vec{1.0, .5, .05}.mult_s(4.8e+1 * 1.56 * 2)
			c: Vec{}
			refl: .diff
		}, /* horizon sun2 */ Sphere{
			rad: 10000
			p: cen + Vec{0, 0, -200}
			e: Vec{0.00063842, 0.02001478, 0.28923243}.mult_s(6e-2 * 8)
			c: Vec{.7, .7, 1}.mult_s(.25)
			refl: .diff
		}, /* sky */ Sphere{
			rad: 100000
			p: Vec{50, -100000, 0}
			e: Vec{}
			c: Vec{.3, .3, .3}
			refl: .diff
		}, /* grnd */ Sphere{
			rad: 110000
			p: Vec{50, -110048.5, 0}
			e: Vec{.9, .5, .05}.mult_s(4)
			c: Vec{}
			refl: .diff
		}, /* horizon brightener */ Sphere{
			rad: 4e+4
			p: Vec{50, -4e+4 - 30, -3000}
			e: Vec{}
			c: Vec{.2, .2, .2}
			refl: .diff
		}, /* mountains */ Sphere{
			rad: 26.5
			p: Vec{22, 26.5, 42}
			e: Vec{}
			c: Vec{1, 1, 1}.mult_s(.596)
			refl: .spec
		}, /* white Mirr */ Sphere{
			rad: 13
			p: Vec{75, 13, 82}
			e: Vec{}
			c: Vec{.96, .96, .96}.mult_s(.96)
			refl: .refr
		}, /* Glas */ Sphere{
			rad: 22
			p: Vec{87, 22, 24}
			e: Vec{}
			c: Vec{.6, .6, .6}.mult_s(.696)
			refl: .refr
		} /* Glas2 */],
		[/* scene 3 Psychedelic */ Sphere{
			rad: 150
			p: Vec{50 + 75, 28, 62}
			e: Vec{1, 1, 1}.mult_s(0e-3)
			c: Vec{1, .9, .8}.mult_s(.93)
			refl: .refr
		}, Sphere{
			rad: 28
			p: Vec{50 + 5, -28, 62}
			e: Vec{1, 1, 1}.mult_s(1e+1)
			c: Vec{1, 1, 1}.mult_s(0)
			refl: .diff
		}, Sphere{
			rad: 300
			p: Vec{50, 28, 62}
			e: Vec{1, 1, 1}.mult_s(0e-3)
			c: Vec{1, 1, 1}.mult_s(.93)
			refl: .spec
		}],
	] // end of scene array
)

//********************************** Utilities ******************************
[inline]
fn clamp(x f64) f64 {
	if x < 0 {
		return 0
	}
	if x > 1 {
		return 1
	}
	return x
}

[inline]
fn to_int(x f64) int {
	p := math.pow(clamp(x), 1.0 / 2.2)
	return int(p * 255.0 + 0.5)
}

fn intersect(r Ray, spheres &Sphere, nspheres int) (bool, f64, int) {
	mut d := 0.0
	mut t := inf
	mut id := 0
	for i := nspheres - 1; i >= 0; i-- {
		d = unsafe { spheres[i] }.intersect(r)
		if d > 0 && d < t {
			t = d
			id = i
		}
	}
	return (t < inf), t, id
}

// some casual random function, try to avoid the 0
fn rand_f64() f64 {
	x := rand.u32() & 0x3FFF_FFFF
	return f64(x) / f64(0x3FFF_FFFF)
}

const (
	cache_len  = 65536 // the 2*pi angle will be splitted in 65536 part
	cache_mask = cache_len - 1 // mask to speed-up the module process
)

struct Cache {
mut:
	sin_tab [65536]f64
	cos_tab [65536]f64
}

fn new_tabs() Cache {
	mut c := Cache{}
	inv_len := 1.0 / f64(cache_len)
	for i in 0 .. cache_len {
		x := f64(i) * math.pi * 2.0 * inv_len
		c.sin_tab[i] = math.sin(x)
		c.cos_tab[i] = math.cos(x)
	}
	return c
}

//************ Cache for sin/cos speed-up table and scene selector **********
const (
	tabs = new_tabs()
)

//****************** main function for the radiance calculation *************
fn radiance(r Ray, depthi int, scene_id int) Vec {
	if depthi > 1024 {
		eprintln('depthi: $depthi')
		return Vec{}
	}
	mut depth := depthi // actual depth in the reflection tree
	mut t := 0.0 // distance to intersection
	mut id := 0 // id of intersected object
	mut res := false // result of intersect

	v_1 := 1.0
	// v_2 := f64(2.0)

	scene := spheres[scene_id]
	// res, t, id = intersect(r, id, tb.scene)
	res, t, id = intersect(r, scene.data, scene.len)
	if !res {
		return Vec{}
	}
	// if miss, return black

	obj := scene[id] // the hit object

	x := r.o + r.d.mult_s(t)
	n := (x - obj.p).norm()

	nl := if n.dot(r.d) < 0.0 { n } else { n.mult_s(-1) }

	mut f := obj.c

	// max reflection
	mut p := f.z
	if f.x > f.y && f.x > f.z {
		p = f.x
	} else {
		if f.y > f.z {
			p = f.y
		}
	}

	depth++
	if depth > 5 {
		if rand_f64() < p {
			f = f.mult_s(f64(1.0) / p)
		} else {
			return obj.e // R.R.
		}
	}

	if obj.refl == .diff { // Ideal DIFFUSE reflection
		// **Full Precision**
		// r1  := f64(2.0 * math.pi) * rand_f64()

		// tabbed speed-up
		r1 := rand.u32() & cache_mask

		r2 := rand_f64()
		r2s := math.sqrt(r2)

		w := nl

		mut u := if math.abs(w.x) > f64(0.1) { Vec{0, 1, 0} } else { Vec{1, 0, 0} }
		u = u.cross(w).norm()

		v := w.cross(u)

		// **Full Precision**
		// d := (u.mult_s(math.cos(r1) * r2s) + v.mult_s(math.sin(r1) * r2s) + w.mult_s(1.0 - r2)).norm()

		// tabbed speed-up
		d := (u.mult_s(tabs.cos_tab[r1] * r2s) + v.mult_s(tabs.sin_tab[r1] * r2s) +
			w.mult_s(math.sqrt(f64(1.0) - r2))).norm()

		return obj.e + f * radiance(Ray{x, d}, depth, scene_id)
	} else {
		if obj.refl == .spec { // Ideal SPECULAR reflection
			return obj.e + f * radiance(Ray{x, r.d - n.mult_s(2.0 * n.dot(r.d))}, depth, scene_id)
		}
	}

	refl_ray := Ray{x, r.d - n.mult_s(2.0 * n.dot(r.d))} // Ideal dielectric REFRACTION
	into := n.dot(nl) > 0 // Ray from outside going in?

	nc := f64(1.0)
	nt := f64(1.5)

	nnt := if into { nc / nt } else { nt / nc }

	ddn := r.d.dot(nl)
	cos2t := v_1 - nnt * nnt * (v_1 - ddn * ddn)
	if cos2t < 0.0 { // Total internal reflection
		return obj.e + f * radiance(refl_ray, depth, scene_id)
	}

	dirc := if into { f64(1) } else { f64(-1) }
	tdir := (r.d.mult_s(nnt) - n.mult_s(dirc * (ddn * nnt + math.sqrt(cos2t)))).norm()

	a := nt - nc
	b := nt + nc
	r0 := a * a / (b * b)
	c := if into { v_1 + ddn } else { v_1 - tdir.dot(n) }

	re := r0 + (v_1 - r0) * c * c * c * c * c
	tr := v_1 - re
	pp := f64(.25) + f64(.5) * re
	rp := re / pp
	tp := tr / (v_1 - pp)

	mut tmp := Vec{}
	if depth > 2 {
		// Russian roulette
		tmp = if rand_f64() < pp {
			radiance(refl_ray, depth, scene_id).mult_s(rp)
		} else {
			radiance(Ray{x, tdir}, depth, scene_id).mult_s(tp)
		}
	} else {
		tmp = (radiance(refl_ray, depth, scene_id).mult_s(re)) +
			(radiance(Ray{x, tdir}, depth, scene_id).mult_s(tr))
	}
	return obj.e + (f * tmp)
}

//*********************** beam scan routine *********************************
fn ray_trace(w int, h int, samps int, file_name string, scene_id int) Image {
	image := new_image(w, h)

	// inverse costants
	w1 := f64(1.0 / f64(w))
	h1 := f64(1.0 / f64(h))
	samps1 := f64(1.0 / f64(samps))

	cam := Ray{Vec{50, 52, 295.6}, Vec{0, -0.042612, -1}.norm()} // cam position, direction
	cx := Vec{f64(w) * 0.5135 / f64(h), 0, 0}
	cy := cx.cross(cam.d).norm().mult_s(0.5135)
	mut r := Vec{}

	// speed-up constants
	v_1 := f64(1.0)
	v_2 := f64(2.0)

	// OpenMP injection point! #pragma omp parallel for schedule(dynamic, 1) shared(c)
	for y := 0; y < h; y++ {
		eprint('\rRendering (${samps * 4} spp) ${(100.0 * f64(y)) / (f64(h) - 1.0):5.2f}%')
		for x in 0 .. w {
			i := (h - y - 1) * w + x
			mut ivec := unsafe { &image.data[i] }
			// we use sx and sy to perform a square subsampling of 4 samples
			for sy := 0; sy < 2; sy++ {
				for sx := 0; sx < 2; sx++ {
					r = Vec{0, 0, 0}
					for _ in 0 .. samps {
						r1 := v_2 * rand_f64()
						dx := if r1 < v_1 { math.sqrt(r1) - v_1 } else { v_1 - math.sqrt(v_2 - r1) }

						r2 := v_2 * rand_f64()
						dy := if r2 < v_1 { math.sqrt(r2) - v_1 } else { v_1 - math.sqrt(v_2 - r2) }

						d := cx.mult_s(((f64(sx) + 0.5 + dx) * 0.5 + f64(x)) * w1 - .5) +
							cy.mult_s(((f64(sy) + 0.5 + dy) * 0.5 + f64(y)) * h1 - .5) + cam.d
						r = r + radiance(Ray{cam.o +
							d.mult_s(140.0), d.norm()}, 0, scene_id).mult_s(samps1)
					}
					tmp_vec := Vec{clamp(r.x), clamp(r.y), clamp(r.z)}.mult_s(.25)
					(*ivec) = *ivec + tmp_vec
				}
			}
		}
	}
	return image
}

fn main() {
	if os.args.len > 6 {
		eprintln('Usage:\n     path_tracing [samples] [image.ppm] [scene_n] [width] [height]')
		exit(1)
	}
	mut width := 320 // width of the rendering in pixels
	mut height := 200 // height of the rendering in pixels
	mut samples := 4 // number of samples per pixel, increase for better quality
	mut scene_id := 0 // scene to render [0 cornell box,1 sunset,2 psyco]
	mut file_name := 'image.ppm' // name of the output file in .ppm format

	if os.args.len >= 2 {
		samples = os.args[1].int() / 4
	}
	if os.args.len >= 3 {
		file_name = os.args[2]
	}
	if os.args.len >= 4 {
		scene_id = os.args[3].int()
	}
	if os.args.len >= 5 {
		width = os.args[4].int()
	}
	if os.args.len == 6 {
		height = os.args[5].int()
	}
	// change the seed for a different result
	rand.seed([u32(2020), 0])

	t1 := time.ticks()

	image := ray_trace(width, height, samples, file_name, scene_id)
	t2 := time.ticks()

	eprintln('\nRendering finished. Took: ${(t2 - t1):5}ms')

	image.save_as_ppm(file_name)
	t3 := time.ticks()

	eprintln('Image saved as [$file_name]. Took: ${(t3 - t2):5}ms')
}
