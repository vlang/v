/**********************************************************************
*
* path tracing demo
*
* Copyright (c) 2019-2020 Dario Deledda. All rights reserved.
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
*
**********************************************************************/
import os
import math
import rand

/******************************************************************************
*
* 3D Vector utility struct
*
******************************************************************************/
struct Vec {     
mut:  
   x f64 = f64(0.0)
   y f64 = f64(0.0)
   z f64 = f64(0.0)
}

[inline]
fn (v Vec) + (b Vec) Vec{
	return Vec{ v.x + b.x , v.y + b.y, v.z + b.z }
}

[inline]
fn (v Vec) - (b Vec) Vec{
	return Vec{ v.x - b.x , v.y - b.y, v.z - b.z }
}

[inline]
fn (v Vec) * (b Vec) Vec{
	return Vec{ v.x * b.x , v.y * b.y, v.z * b.z }
}

[inline]
fn (v Vec) dot (b Vec) f64{
	return v.x * b.x + v.y * b.y + v.z * b.z
}

[inline]
fn (v Vec) mult_s (b f64) Vec{
	return Vec{ v.x * b , v.y * b, v.z * b }
}

[inline]
fn (v Vec) cross (b Vec) Vec{
	return Vec{
		v.y * b.z - v.z * b.y,
		v.z * b.x - v.x * b.z,
		v.x * b.y - v.y * b.x
	}
}

[inline]
fn (v Vec) norm () Vec {
	tmp_norm := f64(1.0) / math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
	return Vec{ v.x * tmp_norm , v.y * tmp_norm, v.z * tmp_norm }
}

/******************************************************************************
*
* Ray
*
******************************************************************************/
struct Ray {
	o Vec
	d Vec
}

// material types, used in radiance()
enum Refl_t { 
	diff, 
	spec, 
	refr 
}  

/******************************************************************************
*
* Sphere
*
******************************************************************************/
struct Sphere {
	rad f64 = f64(0.0)   // radius
	p Vec                // position
	e Vec                // emission
	c Vec                // color
	refl Refl_t          // reflection type => [diffuse, specular, refractive]
}

[inline]
fn (sp Sphere) intersect (r Ray) f64 {
	op        := sp.p - r.o // Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0 
	mut t     := f64(0.0)
	eps       := f64(1e-4)
	b         := op.dot(r.d)
	mut det   := b * b - op.dot(op) + sp.rad * sp.rad

	if det < 0 {
		return f64(0)
	} else {
		det = math.sqrt(det)
	}

	t = b - det
	if t > eps { return t }
	t = b + det
	if t > eps { return t }
	return f64(0)
}

/******************************************************************************
*
* Scenes
*
* 0) Cornell Box with 2 spheres
* 1) Sunset
* 2) Psychedelic
*
* the sphere fileds are: Sphere{radius, position, emission, color, material}
*
******************************************************************************/
const (

Cen = Vec{50, 40.8, -860} // used by scene 1

spheres = [

[// scene 0 cornnel box
	Sphere{rad: 1e+5, p: Vec{ 1e+5 +1,40.8,81.6} , e: Vec{}        , c: Vec{.75,.25,.25}        , refl: .diff},//Left    		
	Sphere{rad: 1e+5, p: Vec{-1e+5 +99,40.8,81.6}, e: Vec{}        , c: Vec{.25,.25,.75}        , refl: .diff},//Rght 
	Sphere{rad: 1e+5, p: Vec{50,40.8, 1e+5}      , e: Vec{}        , c: Vec{.75,.75,.75}        , refl: .diff},//Back 
	Sphere{rad: 1e+5, p: Vec{50,40.8,-1e+5 +170} , e: Vec{}        , c: Vec{1e-16, 1e-16, 1e-16}, refl: .diff},//Frnt 
	Sphere{rad: 1e+5, p: Vec{50, 1e+5, 81.6}     , e: Vec{}        , c: Vec{.75,.75,.75}        , refl: .diff},//Botm 
	Sphere{rad: 1e+5, p: Vec{50,-1e+5 +81.6,81.6}, e: Vec{}        , c: Vec{.75,.75,.75}        , refl: .diff},//Top 
	Sphere{rad: 16.5, p: Vec{27.0,16.5,47.0}     , e: Vec{}        , c: Vec{1,1,1}.mult_s(.999) , refl: .spec},//Mirr 
	Sphere{rad: 16.5, p: Vec{73,16.5,78}         , e: Vec{}        , c: Vec{1,1,1}.mult_s(.999) , refl: .refr},//Glas 
	Sphere{rad: 600 , p: Vec{50,681.6-.27,81.6}  , e: Vec{12,12,12}, c: Vec{1e-16, 1e-16, 1e-16}, refl: .diff} //Lite 
]


,[// scene 1 sunset
	Sphere{rad: 1600,  p: Vec{1.0,0.0,2.0}.mult_s(3000), e: Vec{1.0,.9,.8}.mult_s(1.2e+1*1.56*2)    , c: Vec{}                     , refl: .diff}, // sun
	Sphere{rad: 1560,  p: Vec{1,0,2}.mult_s(3500)      , e: Vec{1.0,.5,.05}.mult_s(4.8e+1*1.56*2)   , c: Vec{}                     ,  refl: .diff}, // horizon sun2
	Sphere{rad: 10000, p: Cen+Vec{0,0,-200}, e: Vec{0.00063842, 0.02001478, 0.28923243}.mult_s(6e-2*8), c: Vec{.7,.7,1}.mult_s(.25),  refl: .diff}, // sky

	Sphere{rad: 100000, p: Vec{50, -100000, 0}     , e: Vec{}                    , c: Vec{.3,.3,.3}   , refl: .diff}, // grnd
	Sphere{rad: 110000, p: Vec{50, -110048.5, 0}   , e: Vec{.9,.5,.05}.mult_s(4) , c: Vec{}, refl: .diff},// horizon brightener
	Sphere{rad: 4e+4  , p: Vec{50, -4e+4-30, -3000}, e: Vec{}                    , c: Vec{.2,.2,.2}   , refl: .diff},// mountains

	Sphere{rad: 26.5, p: Vec{22,26.5,42}, e: Vec{}, c: Vec{1,1,1}.mult_s(.596)     , refl: .spec}, // white Mirr
	Sphere{rad: 13,   p: Vec{75,13,82  }, e: Vec{}, c: Vec{.96,.96,.96}.mult_s(.96), refl: .refr},// Glas
	Sphere{rad: 22,   p: Vec{87,22,24  }, e: Vec{}, c: Vec{.6,.6,.6}.mult_s(.696)  , refl: .refr}    // Glas2
]


,[// scene 3 Psychedelic
	Sphere{rad: 150, p: Vec{50+75,28,62}, e: Vec{1,1,1}.mult_s(0e-3), c: Vec{1,.9,.8}.mult_s(.93), refl: .refr},
	Sphere{rad: 28 , p: Vec{50+5,-28,62}, e: Vec{1,1,1}.mult_s(1e+1), c: Vec{1,1,1}.mult_s(0)    , refl: .diff},
	Sphere{rad: 300, p: Vec{50,28,62}   , e: Vec{1,1,1}.mult_s(0e-3), c: Vec{1,1,1}.mult_s(.93)  , refl: .spec}
]

] // end of scene array

)

/******************************************************************************
*
* Utility
*
******************************************************************************/
[inline]
fn clamp(x f64) f64 {
	if x < f64(0.0) { return f64(0.0) }
	if x > f64(1.0) { return f64(1.0) }
	return x
}

[inline]
fn to_int(x f64) int {
	p := math.pow(clamp(x), f64(1.0/2.2))
	return int(p*f64(255.0)+f64(0.5))
}

[inline]
//fn intersect(r Ray, id1 int, scene int) (bool, f64, int){
fn intersect(r Ray, id1 int, spheres []Sphere) (bool, f64, int){
	mut d  := f64(0)
	inf    := f64(1e+20)
	mut t  := f64(1e+20)
	//mut i  := spheres[scene].len-1
	mut i  := spheres.len-1
	mut id := id1
	for i >= 0 {
		//d = spheres[scene][i].intersect(r)
		d = spheres[i].intersect(r)
		if d != 0.0 && d < t {
			t = d
			id = i
		}
		i--
	}
	return (t < inf) , t, id
}

// some casual random function, try to avoid the 0
[inline]
fn rand_f64() f64 {
 	x := (C.rand()+1) & 0x3FFF_FFFF
 	return f64(x)/f64(0x3FFF_FFFF)
}

/******************************************************************************
*
* Cache for sin/cos speed-up table and scene selector
*
******************************************************************************/
const(
	cache_len = 65536           // the 2*pi angle will be splitted in 65536 part
	cache_mask = cache_len - 1  // mask to speed-up the module process
)

struct Cache {
mut:
	scene int = 0
	sin_tab [cache_len]f64
	cos_tab [cache_len]f64
}

fn (c mut Cache) fill() {
	inv_len := 1.0 / f64(cache_len)
	for i in 0..cache_len {
		x := f64(i) * math.pi * 2.0 * inv_len
		c.sin_tab[i] = math.sin(x)
		c.cos_tab[i] = math.cos(x)
	}
}


/******************************************************************************
*
* main function for the radiance calculation
*
******************************************************************************/
fn radiance(r Ray, depthi int, tb &Cache) Vec {
		mut depth   := depthi      // actual depth in the reflection tree
		mut t       := f64(0)      // distance to intersection 
		mut id      := 0           // id of intersected object
		mut res     := false       // result of intersect

		v_1 := f64(1.0)
		//v_2 := f64(2.0)

		//res, t, id = intersect(r, id, tb.scene)
		res, t, id = intersect(r, id, spheres[tb.scene])
		if !res { return Vec{} }  //if miss, return black 

		obj := spheres[tb.scene][id]        // the hit object 

		x := r.o + r.d.mult_s(t)
		n := (x - obj.p).norm()
		
		mut nl := n
		if n.dot(r.d) >= 0.0 {
			nl = n.mult_s(-1)
		}
		
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
				f = f.mult_s(1.0/p)
			} else {
				return obj.e //R.R.
			}
		}

		if obj.refl == .diff {                  // Ideal DIFFUSE reflection 
			// **Full Precision**
			//r1  := f64(2.0 * math.pi) * rand_f64()
			
			// tabbed speed-up
			r1 := C.rand() & cache_mask
			
			r2  := rand_f64()
			r2s := math.sqrt(r2)

			w   := nl

			mut u := Vec{1, 0, 0}
			if math.abs(w.x) > 0.1 {
				u = Vec{0, 1, 0}
			} 
			u = u.cross(w)
			u = u.norm()

			v := w.cross(u)
			
			// **Full Precision**
			//d := (u.mult_s(math.cos(r1) * r2s) + v.mult_s(math.sin(r1) * r2s) + w.mult_s(1.0 - r2)).norm()
			
			// tabbed speed-up
			d := (u.mult_s(tb.cos_tab[r1] * r2s) + v.mult_s(tb.sin_tab[r1] * r2s) + w.mult_s(1.0 - r2)).norm()
			
			return obj.e + (f * radiance(Ray{x, d}, depth, tb))
		} else {
			if obj.refl == .spec {            // Ideal SPECULAR reflection 
				return obj.e + (f * radiance(Ray{x, r.d - n.mult_s(2.0 * n.dot(r.d)) }, depth, tb))
			}
		}

		refl_ray := Ray{x, r.d - n.mult_s(2.0 * n.dot(r.d))}     // Ideal dielectric REFRACTION
		into     := n.dot(nl) > 0.0                          // Ray from outside going in? 
		
		nc       := f64(1.0)
		nt       := f64(1.5)

		mut nnt      := nt / nc
		if into { nnt = nc / nt }

		ddn      := r.d.dot(nl)
		mut cos2t:= f64(0)
		cos2t = v_1 - nnt * nnt * (v_1 - ddn * ddn)

		if cos2t < 0.0  {   // Total internal reflection
			return obj.e + (f * radiance(refl_ray, depth, tb))
		}

		mut dirc := -1
		if into { dirc = 1 }
		tdir := r.d.mult_s(nnt) -n.mult_s(dirc).mult_s(ddn * nnt + math.sqrt(cos2t)).norm()

		a  := nt - nc
		b  := nt + nc
		r0 := a * a / (b * b) 
		mut c  := v_1 - tdir.dot(n)
		if into { c = v_1 + ddn }

		re := r0 + (v_1 - r0) * c * c * c * c * c
		tr := v_1 - re
		p  = f64(.25) + f64(.5) * re
		rp := re / p
		tp := tr / (v_1 - p)

	mut res_f := obj.e

	mut tmp := radiance(Ray{x, tdir}, depth, tb).mult_s(tp)
	if rand_f64() < p {
		tmp = radiance(refl_ray, depth, tb).mult_s(rp)
	}

	if depth > 2 {
		res_f = res_f + f * tmp
		return res_f
	}

	tmp1 := radiance(refl_ray, depth, tb).mult_s(re) + radiance( Ray{x, tdir}, depth, tb).mult_s(tr)
	res_f = res_f + f * tmp1
	return res_f
} 

/******************************************************************************
*
* beam scan routine
*
******************************************************************************/
fn ray_trace(w int, h int, samps int, file_name string, tb &Cache) {

	// inverse costants
	w1     := f64(1.0 / w)
	h1     := f64(1.0 / h)
	samps1 := f64(1.0 / samps)

	cam   := Ray{Vec{50, 52, 296.5},  Vec{0, -0.042612, -1}.norm()} // cam position, direction
	cx    := Vec{ f64(w) * .5135 / f64(h), 0, 0}
	cy    := ((cx.cross(cam.d)).norm()).mult_s(0.5135)
	mut c := [Vec{}].repeat(w * h)
	mut r := Vec{}
	
	// OpenMP injection point! #pragma omp parallel for schedule(dynamic, 1) shared(c)
	for y:=0; y < h; y++ {
		eprint("\rRendering (${samps * 4} spp) ${(100.0 * f64(y)) / (f64(h) - 1.0)}%")
		for x := 0; x < w; x++ {

			i := (h - y - 1) * w + x
			// we use sx and sy to perform a square subsampling of 4 samples
			for sy := f64(0.5) ; sy < 2.5; sy += 1.0 {
				for sx := f64(0.5); sx < 2.5; sx += 1.0 {
					r.x = 0
					r.y = 0
					r.z = 0
					for s := 0; s < samps; s++ {
						// speed-up constants
						v_1 := f64(1.0)
						v_2 := f64(2.0)

						r1 := v_2 * rand_f64()
						mut dx := v_1 - math.sqrt(v_2 - r1)
						if r1 < v_1 { dx = math.sqrt(r1) - v_1 }

						r2 := v_2 * rand_f64()
						mut dy := v_1 - math.sqrt(v_2 - r2)
						if r2 < v_1 { dy = math.sqrt(r2) - v_1 }

                        d := cx.mult_s( ( (sx + dx)*0.5 + f64(x))*w1 - .5) + 
                             cy.mult_s( ( (sy + dy)*0.5 + f64(y))*h1 - .5) + cam.d

                        r = r + radiance(Ray{cam.o+d.mult_s(140.0), d.norm()}, 0, tb).mult_s(samps1)

					}
					tmp_vec := Vec{clamp(r.x),clamp(r.y),clamp(r.z)}.mult_s(.25)
					c[i] = c[i] + tmp_vec
				}
			}
		} 
	}
	eprintln('\nRendering finished.')

	//
	// write out a .ppm file
	//
	mut f_out := os.create(file_name) or { exit }
	f_out.writeln('P3')
	f_out.writeln('${w} ${h}')
	f_out.writeln('255')
	for i in 0..w*h {
		c_r := to_int(c[i].x)
		c_g := to_int(c[i].y)
		c_b := to_int(c[i].z)
		f_out.write('$c_r $c_g $c_b ')
	}
	f_out.close()

	println("image saved as [${file_name}]")
}

fn main() {
	// init the rand, using the same seed allows to obtain the same result in different runs
	// change the seed from 2020 for different results
	rand.seed(2020)  
	
	// init the sin/cos cache table 
	mut tb := Cache{}
	tb.fill()

	width    := 1280  // width of the rendering in pixels
	height   := 1280  // height of the rendering in pixels
	samples  := 10    // number of samples*4 per pixel, increase for better quality
	tb.scene = 1      // scene to render [0 cornell box,1 sunset,2 psyco]  
	file_name := "image.ppm" // name of the output file in .ppm format

	ray_trace(width, height, samples, file_name, tb)
}
