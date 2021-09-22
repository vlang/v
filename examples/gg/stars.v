module main

import os
import gg
import gx
import rand
import sokol.sgl

const (
	win_width     = 800
	win_height    = 600
	max_stars     = 5000
	max_v_letters = 5
)

struct Star {
mut:
	x f32
	y f32
	z f32
	r f32
	g f32
	b f32
}

struct VLetter {
mut:
	x      f32
	y      f32
	z      f32
	w      f32
	h      f32
	angle  f32
	dz     f32
	dangle f32
}

struct App {
mut:
	gg        &gg.Context
	image     gg.Image
	stars     []Star
	v_letters []VLetter
}

fn main() {
	mut app := &App{
		gg: 0
		stars: []Star{len: max_stars}
		v_letters: []VLetter{len: max_v_letters}
	}
	app.gg = gg.new_context(
		bg_color: gx.black
		width: win_width
		height: win_height
		create_window: true
		window_title: 'Star Vield'
		frame_fn: frame
		init_fn: init_images
		user_data: app
	)
	for i in 0 .. max_stars {
		app.stars[i].x = rand.f32_in_range(-200.0, 200.0)
		app.stars[i].y = rand.f32_in_range(-200.0, 200.0)
		app.stars[i].z = rand.f32_in_range(-200.0, -100.0)
		app.stars[i].r = rand.f32_in_range(0.1, 1.0)
		app.stars[i].g = rand.f32_in_range(0.1, 1.0)
		app.stars[i].b = rand.f32_in_range(0.1, 1.0)
	}
	for i in 0 .. max_v_letters {
		app.v_letters[i].x = rand.f32_in_range(-20.0, 20.0)
		app.v_letters[i].y = rand.f32_in_range(-20.0, 20.0)
		app.v_letters[i].z = rand.f32_in_range(-5.0, -1.0)
		app.v_letters[i].w = rand.f32_in_range(5, 20)
		app.v_letters[i].h = app.v_letters[i].w
		app.v_letters[i].angle = rand.f32_in_range(0, 6.283184)
		app.v_letters[i].dangle = rand.f32_in_range(-0.05, 0.05)
		app.v_letters[i].dz = rand.f32_in_range(-0.1, -0.01)
	}
	app.gg.run()
}

fn init_images(mut app App) {
	app.image = app.gg.create_image(os.resource_abs_path('logo.png'))
}

fn frame(mut app App) {
	app.gg.begin()
	app.draw()
	app.gg.end()
}

// fn C.glPointSize(size f32)
fn (mut app App) draw() {
	sgl.defaults()
	sgl.perspective(sgl.rad(90), 1.0, 1.0, 100.0)
	// C.glPointSize(3.0)
	sgl.begin_points()
	for i in 0 .. app.stars.len {
		s := app.stars[i]
		sgl.v3f_c3f(s.x, s.y, s.z, s.r, s.g, s.b)
		app.stars[i].z += 0.3
		if app.stars[i].z > -1.0 {
			app.stars[i].x = rand.f32_in_range(-200.0, 200.0)
			app.stars[i].y = rand.f32_in_range(-200.0, 200.0)
			app.stars[i].z = rand.f32_in_range(-200.0, -100.0)
		}
	}
	sgl.end()
	// ////
	for i in 0 .. app.v_letters.len {
		v := app.v_letters[i]
		sgl.defaults()
		sgl.perspective(sgl.rad(90), 1.0, 1.0, 100.0)
		sgl.rotate(v.angle, 0, 0, 1)
		app.gg.draw_image_3d(v.x, v.y, v.z, v.w, v.h, app.image)
		//
		app.v_letters[i].z += app.v_letters[i].dz
		app.v_letters[i].angle += app.v_letters[i].dangle
		if app.v_letters[i].z > -60.0 {
			app.v_letters[i].x += rand.f32_in_range(-0.05, 0.05)
			app.v_letters[i].y += rand.f32_in_range(-0.05, 0.05)
		}
		if app.v_letters[i].z < -95.0 {
			app.v_letters[i].h *= 0.8
			app.v_letters[i].w *= 0.8
		}
		if app.v_letters[i].z < -100.0 {
			app.v_letters[i].z = rand.f32_in_range(-5.0, -1.0)
			app.v_letters[i].h = 10.0
			app.v_letters[i].w = 10.0
		}
	}
}
