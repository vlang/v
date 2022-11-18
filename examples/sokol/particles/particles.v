// Copyright(C) 2019 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license file distributed with this software package
module main

import time
import sokol
import sokol.sapp
import sokol.gfx
import sokol.sgl
import particle

const (
	used_import = sokol.used_import
)

fn main() {
	mut app := &App{
		width: 800
		height: 400
		pass_action: gfx.create_clear_pass(0.1, 0.1, 0.1, 1.0)
	}
	app.init()
	app.run()
}

struct App {
	pass_action gfx.PassAction
mut:
	width     int
	height    int
	frame     i64
	last      i64
	ps        particle.System
	alpha_pip sgl.Pipeline
}

fn (mut a App) init() {
	a.frame = 0
	a.last = time.ticks()
	a.ps = particle.System{
		width: a.width
		height: a.height
	}
	a.ps.init(particle.SystemConfig{
		pool: 20000
	})
}

fn (mut a App) cleanup() {
	unsafe {
		a.ps.free()
	}
}

fn (mut a App) run() {
	title := 'V Particle Example'
	desc := sapp.Desc{
		width: a.width
		height: a.height
		user_data: a
		init_userdata_cb: init
		frame_userdata_cb: frame
		event_userdata_cb: event
		window_title: title.str
		html5_canvas_name: title.str
		cleanup_userdata_cb: cleanup
	}
	sapp.run(&desc)
}

fn (a App) draw() {
	sgl.load_pipeline(a.alpha_pip)
	a.ps.draw()
}

fn init(user_data voidptr) {
	mut app := &App(user_data)
	desc := sapp.create_desc()
	gfx.setup(&desc)
	sgl_desc := sgl.Desc{
		max_vertices: 50 * 65536
	}
	sgl.setup(&sgl_desc)
	mut pipdesc := gfx.PipelineDesc{}
	unsafe { vmemset(&pipdesc, 0, int(sizeof(pipdesc))) }

	color_state := gfx.ColorState{
		blend: gfx.BlendState{
			enabled: true
			src_factor_rgb: .src_alpha
			dst_factor_rgb: .one_minus_src_alpha
		}
	}
	pipdesc.colors[0] = color_state

	app.alpha_pip = sgl.make_pipeline(&pipdesc)
}

fn cleanup(user_data voidptr) {
	mut app := &App(user_data)
	app.cleanup()
	gfx.shutdown()
}

fn frame(user_data voidptr) {
	mut app := &App(user_data)
	app.width = sapp.width()
	app.height = sapp.height()
	t := time.ticks()
	dt := f64(t - app.last) / 1000.0
	app.ps.update(dt)
	draw(app)
	gfx.begin_default_pass(&app.pass_action, app.width, app.height)
	sgl.default_pipeline()
	sgl.draw()
	gfx.end_pass()
	gfx.commit()
	app.frame++
	app.last = t
}

fn event(ev &sapp.Event, mut app App) {
	if ev.@type == .mouse_move {
		app.ps.explode(ev.mouse_x, ev.mouse_y)
	}
	if ev.@type == .mouse_up || ev.@type == .mouse_down {
		if ev.mouse_button == .left {
			is_pressed := ev.@type == .mouse_down
			if is_pressed {
				app.ps.explode(ev.mouse_x, ev.mouse_y)
			}
		}
	}
	if ev.@type == .key_up || ev.@type == .key_down {
		if ev.key_code == .r {
			is_pressed := ev.@type == .key_down
			if is_pressed {
				app.ps.reset()
			}
		}
	}
	if ev.@type == .touches_began || ev.@type == .touches_moved {
		if ev.num_touches > 0 {
			touch_point := ev.touches[0]
			app.ps.explode(touch_point.pos_x, touch_point.pos_y)
		}
	}
}

fn draw(a &App) {
	sgl.defaults()
	sgl.matrix_mode_projection()
	sgl.ortho(0, f32(sapp.width()), f32(sapp.height()), 0.0, -1.0, 1.0)
	sgl.push_matrix()
	a.draw()
	sgl.pop_matrix()
}
