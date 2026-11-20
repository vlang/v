module main

// Port of raylib textures_mouse_painting.c
// PORT NOTE: gg has no ExportImage()/image saving API, so the SAVE button only
// shows the confirmation message; no PNG is written to disk.
import gg
import sokol.gfx

const paint_raywhite = gg.Color{245, 245, 245, 255}
const paint_lightgray = gg.Color{211, 211, 211, 255}
const paint_black = gg.Color{0, 0, 0, 255}
const paint_red = gg.Color{255, 0, 0, 255}

fn rt_image(rt &gg.RenderTexture2D, smp gfx.Sampler) gg.Image {
	return gg.Image{
		width:       rt.width
		height:      rt.height
		nr_channels: 4
		ok:          true
		simg_ok:     true
		simg:        rt.texture()
		ssmp:        smp
	}
}

fn flush(ctx &gg.Context) {
	ctx.r.draw()
	ctx.r.rewind()
}

struct App {
mut:
	ctx        &gg.Context         = unsafe { nil }
	rt         &gg.RenderTexture2D = unsafe { nil }
	rt_idx     int                 = -1
	sampler    gfx.Sampler
	colors     []gg.Color
	color_sel  int
	prev_sel   int
	hover      int = -1
	brush      f32 = 20
	save_hover bool
	save_msg   bool
	save_cnt   int
	erase      bool
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.colors = [paint_raywhite, gg.Color{255, 255, 0, 255}, gg.Color{255, 203, 40, 255},
		gg.Color{255, 160, 0, 255}, gg.Color{255, 109, 194, 255}, gg.red, gg.Color{128, 0, 0, 255},
		gg.Color{0, 255, 0, 255}, gg.Color{0, 255, 128, 255}, gg.Color{0, 128, 0, 255},
		gg.Color{135, 206, 235, 255}, gg.Color{0, 121, 241, 255}, gg.Color{0, 42, 141, 255},
		gg.Color{128, 0, 128, 255}, gg.Color{138, 43, 226, 255}, gg.Color{85, 26, 139, 255},
		gg.Color{245, 245, 220, 255}, gg.Color{165, 42, 42, 255}, gg.Color{100, 50, 20, 255},
		gg.Color{211, 211, 211, 255}, gg.Color{128, 128, 128, 255}, gg.Color{80, 80, 80, 255},
		gg.Color{0, 0, 0, 255}]!
	app.sampler = gfx.make_sampler(&gfx.SamplerDesc{
		min_filter: .linear
		mag_filter: .linear
		wrap_u:     .clamp_to_edge
		wrap_v:     .clamp_to_edge
	})
	app.rt = gg.new_render_texture2d(800, 450) or { &gg.RenderTexture2D(unsafe { nil }) }
	if !isnil(app.rt) {
		app.rt_idx = app.ctx.cache_image(rt_image(app.rt, app.sampler))
		app.rt.begin(app.colors[0])
		flush(app.ctx)
		app.rt.end()
	}
}

fn cleanup(data voidptr) {
	app := unsafe { &App(data) }
	if !isnil(app.rt) {
		mut rt := app.rt
		rt.free()
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	mx := f32(app.ctx.mouse_pos_x)
	my := f32(app.ctx.mouse_pos_y)

	if app.ctx.pressed_keys[gg.KeyCode.right] {
		app.color_sel++
	} else if app.ctx.pressed_keys[gg.KeyCode.left] {
		app.color_sel--
	}
	if app.color_sel >= app.colors.len { app.color_sel = app.colors.len - 1 }
	if app.color_sel < 0 { app.color_sel = 0 }

	app.hover = -1
	for i in 0 .. app.colors.len {
		x := 10 + 32 * f32(i)
		if mx >= x && mx <= x + 30 && my >= 10 && my <= 40 {
			app.hover = i
			break
		}
	}
	if app.hover >= 0 && app.ctx.mouse_buttons.has(.left) {
		app.color_sel = app.hover
		app.prev_sel = app.color_sel
	}
	app.brush += f32(app.ctx.scroll_y) * 5
	if app.brush < 2 { app.brush = 2 }
	if app.brush > 50 { app.brush = 50 }
	if app.ctx.pressed_keys[gg.KeyCode.c] && !isnil(app.rt) {
		app.rt.begin(app.colors[0])
		flush(app.ctx)
		app.rt.end()
	}

	if !isnil(app.rt) && my > 50 {
		if app.ctx.mouse_buttons.has(.right) {
			if !app.erase {
				app.prev_sel = app.color_sel
				app.color_sel = 0
			}
			app.erase = true
			app.rt.begin(app.colors[0])
			app.ctx.draw_circle_filled(mx, my, app.brush, app.colors[app.color_sel])
			flush(app.ctx)
			app.rt.end()
		} else {
			if app.erase {
				app.color_sel = app.prev_sel
				app.erase = false
			}
			if app.ctx.mouse_buttons.has(.left) {
				app.rt.begin(app.colors[0])
				app.ctx.draw_circle_filled(mx, my, app.brush, app.colors[app.color_sel])
				flush(app.ctx)
				app.rt.end()
			}
		}
	}

	app.save_hover = mx >= 750 && mx <= 790 && my >= 10 && my <= 40
	if ((app.save_hover && app.ctx.mouse_buttons.has(.left)) || app.ctx.pressed_keys[gg.KeyCode.s])
		&& !app.save_msg {
		app.save_msg = true
	}
	if app.save_msg {
		app.save_cnt++
		if app.save_cnt > 240 {
			app.save_msg = false
			app.save_cnt = 0
		}
	}

	app.ctx.begin()
	if app.rt_idx >= 0 {
		app.ctx.draw_image_with_config(
			img_id:   app.rt_idx
			img_rect: gg.Rect{0, 0, 800, 450}
			flip_y:   true
		)
	}
	if my > 50 {
		if app.erase {
			app.ctx.draw_circle_empty(mx, my, app.brush, paint_lightgray)
		} else {
			app.ctx.draw_circle_filled(mx, my, app.brush, app.colors[app.color_sel])
		}
	}
	app.ctx.draw_rect_filled(0, 0, 800, 50, paint_raywhite)
	app.ctx.draw_line(0, 50, 800, 50, paint_lightgray)
	for i in 0 .. app.colors.len {
		app.ctx.draw_rect_filled(10 + 32 * f32(i), 10, 30, 30, app.colors[i])
	}
	app.ctx.draw_rect_empty(10, 10, 30, 30, paint_lightgray)
	if app.hover >= 0 {
		app.ctx.draw_rect_filled(10 + 32 * f32(app.hover), 10, 30, 30,
			gg.Color{255, 255, 255, 153})
	}
	r := app.color_sel
	app.ctx.draw_rect_empty(8 + 32 * f32(r), 8, 34, 34, paint_black)
	app.ctx.draw_rect_empty(750, 10, 40, 30, if app.save_hover { paint_red } else { paint_black })
	app.ctx.draw_text(755, 20, 'SAVE!',
		size:  10
		color: if app.save_hover { paint_red } else { paint_black }
	)
	if app.save_msg {
		app.ctx.draw_rect_filled(0, 0, 800, 450, gg.Color{245, 245, 245, 204})
		app.ctx.draw_rect_filled(0, 150, 800, 80, paint_black)
		app.ctx.draw_text(150, 180, 'IMAGE SAVED!', size: 20, color: paint_raywhite)
	}
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - mouse painting'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		cleanup_fn:   cleanup
		bg_color:     paint_raywhite
		depth:        true
	)
	app.ctx.run()
}
