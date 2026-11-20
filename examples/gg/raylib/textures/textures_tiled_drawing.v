module main

// Port of raylib textures_tiled_drawing.c
// PORT NOTE: gg has no DrawTextureTiled(); the tiling loop is reproduced here
// with draw_image_with_config(). Per-tile rotation is used because gg rotates
// each quad around its own centre rather than the whole destination rectangle.
import gg

const tiled_raywhite = gg.Color{245, 245, 245, 255}
const tiled_black = gg.Color{0, 0, 0, 255}
const tiled_darkblue = gg.Color{0, 121, 241, 255}

fn draw_tiled(ctx &gg.Context, img &gg.Image, src gg.Rect, dst gg.Rect, rotation f32, scale f32, tint gg.Color) {
	if scale <= 0 || src.width <= 0 || src.height <= 0 || dst.width <= 0 || dst.height <= 0 {
		return
	}
	tw := src.width * scale
	th := src.height * scale
	mut dy := f32(0)
	for dy < dst.height {
		mut part_h := th
		if dy + part_h > dst.height {
			part_h = dst.height - dy
		}
		mut dx := f32(0)
		for dx < dst.width {
			mut part_w := tw
			if dx + part_w > dst.width {
				part_w = dst.width - dx
			}
			ctx.draw_image_with_config(
				img:       img
				img_rect:  gg.Rect{dst.x + dx, dst.y + dy, part_w, part_h}
				part_rect: gg.Rect{src.x, src.y, src.width * part_w / tw, src.height * part_h / th}
				rotation:  rotation
				color:     tint
			)
			dx += tw
		}
		dy += th
	}
}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	tex        gg.Image
	patterns   [6]gg.Rect
	colors     [10]gg.Color
	color_recs [10]gg.Rect
	pattern    int
	color      int
	scale      f32 = 1.0
	rotation   f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.tex = app.ctx.create_image('resources/patterns.png') or { gg.Image{} }
	app.patterns = [gg.Rect{3, 3, 66, 66}, gg.Rect{75, 3, 100, 100}, gg.Rect{3, 75, 66, 66},
		gg.Rect{7, 156, 50, 50}, gg.Rect{85, 106, 90, 45}, gg.Rect{75, 154, 100, 60}]!
	app.colors = [tiled_black, gg.Color{128, 0, 0, 255}, gg.Color{255, 160, 0, 255},
		gg.Color{0, 121, 241, 255}, gg.Color{128, 0, 128, 255}, gg.Color{245, 245, 220, 255},
		gg.Color{0, 255, 0, 255}, gg.red, gg.Color{80, 80, 80, 255}, gg.Color{135, 206, 235, 255}]!
	for i in 0 .. 10 {
		mut x := 0
		mut y := 0
		if i == 4 {
			x = 0
			y = 24
		} else if i > 4 {
			x = (i - 5) * 40
			y = 24
		} else {
			x = i * 40
		}
		app.color_recs[i] = gg.Rect{f32(10 + x), f32(286 + y), 32, 16}
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	draw_tiled(app.ctx, &app.tex, app.patterns[app.pattern],
		gg.Rect{228, 8, 800 - 220 - 16, 450 - 16}, app.rotation, app.scale, app.colors[app.color])

	app.ctx.draw_rect_filled(8, 8, 212, 434, gg.Color{211, 211, 211, 128})
	app.ctx.draw_text(10, 30, 'Select Pattern', size: 10, color: tiled_black)
	app.ctx.draw_image(10, 48, f32(app.tex.width), f32(app.tex.height), &app.tex)
	p := app.patterns[app.pattern]
	app.ctx.draw_rect_filled(10 + p.x, 48 + p.y, p.width, p.height, gg.Color{0, 121, 241, 77})

	app.ctx.draw_text(10, 274, 'Select Color', size: 10, color: tiled_black)
	for i in 0 .. 10 {
		r := app.color_recs[i]
		app.ctx.draw_rect_filled(r.x, r.y, r.width, r.height, app.colors[i])
		if i == app.color {
			app.ctx.draw_rect_empty(r.x - 3, r.y - 3, r.width + 6, r.height + 6,
				gg.Color{255, 255, 255, 128})
		}
	}
	app.ctx.draw_text(10, 344, 'Scale (UP/DOWN to change)', size: 10, color: tiled_black)
	app.ctx.draw_text(10, 356, '${app.scale:.2f}x', size: 20, color: tiled_black)
	app.ctx.draw_text(10, 388, 'Rotation (LEFT/RIGHT to change)', size: 10, color: tiled_black)
	app.ctx.draw_text(10, 400, '${app.rotation:.0f} degrees', size: 20, color: tiled_black)
	app.ctx.draw_text(10, 430, 'Press [SPACE] to reset', size: 10, color: tiled_darkblue)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .mouse_down && e.mouse_button == .left {
		mx := f32(e.mouse_x)
		my := f32(e.mouse_y)
		for i in 0 .. app.patterns.len {
			p := app.patterns[i]
			if mx >= 10 + p.x && mx <= 10 + p.x + p.width && my >= 48 + p.y && my <= 48 + p.y + p.height {
				app.pattern = i
				break
			}
		}
		for i in 0 .. app.color_recs.len {
			r := app.color_recs[i]
			if mx >= r.x && mx <= r.x + r.width && my >= r.y && my <= r.y + r.height {
				app.color = i
				break
			}
		}
	}
	if e.typ == .key_down {
		match e.key_code {
			.up {
				app.scale += 0.25
				if app.scale > 10 {
					app.scale = 10
				}
			}
			.down {
				app.scale -= 0.25
				if app.scale <= 0 {
					app.scale = 0.25
				}
			}
			.left { app.rotation -= 25 }
			.right { app.rotation += 25 }
			.space {
				app.rotation = 0
				app.scale = 1
			}
			else {}
		}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - tiled drawing'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		bg_color:     tiled_raywhite
	)
	app.ctx.run()
}
