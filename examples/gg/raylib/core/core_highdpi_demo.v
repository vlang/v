module main

// raylib [core] example - highdpi demo
// Shows the relationship between logical points and physical pixels.
// gg's Context.scale reports the DPI scale, and ctx.window_size() gives
// logical dimensions while ctx.width/height are framebuffer pixels.

import gg

const screen_width = 800
const screen_height = 450
const cell_size = 50

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const orange = gg.Color{255, 161, 0, 255}
const blue = gg.Color{0, 121, 241, 100}
const dark_blue = gg.Color{0, 82, 172, 255}

struct App {
mut:
	gg &gg.Context = unsafe { nil }
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - highdpi demo'
		bg_color:     ray_white
		resizable:    true
		min_width:    450
		min_height:   450
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn center_text(ctx &gg.Context, text string, x int, y int, size int, c gg.Color) {
	w := ctx.text_width(text)
	ctx.draw_text(x - w / 2, y - size / 2, text, size: size, color: c)
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	logical := ctx.window_size()
	scale := ctx.scale
	ctx.begin()
	center_text(ctx, 'Dpi Scale: ${scale:.2f}', logical.width / 2, 30, 40, dark_gray)
	center_text(ctx, 'Monitor: 1/1 ([N] next monitor)', logical.width / 2, 70, 20, light_gray)
	center_text(ctx, 'Window is ${logical.width} "logical points" wide',
		logical.width / 2, 120, 20, orange)

	odd := true
	mut i := cell_size
	for i < logical.width {
		if odd {
			ctx.draw_rect(x: f32(i), y: 180, w: cell_size, h: 80, color: orange)
		}
		center_text(ctx, i.str(), i, 150, 10, light_gray)
		ctx.draw_line(f32(i), 160, f32(i), 260, gg.gray)
		odd = !odd
		i += cell_size
	}

	odd = true
	last_text_x := -30
	i = cell_size
	for i < ctx.width {
		x := int(f32(i) / scale)
		if odd {
			ctx.draw_rect(x: f32(x), y: 280, w: f32(cell_size) / scale, h: 80, color: blue)
		}
		ctx.draw_line(f32(x), 280, f32(i) / scale, 150, gg.gray)
		if x - last_text_x >= 30 {
			center_text(ctx, i.str(), x, 310, 10, light_gray)
			last_text_x = x
		}
		odd = !odd
		i += cell_size
	}
	center_text(ctx, 'Window is ${ctx.width} "physical pixels" wide',
		logical.width / 2, 390, 20, dark_blue)
	center_text(ctx, 'Can you see this?', logical.width - 100, screen_height - 20,
		20, light_gray)
	ctx.end()
}
