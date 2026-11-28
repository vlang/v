module main

// raylib [shapes] example - colors palette, ported to gg.
import gg

const max_colors = 21
const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const colors = [
	gg.Color{80, 80, 80, 255},
	gg.Color{190, 33, 55, 255},
	gg.Color{255, 161, 0, 255},
	gg.Color{0, 117, 44, 255},
	gg.Color{0, 82, 172, 255},
	gg.Color{112, 31, 126, 255},
	gg.Color{76, 63, 42, 255},
	gg.Color{130, 130, 130, 255},
	gg.Color{230, 41, 55, 255},
	gg.Color{255, 203, 0, 255},
	gg.Color{1, 222, 38, 255},
	gg.Color{0, 121, 241, 255},
	gg.Color{135, 61, 190, 255},
	gg.Color{127, 106, 79, 255},
	gg.Color{200, 200, 200, 255},
	gg.Color{255, 189, 196, 255},
	gg.Color{253, 249, 0, 255},
	gg.Color{0, 158, 47, 255},
	gg.Color{102, 191, 255, 255},
	gg.Color{211, 22, 197, 255},
	gg.Color{211, 176, 131, 255},
]!
const names = [
	'DARKGRAY',
	'MAROON',
	'ORANGE',
	'DARKGREEN',
	'DARKBLUE',
	'DARKPURPLE',
	'DARKBROWN',
	'GRAY',
	'RED',
	'GOLD',
	'LIME',
	'BLUE',
	'VIOLET',
	'BROWN',
	'LIGHTGRAY',
	'PINK',
	'YELLOW',
	'GREEN',
	'SKYBLUE',
	'PURPLE',
	'BEIGE',
]!

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	hover []bool
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(28, 42, 'raylib colors palette', size: 20, color: black_c)
	ctx.draw_text(ctx.width - 180, ctx.height - 40, 'press SPACE to see all colors',
		size:  10
		color: gray_c
	)

	space_down := ctx.pressed_keys[gg.KeyCode.space]
	for i in 0 .. max_colors {
		rx := 20.0 + 100.0 * f32(i % 7) + 10.0 * f32(i % 7)
		ry := 80.0 + 100.0 * f32(i / 7) + 10.0 * f32(i / 7)
		c := colors[i]
		if space_down || app.hover[i] {
			// PORT NOTE: gg has no per-color alpha fade; use the solid color.
			ctx.draw_rect_filled(rx, ry, 100, 100, c)
			ctx.draw_rect_filled(rx, ry + 100 - 26, 100, 20, black_c)
			ctx.draw_rect_empty(rx - 3, ry - 3, 106, 106, gg.Color{0, 0, 0, 90})
			ctx.draw_text(int(rx + 100 - f32(ctx.text_width(names[i])) - 12), int(ry + 100 - 20),
				names[i],
				size:  10
				color: c
			)
		} else {
			ctx.draw_rect_filled(rx, ry, 100, 100, c)
		}
	}
	ctx.end()
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	mx, my := f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
	for i in 0 .. max_colors {
		rx := 20.0 + 100.0 * f32(i % 7) + 10.0 * f32(i % 7)
		ry := 80.0 + 100.0 * f32(i / 7) + 10.0 * f32(i / 7)
		app.hover[i] = mx >= rx && mx <= rx + 100 && my >= ry && my <= ry + 100
	}
}

fn main() {
	mut app := &App{ hover: []bool{len: max_colors} }
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - colors palette'
		user_data:    app
		frame_fn:     on_frame
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
