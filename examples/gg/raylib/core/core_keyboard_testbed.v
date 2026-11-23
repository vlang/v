module main

// raylib [core] example - keyboard testbed
// Draws a US keyboard layout and highlights the keys currently pressed.
// gg exposes gg.KeyCode names matching the sokol/GLFW key codes.

import gg

const screen_width = 800
const screen_height = 450
const spacing = f32(4)
const key_h = f32(30)

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const maroon = gg.Color{128, 0, 0, 255}

struct Key {
mut:
	label string
	code  gg.KeyCode
	width f32
}

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	lines [][]Key
}

fn key(label string, code gg.KeyCode, width f32) Key {
	return Key{label, code, width}
}

fn main() {
	mut app := &App{}
	w := f32(45)
	app.lines = [
		[key('ESC', .escape, w), key('F1', .f1, w), key('F2', .f2, w), key('F3', .f3, w),
			key('F4', .f4, w), key('F5', .f5, w), key('F6', .f6, w), key('F7', .f7, w),
			key('F8', .f8, w), key('F9', .f9, w), key('F10', .f10, w), key('F11', .f11, w),
			key('F12', .f12, w), key('PRTSC', .print_screen, 62)],
		[key('`', .grave_accent, 25), key('1', ._1, w), key('2', ._2, w), key('3', ._3, w),
			key('4', ._4, w), key('5', ._5, w), key('6', ._6, w), key('7', ._7, w), key('8', ._8, w),
			key('9', ._9, w), key('0', ._0, w), key('-', .minus, w), key('=', .equal, w),
			key('BACK', .backspace, 82)],
		[key('TAB', .tab, 50), key('Q', .q, w), key('W', .w, w), key('E', .e, w), key('R', .r, w),
			key('T', .t, w), key('Y', .y, w), key('U', .u, w), key('I', .i, w), key('O', .o, w),
			key('P', .p, w), key('[', .left_bracket, w), key(']', .right_bracket, w),
			key('\\', .backslash, 57)],
		[key('CAPS', .caps_lock, 68), key('A', .a, w), key('S', .s, w), key('D', .d, w),
			key('F', .f, w), key('G', .g, w), key('H', .h, w), key('J', .j, w), key('K', .k, w),
			key('L', .l, w), key(';', .semicolon, w), key("'", .apostrophe, w),
			key('ENTER', .enter, 88)],
		[key('LSHIFT', .left_shift, 80), key('Z', .z, w), key('X', .x, w), key('C', .c, w),
			key('V', .v, w), key('B', .b, w), key('N', .n, w), key('M', .m, w), key(',', .comma, w),
			key('.', .period, w), key('/', .slash, w), key('RSHIFT', .right_shift, 76),
			key('UP', .up, w)],
		[key('LCTRL', .left_control, 80), key('WIN', .left_super, w), key('LALT', .left_alt, w),
			key('SPACE', .space, 208), key('RALT', .right_alt, w), key('RCTRL', .right_control, w),
			key('LEFT', .left, w), key('DOWN', .down, w), key('RIGHT', .right, w)],
	]
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - keyboard testbed'
		bg_color:     ray_white
		frame_fn:     frame
		user_data:    app
	)
	app.gg.run()
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_text(26, 38, 'KEYBOARD LAYOUT: ENG-US', size: 20, color: light_gray)
	for li, line in app.lines {
		y := f32(80 + li * 38)
		mut x := f32(26)
		for k in line {
			down := ctx.pressed_keys[int(k.code)]
			ctx.draw_rect_empty(x, y, k.width, key_h, if down { maroon } else { dark_gray })
			ctx.draw_text(int(x) + 4, int(y) + 4, k.label,
				size:  10
				color: if down { maroon } else { dark_gray }
			)
			if ctx.mouse_pos_x >= x && ctx.mouse_pos_x <= x + k.width
				&& ctx.mouse_pos_y >= y && ctx.mouse_pos_y <= y + key_h {
				ctx.draw_rect(x: x, y: f32(y), w: k.width, h: key_h, color: gg.Color{255, 0, 0, 51})
			}
			x += k.width + spacing
		}
	}
	ctx.end()
}
