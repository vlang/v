module main

// Port of raylib textures_cellular_automata.c
// The automaton image is kept as an RGBA buffer and uploaded to a cached image
// whenever new rows are computed, matching raylib's UpdateTexture() usage.
import gg

const cell_raywhite = gg.Color{245, 245, 245, 255}
const cell_gray = gg.Color{128, 128, 128, 255}
const cell_blue = gg.Color{0, 121, 241, 255}
const cell_black = gg.Color{0, 0, 0, 255}

const img_width = 800
const img_height = 400
const draw_rule_x = 585
const draw_rule_y = 10
const draw_rule_spacing = 15
const draw_rule_group = 50
const draw_rule_size = 14
const preset_w = 42
const preset_h = 22
const lines_per_frame = 4

fn cache_rgba(ctx &gg.Context, w int, h int, buf []u8) int {
	mut img := gg.Image{ width: w, height: h, nr_channels: 4, ok: true, data: buf.data }
	img.init_sokol_image()
	return ctx.cache_image(img)
}

fn set_pixel(buf []u8, x int, y int, c gg.Color) {
	i := (y * img_width + x) * 4
	unsafe {
		buf[i] = c.r
		buf[i + 1] = c.g
		buf[i + 2] = c.b
		buf[i + 3] = c.a
	}
}

fn pixel_is_dark(buf []u8, x int, y int) bool {
	i := (y * img_width + x) * 4
	return unsafe { buf[i] } < 5
}

fn draw_thick_rect(ctx &gg.Context, x f32, y f32, w f32, h f32, t f32, c gg.Color) {
	ctx.draw_rect_filled(x, y, w, t, c)
	ctx.draw_rect_filled(x, y + h - t, w, t, c)
	ctx.draw_rect_filled(x, y, t, h, c)
	ctx.draw_rect_filled(x + w - t, y, t, h, c)
}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	idx        int         = -1
	buf        []u8
	rule       int = 30
	line       int = 1
	mouse_cell int = -1
	presets    [10]int
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.buf = []u8{len: img_width * img_height * 4}
	for i in 0 .. img_width * img_height {
		unsafe {
			app.buf[i * 4] = 245
			app.buf[i * 4 + 1] = 245
			app.buf[i * 4 + 2] = 245
			app.buf[i * 4 + 3] = 255
		}
	}
	set_pixel(app.buf, img_width / 2, 0, cell_black)
	app.presets = [18, 30, 60, 86, 102, 124, 126, 150, 182, 225]!
	app.idx = cache_rgba(app.ctx, img_width, img_height, app.buf)
}

fn compute_line(buf []u8, line int, rule int) {
	for i in 1 .. img_width - 1 {
		mut prev := 0
		if pixel_is_dark(buf, i - 1, line - 1) { prev += 4 }
		if pixel_is_dark(buf, i, line - 1) { prev += 2 }
		if pixel_is_dark(buf, i + 1, line - 1) { prev += 1 }
		if (rule & (1 << prev)) != 0 {
			set_pixel(buf, i, line, cell_black)
		} else {
			set_pixel(buf, i, line, cell_raywhite)
		}
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	mx := f32(app.ctx.mouse_pos_x)
	my := f32(app.ctx.mouse_pos_y)
	app.mouse_cell = -1
	for i in 0 .. 8 {
		cx := draw_rule_x - draw_rule_group * i + draw_rule_spacing
		cy := draw_rule_y + draw_rule_spacing
		if mx >= cx && mx <= cx + draw_rule_size && my >= cy && my <= cy + draw_rule_size {
			app.mouse_cell = i
			break
		}
	}
	if app.mouse_cell < 0 {
		for i in 0 .. app.presets.len {
			cx := 4 + f32((preset_w + 2) * (i / 2))
			cy := 2 + f32((preset_h + 2) * (i % 2))
			if mx >= cx && mx <= cx + preset_w && my >= cy && my <= cy + preset_h {
				app.mouse_cell = i + 8
				break
			}
		}
	}
	if app.ctx.mouse_buttons.has(.left) && app.mouse_cell >= 0 {
		if app.mouse_cell < 8 {
			app.rule ^= 1 << app.mouse_cell
		} else {
			app.rule = app.presets[app.mouse_cell - 8]
		}
		for i in 0 .. img_width * img_height {
			unsafe {
				app.buf[i * 4] = 245
				app.buf[i * 4 + 1] = 245
				app.buf[i * 4 + 2] = 245
				app.buf[i * 4 + 3] = 255
			}
		}
		set_pixel(app.buf, img_width / 2, 0, cell_black)
		app.line = 1
	}
	if app.line < img_height {
		mut k := 0
		for k < lines_per_frame && app.line + k < img_height {
			compute_line(app.buf, app.line + k, app.rule)
			k++
		}
		app.line += lines_per_frame
		app.ctx.update_pixel_data(app.idx, app.buf.data)
	}

	app.ctx.begin()
	if app.idx >= 0 {
		app.ctx.draw_image_by_id(0, 450 - img_height, img_width, img_height, app.idx)
	}
	for i in 0 .. app.presets.len {
		x := 4 + (preset_w + 2) * (i / 2)
		y := 2 + (preset_h + 2) * (i % 2)
		app.ctx.draw_text(8 + x, 4 + y, '${app.presets[i]}', size: 20, color: cell_gray)
		app.ctx.draw_rect_empty(f32(x), f32(y), preset_w, preset_h, cell_blue)
		if app.mouse_cell == i + 8 {
			draw_thick_rect(app.ctx, f32(x) - 2, f32(y) - 2, preset_w + 4, preset_h + 4, 3, gg.red)
		}
	}
	for i in 0 .. 8 {
		base := draw_rule_x - draw_rule_group * i
		for j in 0 .. 3 {
			x := f32(base + draw_rule_spacing * j)
			app.ctx.draw_rect_empty(x, draw_rule_y, draw_rule_size, draw_rule_size, cell_gray)
			if (i & (4 >> j)) != 0 {
				app.ctx.draw_rect_filled(x + 2, draw_rule_y + 2, 10, 10, cell_black)
			}
		}
		ox := f32(base + draw_rule_spacing)
		oy := f32(draw_rule_y + draw_rule_spacing)
		app.ctx.draw_rect_empty(ox, oy, draw_rule_size, draw_rule_size, cell_blue)
		if (app.rule & (1 << i)) != 0 {
			app.ctx.draw_rect_filled(ox + 2, oy + 2, 10, 10, cell_black)
		}
		if app.mouse_cell == i {
			draw_thick_rect(app.ctx, ox - 2, oy - 2, draw_rule_size + 4, draw_rule_size + 4, 3, gg.red)
		}
	}
	app.ctx.draw_text(draw_rule_x + draw_rule_spacing * 4, draw_rule_y + 1, 'RULE: ${app.rule}',
		size:  30
		color: cell_gray
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - cellular automata'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     cell_raywhite
	)
	app.ctx.run()
}
