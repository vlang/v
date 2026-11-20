module main

// raylib [text] example - unicode emojis, ported to gg.
// PORT NOTE: gg's font pipeline has no color-emoji (COLR/CBDT) support and no
// per-range font fallback. Symbols are rendered with the system font; the chat
// bubble interaction and multilingual message text are preserved.
import gg
import math
import rand

const (
	raywhite = gg.Color{245, 245, 245, 255}
	gray_c   = gg.Color{130, 130, 130, 255}
	light_t  = gg.Color{200, 200, 200, 102}
	black_c  = gg.Color{0, 0, 0, 255}

	emoji = ['☺', '☻', '♥', '♦', '♣', '♠', '•', '◘', '○', '◙', '♂', '♀', '♪', '♫', '☼', '►', '◄', '▲', '▼', '↔', '↕', '‼', '§', '¶', '↑', '↓', '→', '←', '∟', '↖', '↗', '↘', '↙', '⊠', '√', '¤']
	langs = ['English', 'Spanish', 'Greek', 'Chinese', 'Japanese', 'Korean']
	msgs = [
		'Hello World!',
		'¡Hola mundo!',
		'Γειά σου κόσμε!',
		'你好世界!',
		'こんにちは世界!',
		'안녕하세요 세계!',
	]!
)

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	idx      []int
	colors   []gg.Color
	msgidx   []int
	hover    int = -1
	selected int = -1
	hx, hy   f32
}

fn (mut app App) randomize() {
	app.idx = []int{len: emoji.len}
	app.colors = []gg.Color{len: emoji.len}
	app.msgidx = []int{len: emoji.len}
	for i in 0 .. emoji.len {
		app.idx[i] = rand.intn(emoji.len) or { 0 }
		h := f32(rand.intn(360) or { 0 })
		app.colors[i] = hsv(h, 0.6, 0.85)
		app.msgidx[i] = rand.intn(msgs.len) or { 0 }
	}
	app.hover = -1
	app.selected = -1
}

fn hsv(h f32, s f32, v f32) gg.Color {
	c := v * s
	x := c * (1 - math.absf((h / 60) % 2 - 1))
	m := v - c
	r, g, b := f32(0), f32(0), f32(0)
	if h < 60 { r, g = c, x } else if h < 120 { r, g = x, c } else if h < 180 { g, b = c, x } else if h < 240 { g, b = x, c } else if h < 300 { r, b = x, c } else { r, b = c, x }
	return gg.Color{u8((r + m) * 255), u8((g + m) * 255), u8((b + m) * 255), 255}
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	px, py := f32(28.8), f32(10)
	for i in 0 .. emoji.len {
		is_hover := f32(app.ctx.mouse_pos_x) >= px && f32(app.ctx.mouse_pos_x) <= px + 32 &&
			f32(app.ctx.mouse_pos_y) >= py && f32(app.ctx.mouse_pos_y) <= py + 32
		col := if app.selected == i { app.colors[i] } else if is_hover { app.colors[i] } else { light_t }
		ctx.draw_text(int(px), int(py), emoji[app.idx[i]], size: 32, color: col)
		if is_hover { app.hover = i }
		if i > 0 && i % 10 == 0 {
			py += 56
			px = 28.8
		} else {
			px += 60.8
		}
	}
	if app.selected >= 0 {
		c := app.colors[app.selected]
		bx, by := app.hx, app.hy - 110
		if by < 10 { by = app.hy + 84 }
		ctx.draw_rect_filled(bx - 38, by, 300, 90, c)
		ctx.draw_triangle_filled(app.hx, by + 90, app.hx + 8, by + 100, app.hx + 10, by + 90, c)
		ctx.draw_text(int(bx - 28), int(by + 10), msgs[app.msgidx[app.selected]], size: 18, color: raywhite)
		info := '${langs[app.msgidx[app.selected]]} ${msgs[app.msgidx[app.selected]].len} bytes'
		ctx.draw_text(int(bx - 28), int(by + 68), info, size: 10, color: raywhite)
	}
	ctx.draw_text(75, ctx.height - 40, 'These emojis have something to tell you, click each to find out!',
		size:  20
		color: gray_c
	)
	ctx.draw_text(158, ctx.height - 16, 'Press [SPACEBAR] to refresh', size: 10, color: gray_c)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space { app.randomize() }
	if e.typ == .mouse_down && e.mouse_button == .left && app.hover >= 0 {
		app.selected = app.hover
		app.hx = e.mouse_x
		app.hy = e.mouse_y
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	app.hover = -1
}

fn main() {
	mut app := &App{}
	app.randomize()
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - unicode emojis'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
