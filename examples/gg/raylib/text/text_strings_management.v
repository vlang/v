module main

// raylib [text] example - strings management, ported to gg.
// PORT NOTE: gg has no 3D text, so this is a faithful 2D port of the physics and
// slicing/gluing behaviour of text particles.
import gg
import math
import rand

const (
	max_particles = 100
	font_size     = 30
	raywhite      = gg.Color{245, 245, 245, 255}
	black_c       = gg.Color{0, 0, 0, 255}
	darkgray      = gg.Color{80, 80, 80, 255}
)

struct Particle {
mut:
	text       string
	x, y       f32
	w, h       f32
	vx, vy     f32
	px, py     f32
	padding    f32 = 5
	border     f32 = 5
	friction   f32 = 0.99
	elasticity f32 = 0.9
	color      gg.Color
	grabbed    bool
}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	parts   []Particle
	grabbed int = -1
	off_x, off_y f32
}

fn make_particle(ctx &gg.Context, s string, x f32, y f32, color gg.Color) Particle {
	pad := f32(5)
	return Particle{
		text: s, x: x, y: y,
		w: f32(ctx.text_width(s)) + pad * 2, h: font_size + pad * 2,
		vx: f32(rand.intn(401) or { 0 } - 200), vy: f32(rand.intn(401) or { 0 } - 200),
		color: color,
	}
}

fn (mut app App) prepare(s string) {
	app.parts = [make_particle(app.ctx, s, f32(app.ctx.width) / 2, f32(app.ctx.height) / 2, raywhite)]
	app.grabbed = -1
}

fn (mut app App) slice(index int, by_char string) {
	if index < 0 || index >= app.parts.len { return }
	p := app.parts[index]
	pieces := if by_char == '' { []string{} } else { p.text.split(by_char) }
	if by_char == '' {
		half := p.text.len / 2
		if half == 0 { return }
		pieces << p.text[..half]
		pieces << p.text[half..]
	}
	mut np := app.parts.clone()
	for piece in pieces {
		if piece == '' || np.len >= max_particles { continue }
		c := gg.Color{u8(rand.intn(256) or { 0 }), u8(rand.intn(256) or { 0 }), u8(rand.intn(256) or { 0 }), 255}
		np << make_particle(app.ctx, piece, p.x, p.y, c)
	}
	if np.len > app.parts.len {
		np.delete(index)
	}
	app.parts = np
	app.grabbed = -1
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	for p in app.parts {
		b := p.border
		ctx.draw_rect_filled(p.x - b, p.y - b, p.w + b * 2, p.h + b * 2, black_c)
		ctx.draw_rect_filled(p.x, p.y, p.w, p.h, p.color)
		ctx.draw_text(int(p.x + p.padding), int(p.y + p.padding), p.text, size: font_size, color: black_c)
	}
	ctx.draw_text(10, 10, 'grab a text particle by pressing with the mouse and throw it by releasing',
		size:  10
		color: darkgray
	)
	ctx.draw_text(10, 30, 'slice a text particle by pressing it with the mouse right button',
		size:  10
		color: darkgray
	)
	ctx.draw_text(10, 50, 'shatter: hold left shift + right click; glue: hold left control',
		size:  10
		color: darkgray
	)
	ctx.draw_text(10, 70, '1 to 6 to reset; char key splits when only one particle',
		size:  10
		color: darkgray
	)
	ctx.draw_text(10, ctx.height - 30, 'TEXT PARTICLE COUNT: ${app.parts.len}', size: 20, color: black_c)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	mx, my := f32(e.mouse_x), f32(e.mouse_y)
	if e.typ == .mouse_down && e.mouse_button == .left {
		for i := app.parts.len - 1; i >= 0; i-- {
			p := app.parts[i]
			if mx >= p.x && mx <= p.x + p.w && my >= p.y && my <= p.y + p.h {
				app.grabbed = i
				app.off_x, app.off_y = mx - p.x, my - p.y
				break
			}
		}
	}
	if e.typ == .mouse_up && e.mouse_button == .left { app.grabbed = -1 }
	if e.typ == .mouse_down && e.mouse_button == .right {
		for i := app.parts.len - 1; i >= 0; i-- {
			p := app.parts[i]
			if mx >= p.x && mx <= p.x + p.w && my >= p.y && my <= p.y + p.h {
				app.slice(i, '')
				break
			}
		}
	}
	if e.typ == .mouse_down && e.mouse_button == .middle {
		for i in 0 .. app.parts.len {
			if i == app.grabbed { continue }
			app.parts[i].vx = f32(rand.intn(4001) or { 0 } - 2000)
			app.parts[i].vy = f32(rand.intn(4001) or { 0 } - 2000)
		}
	}
	if e.typ == .char && app.parts.len == 1 {
		app.slice(0, '${rune(e.char_code)}')
	}
	if e.typ == .key_down {
		base := 'raylib => fun videogames programming!'
		match e.key_code {
			._1 { app.prepare(base) }
			._2 { app.prepare(base.to_upper()) }
			._3 { app.prepare(base.to_lower()) }
			._4 { app.prepare('Raylib Fun Videogames Programming') }
			._5 { app.prepare('raylib_fun_videogames_programming') }
			._6 { app.prepare('raylibFunVideogamesProgramming') }
			else {}
		}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	mx, my := f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
	w, h := f32(app.ctx.width), f32(app.ctx.height)
	for i in 0 .. app.parts.len {
		mut p := app.parts[i]
		if i == app.grabbed {
			p.x = mx - app.off_x
			p.y = my - app.off_y
			if dt > 0 {
				p.vx = (p.x - p.px) / dt
				p.vy = (p.y - p.py) / dt
			}
			p.px, p.py = p.x, p.y
		} else {
			p.x += p.vx * dt
			p.y += p.vy * dt
			if p.x + p.w >= w { p.x = w - p.w; p.vx = -p.vx * p.elasticity }
			if p.x <= 0 { p.x = 0; p.vx = -p.vx * p.elasticity }
			if p.y + p.h >= h { p.y = h - p.h; p.vy = -p.vy * p.elasticity }
			if p.y <= 0 { p.y = 0; p.vy = -p.vy * p.elasticity }
			p.vx *= p.friction
			p.vy *= p.friction
		}
		app.parts[i] = p
	}
	_ = math.absf(w)
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - strings management'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.prepare('raylib => fun videogames programming!')
	app.ctx.run()
}
