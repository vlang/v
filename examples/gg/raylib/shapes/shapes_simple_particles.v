module main

// raylib [shapes] example - simple particles, ported to gg.
import gg
import math
import rand

const max_particles = 3000
const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const skyblue = gg.Color{102, 191, 255, 128}
const blue_c = gg.Color{0, 121, 241, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const yellow_c = gg.Color{253, 249, 0, 255}
const names = ['WATER', 'SMOKE', 'FIRE']!

struct Particle {
mut:
	kind   int
	x      f32
	y      f32
	vx     f32
	vy     f32
	radius f32
	color  gg.Color
	life   f32
	alive  bool
}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	particles []Particle
	head      int
	tail      int
	rate      int = -2
	kind      int
	ex        f32 = 400
	ey        f32 = 225
}

fn (mut app App) emit() {
	if (app.head + 1) % max_particles == app.tail { return }
	app.particles[app.head] = Particle{}
	mut p := app.particles[app.head]
	p.alive = true
	p.x, p.y = app.ex, app.ey
	p.kind = app.kind
	speed := f32(rand.intn(10) or { 0 }) / 5
	match app.kind {
		0 {
			p.radius = 5
			p.color = blue_c
		}
		1 {
			p.radius = 7
			p.color = gray_c
		}
		2 {
			p.radius = 10
			p.color = yellow_c
			speed /= 10
		}
		else {}
	}
	dir := f32(rand.intn(360) or { 0 })
	p.vx = speed * math.cosf(f32(math.radians(dir)))
	p.vy = speed * math.sinf(f32(math.radians(dir)))
	app.particles[app.head] = p
	app.head = (app.head + 1) % max_particles
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	i := app.tail
	for i != app.head {
		p := app.particles[i]
		if p.alive {
			ctx.draw_circle_filled(p.x, p.y, p.radius, p.color)
		}
		i = (i + 1) % max_particles
	}
	ctx.draw_rect_filled(5, 5, 315, 75, skyblue)
	ctx.draw_rect_empty(5, 5, 315, 75, blue_c)
	ctx.draw_text(15, 15, 'CONTROLS:', size: 10, color: black_c)
	ctx.draw_text(15, 35, 'UP/DOWN: Change Particle Emission Rate', size: 10, color: black_c)
	ctx.draw_text(15, 55, 'LEFT/RIGHT: Change Particle Type (Water, Smoke, Fire)',
		size:  10
		color: black_c
	)
	info := if app.rate < 0 {
		'Particles every ${-app.rate} frames | Type: ${names[app.kind]}'
	} else {
		'${app.rate + 1} Particles per frame | Type: ${names[app.kind]}'
	}
	ctx.draw_text(15, 95, info, size: 10, color: darkgray)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down { return }
	match e.key_code {
		.up { app.rate++ }
		.down { app.rate-- }
		.right { app.kind = if app.kind == 2 { 0 } else { app.kind + 1 } }
		.left { app.kind = if app.kind == 0 { 2 } else { app.kind - 1 } }
		else {}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.particles.len == 0 {
		app.particles = []Particle{len: max_particles}
	}
	if app.rate < 0 {
		if rand.intn(-app.rate) or { 1 } == 0 { app.emit() }
	} else {
		for _ in 0 .. app.rate + 1 { app.emit() }
	}
	w := f32(app.ctx.width)
	h := f32(app.ctx.height)
	i := app.tail
	for i != app.head {
		mut p := app.particles[i]
		p.life += 1.0 / 60
		match p.kind {
			0 {
				p.x += p.vx
				p.vy += 0.2
				p.y += p.vy
			}
			1 {
				p.x += p.vx
				p.vy -= 0.05
				p.y += p.vy
				p.radius += 0.5
				if p.color.a >= 4 { p.color.a -= 4 }
				if p.color.a < 4 { p.alive = false }
			}
			2 {
				p.x += p.vx + math.cosf(p.life * 215)
				p.vy -= 0.05
				p.y += p.vy
				p.radius -= 0.15
				if p.color.g >= 3 { p.color.g -= 3 }
				if p.radius <= 0.02 { p.alive = false }
			}
			else {}
		}
		if p.x < -p.radius || p.x > w + p.radius || p.y < -p.radius || p.y > h + p.radius {
			p.alive = false
		}
		app.particles[i] = p
		i = (i + 1) % max_particles
	}
	for app.tail != app.head && !app.particles[app.tail].alive {
		app.tail = (app.tail + 1) % max_particles
	}
	if app.ctx.mouse_buttons.has(.left) {
		app.ex = f32(app.ctx.mouse_pos_x)
		app.ey = f32(app.ctx.mouse_pos_y)
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - simple particles'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
