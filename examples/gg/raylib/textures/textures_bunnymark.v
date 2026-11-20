module main

// Port of raylib textures_bunnymark.c
// PORT NOTE: raylib's automatic batching (8192 quads per draw call) is internal
// to rlgl and has no gg equivalent; every sprite is drawn individually here.
import gg
import rand

const bunny_raywhite = gg.Color{245, 245, 245, 255}
const bunny_black = gg.Color{0, 0, 0, 255}
const bunny_green = gg.Color{0, 255, 0, 255}
const bunny_maroon = gg.Color{128, 0, 0, 255}

struct Bunny {
mut:
	x     f32
	y     f32
	vx    f32
	vy    f32
	color gg.Color
}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	texture gg.Image
	bunnies []Bunny
	count   int
	paused  bool
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.texture = app.ctx.create_image('resources/raybunny.png') or { gg.Image{} }
	app.bunnies = []Bunny{len: 80000}
	app.ctx.fps.show = true
	app.ctx.fps.x = 10
	app.ctx.fps.y = 10
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	if app.ctx.mouse_buttons.has(.left) {
		for _ in 0 .. 100 {
			if app.count >= app.bunnies.len {
				break
			}
			mut b := &app.bunnies[app.count]
			b.x = f32(app.ctx.mouse_pos_x)
			b.y = f32(app.ctx.mouse_pos_y)
			b.vx = f32(rand.intn(501) or { 250 } - 250)
			b.vy = f32(rand.intn(501) or { 250 } - 250)
			b.color = gg.Color{u8(rand.intn(191) or { 100 } + 50), u8(rand.intn(161) or { 80 } + 80), u8(rand.intn(141) or { 100 } + 100), 255}
			app.count++
		}
	}
	if app.paused {
		return
	}
	hw := f32(app.texture.width) / 2
	hh := f32(app.texture.height) / 2
	for i in 0 .. app.count {
		mut b := &app.bunnies[i]
		b.x += b.vx * dt
		b.y += b.vy * dt
		if b.x + hw > 800 || b.x + hw < 0 {
			b.vx *= -1
		}
		if b.y + hh > 450 || b.y + hh - 40 < 0 {
			b.vy *= -1
		}
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	w := f32(app.texture.width)
	h := f32(app.texture.height)
	for i in 0 .. app.count {
		b := app.bunnies[i]
		app.ctx.draw_image_with_config(
			img:       &app.texture
			img_rect:  gg.Rect{b.x, b.y, w, h}
			part_rect: gg.Rect{0, 0, w, h}
			color:     b.color
		)
	}
	app.ctx.draw_rect_filled(0, 0, 800, 40, bunny_black)
	app.ctx.draw_text(120, 10, 'bunnies: ${app.count}', size: 20, color: bunny_green)
	app.ctx.draw_text(320, 10, 'batched draw calls: ${1 + app.count / 8192}',
		size:  20
		color: bunny_maroon
	)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .p {
		app.paused = !app.paused
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - bunnymark'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		update_fn:    update
		bg_color:     bunny_raywhite
	)
	app.ctx.run()
}
