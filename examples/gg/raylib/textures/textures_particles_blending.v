module main

// Port of raylib textures_particles_blending.c
import gg
import rand

const part_darkgray = gg.Color{80, 80, 80, 255}
const part_black = gg.Color{0, 0, 0, 255}

struct Particle {
mut:
	position gg.Rect
	color    gg.Color
	alpha    f32
	size     f32
	rotation f32
	active   bool
}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	texture   gg.Image
	particles []Particle
	gravity   f32 = 3.0
	blending  bool
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.texture = app.ctx.create_image('resources/spark_flame.png') or { gg.Image{} }
	app.particles = []Particle{len: 200}
	for i in 0 .. app.particles.len {
		mut p := &app.particles[i]
		p.color = gg.Color{u8(rand.intn(256) or { 128 }), u8(rand.intn(256) or { 128 }), u8(rand.intn(256) or { 128 }), 255}
		p.alpha = 1.0
		p.size = f32(rand.intn(30) or { 15 }) / 20.0
		p.rotation = f32(rand.intn(360) or { 0 })
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	// Activate one particle per frame at the mouse position.
	for i in 0 .. app.particles.len {
		if !app.particles[i].active {
			mut p := &app.particles[i]
			p.active = true
			p.alpha = 1.0
			p.position.x = f32(app.ctx.mouse_pos_x)
			p.position.y = f32(app.ctx.mouse_pos_y)
			break
		}
	}
	for i in 0 .. app.particles.len {
		mut p := &app.particles[i]
		if p.active {
			p.position.y += app.gravity / 2
			p.alpha -= 0.005
			if p.alpha <= 0 {
				p.active = false
			}
			p.rotation += 2.0
		}
	}

	app.ctx.begin()
	effect := if app.blending { gg.ImageEffect.add } else { gg.ImageEffect.alpha }
	tw := f32(app.texture.width)
	th := f32(app.texture.height)
	for i in 0 .. app.particles.len {
		p := &app.particles[i]
		if p.active {
			sw := tw * p.size
			sh := th * p.size
			app.ctx.draw_image_with_config(
				img:       &app.texture
				img_rect:  gg.Rect{p.position.x - sw / 2, p.position.y - sh / 2, sw, sh}
				part_rect: gg.Rect{0, 0, tw, th}
				rotation:  p.rotation
				color:     gg.Color{p.color.r, p.color.g, p.color.b, u8(p.alpha * 255)}
				effect:    effect
			)
		}
	}
	app.ctx.draw_text(180, 20, 'PRESS SPACE to CHANGE BLENDING MODE', size: 20, color: part_black)
	if app.blending {
		app.ctx.draw_text(280, 410, 'ADDITIVE BLENDING', size: 20, color: gg.white)
	} else {
		app.ctx.draw_text(290, 410, 'ALPHA BLENDING', size: 20, color: part_black)
	}
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.blending = !app.blending
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - particles blending'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		bg_color:     part_darkgray
	)
	app.ctx.run()
}
