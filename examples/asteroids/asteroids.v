// Copyright (c) 2025 Delyan Angelov. All rights reserved.
// The use of this source code is governed by an MIT license that
// can be found in the LICENSE file.
module main

import gg
import gx
import math
import rand
import os.asset
import math.vec

type V2 = vec.Vec2[f32]

fn rads(degrees f32) f32 {
	return f32(math.radians(degrees))
}

fn (a V2) offset(angle f32, scale f32) V2 {
	return a + V2{math.cosf(angle) * scale, math.sinf(angle) * scale}
}

fn (mut a V2) wrap(b V2) {
	a.x = f32(math.mod(a.x + b.x, b.x))
	a.y = f32(math.mod(a.y + b.y, b.y))
}

fn V2.random(b V2) V2 {
	return V2{rand.f32() * b.x, rand.f32() * b.y}
}

struct Body {
mut:
	pos      V2
	vel      V2
	rotation f32
	radius   f32
	active   bool = true
}

struct Bullet {
	Body
}

struct Asteroid {
	Body
mut:
	segments      int
	offsets       []f32
	points        []f32
	rotation_step f32 = 1.0
}

struct Player {
	Body
mut:
	is_engine_on    bool
	bullets         int = 99
	bullets_limit   int = 99
	fuel            int = 9999
	fuel_limit      int = 9999
	cooldown        int
	cooldown_frames int   = 15
	points          []f32 = []f32{len: 8}
}

struct Game {
mut:
	gg        &gg.Context = unsafe { nil }
	screen    V2          = V2{800, 600}
	player    Player
	bullets   []Bullet
	asteroids []Asteroid
	score     int
	highscore int
	ships     int = 5
	level     int = 1
	is_up     bool

	msg Message
}

@[params]
struct Message {
mut:
	frames int
	text   string
	size   int = 40
	color  gx.Color
	align  gx.HorizontalAlign = .center
}

fn (mut a Asteroid) setup() {
	a.segments = int(a.radius / 10) * 10
	a.offsets = []f32{len: a.segments}
	a.points = []f32{len: 2 * a.segments}
	for i in 0 .. a.segments {
		a.offsets[i] = a.radius + 25 * (0.5 - rand.f32())
	}
}

fn (mut p Player) reset(screen V2) {
	p.bullets, p.fuel = p.bullets_limit, p.fuel_limit
	p.pos, p.vel = screen.div_scalar(2), V2{0, 0}
	p.radius, p.rotation = 15, -90
	p.active = true
}

fn (mut game Game) handle_input() {
	mut p := &game.player
	p.is_engine_on = false
	if game.gg.is_key_down(.escape) {
		exit(0)
	}
	if !p.active {
		return
	}
	game.is_up = game.gg.is_key_down(.up) || game.gg.is_key_down(.w)
	is_fire := game.gg.is_key_down(.space)
	is_left := game.gg.is_key_down(.left) || game.gg.is_key_down(.a)
	is_right := game.gg.is_key_down(.right) || game.gg.is_key_down(.d)
	if is_fire && p.cooldown <= 0 && p.active {
		game.add_bullet()
		p.cooldown = p.cooldown_frames
	}
	if p.fuel >= 10 {
		if game.is_up && p.active {
			angle := rads(p.rotation)
			p.vel += V2{0, 0}.offset(angle, 0.1)
			p.fuel -= 10
			p.is_engine_on = true
		}
	}
	if p.fuel >= 1 {
		if is_left {
			p.rotation -= 2
			p.fuel--
		}
		if is_right {
			p.rotation += 2
			p.fuel--
		}
	}
}

fn (mut game Game) update() {
	game.msg.frames = int_max(0, game.msg.frames - 1)
	mut p := &game.player
	p.cooldown = int_max(0, p.cooldown - 1)
	p.pos += p.vel
	p.pos.wrap(game.screen)
	for mut b in game.bullets {
		if !b.active {
			continue
		}
		b.pos += b.vel
		if b.pos.x < 0 || b.pos.x > game.screen.x || b.pos.y < 0 || b.pos.y > game.screen.y {
			b.active = false
		}
	}
	for mut a in game.asteroids {
		if !a.active {
			continue
		}
		a.pos += a.vel
		a.pos.wrap(game.screen)
		a.rotation += a.rotation_step
		if p.active && p.pos.distance(a.pos) <= (p.radius + a.radius) {
			// player/asteroids collision
			p.active = false
			a.active = false
			game.split_asteroid(a, p.vel.mul_scalar(0.5))
			game.player.reset(game.screen)
			game.score += 50
			game.show_message(text: 'Your ship was destroyed.', color: gx.red, frames: 90)
			game.ships--
			if game.ships <= 0 {
				game.ships = 5
				game.score = 0
				game.asteroids = []
				game.add_asteroids(10)
			}
		}
	}
	for mut b in game.bullets {
		if !b.active {
			continue
		}
		for mut a in game.asteroids {
			if !a.active {
				continue
			}
			if b.active && b.pos.distance(a.pos) <= (b.radius + a.radius) {
				// bullet/asteroid collision
				b.active = false
				a.active = false
				game.score += 100
				game.split_asteroid(a, b.vel.mul_scalar(0.2))
			}
		}
	}
	if game.bullets.any(!it.active) {
		game.bullets = game.bullets.filter(it.active)
	}
	if game.asteroids.any(!it.active) {
		game.asteroids = game.asteroids.filter(it.active)
	}
	if game.asteroids.len == 0 {
		game.level++
		game.ships++
		game.player.reset(game.screen)
		game.add_asteroids(10)
		game.show_message(text: 'YOU WIN', color: gx.green, frames: 90)
	}
	game.highscore = int_max(game.score, game.highscore)
}

fn (mut game Game) show_message(params Message) {
	game.msg = params
}

fn (mut game Game) split_asteroid(a &Asteroid, vel V2) {
	if a.radius < 30 {
		return
	}
	shrink_factor := 0.5 + 0.3 * rand.f32()
	mut a1 := Asteroid{
		...*a
		active:        true
		radius:        a.radius * shrink_factor
		rotation_step: -a.rotation_step
	}
	mut a2 := Asteroid{
		...*a
		active:        true
		radius:        a.radius * (1 - shrink_factor)
		rotation_step: 2 * a.rotation_step
	}
	a1.vel = a1.vel.mul_scalar(shrink_factor) * vel
	a2.vel = a2.vel.mul_scalar(1 - shrink_factor) * (V2{0, 0} - vel)
	a1.setup()
	a2.setup()
	game.asteroids << a1
	game.asteroids << a2
}

fn (mut game Game) draw() {
	ws := gg.window_size()
	game.screen = V2{ws.width, ws.height}
	game.gg.draw_rect_filled(0, 0, game.screen.x, game.screen.y, gx.rgba(20, 20, 20, 255))
	game.draw_ship()
	for b in game.bullets {
		game.gg.draw_circle_filled(b.pos.x, b.pos.y, 3, gx.yellow)
	}
	for mut a in game.asteroids {
		game.draw_asteroid(mut a)
	}
	scenter := game.screen.div_scalar(2)
	label1 := 'Level: ${game.level} Ships: ${game.ships}'
	game.gg.draw_text(5, 10, label1, size: 20, color: gx.white, align: .left)
	label2 := 'B: ${game.player.bullets:02} F: ${game.player.fuel:04}'
	game.gg.draw_text(int(scenter.x), 10, label2, size: 20, color: gx.green, align: .center)
	label3 := 'Score: ${game.score} Highscore: ${game.highscore}'
	game.gg.draw_text(int(game.screen.x) - 5, 10, label3, size: 20, color: gx.white, align: .right)
	if game.msg.frames > 0 {
		game.gg.draw_text(int(scenter.x), int(scenter.y / 2), game.msg.text,
			size:  game.msg.size
			color: game.msg.color
			align: game.msg.align
		)
	}
	label4 := 'Use arrows + space to control your ship. Use Escape to end the game.'
	game.gg.draw_text(int(game.screen.x - 5), int(game.screen.y - 20), label4,
		size:  16
		color: gx.gray
		align: .right
	)
}

fn (game &Game) draw_ship() {
	mut p := &game.player
	if !p.active {
		return
	}
	angle := rads(p.rotation)
	p1 := p.pos.offset(angle, p.radius)
	p2 := p.pos.offset(angle + 2.5, 0.6 * p.radius)
	p3 := p.pos.offset(angle, -0.3 * p.radius)
	p4 := p.pos.offset(angle - 2.5, 0.6 * p.radius)
	p.points[0] = p1.x
	p.points[1] = p1.y
	p.points[2] = p2.x
	p.points[3] = p2.y
	p.points[4] = p3.x
	p.points[5] = p3.y
	p.points[6] = p4.x
	p.points[7] = p4.y
	game.gg.draw_convex_poly(p.points, gx.white)
	if p.is_engine_on {
		engine := p.pos.offset(angle + math.pi, 0.7 * p.radius)
		game.gg.draw_circle_filled(engine.x, engine.y, 6, gx.yellow)
	}
}

fn (mut game Game) draw_asteroid(mut a Asteroid) {
	if !a.active {
		return
	}
	game.gg.draw_circle_filled(a.pos.x, a.pos.y, a.radius, gx.rgba(235, 235, 255, 215))
	for i in 0 .. a.segments {
		angle := rads(a.rotation + f32(i) * 360 / a.segments)
		p := a.pos.offset(angle, a.offsets[i])
		a.points[i * 2], a.points[i * 2 + 1] = p.x, p.y
	}
	game.gg.draw_convex_poly(a.points, gx.rgba(155, 155, 148, 245))
}

fn (mut game Game) add_bullet() {
	if game.player.bullets <= 0 {
		return
	}
	game.player.bullets--
	angle := rads(game.player.rotation)
	game.bullets << Bullet{
		pos:    game.player.pos
		radius: 3
		vel:    game.player.vel.offset(angle, 10)
		active: true
	}
}

fn (mut game Game) add_asteroids(count int) {
	for _ in 0 .. count {
		mut npos := V2{}
		new_asteroid_loop: for {
			npos = V2.random(game.screen)
			for a in game.asteroids {
				if a.pos.distance(npos) < (a.radius + 30) {
					continue new_asteroid_loop
				}
			}
			if game.player.pos.distance(npos) < 5 * (game.player.radius + 30) {
				continue new_asteroid_loop
			}
			break
		}
		radius := 50 + 50 * (0.5 - rand.f32())
		mut asteroid := Asteroid{
			pos:           npos
			vel:           (V2.random(game.screen) - V2.random(game.screen)) / game.screen
			radius:        radius
			rotation:      360 * rand.f32()
			rotation_step: 2 * (0.5 - rand.f32())
			active:        true
		}
		asteroid.setup()
		game.asteroids << asteroid
	}
}

fn on_frame(mut game Game) {
	if game.gg.timer.elapsed().milliseconds() > 15 {
		game.gg.timer.restart()
		game.handle_input()
		game.update()
	}
	game.gg.begin()
	game.draw()
	game.gg.end()
}

fn main() {
	mut game := &Game{}
	mut fpath := asset.get_path('../assets', 'fonts/RobotoMono-Regular.ttf')
	game.player.reset(game.screen)
	game.add_asteroids(10)
	game.gg = gg.new_context(
		window_title: 'V Asteroids'
		width:        int(game.screen.x)
		height:       int(game.screen.y)
		frame_fn:     on_frame
		user_data:    game
		sample_count: 2
		font_path:    fpath
	)
	game.gg.run()
}
