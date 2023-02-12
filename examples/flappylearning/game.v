module main

import gg
import gx
import os
import time
import math
import rand
import neuroevolution

const (
	win_width  = 500
	win_height = 512
)

struct Bird {
mut:
	x        f64  = 80
	y        f64  = 250
	width    f64  = 40
	height   f64  = 30
	alive    bool = true
	gravity  f64
	velocity f64 = 0.3
	jump     f64 = -6
}

fn (mut b Bird) flap() {
	b.gravity = b.jump
}

fn (mut b Bird) update() {
	b.gravity += b.velocity
	b.y += b.gravity
}

fn (b Bird) is_dead(height f64, pipes []Pipe) bool {
	if b.y >= height || b.y + b.height <= 0 {
		return true
	}
	for pipe in pipes {
		if !(b.x > pipe.x + pipe.width || b.x + b.width < pipe.x || b.y > pipe.y + pipe.height
			|| b.y + b.height < pipe.y) {
			return true
		}
	}
	return false
}

struct Pipe {
mut:
	x      f64 = 80
	y      f64 = 250
	width  f64 = 40
	height f64 = 30
	speed  f64 = 3
}

fn (mut p Pipe) update() {
	p.x -= p.speed
}

fn (p Pipe) is_out() bool {
	return p.x + p.width < 0
}

struct App {
mut:
	gg               &gg.Context = unsafe { nil }
	background       gg.Image
	bird             gg.Image
	pipetop          gg.Image
	pipebottom       gg.Image
	pipes            []Pipe
	birds            []Bird
	score            int
	max_score        int
	width            f64 = win_width
	height           f64 = win_height
	spawn_interval   f64 = 90
	interval         f64
	nv               neuroevolution.Generations
	gen              []neuroevolution.Network
	alives           int
	generation       int
	background_speed f64 = 0.5
	background_x     f64
	timer_period_ms  int = 24
}

fn (mut app App) start() {
	app.interval = 0
	app.score = 0
	app.pipes = []
	app.birds = []
	app.gen = app.nv.generate()
	for _ in 0 .. app.gen.len {
		app.birds << Bird{}
	}
	app.generation++
	app.alives = app.birds.len
}

fn (app &App) is_it_end() bool {
	for i in 0 .. app.birds.len {
		if app.birds[i].alive {
			return false
		}
	}
	return true
}

fn (mut app App) update() {
	app.background_x += app.background_speed
	mut next_holl := f64(0)
	if app.birds.len > 0 {
		for i := 0; i < app.pipes.len; i += 2 {
			if app.pipes[i].x + app.pipes[i].width > app.birds[0].x {
				next_holl = app.pipes[i].height / app.height
				break
			}
		}
	}
	for j, mut bird in app.birds {
		if bird.alive {
			inputs := [
				bird.y / app.height,
				next_holl,
			]
			res := app.gen[j].compute(inputs)
			if res[0] > 0.5 {
				bird.flap()
			}
			bird.update()
			if bird.is_dead(app.height, app.pipes) {
				bird.alive = false
				app.alives--
				app.nv.network_score(app.gen[j], app.score)
				if app.is_it_end() {
					app.start()
				}
			}
		}
	}
	for k := 0; k < app.pipes.len; k++ {
		app.pipes[k].update()
		if app.pipes[k].is_out() {
			app.pipes.delete(k)
			k--
		}
	}
	if app.interval == 0 {
		delta_bord := f64(50)
		pipe_holl := f64(120)
		holl_position := math.round(rand.f64() * (app.height - delta_bord * 2.0 - pipe_holl)) +
			delta_bord
		app.pipes << Pipe{
			x: app.width
			y: 0
			height: holl_position
		}
		app.pipes << Pipe{
			x: app.width
			y: holl_position + pipe_holl
			height: app.height
		}
	}
	app.interval++
	if app.interval == app.spawn_interval {
		app.interval = 0
	}
	app.score++
	app.max_score = if app.score > app.max_score { app.score } else { app.max_score }
}

fn main() {
	mut app := &App{
		gg: 0
	}
	mut font_path := os.resource_abs_path(os.join_path('..', 'assets', 'fonts', 'RobotoMono-Regular.ttf'))
	$if android {
		font_path = 'fonts/RobotoMono-Regular.ttf'
	}
	app.gg = gg.new_context(
		bg_color: gx.white
		width: win_width
		height: win_height
		create_window: true
		window_title: 'flappylearning-v'
		frame_fn: frame
		event_fn: on_event
		user_data: app
		init_fn: init_images
		font_path: font_path
	)
	app.nv = neuroevolution.Generations{
		population: 50
		network: [2, 2, 1]
	}
	app.start()
	spawn app.run()
	app.gg.run()
}

fn (mut app App) run() {
	for {
		app.update()
		time.sleep(app.timer_period_ms * time.millisecond)
	}
}

fn init_images(mut app App) {
	$if android {
		background := os.read_apk_asset('img/background.png') or { panic(err) }
		app.background = app.gg.create_image_from_byte_array(background)
		bird := os.read_apk_asset('img/bird.png') or { panic(err) }
		app.bird = app.gg.create_image_from_byte_array(bird)
		pipetop := os.read_apk_asset('img/pipetop.png') or { panic(err) }
		app.pipetop = app.gg.create_image_from_byte_array(pipetop)
		pipebottom := os.read_apk_asset('img/pipebottom.png') or { panic(err) }
		app.pipebottom = app.gg.create_image_from_byte_array(pipebottom)
	} $else {
		app.background = app.gg.create_image(os.resource_abs_path('assets/img/background.png'))
		app.bird = app.gg.create_image(os.resource_abs_path('assets/img/bird.png'))
		app.pipetop = app.gg.create_image(os.resource_abs_path('assets/img/pipetop.png'))
		app.pipebottom = app.gg.create_image(os.resource_abs_path('assets/img/pipebottom.png'))
	}
}

fn frame(app &App) {
	app.gg.begin()
	app.draw()
	app.gg.end()
}

fn (app &App) display() {
	for i := 0; i < int(math.ceil(app.width / app.background.width) + 1.0); i++ {
		background_x := i * app.background.width - math.floor(int(app.background_x) % int(app.background.width))
		app.gg.draw_image(f32(background_x), 0, app.background.width, app.background.height,
			app.background)
	}
	for i, pipe in app.pipes {
		if i % 2 == 0 {
			app.gg.draw_image(f32(pipe.x), f32(pipe.y + pipe.height - app.pipetop.height),
				app.pipetop.width, app.pipetop.height, app.pipetop)
		} else {
			app.gg.draw_image(f32(pipe.x), f32(pipe.y), app.pipebottom.width, app.pipebottom.height,
				app.pipebottom)
		}
	}
	for bird in app.birds {
		if bird.alive {
			app.gg.draw_image(f32(bird.x), f32(bird.y), app.bird.width, app.bird.height,
				app.bird)
		}
	}
	app.gg.draw_text_def(10, 25, 'Score: ${app.score}')
	app.gg.draw_text_def(10, 50, 'Max Score: ${app.max_score}')
	app.gg.draw_text_def(10, 75, 'Generation: ${app.generation}')
	app.gg.draw_text_def(10, 100, 'Alive: ${app.alives} / ${app.nv.population}')
}

fn (app &App) draw() {
	app.display()
}

fn on_event(e &gg.Event, mut app App) {
	if e.typ == .key_down {
		app.key_down(e.key_code)
	}
}

fn (mut app App) key_down(key gg.KeyCode) {
	// global keys
	match key {
		.escape {
			app.gg.quit()
		}
		._0 {
			app.timer_period_ms = 0
		}
		.space {
			if app.timer_period_ms == 24 {
				app.timer_period_ms = 4
			} else {
				app.timer_period_ms = 24
			}
		}
		else {}
	}
}
