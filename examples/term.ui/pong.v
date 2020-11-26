// Copyright (c) 2020 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by the MIT license distributed with this software.
import term
import term.ui
import time

enum Mode {
	menu
	game
}

const (
	player_one = 1 // Human control this racket
	player_two = 0 // Take over this AI controller
	white      = ui.Color{255, 255, 255}
	orange     = ui.Color{255, 140, 0}
)

struct App {
mut:
	tui    &ui.Context = 0
	mode   Mode = Mode.menu
	width  int
	height int
	game   &Game = 0
	dt     f32
	ticks  i64
}

fn (mut a App) init() {
	a.game = &Game{
		app: a
	}
	w, h := a.tui.window_width, a.tui.window_height
	a.width = w
	a.height = h
	term.erase_del_clear()
	term.set_cursor_position({
		x: 0
		y: 0
	})
}

fn (mut a App) start_game() {
	if a.mode != .game {
		a.mode = .game
	}
	a.game.init()
}

fn (mut a App) frame() {
	ticks := time.ticks()
	a.dt = f32(ticks - a.ticks) / 1000.0
	a.width, a.height = a.tui.window_width, a.tui.window_height
	if a.mode == .game {
		a.game.update()
	}
	a.tui.clear()
	a.render()
	a.tui.flush()
	a.ticks = ticks
}

fn (mut a App) quit() {
	if a.mode != .menu {
		a.game.quit()
		return
	}
	term.set_cursor_position({
		x: 0
		y: 0
	})
	exit(0)
}

fn (mut a App) event(e &ui.Event) {
	match e.typ {
		.mouse_move {
			if a.mode != .game {
				return
			}
			// TODO mouse movement for real Pong sharks
			// a.game.move_player(player_one, 0, -1)
		}
		.key_down {
			match e.code {
				.escape, .q {
					a.quit()
				}
				.w {
					if a.mode != .game {
						return
					}
					a.game.move_player(player_one, 0, -1)
				}
				.a {
					if a.mode != .game {
						return
					}
					a.game.move_player(player_one, 0, -1)
				}
				.s {
					if a.mode != .game {
						return
					}
					a.game.move_player(player_one, 0, 1)
				}
				.d {
					if a.mode != .game {
						return
					}
					a.game.move_player(player_one, 0, 1)
				}
				.left {
					if a.mode != .game {
						return
					}
					a.game.move_player(player_two, 0, -1)
				}
				.right {
					if a.mode != .game {
						return
					}
					a.game.move_player(player_two, 0, 1)
				}
				.up {
					if a.mode != .game {
						return
					}
					a.game.move_player(player_two, 0, -1)
				}
				.down {
					if a.mode != .game {
						return
					}
					a.game.move_player(player_two, 0, 1)
				}
				.enter, .space {
					if a.mode == .menu {
						a.start_game()
					}
				}
				else {}
			}
		}
		else {}
	}
}

fn (mut a App) free() {
	unsafe {
		a.game.free()
		free(a.game)
	}
}

fn (mut a App) render() {
	match a.mode {
		.menu { a.draw_menu() }
		else { a.draw_game() }
	}
}

fn (mut a App) draw_menu() {
	cx := int(f32(a.width) * 0.5)
	y025 := int(f32(a.height) * 0.25)
	y075 := int(f32(a.height) * 0.75)
	cy := int(f32(a.height) * 0.5)
	//
	a.tui.set_color(white)
	a.tui.bold()
	a.tui.draw_text(cx - 2, y025, 'VONG')
	a.tui.reset()
	a.tui.draw_text(cx - 13, y025 + 1, '(A game of Pong written in V)')
	//
	a.tui.set_color(white)
	a.tui.bold()
	a.tui.draw_text(cx - 3, cy + 1, 'START')
	a.tui.reset()
	//
	a.tui.draw_text(cx - 9, y075 + 1, 'Press SPACE to start')
	a.tui.reset()
	a.tui.draw_text(cx - 5, y075 + 3, 'ESC to Quit')
	a.tui.reset()
}

fn (mut a App) draw_game() {
	a.game.draw()
}

struct Player {
mut:
	game        &Game
	pos         Vec
	racket_size int = 4
	score       int
	ai          bool
}

fn (mut p Player) move(x f32, y f32) {
	p.pos.x += x
	p.pos.y += y
}

fn (mut p Player) update() {
	if !p.ai {
		return
	}
	if isnil(p.game) {
		return
	}
	// dt := p.game.app.dt
	ball := &p.game.ball
	// Evil AI that eventually will take over the world
	p.pos.y = ball.pos.y - int(f32(p.racket_size) * 0.5)
}

struct Vec {
mut:
	x f32
	y f32
}

fn (mut v Vec) set(x f32, y f32) {
	v.x = x
	v.y = y
}

struct Ball {
mut:
	pos Vec
	vel Vec
	acc Vec
}

fn (mut b Ball) update(dt f32) {
	b.pos.x += b.vel.x * b.acc.x * dt
	b.pos.y += b.vel.y * b.acc.y * dt
}

struct Game {
mut:
	app     &App = 0
	players []Player
	ball    Ball
}

fn (mut g Game) move_player(id int, x int, y int) {
	mut p := &g.players[id]
	if p.ai { // disable AI when moved
		p.ai = false
	}
	p.move(x, y)
}

fn (mut g Game) init() {
	if g.players.len == 0 {
		g.players = []Player{len: 2, init: Player{ // <- BUG omitting the init will result in smaller racket sizes???
			game: g
		}}
	}
	g.reset()
}

fn (mut g Game) reset() {
	mut i := 0
	for mut p in g.players {
		p.score = 0
		if i != player_one {
			p.ai = true
		}
		i++
	}
	g.new_round()
}

fn (mut g Game) new_round() {
	mut i := 0
	for mut p in g.players {
		p.pos.x = if i == 0 { 3 } else { g.app.width - 2 }
		p.pos.y = f32(g.app.height) * 0.5 - f32(p.racket_size) * 0.5
		i++
	}
	g.ball.pos.set(f32(g.app.width) * 0.5, f32(g.app.height) * 0.5)
	g.ball.vel.set(-8, -15)
	g.ball.acc.set(2.0, 1.0)
}

fn (mut g Game) update() {
	dt := g.app.dt
	mut b := &g.ball
	for mut p in g.players {
		p.update()
		// Keep rackets within the game area
		if p.pos.y <= 0 {
			p.pos.y = 1
		}
		if p.pos.y + p.racket_size >= g.app.height {
			p.pos.y = g.app.height - p.racket_size - 1
		}
		// Check ball collision
		// Player left side
		if p.pos.x < f32(g.app.width) * 0.5 {
			// Racket collision
			if b.pos.x <= p.pos.x + 1 {
				if b.pos.y >= p.pos.y && b.pos.y <= p.pos.y + p.racket_size {
					b.vel.x *= -1
				}
			}
			// Behind racket
			if b.pos.x < p.pos.x {
				g.players[1].score++
				g.new_round()
			}
		} else {
			// Player right side
			if b.pos.x >= p.pos.x - 1 {
				if b.pos.y >= p.pos.y && b.pos.y <= p.pos.y + p.racket_size {
					b.vel.x *= -1
				}
			}
			if b.pos.x > p.pos.x {
				g.players[0].score++
				g.new_round()
			}
		}
	}
	if b.pos.x <= 1 || b.pos.x >= g.app.width {
		b.vel.x *= -1
	}
	if b.pos.y <= 2 || b.pos.y >= g.app.height {
		b.vel.y *= -1
	}
	b.update(dt)
}

fn (mut g Game) quit() {
	if g.app.mode != .game {
		return
	}
	g.app.mode = .menu
}

fn (mut g Game) draw_big_digit(px f32, py f32, digit int) {
	// TODO use draw_line or draw_point to fix tearing with non-monospaced terminal fonts
	mut gfx := g.app.tui
	x, y := int(px), int(py)
	match digit {
		0 {
			gfx.draw_text(x, y + 0, '█████')
			gfx.draw_text(x, y + 1, '█   █')
			gfx.draw_text(x, y + 2, '█   █')
			gfx.draw_text(x, y + 3, '█   █')
			gfx.draw_text(x, y + 4, '█████')
		}
		1 {
			gfx.draw_text(x + 3, y + 0, '█')
			gfx.draw_text(x + 3, y + 1, '█')
			gfx.draw_text(x + 3, y + 2, '█')
			gfx.draw_text(x + 3, y + 3, '█')
			gfx.draw_text(x + 3, y + 4, '█')
		}
		2 {
			gfx.draw_text(x, y + 0, '█████')
			gfx.draw_text(x, y + 1, '    █')
			gfx.draw_text(x, y + 2, '█████')
			gfx.draw_text(x, y + 3, '█')
			gfx.draw_text(x, y + 4, '█████')
		}
		3 {
			gfx.draw_text(x, y + 0, '█████')
			gfx.draw_text(x, y + 1, '   ██')
			gfx.draw_text(x, y + 2, ' ████')
			gfx.draw_text(x, y + 3, '   ██')
			gfx.draw_text(x, y + 4, '█████')
		}
		4 {
			gfx.draw_text(x, y + 0, '█   █')
			gfx.draw_text(x, y + 1, '█   █')
			gfx.draw_text(x, y + 2, '█████')
			gfx.draw_text(x, y + 3, '    █')
			gfx.draw_text(x, y + 4, '    █')
		}
		5 {
			gfx.draw_text(x, y + 0, '█████')
			gfx.draw_text(x, y + 1, '█')
			gfx.draw_text(x, y + 2, '█████')
			gfx.draw_text(x, y + 3, '    █')
			gfx.draw_text(x, y + 4, '█████')
		}
		6 {
			gfx.draw_text(x, y + 0, '█████')
			gfx.draw_text(x, y + 1, '█')
			gfx.draw_text(x, y + 2, '█████')
			gfx.draw_text(x, y + 3, '█   █')
			gfx.draw_text(x, y + 4, '█████')
		}
		7 {
			gfx.draw_text(x, y + 0, '█████')
			gfx.draw_text(x, y + 1, '    █')
			gfx.draw_text(x, y + 2, '    █')
			gfx.draw_text(x, y + 3, '    █')
			gfx.draw_text(x, y + 4, '    █')
		}
		8 {
			gfx.draw_text(x, y + 0, '█████')
			gfx.draw_text(x, y + 1, '█   █')
			gfx.draw_text(x, y + 2, '█████')
			gfx.draw_text(x, y + 3, '█   █')
			gfx.draw_text(x, y + 4, '█████')
		}
		9 {
			gfx.draw_text(x, y + 0, '█████')
			gfx.draw_text(x, y + 1, '█   █')
			gfx.draw_text(x, y + 2, '█████')
			gfx.draw_text(x, y + 3, '    █')
			gfx.draw_text(x, y + 4, '█████')
		}
		else {}
	}
}

fn (mut g Game) draw() {
	mut gfx := g.app.tui
	gfx.set_bg_color(white)
	// Border
	gfx.draw_empty_rect(1, 1, g.app.width, g.app.height)
	// Center line
	gfx.draw_dashed_line(int(f32(g.app.width) * 0.5), 0, int(f32(g.app.width) * 0.5),
		int(g.app.height))
	border := 1
	mut y, mut x := 0, 0
	for p in g.players {
		x = int(p.pos.x)
		y = int(p.pos.y)
		gfx.reset_bg_color()
		gfx.set_color(white)
		if x < f32(g.app.width) * 0.5 {
			g.draw_big_digit(f32(g.app.width) * 0.25, 3, p.score)
		} else {
			g.draw_big_digit(f32(g.app.width) * 0.75, 3, p.score)
		}
		gfx.reset_color()
		gfx.set_bg_color(white)
		// Racket
		gfx.draw_line(x, y + border, x, y + p.racket_size)
	}
	// Ball
	gfx.draw_point(int(g.ball.pos.x), int(g.ball.pos.y))
	// gfx.draw_text(22,2,'$g.ball.pos')
	gfx.reset_bg_color()
}

fn (mut g Game) free() {
	g.players.clear()
}

// TODO Remove these wrapper functions when we can assign methods as callbacks
fn init(x voidptr) {
	mut app := &App(x)
	app.init()
}

fn frame(x voidptr) {
	mut app := &App(x)
	app.frame()
}

fn cleanup(x voidptr) {
	mut app := &App(x)
	app.free()
}

fn fail(error string) {
	eprintln(error)
}

fn event(e &ui.Event, x voidptr) {
	mut app := &App(x)
	app.event(e)
}

// main
mut app := &App{}
app.tui = ui.init({
	user_data: app
	init_fn: init
	frame_fn: frame
	cleanup_fn: cleanup
	event_fn: event
	fail_fn: fail
	capture_events: true
	hide_cursor: true
	frame_rate: 60
})
app.tui.run()
