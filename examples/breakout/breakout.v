import gg
import gx
import math
import rand
import sokol.audio
import os.asset
import sokol.sgl

const designed_width = 600
const designed_height = 800
const brick_width = 53
const brick_height = 20

struct Brick {
mut:
	x     f32
	y     f32
	w     f32 = brick_width
	h     f32 = brick_height
	c     gg.Color
	value int
	alive bool = true
}

struct Game {
mut:
	width     int = designed_width
	height    int = designed_height
	ball_x    f32
	ball_y    f32
	ball_r    f32 = 10.0
	ball_dx   f32 = 4
	ball_dy   f32 = -4
	paddle_x  f32 = 250
	paddle_w  f32 = 100
	paddle_h  f32 = 20
	paddle_dx f32 = 8
	bricks    []Brick
	nbricks   int
	npaddles  int = 10
	npoints   int
	nlevels   int = 1
	nevent    int
	sound     SoundManager
	ctx       &gg.Context = unsafe { nil }
}

fn Game.new() &Game {
	mut g := &Game{}
	g.ball_x, g.ball_y = g.width / 2, g.height - g.paddle_h
	g.init_bricks()
	return g
}

enum SoundKind {
	paddle
	brick
	wall
	lose_ball
}

struct SoundManager {
mut:
	sounds      [4][]f32 // TODO: using map[SoundKind][]f32 here breaks emscripten; use map after the fix
	initialised bool
}

fn (mut sm SoundManager) init() {
	all_kinds := [SoundKind.paddle, .brick, .wall, .lose_ball]!
	sample_rate := f32(audio.sample_rate())
	duration, volume := 0.09, f32(.25)
	nframes := int(sample_rate * duration)
	for i in 0 .. nframes {
		t := f32(i) / sample_rate
		sm.sounds[int(SoundKind.paddle)] << volume * math.sinf(t * 936.0 * 2 * math.pi)
		sm.sounds[int(SoundKind.brick)] << volume * math.sinf(t * 432.0 * 2 * math.pi)
		sm.sounds[int(SoundKind.wall)] << volume * math.sinf(t * 174.0 * 2 * math.pi)
		sm.sounds[int(SoundKind.lose_ball)] << math.sinf(t * 123.0 * 2 * math.pi)
	}
	border_samples := 2000
	for k, s := f32(0), 0; s <= border_samples; k, s = k + 1.0 / f32(border_samples), s + 1 {
		rk := f32(1) - k
		rs := nframes - border_samples - 1 + s
		for kind in all_kinds {
			sm.sounds[int(kind)][s] *= k
			sm.sounds[int(kind)][rs] *= rk
		}
	}
	sm.initialised = true
}

fn (mut g Game) play(k SoundKind) {
	if g.sound.initialised {
		s := g.sound.sounds[int(k)]
		audio.push(s.data, s.len)
	}
}

fn (mut g Game) init_bricks() {
	yoffset, xoffset := f32(50 + rand.intn(100) or { 0 }), f32(0 + rand.intn(50) or { 0 })
	g.bricks.clear()
	g.nbricks = 0
	for row in 0 .. 10 {
		for col in 0 .. 10 {
			g.bricks << Brick{
				x:     col * (brick_width + 1) + xoffset
				y:     row * (brick_height + 1) + yoffset
				c:     gx.rgb(0x40 | rand.u8(), 0x40 | rand.u8(), 0x40 | rand.u8())
				value: 10 - row
			}
			g.nbricks++
		}
	}
	for _ in 0 .. 5 + rand.intn(10) or { 0 } {
		i := rand.intn(g.bricks.len - 1) or { 0 }
		if g.bricks[i].alive {
			g.bricks[i].alive = false
			g.nbricks--
		}
	}
}

fn (mut g Game) draw() {
	ws := gg.window_size()
	g.ctx.begin()
	sgl.push_matrix()
	sgl.scale(f32(ws.width) / f32(designed_width), f32(ws.height) / f32(designed_height),
		0)

	roffset, rradius := -5, 18
	g.ctx.draw_circle_filled(g.paddle_x - roffset, g.height, rradius, gx.blue)
	g.ctx.draw_circle_filled(g.paddle_x + g.paddle_w + roffset, g.height, rradius, gx.blue)
	g.ctx.draw_rect_filled(g.paddle_x, g.height - g.paddle_h + 2, g.paddle_w, g.paddle_h,
		gx.blue)
	g.ctx.draw_circle_filled(g.ball_x, g.ball_y, g.ball_r, gx.red)
	for brick in g.bricks {
		if brick.alive {
			g.ctx.draw_rect_filled(brick.x, brick.y, brick.w, brick.h, brick.c)
		}
	}
	label1 := 'Level: ${g.nlevels:02}  Points: ${g.npoints:06}'
	label2 := 'Bricks: ${g.nbricks:03}  Paddles: ${g.npaddles:02}'
	g.ctx.draw_text(5, 3, label1, size: 24, color: gx.rgb(255, 255, 255))
	g.ctx.draw_text(320, 3, label2, size: 24, color: gx.rgb(255, 255, 255))

	sgl.pop_matrix()
	g.ctx.end()
}

fn (mut g Game) game_over() {
	g.init_bricks()
	g.npoints, g.nlevels, g.npaddles = 0, 1, 5
}

fn (mut g Game) goto_next_level() {
	g.init_bricks()
	g.npaddles++
	g.nlevels++
}

fn (mut g Game) update() {
	g.ball_x, g.ball_y = g.ball_x + g.ball_dx, g.ball_y + g.ball_dy
	// Wall collisions
	if g.ball_x < g.ball_r || g.ball_x > g.width - g.ball_r {
		g.ball_dx *= -1
		g.play(.wall)
	}
	if g.ball_y < g.ball_r {
		g.ball_dy *= -1
		g.play(.wall)
	}
	if g.ball_y > g.height {
		g.ball_x, g.ball_y = g.paddle_x + g.paddle_w / 2, g.height - g.paddle_h
		g.ball_dy = -4
		g.npaddles--
		g.play(.lose_ball)
		if g.npaddles <= 0 {
			g.game_over()
		}
	}
	// Paddle collision
	is_ball_on_paddle_y := g.ball_y + g.ball_r > g.height - g.paddle_h
		&& g.ball_y < g.height - g.ball_r
	is_ball_on_paddle_x := g.ball_x > g.paddle_x - 10 && g.ball_x < g.paddle_x + g.paddle_w + 10
	if is_ball_on_paddle_y && is_ball_on_paddle_x {
		g.play(.paddle)
		g.ball_dy = -math.abs(g.ball_dy)
		x_in_paddle := g.ball_x - g.paddle_x
		rmargin := 10
		if x_in_paddle < rmargin || x_in_paddle + rmargin > g.paddle_w {
			g.ball_dx *= -1
		} else if !(x_in_paddle > 40 && x_in_paddle < 60) {
			r := 10 * (-0.5 + rand.f32())
			g.ball_dx += r
			g.ball_dx = int_min(int_max(-80, int(g.ball_dx * 10)), 80) / 10
		}
	}
	// Brick collisions
	for mut brick in g.bricks {
		if brick.alive && g.ball_y - g.ball_r < brick.y + brick.h && g.ball_y + g.ball_r > brick.y
			&& g.ball_x > brick.x && g.ball_x < brick.x + brick.w {
			g.play(.brick)
			brick.alive = false
			g.nbricks--
			g.npoints += brick.value
			g.ball_dy *= -1
			if g.nbricks == 0 {
				g.goto_next_level()
			}
		}
	}
}

fn frame(mut g Game) {
	if g.ctx.pressed_keys[gg.KeyCode.left] {
		if g.paddle_x > 0 {
			g.paddle_x -= g.paddle_dx
		}
	}
	if g.ctx.pressed_keys[gg.KeyCode.right] {
		if g.paddle_x < g.width - g.paddle_w {
			g.paddle_x += g.paddle_dx
		}
	}
	g.update()
	g.draw()
}

fn key_down(key gg.KeyCode, _ gg.Modifier, mut g Game) {
	g.handle_event()
	match key {
		.r {
			g.game_over()
		}
		.escape {
			exit(0)
		}
		else {}
	}
}

@[if wasm32_emscripten]
fn (mut g Game) handle_event() {
	if g.nevent > 0 {
		return
	}
	// the audio has to be started when the wasm canvas has received user interaction, unlike on desktop platforms
	audio.setup(buffer_frames: 1024)
	g.sound.init()
	g.nevent++
}

fn main() {
	mut g := Game.new()
	mut fpath := asset.get_path('../assets', 'fonts/RobotoMono-Regular.ttf')
	$if !wasm32_emscripten {
		audio.setup(buffer_frames: 128)
		g.sound.init()
		fpath = ''
	}
	g.ctx = gg.new_context(
		width:        g.width
		height:       g.height
		window_title: 'V Breakout'
		sample_count: 2
		frame_fn:     frame
		keydown_fn:   key_down
		click_fn:     fn (x f32, y f32, btn gg.MouseButton, mut g Game) {
			g.handle_event()
		}
		user_data:    g
		font_path:    fpath
	)
	g.ctx.run()
}
