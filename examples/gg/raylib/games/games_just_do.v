module main

// raylib [games] - just do (GGJ15), faithfully ported to gg.
// An experimental mouse-driven shape/colour puzzle: an animated logo gives way
// to ten small puzzles, each with its own click-to-solve rule. Left-click (or
// click-and-hold) on the shapes to change them; each level records the seconds
// taken and asks for one more click before advancing. Keys 0-9 jump to a level.
// PORT NOTE: sound/music (win.wav, ambient.ogg) are omitted — gg examples have
// no bundled audio assets. All visuals use gg text/rect/circle/poly primitives.

import gg
import math
import rand

const screen_width = 1280
const screen_height = 720

const ray_white = gg.Color{245, 245, 245, 255}
const black = gg.Color{0, 0, 0, 255}
const gray = gg.Color{130, 130, 130, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const dark_gray = gg.Color{80, 80, 80, 255}

const level_titles = [
	'LEVEL 00',
	'LEVEL 01',
	'LEVEL 02',
	'LEVEL 03',
	'LEVEL 04',
	'LEVEL 05',
	'LEVEL 06',
	'LEVEL 07',
	'LEVEL 08',
	'LEVEL 09',
]

enum GameScreen {
	logo
	level00
	level01
	level02
	level03
	level04
	level05
	level06
	level07
	level08
	level09
}

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	screen GameScreen  = .logo

	// screen transition state
	on_transition  bool
	trans_fade_out bool
	trans_alpha    f32
	trans_to       GameScreen = .logo

	// shared input / timing state
	mouse_x          f32
	mouse_y          f32
	mouse_held       bool
	click            bool
	frames_counter   int
	done             bool
	level_finished   bool
	level_time_sec   int
	finish_requested bool

	// logo animation
	logo_state    int
	logo_letters  int
	logo_top_w    int
	logo_left_h   int
	logo_bottom_w int
	logo_right_h  int
	logo_alpha    f32 = 1.0

	// level 00 - dropping "U" and stepped "O"
	l0_u_x      f32
	l0_u_y      f32
	l0_u_w      f32
	l0_o_x      f32
	l0_o_y      f32
	l0_o_w      f32
	l0_placed_u bool
	l0_placed_o bool
	l0_over_u   bool
	l0_over_o   bool

	// level 01 - grow/shrink opposite inner squares
	l1_il_x f32
	l1_il_y f32
	l1_il_w f32
	l1_il_h f32
	l1_ir_x f32
	l1_ir_y f32
	l1_ir_w f32
	l1_ir_h f32

	// level 02 - bouncing ball and draggable hole
	l2_ball_x  f32
	l2_ball_y  f32
	l2_ball_sx f32
	l2_ball_sy f32
	l2_hole_x  f32
	l2_hole_y  f32

	// level 03 - drag the white piece onto the gray hole
	l3_piece_x    f32
	l3_piece_y    f32
	l3_show_piece bool
	l3_piece_held bool

	// level 04 - inner/outer concentric circles
	l4_inner_r f32
	l4_outer_r f32

	// level 05 - ten rings to toggle (win: all GRAY, matching the original)
	l5_radius [10]f32
	l5_white  [10]bool

	// level 06 - stop the right moving boxes inside the centre strip
	l6_rec_x   [4]f32
	l6_speed   [4]int
	l6_stopped [4]bool
	l6_hover   int = -1

	// level 07 - three buttons toggle three circles
	l7_active [3]bool
	l7_white  [3]bool

	// level 08 - bouncing box through switchable columns
	l8_box_x   f32
	l8_speed   int
	l8_columns [3]bool

	// level 09 - timed cascade of black/white boxes (0 gray, 1 light, 2 white)
	l9_col       [21]int
	l9_activated [21]bool
	l9_counter   int
	l9_enabled   bool
}

// clamp01 bounds a fade factor to [0,1].
fn clamp01(v f32) f32 {
	if v < 0 {
		return 0
	}
	if v > 1 {
		return 1
	}
	return v
}

// fade mirrors raylib's Fade() over a known background: gg.Color carries no
// separate blend factor, so each channel is interpolated toward bg.
fn fade(col gg.Color, bg gg.Color, alpha f32) gg.Color {
	a := clamp01(alpha)
	return gg.Color{
		r: u8(f32(col.r) * a + f32(bg.r) * (1 - a))
		g: u8(f32(col.g) * a + f32(bg.g) * (1 - a))
		b: u8(f32(col.b) * a + f32(bg.b) * (1 - a))
		a: 255
	}
}

// overlay_color keeps the source colour but puts the fade in alpha, which gg
// blends over whatever is already drawn (used for the full-screen transition).
fn overlay_color(col gg.Color, alpha f32) gg.Color {
	return gg.Color{col.r, col.g, col.b, u8(clamp01(alpha) * 255)}
}

fn in_rect(px f32, py f32, x f32, y f32, w f32, h f32) bool {
	return px >= x && px <= x + w && py >= y && py <= y + h
}

fn in_circle(px f32, py f32, cx f32, cy f32, r f32) bool {
	dx := px - cx
	dy := py - cy
	return dx * dx + dy * dy <= r * r
}

fn rects_overlap(ax f32, ay f32, aw f32, ah f32, bx f32, by f32, bw f32, bh f32) bool {
	return ax < bx + bw && ax + aw > bx && ay < by + bh && ay + ah > by
}

fn distance(ax f32, ay f32, bx f32, by f32) f32 {
	dx := ax - bx
	dy := ay - by
	return math.sqrtf(dx * dx + dy * dy)
}

fn rand_range(lo int, hi int) int {
	return lo + (rand.intn(hi - lo + 1) or { 0 })
}

fn pad3(n int) string {
	mut s := n.str()
	for s.len < 3 {
		s = '0' + s
	}
	return s
}

fn draw_centered(ctx &gg.Context, msg string, y int, size int, c gg.Color) {
	w := ctx.text_width_cfg(msg, size: size)
	ctx.draw_text(screen_width / 2 - w / 2, y, msg, size: size, color: c)
}

fn draw_border(ctx &gg.Context, w f32, h f32, bs f32, c gg.Color) {
	ctx.draw_rect_filled(0, 0, w, bs, c)
	ctx.draw_rect_filled(0, bs, bs, h - 2 * bs, c)
	ctx.draw_rect_filled(w - bs, bs, bs, h - 2 * bs, c)
	ctx.draw_rect_filled(0, h - bs, w, bs, c)
}

fn measure(ctx &gg.Context, text string, size int) f32 {
	if isnil(ctx) {
		return f32(size * 2 / 3 + 40)
	}
	return f32(ctx.text_width_cfg(text, size: size) + 40)
}

fn reset_common(mut app &App) {
	app.frames_counter = 0
	app.done = false
	app.level_finished = false
	app.level_time_sec = 0
	app.finish_requested = false
}

// common_finish records the completion time and handles the click-to-continue.
fn common_finish(mut app &App, click bool, delay int) {
	if app.done && !app.level_finished {
		app.level_time_sec = app.frames_counter / 60
		app.level_finished = true
		app.frames_counter = 0
	}
	if app.level_finished {
		app.frames_counter++
		if app.frames_counter > delay && click {
			app.finish_requested = true
		}
	}
}

fn draw_finish(ctx &gg.Context, level int, secs int, overlay gg.Color) {
	draw_border(ctx, f32(screen_width), f32(screen_height), 60, overlay)
	draw_centered(ctx, level_titles[level], 20, 30, gray)
	msg := 'DONE! (Seconds: ${pad3(secs)})'
	w := ctx.text_width_cfg('DONE! (Seconds: 000)', size: 30)
	ctx.draw_text(screen_width / 2 - w / 2, screen_height - 40, msg, size: 30, color: gray)
}

// ---------------------------------------------------------------------------
// Initialisation
// ---------------------------------------------------------------------------

fn init_logo(mut app &App) {
	reset_common(mut app)
	app.logo_state = 0
	app.logo_letters = 0
	app.logo_top_w = 16
	app.logo_left_h = 16
	app.logo_bottom_w = 16
	app.logo_right_h = 16
	app.logo_alpha = 1.0
}

fn init_level00(mut app &App) {
	reset_common(mut app)
	app.l0_u_x = f32(screen_width / 2 - 265)
	app.l0_u_y = -200
	app.l0_u_w = measure(app.ctx, 'U', 160)
	app.l0_o_x = f32(screen_width - 370)
	app.l0_o_y = -30
	app.l0_o_w = measure(app.ctx, 'O', 160)
	app.l0_placed_u = false
	app.l0_placed_o = false
	app.l0_over_u = false
	app.l0_over_o = false
}

fn init_level01(mut app &App) {
	reset_common(mut app)
	app.l1_il_x = f32(screen_width / 4 - 200)
	app.l1_il_y = f32(screen_height / 2 - 200)
	app.l1_il_w = 400
	app.l1_il_h = 400
	app.l1_ir_x = f32(screen_width / 2 + screen_width / 4 - 200)
	app.l1_ir_y = f32(screen_height / 2 - 200)
	app.l1_ir_w = 400
	app.l1_ir_h = 400
}

fn init_level02(mut app &App) {
	reset_common(mut app)
	app.l2_ball_x = 120
	app.l2_ball_y = 80
	app.l2_ball_sx = 6
	app.l2_ball_sy = 8
	app.l2_hole_x = f32(screen_width / 2)
	app.l2_hole_y = f32(screen_height / 2)
}

fn init_level03(mut app &App) {
	reset_common(mut app)
	app.l3_piece_x = 200
	app.l3_piece_y = 400
	app.l3_show_piece = false
	app.l3_piece_held = false
}

fn init_level04(mut app &App) {
	reset_common(mut app)
	app.l4_inner_r = 40
	app.l4_outer_r = 300
}

fn init_level05(mut app &App) {
	reset_common(mut app)
	for i in 0 .. 10 {
		app.l5_radius[i] = f32(76 * (10 - i))
		app.l5_white[i] = false
	}
	// initial colours: white at 8,7,5,1 (from the original "dirty hack")
	app.l5_white[8] = true
	app.l5_white[7] = true
	app.l5_white[5] = true
	app.l5_white[1] = true
}

fn init_level06(mut app &App) {
	reset_common(mut app)
	for i in 0 .. 4 {
		app.l6_rec_x[i] = f32(rand_range(0, 5) * 150)
		app.l6_speed[i] = rand_range(4, 8)
		app.l6_stopped[i] = false
	}
	app.l6_hover = -1
}

fn init_level07(mut app &App) {
	reset_common(mut app)
	app.l7_active[0] = false
	app.l7_active[1] = true
	app.l7_active[2] = false
	for i in 0 .. 3 {
		app.l7_white[i] = false
	}
}

fn init_level08(mut app &App) {
	reset_common(mut app)
	app.l8_box_x = 20
	app.l8_speed = 4
	app.l8_columns[0] = true
	app.l8_columns[1] = false
	app.l8_columns[2] = true
}

fn init_level09(mut app &App) {
	reset_common(mut app)
	for i in 0 .. 21 {
		if i % 2 == 0 {
			app.l9_col[i] = 1 // light gray
		} else {
			app.l9_col[i] = 0 // gray
		}
		app.l9_activated[i] = false
	}
	app.l9_col[10] = 2 // white
	app.l9_counter = 0
	app.l9_enabled = false
}

fn init_screen(mut app &App) {
	match app.screen {
		.logo { init_logo(mut app) }
		.level00 { init_level00(mut app) }
		.level01 { init_level01(mut app) }
		.level02 { init_level02(mut app) }
		.level03 { init_level03(mut app) }
		.level04 { init_level04(mut app) }
		.level05 { init_level05(mut app) }
		.level06 { init_level06(mut app) }
		.level07 { init_level07(mut app) }
		.level08 { init_level08(mut app) }
		.level09 { init_level09(mut app) }
	}
}

// ---------------------------------------------------------------------------
// Updates
// ---------------------------------------------------------------------------

fn update_logo(mut app &App) {
	match app.logo_state {
		0 {
			app.frames_counter++
			if app.frames_counter == 80 {
				app.logo_state = 1
				app.frames_counter = 0
			}
		}
		1 {
			app.logo_top_w += 8
			app.logo_left_h += 8
			if app.logo_top_w == 256 {
				app.logo_state = 2
			}
		}
		2 {
			app.logo_bottom_w += 8
			app.logo_right_h += 8
			if app.logo_bottom_w == 256 {
				app.logo_state = 3
			}
		}
		3 {
			app.frames_counter++
			if app.logo_letters < 10 {
				if app.frames_counter / 12 > 0 {
					app.logo_letters++
					app.frames_counter = 0
				}
			} else if app.frames_counter > 200 {
				app.logo_alpha -= 0.02
				if app.logo_alpha <= 0 {
					app.logo_alpha = 0
					app.done = true
				}
			}
		}
		else {}
	}
}

fn update_level00(mut app &App, click bool) {
	if !app.done {
		app.frames_counter++
	}
	if !app.done {
		if !app.l0_placed_u {
			app.l0_u_y += 2
			if app.l0_u_y >= f32(screen_height) {
				app.l0_u_y = -160
			}
		}
		if in_rect(app.mouse_x, app.mouse_y, app.l0_u_x, app.l0_u_y, app.l0_u_w, 160) {
			app.l0_over_u = true
			if !app.l0_placed_u && click {
				if app.l0_u_y > f32(screen_height / 2 - 110)
					&& (app.l0_u_y + 160) < f32(screen_height / 2 + 100) {
					app.l0_placed_u = true
				}
			}
		} else {
			app.l0_over_u = false
		}
		if in_rect(app.mouse_x, app.mouse_y, app.l0_o_x, app.l0_o_y, app.l0_o_w, 160) {
			app.l0_over_o = true
			if !app.l0_placed_o && click {
				app.l0_o_y += 100
				if app.l0_o_y >= f32(screen_height / 2 - 130) {
					app.l0_placed_o = true
				}
			}
		} else {
			app.l0_over_o = false
		}
		if app.l0_placed_u && app.l0_placed_o {
			app.done = true
		}
	}
	common_finish(mut app, click, 30)
}

fn update_level01(mut app &App, click bool) {
	app.frames_counter++
	if !app.done {
		// outer left/right halves span the whole screen
		outer_left := app.mouse_x < f32(screen_width / 2)
		if click {
			if in_rect(app.mouse_x, app.mouse_y, app.l1_il_x, app.l1_il_y, app.l1_il_w, app.l1_il_h) {
				if app.l1_ir_w > 0 {
					app.l1_ir_x += 20
					app.l1_ir_y += 20
					app.l1_ir_w -= 40
					app.l1_ir_h -= 40
				}
			} else if in_rect(app.mouse_x, app.mouse_y, app.l1_ir_x, app.l1_ir_y, app.l1_ir_w, app.l1_ir_h) {
				if app.l1_il_w > 0 {
					app.l1_il_x += 20
					app.l1_il_y += 20
					app.l1_il_w -= 40
					app.l1_il_h -= 40
				}
			} else if outer_left {
				app.l1_il_x -= 20
				app.l1_il_y -= 20
				app.l1_il_w += 40
				app.l1_il_h += 40
			} else {
				app.l1_ir_x -= 20
				app.l1_ir_y -= 20
				app.l1_ir_w += 40
				app.l1_ir_h += 40
			}
		}
		if (app.l1_ir_w == 0 && app.l1_il_h >= f32(screen_height))
			|| (app.l1_il_w == 0 && app.l1_ir_h >= f32(screen_height)) {
			app.done = true
		}
	}
	common_finish(mut app, click, 90)
}

fn update_level02(mut app &App, click bool) {
	app.frames_counter++
	if !app.done {
		app.l2_ball_x += app.l2_ball_sx
		app.l2_ball_y += app.l2_ball_sy
		if app.l2_ball_x - 40 <= 0 || app.l2_ball_x + 40 >= f32(screen_width) {
			app.l2_ball_sx *= -1
		}
		if app.l2_ball_y - 40 <= 0 || app.l2_ball_y + 40 >= f32(screen_height) {
			app.l2_ball_sy *= -1
		}
		if in_circle(app.mouse_x, app.mouse_y, app.l2_ball_x, app.l2_ball_y, 120) {
			app.l2_ball_x = f32(rand_range(80, 1200))
			app.l2_ball_y = f32(rand_range(80, 650))
		}
		if in_circle(app.mouse_x, app.mouse_y, app.l2_hole_x, app.l2_hole_y, 120) && app.mouse_held {
			app.l2_hole_x = app.mouse_x
			app.l2_hole_y = app.mouse_y
			if app.l2_hole_x - 50 <= 0 {
				app.l2_hole_x = 50
			} else if app.l2_hole_x + 50 >= f32(screen_width) {
				app.l2_hole_x = f32(screen_width - 50)
			}
			if app.l2_hole_y - 50 <= 0 {
				app.l2_hole_y = 50
			} else if app.l2_hole_y + 50 >= f32(screen_height) {
				app.l2_hole_y = f32(screen_height - 50)
			}
		}
		if distance(app.l2_ball_x, app.l2_ball_y, app.l2_hole_x, app.l2_hole_y) < 20 {
			app.done = true
		}
	}
	common_finish(mut app, click, 90)
}

fn update_level03(mut app &App, click bool) {
	app.frames_counter++
	if !app.done {
		over_hole := in_rect(app.mouse_x, app.mouse_y,
			f32(screen_width / 2 - 50), f32(screen_height / 2 - 50), 100, 100)
		app.l3_show_piece = over_hole
		if app.mouse_held {
			if in_rect(app.mouse_x, app.mouse_y, app.l3_piece_x, app.l3_piece_y, 100, 100) {
				app.l3_piece_held = true
				app.l3_piece_x = f32(int(app.mouse_x) - 50)
				app.l3_piece_y = f32(int(app.mouse_y) - 50)
			}
		}
		if app.l3_piece_x == f32(screen_width / 2 - 50) && !over_hole {
			app.done = true
		}
	}
	common_finish(mut app, click, 90)
}

fn update_level04(mut app &App, click bool) {
	app.frames_counter++
	if !app.done {
		inside_inner := in_circle(app.mouse_x, app.mouse_y,
			f32(screen_width / 2), f32(screen_height / 2), app.l4_inner_r)
		inside_outer := in_circle(app.mouse_x, app.mouse_y,
			f32(screen_width / 2), f32(screen_height / 2), app.l4_outer_r)
		if app.mouse_held {
			if inside_inner {
				app.l4_inner_r += 2
			} else if inside_outer {
				app.l4_outer_r += 2
			} else {
				app.l4_outer_r -= 2
				if app.l4_outer_r <= 260 {
					app.l4_outer_r = 260
				}
			}
		} else {
			app.l4_inner_r -= 2
			if app.l4_outer_r > 300 {
				app.l4_outer_r -= 2
			}
		}
		if app.l4_inner_r >= 270 {
			app.l4_inner_r = 270
		} else if app.l4_inner_r <= 40 {
			app.l4_inner_r = 40
		}
		if app.l4_outer_r >= 600 {
			app.l4_outer_r = 600
		}
		if app.l4_inner_r >= app.l4_outer_r {
			app.done = true
		}
	}
	common_finish(mut app, click, 90)
}

fn update_level05(mut app &App, click bool) {
	app.frames_counter++
	if !app.done {
		if click {
			for k in 0 .. 10 {
				// iterate from the smallest ring outwards, like the original
				i := 9 - k
				if in_circle(app.mouse_x, app.mouse_y, f32(screen_width / 2),
					f32(screen_height / 2), app.l5_radius[i]) {
					if i == 0 {
						app.l5_white[8] = !app.l5_white[8]
					} else if i == 2 {
						app.l5_white[5] = !app.l5_white[5]
					} else if i == 3 {
						app.l5_white[6] = !app.l5_white[6]
					} else {
						app.l5_white[i] = !app.l5_white[i]
					}
					break
				}
			}
		}
		// Original win check sets done=true only if NO ring is white (all gray).
		mut all_gray := true
		for j in 0 .. 10 {
			if app.l5_white[j] {
				all_gray = false
				break
			}
		}
		if all_gray {
			app.done = true
		}
	}
	common_finish(mut app, click, 90)
}

fn update_level06(mut app &App, click bool) {
	app.frames_counter++
	if !app.done {
		app.l6_hover = -1
		for i in 0 .. 4 {
			if !app.l6_stopped[i] {
				app.l6_rec_x[i] += f32(app.l6_speed[i])
			}
			if app.l6_rec_x[i] >= f32(screen_width) {
				app.l6_rec_x[i] = -100
			}
			y := f32(i * 150 + 90)
			if in_rect(app.mouse_x, app.mouse_y, app.l6_rec_x[i], y, 100, 100) {
				app.l6_hover = i
				if click {
					match i {
						0 { app.l6_stopped[3] = !app.l6_stopped[3] }
						1 { app.l6_stopped[2] = !app.l6_stopped[2] }
						2 { app.l6_stopped[0] = !app.l6_stopped[0] }
						3 { app.l6_stopped[1] = !app.l6_stopped[1] }
						else {}
					}
				}
			}
		}
		mut aligned := true
		for i in 0 .. 4 {
			if !(app.l6_rec_x[i] > 540 && app.l6_rec_x[i] + 100 < 740) {
				aligned = false
				break
			}
		}
		if aligned {
			app.done = true
		}
	}
	common_finish(mut app, click, 90)
}

fn update_level07(mut app &App, click bool) {
	app.frames_counter++
	if !app.done {
		cx := [f32(screen_width / 2 - 340), f32(screen_width / 2), f32(screen_width / 2 + 340)]
		by := f32(screen_height / 2 + 120)
		cy := f32(screen_height / 2 - 100)
		if click {
			if in_circle(app.mouse_x, app.mouse_y, cx[0], by, 80) {
				app.l7_active[0] = !app.l7_active[0]
			} else if in_circle(app.mouse_x, app.mouse_y, cx[1], by, 80) {
				app.l7_active[1] = !app.l7_active[1]
			} else if in_circle(app.mouse_x, app.mouse_y, cx[2], by, 80) {
				app.l7_active[2] = !app.l7_active[2]
			}
			if app.l7_active[2] && in_circle(app.mouse_x, app.mouse_y, cx[0], cy, 100) {
				app.l7_white[0] = !app.l7_white[0]
			}
			if app.l7_active[1] && in_circle(app.mouse_x, app.mouse_y, cx[1], cy, 100) {
				app.l7_white[1] = !app.l7_white[1]
			}
			if app.l7_active[2] && app.l7_active[0]
				&& in_circle(app.mouse_x, app.mouse_y, cx[2], cy, 100) {
				app.l7_white[2] = !app.l7_white[2]
			}
		}
		if app.l7_white[0] && app.l7_white[1] && app.l7_white[2]
			&& !app.l7_active[0] && !app.l7_active[1] && !app.l7_active[2] {
			app.done = true
		}
	}
	common_finish(mut app, click, 90)
}

fn update_level08(mut app &App, click bool) {
	app.frames_counter++
	if !app.done {
		app.l8_box_x += f32(app.l8_speed)
		if app.l8_box_x <= 0 {
			app.l8_speed *= -1
		}
		cols := [f32(240), f32(screen_width / 2 - 50), f32(920)]
		by := f32(screen_height / 2 - 20)
		for i in 0 .. 3 {
			if app.l8_columns[i] && rects_overlap(app.l8_box_x, by, 40, 40,
				cols[i], 0, 100, f32(screen_height)) {
				app.l8_speed *= -1
			}
		}
		if click {
			if in_rect(app.mouse_x, app.mouse_y, cols[0], 0, 100, f32(screen_height)) {
				app.l8_columns[1] = false
				app.l8_columns[2] = true
			} else if in_rect(app.mouse_x, app.mouse_y, cols[1], 0, 100, f32(screen_height)) {
				app.l8_columns[2] = false
				app.l8_columns[0] = true
			} else if in_rect(app.mouse_x, app.mouse_y, cols[2], 0, 100, f32(screen_height)) {
				app.l8_columns[0] = false
				app.l8_columns[1] = true
			}
		}
		if app.l8_box_x >= 1100 {
			app.done = true
		}
	}
	common_finish(mut app, click, 90)
}

fn update_level09(mut app &App, click bool) {
	app.frames_counter++
	if app.l9_enabled {
		app.l9_counter++
	}
	if !app.done {
		if click {
			for i in 0 .. 21 {
				x := f32(screen_width / 7 * (i % 7))
				y := f32(screen_height / 3 * (i / 7))
				if !in_rect(app.mouse_x, app.mouse_y, x, y,
					f32(screen_width / 7), f32(screen_height / 3)) {
					continue
				}
				if i == 10 {
					if app.l9_col[10] == 2 {
						app.l9_col[10] = 1
						app.l9_enabled = true
						app.l9_counter = 0
						app.l9_activated[1] = true
					} else {
						app.l9_col[10] = 2
						app.l9_enabled = false
						app.l9_counter = 300
						for k in 0 .. 21 {
							app.l9_activated[k] = false
						}
					}
				} else if i % 2 == 1 && app.l9_enabled {
					if app.l9_activated[i] {
						app.l9_col[i] = 1
						if i != 19 {
							app.l9_activated[i + 2] = true
						}
					}
				}
			}
		}
		if app.l9_counter > 250 {
			for i in 0 .. 21 {
				if i % 2 == 0 {
					app.l9_col[i] = 1
				} else {
					app.l9_col[i] = 0
				}
				app.l9_activated[i] = false
			}
			app.l9_col[10] = 2
			app.l9_enabled = false
			app.l9_counter = 0
		}
		mut all_light := true
		for i in 0 .. 21 {
			if app.l9_col[i] != 1 {
				all_light = false
				break
			}
		}
		if all_light {
			app.done = true
		}
	}
	common_finish(mut app, click, 90)
}

// ---------------------------------------------------------------------------
// Drawing
// ---------------------------------------------------------------------------

fn draw_logo(ctx &gg.Context, app &App) {
	lx := f32(screen_width / 2 - 128)
	ly := f32(screen_height / 2 - 128)
	match app.logo_state {
		0 {
			if (app.frames_counter / 10) % 2 == 1 {
				ctx.draw_rect_filled(lx, ly, 16, 16, black)
			}
		}
		1 {
			ctx.draw_rect_filled(lx, ly, f32(app.logo_top_w), 16, black)
			ctx.draw_rect_filled(lx, ly, 16, f32(app.logo_left_h), black)
		}
		2 {
			ctx.draw_rect_filled(lx, ly, f32(app.logo_top_w), 16, black)
			ctx.draw_rect_filled(lx, ly, 16, f32(app.logo_left_h), black)
			ctx.draw_rect_filled(lx + 240, ly, 16, f32(app.logo_right_h), black)
			ctx.draw_rect_filled(lx, ly + 240, f32(app.logo_bottom_w), 16, black)
		}
		3 {
			a := app.logo_alpha
			ctx.draw_rect_filled(lx, ly, f32(app.logo_top_w), 16, fade(black, ray_white, a))
			ctx.draw_rect_filled(lx, ly + 16, 16, f32(app.logo_left_h - 32), fade(black, ray_white, a))
			ctx.draw_rect_filled(lx + 240, ly + 16, 16, f32(app.logo_right_h - 32), fade(black, ray_white, a))
			ctx.draw_rect_filled(lx, ly + 240, f32(app.logo_bottom_w), 16, fade(black, ray_white, a))
			ctx.draw_rect_filled(f32(screen_width / 2 - 112), f32(screen_height / 2 - 112),
				224, 224, fade(ray_white, ray_white, a))
			typed := 'raylib'[0..app.logo_letters]
			ctx.draw_text(screen_width / 2 - 44, screen_height / 2 + 48, typed,
				size:  50
				color: fade(black, ray_white, a)
			)
			if app.frames_counter > 20 {
				ctx.draw_text(int(lx), int(ly) - 27, 'powered by',
					size:  20
					color: fade(dark_gray, ray_white, a)
				)
			}
		}
		else {}
	}
}

fn draw_level00(ctx &gg.Context, app &App) {
	ctx.draw_text(int(app.l0_u_x), int(app.l0_u_y) + 10, 'U', size: 160, color: gray)
	jw := ctx.text_width_cfg('JUST DO', size: 160)
	base := screen_width / 2 - jw / 2
	ctx.draw_text(base, screen_height / 2 - 80, 'J', size: 160, color: gray)
	ctx.draw_text(base + 210, screen_height / 2 - 80, 'ST D', size: 160, color: gray)
	ctx.draw_text(int(app.l0_o_x), int(app.l0_o_y) + 10, 'O', size: 160, color: gray)
	ctx.draw_text(370, screen_height / 2 + 100, 'by RAMON SANTAMARIA (@raysan5)',
		size:  30
		color: fade(light_gray, ray_white, 0.4)
	)
	if app.l0_over_u && !app.l0_placed_u {
		ctx.draw_rect_empty(app.l0_u_x - 20, app.l0_u_y, app.l0_u_w, 160,
			fade(light_gray, ray_white, 0.8))
	}
	if app.l0_over_o && !app.l0_placed_o {
		ctx.draw_rect_empty(app.l0_o_x - 20, app.l0_o_y, app.l0_o_w, 160,
			fade(light_gray, ray_white, 0.8))
	}
	if app.level_finished {
		draw_finish(ctx, 0, app.level_time_sec, fade(light_gray, ray_white, 0.6))
	} else {
		draw_centered(ctx, level_titles[0], 20, 30, light_gray)
	}
}

fn draw_level01(ctx &gg.Context, app &App) {
	if app.level_finished {
		ctx.draw_rect_filled(60, 60, f32(screen_width - 120), f32(screen_height - 120), light_gray)
	} else {
		ctx.draw_rect_filled(0, 0, f32(screen_width), f32(screen_height), light_gray)
	}
	ctx.draw_rect_filled(0, 0, f32(screen_width / 2), f32(screen_height), gray)
	ctx.draw_rect_filled(app.l1_il_x, app.l1_il_y, app.l1_il_w, app.l1_il_h, ray_white)
	ctx.draw_rect_filled(f32(screen_width / 2), 0, f32(screen_width / 2), f32(screen_height), ray_white)
	ctx.draw_rect_filled(app.l1_ir_x, app.l1_ir_y, app.l1_ir_w, app.l1_ir_h, gray)
	if app.level_finished {
		draw_finish(ctx, 1, app.level_time_sec, fade(light_gray, ray_white, 0.6))
	} else {
		draw_centered(ctx, level_titles[1], 20, 30, light_gray)
	}
}

fn draw_level02(ctx &gg.Context, app &App) {
	ctx.draw_circle_filled(app.l2_hole_x, app.l2_hole_y, 50, light_gray)
	ctx.draw_circle_filled(app.l2_ball_x, app.l2_ball_y, 40, dark_gray)
	ctx.draw_circle_empty(app.l2_ball_x, app.l2_ball_y, 120, fade(light_gray, ray_white, 0.8))
	if app.level_finished {
		draw_finish(ctx, 2, app.level_time_sec, fade(light_gray, ray_white, 0.6))
	} else {
		draw_centered(ctx, level_titles[2], 20, 30, light_gray)
	}
}

fn draw_level03(ctx &gg.Context, app &App) {
	ctx.draw_rect_filled(f32(screen_width / 2 - 50), f32(screen_height / 2 - 50),
		100, 100, gray)
	ctx.draw_rect_filled(app.l3_piece_x, app.l3_piece_y, 100, 100, ray_white)
	if app.l3_show_piece {
		ctx.draw_rect_empty(app.l3_piece_x, app.l3_piece_y, 100, 100,
			fade(light_gray, ray_white, 0.8))
	}
	if app.level_finished {
		draw_finish(ctx, 3, app.level_time_sec, fade(light_gray, ray_white, 0.6))
	} else {
		draw_centered(ctx, level_titles[3], 20, 30, light_gray)
	}
}

fn draw_level04(ctx &gg.Context, app &App) {
	ctx.draw_circle_filled(f32(screen_width / 2), f32(screen_height / 2), app.l4_outer_r, gray)
	ctx.draw_circle_filled(f32(screen_width / 2), f32(screen_height / 2), app.l4_inner_r, ray_white)
	if app.level_finished {
		draw_finish(ctx, 4, app.level_time_sec, fade(light_gray, ray_white, 0.6))
	} else {
		draw_centered(ctx, level_titles[4], 20, 30, light_gray)
	}
}

fn draw_level05(ctx &gg.Context, app &App) {
	for i in 0 .. 10 {
		c := if app.l5_white[i] { ray_white } else { gray }
		ctx.draw_polygon_filled(f32(screen_width / 2), f32(screen_height / 2),
			app.l5_radius[i], 64, 0, c)
	}
	if app.level_finished {
		draw_finish(ctx, 5, app.level_time_sec, fade(light_gray, ray_white, 0.6))
	} else {
		draw_centered(ctx, level_titles[5], 20, 30, light_gray)
	}
}

fn draw_level06(ctx &gg.Context, app &App) {
	ctx.draw_rect_filled(f32(screen_width / 2 - 100), 0, 200, f32(screen_height), light_gray)
	for i in 0 .. 4 {
		ctx.draw_rect_filled(app.l6_rec_x[i], f32(i * 150 + 90), 100, 100, gray)
	}
	if !app.done && app.l6_hover >= 0 {
		n := app.l6_hover
		ctx.draw_rect_empty(app.l6_rec_x[n] - 5, f32(n * 150 + 90) - 5, 110, 110,
			fade(light_gray, ray_white, 0.8))
	}
	if app.level_finished {
		draw_finish(ctx, 6, app.level_time_sec, fade(light_gray, ray_white, 0.6))
	} else {
		draw_centered(ctx, level_titles[6], 20, 30, light_gray)
	}
}

fn draw_level07(ctx &gg.Context, app &App) {
	cx := [f32(screen_width / 2 - 340), f32(screen_width / 2), f32(screen_width / 2 + 340)]
	cy := f32(screen_height / 2 - 100)
	for i in 0 .. 3 {
		cc := if app.l7_white[i] { light_gray } else { gray }
		ctx.draw_circle_filled(cx[i], cy, 100, cc)
	}
	by := f32(screen_height / 2 + 120)
	for i in 0 .. 3 {
		bc := if app.l7_active[i] { gray } else { light_gray }
		ctx.draw_circle_filled(cx[i], by, 80, bc)
	}
	if app.level_finished {
		draw_finish(ctx, 7, app.level_time_sec, fade(light_gray, ray_white, 0.6))
	} else {
		draw_centered(ctx, level_titles[7], 20, 30, light_gray)
	}
}

fn draw_level08(ctx &gg.Context, app &App) {
	ctx.draw_rect_filled(1100, f32(screen_height / 2 - 20), 40, 40, gray)
	ctx.draw_rect_filled(app.l8_box_x, f32(screen_height / 2 - 20), 40, 40, light_gray)
	cols := [f32(240), f32(screen_width / 2 - 50), f32(920)]
	for i in 0 .. 3 {
		if app.l8_columns[i] {
			ctx.draw_rect_filled(cols[i], 0, 100, f32(screen_height), gray)
		}
	}
	if app.level_finished {
		draw_finish(ctx, 8, app.level_time_sec, fade(light_gray, ray_white, 0.6))
	} else {
		draw_centered(ctx, level_titles[8], 20, 30, light_gray)
	}
}

fn draw_level09(ctx &gg.Context, app &App) {
	w := f32(screen_width / 7)
	h := f32(screen_height / 3)
	for i in 0 .. 21 {
		x := f32(screen_width / 7 * (i % 7))
		y := f32(screen_height / 3 * (i / 7))
		c := match app.l9_col[i] {
			0 { gray }
			1 { light_gray }
			else { ray_white }
		}
		ctx.draw_rect_filled(x, y, w, h, c)
	}
	if app.level_finished {
		draw_finish(ctx, 9, app.level_time_sec, fade(ray_white, ray_white, 0.6))
	} else {
		draw_centered(ctx, level_titles[9], 20, 30, light_gray)
	}
}

// ---------------------------------------------------------------------------
// Transition / screen management
// ---------------------------------------------------------------------------

fn next_screen(s GameScreen) GameScreen {
	return match s {
		.logo { .level00 }
		.level00 { .level01 }
		.level01 { .level02 }
		.level02 { .level03 }
		.level03 { .level04 }
		.level04 { .level05 }
		.level05 { .level06 }
		.level06 { .level07 }
		.level07 { .level08 }
		.level08 { .level09 }
		.level09 { .level00 }
	}
}

fn transition_to(mut app &App, to GameScreen) {
	app.on_transition = true
	app.trans_fade_out = false
	app.trans_alpha = 0
	app.trans_to = to
	app.click = false
}

fn update_transition(mut app &App) {
	if !app.trans_fade_out {
		app.trans_alpha += 0.02
		if app.trans_alpha >= 1.0 {
			app.trans_alpha = 1.0
			app.screen = app.trans_to
			init_screen(mut app) // the screen is swapped while fully faded to white
			app.trans_fade_out = true
			app.frames_counter = 0
		}
	} else {
		app.trans_alpha -= 0.02
		if app.trans_alpha <= 0 {
			app.trans_alpha = 0
			app.trans_fade_out = false
			app.on_transition = false
		}
	}
}

// ---------------------------------------------------------------------------
// gg callbacks
// ---------------------------------------------------------------------------

fn on_frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	match app.screen {
		.logo { draw_logo(ctx, app) }
		.level00 { draw_level00(ctx, app) }
		.level01 { draw_level01(ctx, app) }
		.level02 { draw_level02(ctx, app) }
		.level03 { draw_level03(ctx, app) }
		.level04 { draw_level04(ctx, app) }
		.level05 { draw_level05(ctx, app) }
		.level06 { draw_level06(ctx, app) }
		.level07 { draw_level07(ctx, app) }
		.level08 { draw_level08(ctx, app) }
		.level09 { draw_level09(ctx, app) }
	}
	if app.on_transition {
		ctx.draw_rect_filled(0, 0, f32(screen_width), f32(screen_height),
			overlay_color(ray_white, app.trans_alpha))
	}
	ctx.end()
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.on_transition {
		update_transition(mut app)
		app.click = false
		return
	}
	click := app.click
	app.click = false
	match app.screen {
		.logo { update_logo(mut app) }
		.level00 { update_level00(mut app, click) }
		.level01 { update_level01(mut app, click) }
		.level02 { update_level02(mut app, click) }
		.level03 { update_level03(mut app, click) }
		.level04 { update_level04(mut app, click) }
		.level05 { update_level05(mut app, click) }
		.level06 { update_level06(mut app, click) }
		.level07 { update_level07(mut app, click) }
		.level08 { update_level08(mut app, click) }
		.level09 { update_level09(mut app, click) }
	}
	if app.screen == .logo {
		if app.done {
			transition_to(mut app, next_screen(app.screen))
		}
	} else if app.finish_requested {
		app.finish_requested = false
		transition_to(mut app, next_screen(app.screen))
	}
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .mouse_move || e.typ == .mouse_down || e.typ == .mouse_up {
		app.mouse_x = e.mouse_x
		app.mouse_y = e.mouse_y
	}
	if e.typ == .mouse_down {
		app.mouse_held = true
		app.click = true
	}
	if e.typ == .mouse_up {
		app.mouse_held = false
	}
	if e.typ == .key_down && !e.key_repeat && !app.on_transition {
		k := int(e.key_code)
		// raylib uses character keys '0'..'9' (ASCII 48..57); gg KeyCode matches.
		// PORT NOTE: the original jumps to LEVEL08 on '9' by typo; we init the
		// selected level correctly.
		if k >= 48 && k <= 57 {
			jump_levels := [
				GameScreen.level00,
				GameScreen.level01,
				GameScreen.level02,
				GameScreen.level03,
				GameScreen.level04,
				GameScreen.level05,
				GameScreen.level06,
				GameScreen.level07,
				GameScreen.level08,
				GameScreen.level09,
			]
			transition_to(mut app, jump_levels[k - 48])
		}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'JUST DO [GGJ15]'
		bg_color:     ray_white
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
	)
	init_screen(mut app)
	app.ctx.run()
}
