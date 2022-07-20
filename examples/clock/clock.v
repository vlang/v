// An X11 clock modeled after https://en.wikipedia.org/wiki/Station_clock
// This is a small V example that was based off of the fireworks example.
// Written by Stefan Schroeder in 2021 for the v project examples.
// See LICENSE for license information.
import os
import gg
import gx
import math
import time

const (
	// All coordinates are designed for a clock size of this many pixel.
	// You cannot change the size of the clock by adjusting this value.
	design_size = 700
	center      = 350

	// Half the width of a tic-mark.
	tw          = 9
	// Height of a minute tic-mark. (hour is twice, 3-hour is thrice)
	th          = 25
	// Padding of tic-mark to window border
	tp          = 10

	tic_color   = gx.Color{
		r: 50
		g: 50
		b: 50
	}
	hand_color        = gx.black
	second_hand_color = gx.red
)

struct App {
	minutes_tic []f32 = [f32(center - tw), tp, center + tw, tp, center + tw, tp, center + tw,
	tp + 1 * th, center - tw, tp + 1 * th]
	hours_tic []f32 = [f32(center - tw), tp, center + tw, tp, center + tw, tp, center + tw, tp + 2 * th,
	center - tw, tp + 2 * th]
	hours3_tic []f32 = [f32(center - tw), tp, center + tw, tp, center + tw, tp, center + tw, tp + 3 * th,
	center - tw, tp + 3 * th]

	hour_hand   []f32 = [f32(329), 161, 350, 140, 371, 161, 371, 413, 329, 413]
	minute_hand []f32 = [f32(334.25), 40.25, 350, 24.5, 365.75, 40.25, 365.75, 427, 334.25, 427]
	second_hand []f32 = [f32(345.8), 38.5, 350, 34.3, 354.2000, 38.5, 358.75, 427, 341.25, 427]
mut:
	gg        &gg.Context = unsafe { 0 }
	draw_flag bool        = true
	dpi_scale f32 = 1.0
}

fn on_frame(mut app App) {
	if !app.draw_flag {
		return
	}
	app.gg.begin()

	for i in 0 .. 60 { // draw minute tics
		draw_convex_poly_rotate(mut app.gg, app.dpi_scale, app.minutes_tic, tic_color,
			i * 6)
	}
	for i in 0 .. 12 { // hours
		draw_convex_poly_rotate(mut app.gg, app.dpi_scale, app.hours_tic, tic_color, i * 30)
	}
	for i in 0 .. 4 { // 3 hours
		draw_convex_poly_rotate(mut app.gg, app.dpi_scale, app.hours3_tic, tic_color,
			i * 90)
	}

	n := time.now()

	// draw hour hand
	i := f32(n.hour) + f32(n.minute) / 60.0
	draw_convex_poly_rotate(mut app.gg, app.dpi_scale, app.hour_hand, hand_color, i * 30)

	// draw minute hand
	mut j := f32(n.minute)
	if n.second == 59 { // make minute hand move smoothly
		j += f32(math.sin(f32(n.microsecond) / 1e6 * math.pi / 2.0))
	}
	draw_convex_poly_rotate(mut app.gg, app.dpi_scale, app.minute_hand, hand_color, j * 6)

	// draw second hand with smooth transition
	k := f32(n.second) + f32(math.sin(f32(n.microsecond) / 1e6 * math.pi / 2.0))
	draw_convex_poly_rotate(mut app.gg, app.dpi_scale, app.second_hand, second_hand_color,
		0 + k * 6)

	app.gg.end()
}

// Rotate a polygon round the centerpoint
[manualfree]
fn draw_convex_poly_rotate(mut ctx gg.Context, dpi_scale f32, points []f32, c gx.Color, angle f32) {
	sa := math.sin(math.pi * angle / 180.0)
	ca := math.cos(math.pi * angle / 180.0)

	mut rotated_points := []f32{cap: points.len}
	for i := 0; i < points.len / 2; i++ {
		x := points[2 * i]
		y := points[2 * i + 1]
		xn := f32((x - center) * ca - (y - center) * sa)
		yn := f32((x - center) * sa + (y - center) * ca)
		rotated_points << (xn + center) * dpi_scale
		rotated_points << (yn + center) * dpi_scale
	}
	ctx.draw_convex_poly(rotated_points, c)
	unsafe { rotated_points.free() }
}

fn (mut app App) resize() {
	size := gg.window_size()
	// avoid calls when minimized
	if size.width < 2 && size.height < 2 {
		return
	}
	w := f32(size.width) / design_size
	h := f32(size.height) / design_size
	app.dpi_scale = if w < h { w } else { h }
}

fn on_event(e &gg.Event, mut app App) {
	match e.typ {
		.resized, .resumed {
			app.resize()
		}
		.iconified {
			app.draw_flag = false
		}
		.restored {
			app.draw_flag = true
			app.resize()
		}
		else {
			if e.typ == .key_down {
				match e.key_code {
					.q {
						println('Good bye.')
						// do we need to free anything here?
						app.gg.quit()
					}
					else {}
				}
			}
		}
	}
}

fn on_init(mut app App) {
	app.resize()
}

// is needed for easier diagnostics on windows
[console]
fn main() {
	println("Press 'q' to quit.")
	mut font_path := os.resource_abs_path(os.join_path('..', 'assets', 'fonts', 'RobotoMono-Regular.ttf'))
	$if android {
		font_path = 'fonts/RobotoMono-Regular.ttf'
	}

	mut app := &App{}

	app.gg = gg.new_context(
		width: design_size
		height: design_size
		window_title: 'Clock!'
		bg_color: gx.white
		user_data: app
		frame_fn: on_frame
		event_fn: on_event
		init_fn: on_init
		font_path: font_path
	)

	app.gg.run()
}
