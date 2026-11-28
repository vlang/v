module main

// raylib [core] example - smooth pixelperfect
// A tiny 160x90 world is scaled up to 640x360. Sub-pixel camera offsets
// can be applied to smooth the movement of the scaled image.
// PORT NOTE: raylib renders the world into a RenderTexture2D and uses two
// Camera2Ds (integer world, fractional screen). gg expresses the same
// result with a scaled viewport plus a fractional camera offset.

import gg
import math

const screen_width = 800
const screen_height = 450
const virtual_width = 160
const virtual_height = 90
const virtual_ratio = f32(screen_width) / f32(virtual_width)

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const dark_blue = gg.Color{0, 82, 172, 255}
const dark_green = gg.Color{0, 121, 64, 255}
const red = gg.Color{255, 0, 0, 255}
const blue = gg.Color{0, 0, 255, 255}

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	rotation  f32
	camera_x  f32
	camera_y  f32
	smooth_on bool = true
	overscan  bool
	time      f32
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - smooth pixelperfect'
		bg_color:     light_gray
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		user_data:    app
	)
	app.gg.fps.show = true
	app.gg.fps.x = screen_width - 95
	app.gg.fps.y = 10
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down || e.key_repeat {
		return
	}
	if e.key_code == .s {
		app.smooth_on = !app.smooth_on
	}
	if e.key_code == .o {
		app.overscan = !app.overscan
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.rotation += 60 * dt
	app.time += dt
}

fn draw_rotated_rect(ctx &gg.Context, x f32, y f32, w f32, h f32, angle f32, c gg.Color) {
	rad := angle * f32(math.pi) / 180
	co := math.cosf(rad)
	si := math.sinf(rad)
	cx, cy := x + w / 2, y + h / 2
	corners := [
		gg.Vec2{-w / 2, -h / 2},
		gg.Vec2{w / 2, -h / 2},
		gg.Vec2{w / 2, h / 2},
		gg.Vec2{-w / 2, h / 2},
	]
	mut pts := [gg.Vec2{}, gg.Vec2{}, gg.Vec2{}, gg.Vec2{}]!
	for i, p in corners {
		pts[i] = gg.Vec2{cx + p.x * co - p.y * si, cy + p.x * si + p.y * co}
	}
	ctx.draw_triangle_filled(pts[0].x, pts[0].y, pts[1].x, pts[1].y, pts[2].x, pts[2].y, c)
	ctx.draw_triangle_filled(pts[0].x, pts[0].y, pts[2].x, pts[2].y, pts[3].x, pts[3].y, c)
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg

	cam_x := math.sinf(app.time) * 50 - 10
	cam_y := math.cosf(app.time) * 30
	world_x := f32(int(cam_x))
	world_y := f32(int(cam_y))
	smooth_x := if app.smooth_on { (cam_x - world_x) * virtual_ratio } else { 0 }
	smooth_y := if app.smooth_on { (cam_y - world_y) * virtual_ratio } else { 0 }
	app.camera_x = world_x
	app.camera_y = world_y

	dest_x, dest_y, dest_w, dest_h := if app.overscan {
		-virtual_ratio, -virtual_ratio, f32(screen_width) + virtual_ratio * 2, f32(screen_height) + virtual_ratio * 2
	} else {
		f32(screen_width - screen_width / 5) * 0.5, f32(screen_height - screen_height / 5) * 0.5, f32(screen_width) / 1.25, f32(screen_height) / 1.25
	}
	sx := dest_w / f32(virtual_width)
	sy := dest_h / f32(virtual_height)

	ctx.begin()
	ctx.set_viewport(int(dest_x + smooth_x), int(dest_y + smooth_y), int(dest_w), int(dest_h))
	ctx.draw_rect(x: 0, y: 0, w: dest_w, h: dest_h, color: ray_white)
	draw_rotated_rect(ctx, (70 - world_x) * sx, (35 - world_y) * sy, 20 * sx, 20 * sy,
		app.rotation, gg.black)
	draw_rotated_rect(ctx, (90 - world_x) * sx, (55 - world_y) * sy, 30 * sx, 10 * sy,
		-app.rotation, red)
	draw_rotated_rect(ctx, (80 - world_x) * sx, (65 - world_y) * sy, 15 * sx, 25 * sy,
		app.rotation + 45, blue)
	ctx.set_viewport(0, 0, ctx.width, ctx.height)

	ctx.draw_text(10, 10, 'Screen resolution: ${screen_width}x${screen_height}',
		size:  20
		color: dark_blue
	)
	ctx.draw_text(10, 40, 'World resolution: ${virtual_width}x${virtual_height}',
		size:  20
		color: dark_green
	)
	ctx.draw_text(10, screen_height - 60, 'Smooth: ${if app.smooth_on { 'ON' } else { 'OFF' }}',
		size:  20
		color: red
	)
	ctx.draw_text(10, screen_height - 30, 'Overscan: ${if app.overscan { 'ON' } else { 'OFF' }}',
		size:  20
		color: red
	)
	ctx.end()
}
