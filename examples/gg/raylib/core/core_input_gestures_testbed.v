module main

// raylib [core] example - input gestures testbed
// A visual testbed for gestures: last gesture indicators, a scrolling log
// and an angle protractor for swipe/twist gestures.
// PORT NOTE: gg.GestureTracker reports drag distance (pixels) and twist
// radians rather than raylib's per-gesture angle values.

import gg
import math

const screen_width = 800
const screen_height = 450
const log_size = 20

const ray_white = gg.Color{245, 245, 245, 255}
const black = gg.Color{0, 0, 0, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const gray = gg.Color{130, 130, 130, 255}
const red = gg.Color{255, 0, 0, 255}
const blue = gg.Color{0, 0, 255, 255}
const lime = gg.Color{124, 252, 0, 255}
const violet = gg.Color{238, 130, 238, 255}
const orange = gg.Color{255, 161, 0, 255}
const sky_blue = gg.Color{102, 191, 255, 255}

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	tracker   gg.GestureTracker
	last      gg.Gesture = .none
	log       []string
	log_index int
	angle     f32
	position  gg.Vec2
	active    bool
}

fn gesture_name(k gg.Gesture) string {
	return match k {
		.none { 'None' }
		.tap { 'Tap' }
		.double_tap { 'Double Tap' }
		.hold { 'Hold' }
		.drag { 'Drag' }
		.swipe_right { 'Swipe Right' }
		.swipe_left { 'Swipe Left' }
		.swipe_up { 'Swipe Up' }
		.swipe_down { 'Swipe Down' }
		.pinch { 'Pinch' }
		.twist { 'Twist' }
	}
}

fn color_for(k gg.Gesture) gg.Color {
	return match k {
		.tap { blue }
		.double_tap { sky_blue }
		.drag { lime }
		.twist { violet }
		.pinch { orange }
		.swipe_right, .swipe_left, .swipe_up, .swipe_down { red }
		else { black }
	}
}

fn main() {
	mut app := &App{}
	for _ in 0 .. log_size {
		app.log << ''
	}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - input gestures testbed'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if gesture := app.tracker.update(e) {
		app.last = gesture.kind
		app.position = gesture.position
		app.active = true
		app.angle = match gesture.kind {
			.twist { gesture.delta * 180 / f32(math.pi) }
			.drag { gesture.delta }
			else { 0 }
		}
		app.log[app.log_index] = gesture_name(gesture.kind)
		app.log_index = (app.log_index + log_size - 1) % log_size
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_text(160, 12,
		'Example optimized for touch screens.\nUse mouse or touch to generate gestures.',
		size:  10
		color: black
	)

	ctx.draw_text(198, 83, 'Last gesture', size: 20, color: black)
	ctx.draw_text(182, 112, 'Swipe   Tap    Pinch Touch', size: 10, color: black)
	lp := gg.Vec2{165, 130}
	ctx.draw_rect(
		x:     lp.x + 20
		y:     lp.y
		w:     20
		h:     20
		color: if app.last == .swipe_up { red } else { light_gray }
	)
	ctx.draw_rect(
		x:     lp.x
		y:     lp.y + 20
		w:     20
		h:     20
		color: if app.last == .swipe_left { red } else { light_gray }
	)
	ctx.draw_rect(
		x:     lp.x + 40
		y:     lp.y + 20
		w:     20
		h:     20
		color: if app.last == .swipe_right {
			red
		} else {
			light_gray
		}
	)
	ctx.draw_rect(
		x:     lp.x + 20
		y:     lp.y + 40
		w:     20
		h:     20
		color: if app.last == .swipe_down {
			red
		} else {
			light_gray
		}
	)
	ctx.draw_circle_filled(lp.x + 80, lp.y + 16, 10, if app.last == .tap {
		blue
	} else {
		light_gray
	})
	ctx.draw_circle_filled(lp.x + 80, lp.y + 43, 10,
		if app.last == .double_tap { sky_blue } else { light_gray })
	ctx.draw_triangle_filled(lp.x + 122, lp.y + 16, lp.x + 137, lp.y + 26, lp.x + 137,
		lp.y + 6, if app.last == .pinch { orange } else { light_gray })

	ctx.draw_text(10, 10, 'Log', size: 20, color: black)
	for i in 0 .. log_size {
		idx := (app.log_index + i) % log_size
		ctx.draw_text(10, 420 - i * 20, app.log[idx],
			size:  20
			color: if i == 0 { color_for(app.last) } else { light_gray }
		)
	}

	pp := gg.Vec2{266, 315}
	ctx.draw_text(int(pp.x + 55), int(pp.y + 76), 'Angle', size: 10, color: black)
	ctx.draw_text(int(pp.x + 55), int(pp.y + 92), '${app.angle:.2f}',
		size:  20
		color: color_for(app.last)
	)
	ctx.draw_circle_filled(pp.x, pp.y, 80, ray_white)
	ctx.draw_line(pp.x - 90, pp.y, pp.x + 90, pp.y, light_gray)
	ctx.draw_line(pp.x, pp.y - 90, pp.x, pp.y + 90, light_gray)
	ctx.draw_line(pp.x - 80, pp.y - 45, pp.x + 80, pp.y + 45, green)
	ctx.draw_line(pp.x - 80, pp.y + 45, pp.x + 80, pp.y - 45, green)
	rad := (app.angle + 90) * f32(math.pi) / 180
	end := gg.Vec2{pp.x + 90 * math.sinf(rad), pp.y + 90 * math.cosf(rad)}
	if app.angle != 0 {
		ctx.draw_line(pp.x, pp.y, end.x, end.y, color_for(app.last))
	}
	if app.active {
		ctx.draw_circle_filled(app.position.x, app.position.y, 35,
			gg.Color{color_for(app.last).r, color_for(app.last).g, color_for(app.last).b, 128})
		ctx.draw_circle_filled(app.position.x, app.position.y, 5, color_for(app.last))
	}
	ctx.end()
}

const green = gg.Color{0, 158, 47, 255}
