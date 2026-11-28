module main

// raylib [core] example - input gestures
// Uses gg.GestureTracker to recognise taps, drags, swipes, holds, pinches
// and twists from the mouse/touch event stream.

import gg

const screen_width = 800
const screen_height = 450
const max_strings = 20

const ray_white = gg.Color{245, 245, 245, 255}
const gray = gg.Color{130, 130, 130, 255}
const light_gray = gg.Color{200, 200, 200, 128}
const dark_gray = gg.Color{80, 80, 80, 255}
const maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	gg       &gg.Context = unsafe { nil }
	tracker  gg.GestureTracker
	messages []string
	position gg.Vec2
	kind     gg.Gesture = .none
}

fn gesture_name(k gg.Gesture) string {
	return match k {
		.tap { 'GESTURE TAP' }
		.double_tap { 'GESTURE DOUBLETAP' }
		.hold { 'GESTURE HOLD' }
		.drag { 'GESTURE DRAG' }
		.swipe_right { 'GESTURE SWIPE RIGHT' }
		.swipe_left { 'GESTURE SWIPE LEFT' }
		.swipe_up { 'GESTURE SWIPE UP' }
		.swipe_down { 'GESTURE SWIPE DOWN' }
		.pinch { 'GESTURE PINCH' }
		.twist { 'GESTURE TWIST' }
		else { '' }
	}
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - input gestures'
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
		if gesture.position.x >= 220 {
			app.kind = gesture.kind
			app.position = gesture.position
			app.messages << gesture_name(gesture.kind)
			if app.messages.len >= max_strings {
				app.messages.clear()
			}
		}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_rect(x: 220, y: 10, w: screen_width - 230, h: screen_height - 20, color: gray)
	ctx.draw_rect(x: 225, y: 15, w: screen_width - 240, h: screen_height - 30, color: ray_white)
	ctx.draw_text(screen_width - 270, screen_height - 40, 'GESTURES TEST AREA',
		size:  20
		color: gg.Color{130, 130, 130, 128}
	)
	for i, msg in app.messages {
		ctx.draw_rect(x: 10, y: f32(30 + 20 * i), w: 200, h: 20, color: if i % 2 == 0 { light_gray } else { gg.Color{200, 200, 200, 77} })
		ctx.draw_text(35, 36 + 20 * i, msg,
			size:  10
			color: if i < app.messages.len - 1 { dark_gray } else { maroon }
		)
	}
	ctx.draw_rect_empty(10, 29, 200, screen_height - 50, gray)
	ctx.draw_text(50, 15, 'DETECTED GESTURES', size: 10, color: gray)
	if app.kind != .none {
		ctx.draw_circle_filled(app.position.x, app.position.y, 30, maroon)
	}
	ctx.end()
}
