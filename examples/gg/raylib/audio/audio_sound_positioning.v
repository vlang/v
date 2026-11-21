// Port of raylib [audio] example - sound positioning.
// PORT NOTE: gg has no SetSoundVolume/SetSoundPan. The attenuation and pan are computed and
// displayed for the 3D scene, but the WAV plays at a fixed volume (no spatialization).
module main

import gg
import math

const win_width = 800
const win_height = 450

struct App {
mut:
	gg    &gg.Context = unsafe { nil }
	cam   gg.Camera3D
	sound &gg.Sound = unsafe { nil }
	angle f32
	atten f32
	pan   f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	gg.setup_sound_audio(sample_rate: 44100, channels: 1)
	s := gg.load_wav('resources/coin.wav') or {
		eprintln('coin.wav: ${err}')
		return
	}
	app.sound = &s
	app.cam = gg.Camera3D{ position: gg.Vec3{0, 5, 5} }
	app.cam.look_at(gg.Vec3{}, gg.Vec3{0, 1, 0})
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.angle += dt
	app.cam.update(.free, app.gg, dt)

	sphere := gg.Vec3{5 * math.cosf(app.angle), 0, 5 * math.sinf(app.angle)}
	dir := sphere - app.cam.position
	dist := f32(math.sqrt(f64(dir.x * dir.x + dir.y * dir.y + dir.z * dir.z)))
	max_dist := f32(1.0)
	app.atten = 1.0 / (1.0 + dist / max_dist)
	if app.atten > 1 {
		app.atten = 1
	}
	forward := app.cam.forward()
	nd := dir.normalized()
	right := app.cam.right()
	dot := forward.dot(nd)
	if dot < 0 {
		app.atten *= 1 + dot * 0.5
	}
	app.pan = nd.dot(right)

	if !isnil(app.sound) && !gg.is_sound_playing(app.sound) {
		gg.play_sound(app.sound)
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	ctx.draw_grid(10, 2, gg.gray)
	sphere := gg.Vec3{5 * math.cosf(app.angle), 0, 5 * math.sinf(app.angle)}
	ctx.draw_sphere(sphere, 0.5, 8, 8, gg.red)
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Sound position: attenuation ${app.atten:.2f}, pan ${app.pan:.2f}',
		size:  16
		color: gg.dark_gray
	)
	ctx.draw_text(10, 30, 'gg has no spatial audio; values shown for reference',
		size:  14
		color: gg.gray
	)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [audio] example - sound positioning'
		bg_color:      gg.white
		depth:         true
		user_data:     app
		init_fn:       init
		update_fn:     update
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}
