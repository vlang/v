import os
import objects
import gg
import gx
import rand

struct App {
mut:
	gg      &gg.Context = 0
	rockets []objects.Rocket
	frames  [][]objects.Rocket
	// i thought about using a fixed fifo queue for the frames but the array 
	// seemed to work fine, if you'd like a challenge try implementing it with the queue :)
}

fn on_frame(mut app App) {
	app.gg.begin()

	// drawing previous frames
	for mut frame in app.frames {
		for mut rocket in frame {
			if !rocket.exploded {
				rocket.color.a = byte(f32_max(rocket.color.a - 8, 0))
				rocket.draw(mut app.gg)
			}
		}
	}

	// chance of firing new rocket
	if rand.intn(30) == 0 {
		app.rockets << objects.new_rocket()
	}
	// simulating rockets
	app.rockets = app.rockets.filter(!it.dead)

	for mut rocket in app.rockets {
		rocket.tick(mut app.gg)
	}

	// adding frame
	mut frame := app.rockets.clone()

	for mut rocket in frame {
		rocket.particles = []
	}

	app.frames << frame

	// trimming out frames
	if app.frames.len > 30 {
		app.frames.delete(0)
	}

	app.gg.end()
}

fn main() {
	mut font_path := os.resource_abs_path(os.join_path('../assets/fonts/', 'RobotoMono-Regular.ttf'))
	$if android {
		font_path = 'fonts/RobotoMono-Regular.ttf'
	}

	mut app := &App{}

	app.gg = gg.new_context(
		width: objects.width
		height: objects.height
		window_title: 'Fireworks!'
		bg_color: gx.black
		use_ortho: true
		user_data: app
		frame_fn: on_frame
		font_path: font_path
	)

	app.gg.run()
}
