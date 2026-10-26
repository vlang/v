import gg
import os.asset

// If you have emscripten (see https://emscripten.org/docs/getting_started/index.html), you can compile this program to WASM, using:
// `v -os wasm32_emscripten -o examples/gg/rotating_textured_quad.html examples/gg/rotating_textured_quad.v`, and later check with:
// `emrun examples/gg/rotating_textured_quad.html` .
#flag wasm32_emscripten --embed-file @VEXEROOT/examples/assets/logo.png@/assets/logo.png
#flag wasm32_emscripten --embed-file @VEXEROOT/examples/assets/fonts/RobotoMono-Regular.ttf@/assets/RobotoMono-Regular.ttf

@[heap]
pub struct Window {
pub mut:
	ctx &gg.Context = unsafe { nil }
	img gg.Image
}

pub fn (mut window Window) init() {
	image_path := asset.get_path('../assets', 'logo.png')
	window.img = window.ctx.create_image(image_path) or { panic(err) }
}

pub fn (mut window Window) draw() {
	angle := f32(window.ctx.frame) / 64 // since window.ctx.frame is increased by 1 on every frame -> the angle will be increasing too
	window.ctx.begin()
	window.ctx.r.load_pipeline(window.ctx.pipeline.alpha)

	window.ctx.r.translate(250, 250, 0) // center of the screen
	window.ctx.r.rotate(angle, 0.0, 0.0, 1.0) // rotate around the Z axis pointing towards the camera

	window.ctx.r.enable_texture()
	window.ctx.r.texture(window.img.simg, window.img.ssmp)
	window.ctx.r.begin_quads()
	window.ctx.r.c4b(255, 255, 255, 255)
	window.ctx.r.v3f_t2f(200, 200, 0, 1.0, 1.0)
	window.ctx.r.v3f_t2f(200, -200, 0, 1.0, 0.0)
	window.ctx.r.v3f_t2f(-200, -200, 0, 0.0, 0.0)
	window.ctx.r.v3f_t2f(-200, 200, 0, 0.0, 1.0)
	window.ctx.r.end()
	window.ctx.r.disable_texture()
	window.ctx.end()
}

fn main() {
	mut window := &Window{}
	window.ctx = gg.new_context(
		window_title: 'Rotating V logo'
		bg_color:     gg.light_green
		width:        500
		height:       500
		user_data:    window
		init_fn:      window.init
		frame_fn:     window.draw
		font_path:    asset.get_path('../assets', 'RobotoMono-Regular.ttf')
	)
	window.ctx.run()
}
