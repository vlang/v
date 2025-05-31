import gg
import gx
import os.asset
import sokol.sgl

// If you have emscripten (see https://emscripten.org/docs/getting_started/index.html), you can compile this program to WASM, using:
// `v -os wasm32_emscripten -o examples/gg/rotating_textured_quad.html examples/gg/rotating_textured_quad.v`, and later check with:
// `emrun examples/gg/rotating_textured_quad.html` .
#flag wasm32_emscripten --embed-file @VEXEROOT/examples/assets/logo.png@/assets/logo.png
#flag wasm32_emscripten --embed-file @VEXEROOT/examples/assets/fonts/RobotoMono-Regular.ttf@/assets/RobotoMono-Regular.ttf

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
	sgl.load_pipeline(window.ctx.pipeline.alpha)

	sgl.translate(250, 250, 0) // center of the screen
	sgl.rotate(angle, 0.0, 0.0, 1.0) // rotate around the Z axis pointing towards the camera

	sgl.enable_texture()
	sgl.texture(window.img.simg, window.img.ssmp)
	sgl.begin_quads()
	sgl.c4b(255, 255, 255, 255)
	sgl.v3f_t2f(200, 200, 0, 1.0, 1.0)
	sgl.v3f_t2f(200, -200, 0, 1.0, 0.0)
	sgl.v3f_t2f(-200, -200, 0, 0.0, 0.0)
	sgl.v3f_t2f(-200, 200, 0, 0.0, 1.0)
	sgl.end()
	sgl.disable_texture()
	window.ctx.end()
}

fn main() {
	mut window := &Window{}
	window.ctx = gg.new_context(
		window_title: 'Rotating V logo'
		bg_color:     gx.light_green
		width:        500
		height:       500
		user_data:    window
		init_fn:      window.init
		frame_fn:     window.draw
		font_path:    asset.get_path('../assets', 'RobotoMono-Regular.ttf')
	)
	window.ctx.run()
}
