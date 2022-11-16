import gg
import gx
import os
import sokol.sgl

pub struct Window {
pub mut:
	ctx &gg.Context = unsafe { nil }
	img gg.Image
}

pub fn (mut window Window) init() {
	window.img = window.ctx.create_image(os.resource_abs_path('../assets/logo.png'))
}

pub fn (mut window Window) draw() {
	angle := f32(window.ctx.frame) / 64 // since window.ctx.frame is increased by 1 on every frame -> the angle will be increasing too
	window.ctx.begin()
	sgl.load_pipeline(window.ctx.pipeline.alpha)

	sgl.translate(400, 400, 0) // center of the screen
	sgl.rotate(angle, 0.0, 0.0, 1.0) // rotate around the Z axis pointing towards the camera

	sgl.enable_texture()
	sgl.texture(window.img.simg)
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
		bg_color: gx.light_green
		width: 800
		height: 800
		user_data: window
		init_fn: window.init
		frame_fn: window.draw
	)
	window.ctx.run()
}
