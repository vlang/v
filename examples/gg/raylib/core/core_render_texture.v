module main

// raylib [core] example - render texture
// A ball bounces inside an offscreen gg.RenderTexture2D; the resulting image
// is drawn rotated in the main framebuffer.
// PORT NOTE: gg records 2D draw commands and commits them in ctx.end(), so
// the render pass must be active for the whole frame. The render texture is
// wrapped in a gg.Image so ctx.draw_image_with_config() can sample it.

import gg
import sokol.gfx

const screen_width = 800
const screen_height = 450
const rt_width = 300
const rt_height = 300

const ray_white = gg.Color{245, 245, 245, 255}
const sky_blue = gg.Color{102, 191, 255, 255}
const black = gg.Color{0, 0, 0, 255}
const maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	gg       &gg.Context         = unsafe { nil }
	target   &gg.RenderTexture2D = unsafe { nil }
	tex_img  &gg.Image           = unsafe { nil }
	ball_pos gg.Vec2             = gg.Vec2{150, 150}
	ball_vel gg.Vec2             = gg.Vec2{5, 4}
	radius   f32                 = 20
	rotation f32
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - render texture'
		bg_color:     ray_white
		frame_fn:     frame
		init_fn:      init
		user_data:    app
	)
	app.gg.fps.show = true
	app.gg.fps.x = 10
	app.gg.fps.y = 10
	app.gg.run()
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.target = gg.new_render_texture2d(rt_width, rt_height) or { panic(err.msg()) }
	app.tex_img = make_texture_image(app.gg, app.target)
}

fn make_texture_image(ctx &gg.Context, rt &gg.RenderTexture2D) &gg.Image {
	mut img := &gg.Image{
		width:       rt.width
		height:      rt.height
		nr_channels: 4
		ok:          true
		simg_ok:     true
		simg:        rt.texture()
		ssmp:        gfx.make_sampler(&gfx.SamplerDesc{
			min_filter:    .linear
			mag_filter:    .linear
			mipmap_filter: .linear
			wrap_u:        .clamp_to_edge
			wrap_v:        .clamp_to_edge
		})
	}
	mut ctx_mut := unsafe { &gg.Context(ctx) }
	img.id = ctx_mut.cache_image(*img)
	return img
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg

	app.ball_pos = app.ball_pos + app.ball_vel
	if app.ball_pos.x >= f32(rt_width) - app.radius || app.ball_pos.x <= app.radius {
		app.ball_vel.x *= -1
	}
	if app.ball_pos.y >= f32(rt_height) - app.radius || app.ball_pos.y <= app.radius {
		app.ball_vel.y *= -1
	}
	app.rotation += 0.5

	// Draw into the offscreen target.
	app.target.begin(sky_blue)
	ctx.set_viewport(0, 0, rt_width, rt_height)
	ctx.begin()
	ctx.draw_rect(x: 0, y: 0, w: 20, h: 20, color: gg.red)
	ctx.draw_circle_filled(app.ball_pos.x, app.ball_pos.y, app.radius, maroon)
	ctx.end()
	app.target.end()
	ctx.set_viewport(0, 0, ctx.width, ctx.height)

	// Composite the texture into the main framebuffer.
	ctx.begin()
	ctx.draw_image_with_config(
		img:      app.tex_img
		img_rect: gg.Rect{250, 75, rt_width, rt_height}
		rotation: app.rotation
		color:    gg.white
	)
	ctx.draw_text(10, screen_height - 40, 'DRAWING BOUNCING BALL INSIDE RENDER TEXTURE!',
		size:  20
		color: black
	)
	ctx.end()
}
