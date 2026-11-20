module main

// Port of raylib textures_magnifying_glass.c
// PORT NOTE: gg has no per-pixel alpha masking or custom blend-factor API
// (rlSetBlendFactorsSeparate). The magnified view is drawn as a square region
// with a circular ring overlay, and hidden bunnies use gg's multiply effect.
import gg
import sokol.gfx

const glass_raywhite = gg.Color{245, 245, 245, 255}
const glass_black = gg.Color{0, 0, 0, 255}

fn rt_image(rt &gg.RenderTexture2D, smp gfx.Sampler) gg.Image {
	return gg.Image{
		width:       rt.width
		height:      rt.height
		nr_channels: 4
		ok:          true
		simg_ok:     true
		simg:        rt.texture()
		ssmp:        smp
	}
}

fn flush(ctx &gg.Context) {
	ctx.r.draw()
	ctx.r.rewind()
}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	bunny   gg.Image
	parrots gg.Image
	rt      &gg.RenderTexture2D = unsafe { nil }
	rt_idx  int                 = -1
	sampler gfx.Sampler
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.bunny = app.ctx.create_image('resources/raybunny.png') or { gg.Image{} }
	app.parrots = app.ctx.create_image('resources/parrots.png') or { gg.Image{} }
	app.sampler = gfx.make_sampler(&gfx.SamplerDesc{
		min_filter: .linear
		mag_filter: .linear
		wrap_u:     .clamp_to_edge
		wrap_v:     .clamp_to_edge
	})
	app.rt = gg.new_render_texture2d(256, 256) or { &gg.RenderTexture2D(unsafe { nil }) }
	if !isnil(app.rt) {
		app.rt_idx = app.ctx.cache_image(rt_image(app.rt, app.sampler))
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	mx := f32(app.ctx.mouse_pos_x)
	my := f32(app.ctx.mouse_pos_y)

	app.ctx.begin()
	app.ctx.draw_image(144, 33, f32(app.parrots.width), f32(app.parrots.height), &app.parrots)
	app.ctx.draw_text(154, 6, 'Use the magnifying glass to find hidden bunnies!',
		size:  20
		color: glass_black
	)

	if !isnil(app.rt) {
		app.rt.begin(glass_raywhite)
		// zoom 2x: a 256x256 view shows 128x128 source pixels around the cursor.
		app.ctx.draw_image_with_config(
			img:       &app.parrots
			img_rect:  gg.Rect{0, 0, 256, 256}
			part_rect: gg.Rect{mx - 144 - 64, my - 33 - 64, 128, 128}
		)
		bunnies := [gg.Rect{250, 350, 0, 0}, gg.Rect{500, 100, 0, 0}, gg.Rect{420, 300, 0, 0},
			gg.Rect{650, 10, 0, 0}]!
		for b in bunnies {
			app.ctx.draw_image_with_config(
				img:       &app.bunny
				img_rect:  gg.Rect{b.x - 144, b.y - 33, f32(app.bunny.width), f32(app.bunny.height)}
				part_rect: gg.Rect{0, 0, f32(app.bunny.width), f32(app.bunny.height)}
				effect:    gg.ImageEffect.multiply
			)
		}
		flush(app.ctx)
		app.rt.end()
	}

	if app.rt_idx >= 0 {
		app.ctx.draw_image_with_config(
			img_id:   app.rt_idx
			img_rect: gg.Rect{mx - 128, my - 128, 256, 256}
			flip_y:   true
		)
	}
	app.ctx.draw_circle_empty(mx, my, 126, glass_black)
	app.ctx.draw_circle_empty(mx, my, 130, glass_black)
	rx := mx / 800
	ry := my / 800
	app.ctx.draw_circle_filled(mx - 64 * rx - 32, my - 64 * ry - 32, 4,
		gg.Color{255, 255, 255, 128})
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - magnifying glass'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     glass_raywhite
		depth:        true
	)
	app.ctx.run()
}
