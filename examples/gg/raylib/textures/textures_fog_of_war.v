module main

// Port of raylib textures_fog_of_war.c
// PORT NOTE: gg buffers draw commands until ctx.end(), so each render-texture
// pass is flushed explicitly with ctx.r.draw()/ctx.r.rewind() before EndTexture.
// The bilinear fog smoothing comes from the texture filter on the RT image.
import gg
import sokol.gfx

const fog_raywhite = gg.Color{245, 245, 245, 255}
const fog_blue = gg.Color{0, 121, 241, 255}
const fog_darkblue = gg.Color{0, 42, 141, 255}

const map_tile_size = 32
const player_size = 16
const visibility = 2

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
	ctx       &gg.Context         = unsafe { nil }
	rt        &gg.RenderTexture2D = unsafe { nil }
	rt_idx    int                 = -1
	sampler   gfx.Sampler
	tile_ids  []u8
	tile_fog  []u8
	tiles_x   int = 25
	tiles_y   int = 15
	player_x  f32 = 180
	player_y  f32 = 130
	player_tx int
	player_ty int
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.tile_ids = []u8{len: app.tiles_x * app.tiles_y}
	app.tile_fog = []u8{len: app.tiles_x * app.tiles_y}
	for i in 0 .. app.tile_ids.len {
		app.tile_ids[i] = u8(i % 2)
	}
	app.sampler = gfx.make_sampler(&gfx.SamplerDesc{
		min_filter: .linear
		mag_filter: .linear
		wrap_u:     .clamp_to_edge
		wrap_v:     .clamp_to_edge
	})
	app.rt = gg.new_render_texture2d(app.tiles_x, app.tiles_y) or { &gg.RenderTexture2D(unsafe { nil }) }
	if !isnil(app.rt) {
		app.rt_idx = app.ctx.cache_image(rt_image(app.rt, app.sampler))
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	if app.ctx.pressed_keys[gg.KeyCode.right] {
		app.player_x += 5
	}
	if app.ctx.pressed_keys[gg.KeyCode.left] {
		app.player_x -= 5
	}
	if app.ctx.pressed_keys[gg.KeyCode.down] {
		app.player_y += 5
	}
	if app.ctx.pressed_keys[gg.KeyCode.up] {
		app.player_y -= 5
	}
	map_w := f32(app.tiles_x * map_tile_size)
	map_h := f32(app.tiles_y * map_tile_size)
	if app.player_x < 0 { app.player_x = 0 }
	if app.player_x + player_size > map_w { app.player_x = map_w - player_size }
	if app.player_y < 0 { app.player_y = 0 }
	if app.player_y + player_size > map_h { app.player_y = map_h - player_size }

	for i in 0 .. app.tile_fog.len {
		if app.tile_fog[i] == 1 {
			app.tile_fog[i] = 2
		}
	}
	app.player_tx = int((app.player_x + f32(map_tile_size) / 2) / map_tile_size)
	app.player_ty = int((app.player_y + f32(map_tile_size) / 2) / map_tile_size)
	mut y := app.player_ty - visibility
	for y < app.player_ty + visibility {
		mut x := app.player_tx - visibility
		for x < app.player_tx + visibility {
			if x >= 0 && x < app.tiles_x && y >= 0 && y < app.tiles_y {
				app.tile_fog[y * app.tiles_x + x] = 1
			}
			x++
		}
		y++
	}

	// Draw the small fog map into a render texture.
	if !isnil(app.rt) {
		app.rt.begin(gg.Color{0, 0, 0, 0})
		mut ty := 0
		for ty < app.tiles_y {
			mut tx := 0
			for tx < app.tiles_x {
				state := app.tile_fog[ty * app.tiles_x + tx]
				if state == 0 {
					app.ctx.draw_rect_filled(f32(tx), f32(ty), 1, 1, gg.Color{0, 0, 0, 255})
				} else if state == 2 {
					app.ctx.draw_rect_filled(f32(tx), f32(ty), 1, 1, gg.Color{0, 0, 0, 204})
				}
				tx++
			}
			ty++
		}
		flush(app.ctx)
		app.rt.end()
	}

	app.ctx.begin()
	for ty in 0 .. app.tiles_y {
		for tx in 0 .. app.tiles_x {
			x := f32(tx * map_tile_size)
			y := f32(ty * map_tile_size)
			c := if app.tile_ids[ty * app.tiles_x + tx] == 0 {
				fog_blue
			} else {
				gg.Color{0, 121, 241, 230}
			}
			app.ctx.draw_rect_filled(x, y, map_tile_size, map_tile_size, c)
			app.ctx.draw_rect_empty(x, y, map_tile_size, map_tile_size, gg.Color{0, 42, 141, 128})
		}
	}
	app.ctx.draw_rect_filled(app.player_x, app.player_y, player_size, player_size, gg.red)
	if app.rt_idx >= 0 {
		app.ctx.draw_image_with_config(
			img_id:   app.rt_idx
			img_rect: gg.Rect{0, 0, map_w, map_h}
			flip_y:   true
		)
	}
	app.ctx.draw_text(10, 10, 'Current tile: [${app.player_tx},${app.player_ty}]',
		size:  20
		color: fog_raywhite
	)
	app.ctx.draw_text(10, 425, 'ARROW KEYS to move', size: 20, color: fog_raywhite)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - fog of war'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     fog_raywhite
		depth:        true
	)
	app.ctx.run()
}
