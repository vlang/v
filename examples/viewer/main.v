/**********************************************************************
*
* Multiplatform image viewer application
*
* Copyright (c) 2026 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
**********************************************************************/

module main

// Minimal multiplatform image viewer.
//
// Rendering uses `gg` (a sokol-backed 2D layer) in immediate mode: the
// decoded image is drawn as a single textured quad and panned / zoomed /
// rotated through the renderer's matrix stack. This path works on every
// native sokol backend (OpenGL on Linux, D3D11 on Windows, Metal on macOS),
// so no custom shader compilation is required for the viewer itself.
//
// Image bytes come from our own `decoder` module (no stbi). Files, folders
// and zip archives are scanned by `file_scan.v` / `zip_container.v`.

import os
import gg
import sokol.gfx
import sokol.sapp
import image.decoders.decoder
import image.decoders.gif
import image.decoders.webp
import compress.szip

const help_text_rows = [
	'Image Viewer help.',
	'',
	'ESC/q - Quit',
	'cur. right - Next image',
	'cur. left  - Previous image',
	'cur. up    - Next folder',
	'cur. down  - Previous folder',
	'F - Toggle full screen',
	'R - Rotate image of 90 degree',
	'I - Toggle the info text',
	'P - Pause/play animation',
	'H - Toggle this help',
	'',
	'1 - Sort hierarchy by file name',
	'2 - Sort hierarchy by file date',
	'3 - Sort hierarchy as loaded (default)',
	'',
	'mouse wheel - next/previous images',
	'keep pressed left  Mouse button - Pan on the image',
	'keep pressed right Mouse button - Zoom on the image',
	'click middle Mouse button - Reset view / fill window',
]

// AnimPlayer is the viewer-facing playback interface implemented by both
// gif.GifPlayer and webp.WebpPlayer: step() returns the next composited
// full-canvas RGBA, reset() rewinds to frame 0, and free() releases the
// player's buffers.
interface AnimPlayer {
mut:
	frame_count() int
	loop_count() int
	delay_ms(i int) int
	reset() void
	step() ![]u8
	free() void
	canvas_width() int
	canvas_height() int
}

// NullPlayer is the idle AnimPlayer App.player starts with. Every use of
// App.player is guarded by App.is_anim, so its methods never run while it
// is the active player; it exists because V requires interface fields to
// be initialized.
struct NullPlayer {}

fn (_ NullPlayer) frame_count() int { return 0 }

fn (_ NullPlayer) loop_count() int { return 0 }

fn (_ NullPlayer) delay_ms(_ int) int { return 0 }

fn (mut _ NullPlayer) reset() {}

fn (mut _ NullPlayer) step() ![]u8 { return error('no animation is playing') }

fn (mut _ NullPlayer) free() {}

fn (_ NullPlayer) canvas_width() int { return 0 }

fn (_ NullPlayer) canvas_height() int { return 0 }

// App holds the window context, the current image texture, the navigation
// list, and the view transform state.
struct App {
mut:
	gg &gg.Context = unsafe { nil }

	// current image as a GPU texture
	texture   gfx.Image
	sampler   gfx.Sampler
	img_w     int
	img_h     int
	has_image bool

	// persistent CPU staging buffer for texture uploads. Reused across
	// images; only reallocated (freeing the previous block) when a larger
	// image arrives, so browsing many images does not churn large buffers.
	img_buf []u8

	// animation playback (animated GIF/WEBP): incremental player + clock.
	// `is_anim` doubles as the "player is live" flag (V interfaces cannot
	// be compared to nil).
	player        AnimPlayer
	anim_bytes    []u8 // encoded bytes borrowed by the active GIF player
	anim_frame    int
	anim_accum_ms f64
	anim_paused   bool
	is_anim       bool

	// navigation
	item_list Item_list
	zip       &szip.Zip = unsafe { nil }
	zip_index int       = -1

	// view transform (window pixel space, y-down)
	win_w f32
	win_h f32
	pan_x f32
	pan_y f32
	zoom  f32 = 1.0

	// interaction bookkeeping
	mouse_x        f32 = -1.0
	mouse_y        f32 = -1.0
	panning        bool
	last_mx        f32
	last_my        f32
	zooming        bool
	last_zx        f32
	last_zy        f32
	show_info_flag bool
	show_help_flag bool

	status string
}

// min_f32 returns the smaller of two f32 values.
fn min_f32(a f32, b f32) f32 {
	if a < b {
		return a
	}
	return b
}

// make_rgba_texture uploads an RGBA8 buffer as a sokol image. The image
// is immutable (created with its data), which is the path that reliably
// displays on every sokol backend (D3D11 skips the initial upload for
// dynamic images, leaving them black until the first update).
@[unsafe]
fn make_rgba_texture(w int, h int, rgba []u8) gfx.Image {
	mut desc := gfx.ImageDesc{
		width:       w
		height:      h
		num_mipmaps: 0
		label:       &char(unsafe { nil })
	}
	desc.data.subimage[0][0] = gfx.Range{
		ptr:  rgba.data
		size: uint(w * h * 4)
	}
	return gfx.make_image(&desc)
}

// release_player releases the buffers and the heap-allocated concrete player.
fn release_player(mut player AnimPlayer) {
	player.free()
	match mut player {
		gif.GifPlayer {
			unsafe { free(player) }
		}
		webp.WebpPlayer {
			unsafe { free(player) }
		}
		else {}
	}
}

// clear_image destroys the current GPU texture, if any, and resets the
// animation playback state.
fn (mut app App) clear_image() {
	if app.has_image {
		gfx.destroy_image(app.texture)
		app.has_image = false
		app.img_w = 0
		app.img_h = 0
	}
	if app.is_anim {
		release_player(mut app.player)
	}
	unsafe { app.anim_bytes.free() }
	app.anim_bytes = []u8{}
	app.anim_frame = 0
	app.anim_accum_ms = 0
	app.anim_paused = false
	app.is_anim = false
}

// start_anim activates an animation player: steps frame 0, uploads it as
// the display texture, and marks the app as animating. When free_bytes is
// set the encoded file bytes are released because the player does not
// reference them (WEBP); the GIF player borrows them and keeps them alive.
fn (mut app App) start_anim(mut player AnimPlayer, bytes []u8, free_bytes bool) {
	player.reset()
	rgba := player.step() or {
		release_player(mut player)
		unsafe { bytes.free() }
		app.status = 'decode error: ${err}'
		return
	}
	app.player = player
	app.is_anim = true
	app.anim_frame = 0
	app.anim_accum_ms = 0
	app.set_texture(player.canvas_width(), player.canvas_height(), rgba)
	app.status = ''
	app.reset_view()
	if free_bytes {
		unsafe { bytes.free() }
	} else {
		// Retain the owned input until the borrowing GIF player is released.
		app.anim_bytes = bytes
	}
}

// set_texture (re)creates the display texture from an RGBA buffer. The
// old texture is destroyed first if one is live. The pixels are copied
// into a persistent staging buffer (`app.img_buf`) that is reused across
// images and only grown — freeing the previous block — when a larger
// image arrives, so navigating between images does not repeatedly
// allocate large buffers. This immutable create-with-data path is the one
// that reliably displays on every sokol backend.
fn (mut app App) set_texture(w int, h int, rgba []u8) {
	if app.has_image {
		gfx.destroy_image(app.texture)
	}
	needed := w * h * 4
	if app.img_buf.len < needed {
		unsafe { app.img_buf.free() }
		app.img_buf = []u8{len: needed}
	}
	copy(mut app.img_buf, rgba)
	app.texture = make_rgba_texture(w, h, app.img_buf)
	app.img_w = w
	app.img_h = h
	app.has_image = true
}

// anim_delay_ms returns the display duration of the current animation
// frame in milliseconds, clamped so 0-delay frames do not spin the CPU.
fn (app &App) anim_delay_ms() f64 {
	mut d := f64(app.player.delay_ms(app.anim_frame))
	if d < 10 {
		d = 10
	}
	return d
}

// tick_animation advances the GIF playback clock by `ms` milliseconds,
// stepping the player forward one frame per elapsed delay. A finished
// loop restarts from frame 0 (resetting the canvas). At most one texture
// is rebuilt per tick, even if several frames elapsed.
fn (mut app App) tick_animation(ms f64) {
	if !app.is_anim || app.anim_paused {
		return
	}
	n := app.player.frame_count()
	if n == 0 {
		return
	}
	app.anim_accum_ms += ms
	mut changed := false
	mut last_rgba := []u8{}
	for app.anim_accum_ms >= app.anim_delay_ms() {
		app.anim_accum_ms -= app.anim_delay_ms()
		if app.anim_frame + 1 >= n {
			app.player.reset()
			app.anim_frame = 0
		} else {
			app.anim_frame++
		}
		last_rgba = app.player.step() or {
			return
		}
		changed = true
	}
	if changed {
		app.set_texture(app.player.canvas_width(), app.player.canvas_height(), last_rgba)
	}
}

// app_update is the gg update callback: it drives GIF animation timing
// with the per-frame delta time (seconds) reported by sokol.
fn app_update(dt f32, mut app App) {
	app.tick_animation(f64(dt) * 1000.0)
}

// load_current_image decodes the selected item and rebuilds the texture.
// Animated GIFs enter playback mode; everything else (including
// single-frame GIFs) is shown as a static image.
fn (mut app App) load_current_image() {
	app.clear_image()
	bytes := app.load_current_bytes() or {
		app.status = 'load error: ${err}'
		return
	}
	match decoder.sniff(bytes) {
		'gif' {
			if mut player := gif.new_gif_player(bytes) {
				if player.frame_count() > 1 {
					// The GIF player borrows the encoded bytes for
					// incremental decoding; they stay alive with the player.
					app.start_anim(mut player, bytes, false)
					return
				}
				player.free()
				unsafe { free(player) }
			}
		}
		'webp' {
			if mut player := webp.new_webp_player(bytes) {
				if player.frame_count() > 1 {
					// The WEBP player owns fully composited frames and does
					// not reference the encoded bytes.
					app.start_anim(mut player, bytes, true)
					return
				}
				player.free()
				unsafe { free(player) }
			}
		}
		else {}
	}
	img := decoder.decode(bytes) or {
		unsafe { bytes.free() }
		app.status = 'decode error: ${err}'
		return
	}
	app.set_texture(img.width, img.height, img.rgba)
	// The pixels now live in the persistent staging buffer and on the GPU.
	// Release the decoder's output and the encoded file bytes immediately
	// rather than waiting for the GC (the static path retains neither).
	unsafe {
		img.rgba.free()
		bytes.free()
	}
	app.status = ''
	app.reset_view()
}

// fit_zoom returns the zoom that fits the (rotation-aware) image in the window.
fn (app &App) fit_zoom() f32 {
	if app.img_w <= 0 || app.img_h <= 0 || app.win_w <= 0 || app.win_h <= 0 {
		return 1.0
	}
	mut w := f32(app.img_w)
	mut h := f32(app.img_h)
	if app.item_list.lst[app.item_list.item_index].rotation & 1 == 1 {
		w = f32(app.img_h)
		h = f32(app.img_w)
	}
	return min_f32(app.win_w / w, app.win_h / h)
}

// reset_view recenters the image and fits it to the window.
fn (mut app App) reset_view() {
	app.pan_x = 0
	app.pan_y = 0
	app.zoom = app.fit_zoom()
}

// draw_image renders the current image as a textured quad with the view
// transform applied, in gg's pixel-space orthographic projection.
fn draw_image(app &App) {
	rot := app.item_list.lst[app.item_list.item_index].rotation
	angle := f32(rot) * 1.5707963 // 90 degrees per step

	app.gg.r.matrix_mode_modelview()
	app.gg.r.push_matrix()
	app.gg.r.translate(app.win_w / 2 + app.pan_x, app.win_h / 2 + app.pan_y, 0.0)
	app.gg.r.rotate(angle, 0.0, 0.0, 1.0)
	app.gg.r.scale(app.zoom, app.zoom, 1.0)

	hw := f32(app.img_w) / 2
	hh := f32(app.img_h) / 2
	app.gg.r.enable_texture()
	app.gg.r.texture(app.texture, app.sampler)
	app.gg.r.begin_quads()
	app.gg.r.v2f_t2f_c3b(-hw, -hh, 0, 0, 255, 255, 255)
	app.gg.r.v2f_t2f_c3b(hw, -hh, 1, 0, 255, 255, 255)
	app.gg.r.v2f_t2f_c3b(hw, hh, 1, 1, 255, 255, 255)
	app.gg.r.v2f_t2f_c3b(-hw, hh, 0, 1, 255, 255, 255)
	app.gg.r.end()
	app.gg.r.disable_texture()
	app.gg.r.pop_matrix()
}

// draw_help paints the help overlay.
fn draw_help(app &App) {
	mut cfg := gg.TextCfg{
		color: gg.white
		align: .left
		size:  16
	}
	mut y := 80
	for row in help_text_rows {
		app.gg.draw_text(10, y, row, cfg)
		y += 20
	}
}

// draw_overlay paints the info line (and any status/error message).
//
// Memory note: this runs EVERY frame. The interpolated strings below are
// GC-managed (Boehm) and are collected automatically, so they are NOT a
// leak under the default build. The explicit `free()` calls are kept as a
// defensive measure for `-gc none` builds (where `v_free` becomes a real
// free and these would otherwise leak); under the Boehm build `v_free` is
// a no-op, so they are harmless. `gg.draw_text` consumes the text
// immediately and does not retain it, so freeing after the call is safe
// in either mode. Literals/empty strings are no-ops in `string.free()`.
fn draw_overlay(app &App) {
	mut cfg := gg.TextCfg{
		color: gg.white
		align: .left
		size:  16
	}
	mut y := 10
	if app.show_info_flag {
		mut line := ''
		mut src := ''
		mut anim_part := ''
		if app.has_image {
			it := app.item_list.lst[app.item_list.item_index]
			// Own the source string (clone/interpolate) so it can be
			// freed after drawing; get_file_path() returns a borrowed
			// view of stored fields on some branches and must not be
			// freed here.
			if it.need_extract {
				src = 'zip: ${it.path}'
			} else if it.path != '' {
				src = '${it.path}${app.item_list.path_sep}${it.name}'
			} else {
				src = it.name.clone()
			}
			if app.is_anim {
				paused := if app.anim_paused { '   [paused]' } else { '' }
				// Single interpolation: `+=` would leak the previous
				// intermediate under the no-GC build.
				anim_part = 'frame ${app.anim_frame + 1}/${app.player.frame_count()}   loop ${app.player.loop_count()}${paused}'
			}
			// Single interpolation: `a + b` of two interpolated parts
			// would leak the left temporary.
			line = '${it.name}   ${app.item_list.item_index + 1}/${app.item_list.n_item}   ${app.img_w}x${app.img_h}   ${anim_part}zoom ${app.zoom:.2}   [${src}]'
		} else {
			line = 'No image. Pass a file/folder/zip path, or drop files.  [esc] quit'
		}
		app.gg.draw_text(10, y, line, cfg)
		unsafe {
			line.free()
			src.free()
			anim_part.free()
		}
		y += 22
	}
	if app.item_list.last_error != '' {
		scan_line := 'scan: ${app.item_list.last_error}'
		app.gg.draw_text(10, y, scan_line, cfg)
		unsafe { scan_line.free() }
		y += 22
	}
	if app.status != '' {
		app.gg.draw_text(10, y, app.status, cfg)
	}
	if app.show_help_flag {
		draw_help(app)
	}
}

// frame is the gg draw callback.
fn frame(mut app App) {
	ws := gg.window_size_real_pixels()
	app.win_w = f32(ws.width)
	app.win_h = f32(ws.height)

	// image pass (uses gg's pixel-space ortho + our modelview transform)
	app.gg.begin()
	if app.has_image {
		draw_image(app)
	}
	// overlay pass (reset to a clean pixel-space ortho for text)
	app.gg.begin()
	draw_overlay(app)
	app.gg.end()
}

// app_init runs once after sokol is set up.
fn app_init(mut app App) {
	mut sd := gfx.SamplerDesc{
		min_filter: .linear
		mag_filter: .linear
		wrap_u:     .clamp_to_edge
		wrap_v:     .clamp_to_edge
	}
	app.sampler = gfx.make_sampler(&sd)
	app.load_current_image()
}

// next / prev / rotate_view drive navigation and reload the texture.
fn (mut app App) next() {
	if !app.item_list.loaded {
		return
	}
	app.item_list.get_next_item(1)
	app.load_current_image()
}

fn (mut app App) prev() {
	if !app.item_list.loaded {
		return
	}
	app.item_list.get_next_item(-1)
	app.load_current_image()
}

fn (mut app App) next_container() {
	if !app.item_list.loaded {
		return
	}
	app.item_list.go_to_next_container(1)
	app.load_current_image()
}

fn (mut app App) prev_container() {
	if !app.item_list.loaded {
		return
	}
	app.item_list.go_to_next_container(-1)
	app.load_current_image()
}

fn (mut app App) rotate_view() {
	if !app.has_image {
		return
	}
	app.item_list.rotate(1)
	app.reset_view()
}

// set_sort reorders the navigation hierarchy by `mode` (name / date / loaded).
// The selection follows the same file, so the current texture is kept and only
// the navigation order (and the overlay position) change.
fn (mut app App) set_sort(mode Sort_mode) {
	if app.item_list.sort_mode == mode {
		return
	}
	app.item_list.resort(mode)
}

// my_event handles keyboard / mouse / drag-and-drop input.
fn my_event(mut ev gg.Event, mut app App) {
	if ev.typ == .quit_requested {
		exit(0)
	}

	// window resize: refit the current image to the new window size
	if ev.typ == .resized {
		ws := gg.window_size_real_pixels()
		app.win_w = f32(ws.width)
		app.win_h = f32(ws.height)
		if app.has_image {
			app.reset_view()
		}
	}

	// mouse wheel: next/previous image (same command as the example viewer)
	if ev.typ == .mouse_scroll && ev.scroll_y != 0 {
		inc := if ev.scroll_y > 0 { -1 } else { 1 }
		if app.item_list.n_item > 0 {
			app.item_list.get_next_item(inc)
			app.load_current_image()
		}
	}

	// keep the latest mouse position for drag operations
	if ev.typ == .mouse_move {
		app.mouse_x = ev.mouse_x
		app.mouse_y = ev.mouse_y
	}

	// middle button: reset zoom/pan and refit the image to the window
	if ev.typ == .mouse_down && ev.mouse_button == .middle {
		if app.has_image {
			app.reset_view()
		}
	}

	// left button: pan
	if ev.typ == .mouse_down && ev.mouse_button == .left {
		app.mouse_x = ev.mouse_x
		app.mouse_y = ev.mouse_y
		app.panning = true
		app.last_mx = ev.mouse_x
		app.last_my = ev.mouse_y
	}
	if ev.typ == .mouse_up && ev.mouse_button == .left {
		app.panning = false
	}
	if ev.typ == .mouse_move && app.panning {
		app.pan_x += ev.mouse_x - app.last_mx
		app.pan_y += ev.mouse_y - app.last_my
		app.last_mx = ev.mouse_x
		app.last_my = ev.mouse_y
	}

	// right button: zoom
	if ev.typ == .mouse_down && ev.mouse_button == .right {
		app.mouse_x = ev.mouse_x
		app.mouse_y = ev.mouse_y
		app.zooming = true
		app.last_zx = ev.mouse_x
		app.last_zy = ev.mouse_y
	}
	if ev.typ == .mouse_up && ev.mouse_button == .right {
		app.zooming = false
	}
	if ev.typ == .mouse_move && app.zooming {
		dx := ev.mouse_x - app.last_zx
		app.zoom += dx / 100.0
		if app.zoom < 0.1 {
			app.zoom = 0.1
		}
		if app.zoom > 32.0 {
			app.zoom = 32.0
		}
		app.last_zx = ev.mouse_x
		app.last_zy = ev.mouse_y
	}

	// keyboard commands (same bindings as the example viewer)
	if ev.typ == .key_down {
		if ev.key_code == .escape || ev.key_code == .q {
			exit(0)
		}
		if ev.key_code == .i {
			app.show_info_flag = !app.show_info_flag
		}
		if ev.key_code == .p {
			if app.is_anim {
				app.anim_paused = !app.anim_paused
			}
		}
		if ev.key_code == .h {
			app.show_help_flag = !app.show_help_flag
		}
		if app.item_list.loaded {
			if ev.key_code == ._1 {
				app.set_sort(.name)
			}
			if ev.key_code == ._2 {
				app.set_sort(.date)
			}
			if ev.key_code == ._3 {
				app.set_sort(.loaded)
			}
		}

		if app.item_list.loaded && app.item_list.n_item > 0 {
			if ev.key_code == .left {
				app.prev()
			}
			if ev.key_code == .right {
				app.next()
			}
			if ev.key_code == .up {
				app.next_container()
			}
			if ev.key_code == .down {
				app.prev_container()
			}
			if ev.key_code == .r {
				app.rotate_view()
			}
			if ev.key_code == .f {
				sapp.toggle_fullscreen()
			}
		}
	}

	// drag & drop: rebuild the list from the dropped paths
	if ev.typ == .files_dropped {
		if app.zip_index >= 0 {
			app.zip.close()
			app.zip_index = -1
		}
		app.clear_image()
		mut paths := []string{}
		n := sapp.get_num_dropped_files()
		for i in 0 .. n {
			paths << sapp.get_dropped_file_path(i)
		}
		app.item_list = Item_list{}
		app.item_list.get_items_list(paths)
		if app.item_list.n_item == 0 && app.item_list.last_error == '' {
			app.item_list.last_error = 'no supported images found in drop'
		}
		app.load_current_image()
	}
}

fn main() {
	mut app := &App{ player: NullPlayer{} }
	app.item_list.get_items_list(os.args[1..])

	app.gg = gg.new_context(
		width:             1024
		height:            768
		create_window:     true
		window_title:      'V Image Viewer'
		user_data:         app
		bg_color:          gg.rgb(30, 30, 30)
		frame_fn:          frame
		update_fn:         app_update
		init_fn:           app_init
		event_fn:          my_event
		enable_dragndrop:  true
		max_dropped_files: 65536
	)
	app.gg.run()
}
