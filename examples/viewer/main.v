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
import time
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
	'W - Toggle scan warnings',
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

	// paths given on the command line; the scan starts in app_init (once the
	// window exists) so the progress bar is visible while they are scanned.
	scan_pending []string

	// true on the very first launch (no placement file yet): only then the
	// 1/3 x 1/2 centered default is applied in app_init. On later launches the
	// rectangle saved by placement_save() is restored instead (see below).
	place_first bool

	// Rectangle read back from the placement file on a later launch and applied
	// in app_init (Windows). place_restore is false when there is no usable
	// saved placement, so the default startup size is kept.
	place_restore bool
	place_x       int
	place_y       int
	place_w       int
	place_h       int

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
	show_warn_flag bool

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

// scan_frame_budget_ms caps how long the incremental scan may run inside a
// single update, so the window keeps redrawing (and the progress bar keeps
// advancing) while a huge tree is walked.
const scan_frame_budget_ms = 8

// app_update is the gg update callback: it drives the incremental scan
// (within a small per-frame time budget) and the GIF animation timing with
// the per-frame delta time (seconds) reported by sokol.
fn app_update(dt f32, mut app App) {
	if app.item_list.scanning {
		t0 := time.clock()
		mut more := true
		for more {
			more = app.item_list.scan_step()
			if !more || time.clock() - t0 >= scan_frame_budget_ms {
				break
			}
		}
		if !more {
			app.item_list.scan_finish()
			if app.item_list.n_item == 0 && app.item_list.last_error == '' {
				app.item_list.last_error = 'no supported images found'
			}
			app.load_current_image()
		}
	}
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

// draw_scan_progress paints the centered scan progress bar: a horizontal
// bar that fills as entries are processed, with the current/total entry
// count and the completion percentage below it.
// Note: gg's drawing API works in DPI-logical units (it multiplies the
// coordinates by ctx.scale internally), while win_w/win_h are physical
// framebuffer pixels, so they must be divided by the DPI scale first.
fn draw_scan_progress(app &App) {
	sc := gg.dpi_scale()
	lw := app.win_w / sc
	lh := app.win_h / sc
	mut bw := lw * 0.5
	if bw < 300 {
		bw = 300
	}
	if bw > 800 {
		bw = 800
	}
	if bw > lw - 20 {
		bw = lw - 20
	}
	if bw < 40 {
		bw = 40
	}
	bh := f32(26)
	x := (lw - bw) / 2
	y := (lh - bh) / 2
	total := app.item_list.scan_total
	done := app.item_list.scan_done
	pct := if total > 0 { f32(done) * 100.0 / f32(total) } else { 0.0 }
	// Frame: light border rectangle, dark inset track, green fill.
	app.gg.draw_rect_filled(x - 2, y - 2, bw + 4, bh + 4, gg.light_gray)
	app.gg.draw_rect_filled(x, y, bw, bh, gg.dark_gray)
	fill := bw * pct / 100.0
	if fill > 0 {
		app.gg.draw_rect_filled(x, y, fill, bh, gg.green)
	}
	mut cfg := gg.TextCfg{
		color: gg.white
		align: .center
		size:  18
	}
	line := 'Scanning... ${done}/${total}  (${pct:.0}%)'
	app.gg.draw_text(int(lw / 2), int(y + bh + 10), line, cfg)
	unsafe { line.free() }
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
		} else if !app.item_list.scanning {
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
	// Scan warnings: one line with the count; `W` toggles the full list
	// (capped at 20 visible rows). Same per-frame free rule as above.
	if app.item_list.scan_warnings.len > 0 {
		warn_line := 'scan warnings: ${app.item_list.scan_warnings.len}  [W]'
		app.gg.draw_text(10, y, warn_line, cfg)
		unsafe { warn_line.free() }
		y += 22
		if app.show_warn_flag {
			for wi, w in app.item_list.scan_warnings {
				if wi >= 20 {
					more_line := '  ... ${app.item_list.scan_warnings.len - 20} more'
					app.gg.draw_text(10, y, more_line, cfg)
					unsafe { more_line.free() }
					break
				}
				row := '  ${w}'
				app.gg.draw_text(10, y, row, cfg)
				unsafe { row.free() }
				y += 22
			}
		}
	}
	if app.status != '' {
		app.gg.draw_text(10, y, app.status, cfg)
	}
	if app.item_list.scanning {
		draw_scan_progress(app)
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

// Win32 queries used to size and place the startup window on the monitor
// the app opens on. Declared here because this V checkout has no win32 vlib.
// The structs are plain V structs with the exact C layouts, passed through
// `voidptr` (implicit conversion in C): re-declaring the C tag types would
// collide with the windows.h definitions.
$if windows {
	struct WinPoint {
		x i32
		y i32
	}

	struct WinRect {
		left   i32
		top    i32
		right  i32
		bottom i32
	}

	struct WinMonitorInfo {
		cb         u32
		rc_monitor WinRect
		rc_work    WinRect
		flags      u32
	}

	fn C.GetCursorPos(voidptr) bool
	fn C.MonitorFromRect(voidptr, u32) voidptr
	fn C.MonitorFromWindow(voidptr, u32) voidptr
	fn C.GetMonitorInfoW(voidptr, voidptr) bool
	fn C.GetSystemMetrics(int) int

	// gg already declares C.RECT and C.GetWindowRect with these exact
	// signatures; repeating them identically keeps V's cross-module
	// deduplication happy.
	@[typedef]
	struct C.RECT {
		left   i32
		top    i32
		right  i32
		bottom i32
	}

	fn C.GetWindowRect(hwnd voidptr, rect &C.RECT) int
	fn C.SetWindowPos(hwnd voidptr, hwnd_insert_after voidptr, x int, y int, cx int, cy int, flags u32) int
}

// X11: sokol_app.h already includes X11/Xlib.h in the generated C and gg
// links libX11, so these declarations need no extra setup (same pattern as
// the Win32 declarations above; XOpenDisplay repeats gg's declaration
// exactly, which V's cross-module deduplication requires).
$if linux {
	fn C.XOpenDisplay(voidptr) voidptr
	fn C.XCloseDisplay(voidptr) int
	fn C.XGetDefault(voidptr, charptr, charptr) charptr
}

// monitor_rect returns the pixel rect (x, y, w, h) of the monitor the app
// should open on: the monitor under the mouse cursor, falling back to the
// primary display. Returns zeros where the query is unavailable.
fn monitor_rect() (int, int, int, int) {
	$if windows {
		mut pt := WinPoint{}
		if C.GetCursorPos(voidptr(&pt)) {
			// A zero-size rect at the cursor makes MonitorFromRect behave
			// exactly like MonitorFromPoint.
			rc := WinRect{
				left:   pt.x
				top:    pt.y
				right:  pt.x
				bottom: pt.y
			}
			// MONITOR_DEFAULTTONEAREST == 2
			hmon := C.MonitorFromRect(voidptr(&rc), 2)
			mut mi := WinMonitorInfo{ cb: u32(unsafe { sizeof(WinMonitorInfo) }) }
			if hmon != unsafe { nil } && C.GetMonitorInfoW(hmon, voidptr(&mi)) {
				return mi.rc_monitor.right - mi.rc_monitor.left, mi.rc_monitor.top, mi.rc_monitor.right - mi.rc_monitor.left, mi.rc_monitor.bottom - mi.rc_monitor.top
			}
		}
		// SM_CXSCREEN == 0, SM_CYSCREEN == 1 (primary display fallback)
		return 0, 0, C.GetSystemMetrics(0), C.GetSystemMetrics(1)
	} $else {
		// macOS/Linux: gg exposes the primary screen size on every platform
		// (XRandR on X11, NSScreen on macOS); the window manager places and
		// centers the window itself.
		ss := gg.screen_size()
		return 0, 0, ss.width, ss.height
	}
}

// startup_window_size returns the initial window size: one third of the
// monitor width and one half of its height. Falls back to the historical
// 1024x768 where the monitor query is unavailable.
//
// On X11 the monitor query is in physical pixels while sokol_app scales the
// requested window size by Xft.dpi / 96, so the fraction has to be converted
// back to logical units first (see x11_window_scale).
fn startup_window_size() (int, int) {
	_, _, mw, mh := monitor_rect()
	if mw <= 0 || mh <= 0 {
		return 1024, 768
	}
	mut w := mw / 3
	mut h := mh / 2
	$if linux {
		mut scale := x11_window_scale()
		if scale < 0.01 {
			scale = 1.0
		}
		w = int(f64(mw) / 3.0 / scale)
		h = int(f64(mh) / 2.0 / scale)
	}
	return w, h
}

// x11_window_scale returns the factor sokol_app multiplies the requested
// window size by on X11: it reads the `Xft.dpi` X resource and divides it by
// 96, while gg.screen_size() reports the physical XRandR resolution. On a
// 3840x2160 desktop at 200% scaling (Xft.dpi 192) the unconverted size would
// therefore come out twice as large as intended. Returns 1.0 when no display
// or no `Xft.dpi` resource is available, matching sokol's own fallback.
$if linux {
	fn x11_window_scale() f64 {
		display := C.XOpenDisplay(0)
		if display == unsafe { nil } {
			return 1.0
		}
		mut scale := 1.0
		res := C.XGetDefault(display, c'Xft', c'dpi')
		if unsafe { res != nil } {
			dpi := unsafe { res.vstring() }.int()
			if dpi > 0 {
				scale = f64(dpi) / 96.0
			}
		}
		C.XCloseDisplay(display)
		return scale
	}
}

// window_settings_path returns the file that remembers the window placement
// (written on exit, read at startup). Empty on platforms without placement
// persistence.
fn window_settings_path() string {
	$if windows {
		// os.getenv returns a plain string in this V version: empty means unset.
		appdata := os.getenv('APPDATA')
		dir_base := if appdata != '' { appdata } else { os.home_dir() }
		dir := os.join_path(dir_base, 'vviewer')
		return os.join_path(dir, 'window.txt')
	} $else {
		return ''
	}
}

// placement_save stores the current window rect ("left top width height")
// so the next launch reopens where the user left it. Skipped in fullscreen:
// the fullscreen rect is not a placement worth restoring.
fn placement_save() {
	$if windows {
		if sapp.is_fullscreen() {
			return
		}
		hwnd := sapp.win32_get_hwnd()
		if hwnd == unsafe { nil } {
			return
		}
		mut rc := C.RECT{}
		if C.GetWindowRect(hwnd, &rc) == 0 {
			return
		}
		settings := window_settings_path()
		os.mkdir_all(os.dir(settings)) or { return }
		txt := '${rc.left} ${rc.top} ${rc.right - rc.left} ${rc.bottom - rc.top}'
		os.write_file(settings, txt) or {}
	}
}

// window_is_first_launch reports whether the viewer never completed a run
// on this account (no placement file). Only then the 1/3 x 1/2 centered
// default is applied: on every later launch Windows itself restores the
// previous window placement, and touching it would fight that memory.
fn window_is_first_launch() bool {
	return !os.exists(window_settings_path())
}

// apply_first_launch_placement sizes the window to 1/3 x 1/2 of the monitor
// it was created on and centers it there. Runs in app_init on the first
// launch only. No-op off Windows (the gg default size is kept).
fn apply_first_launch_placement() {
	$if windows {
		hwnd := sapp.win32_get_hwnd()
		if hwnd == unsafe { nil } {
			return
		}
		mut mi := WinMonitorInfo{ cb: u32(unsafe { sizeof(WinMonitorInfo) }) }
		// MONITOR_DEFAULTTONEAREST == 2
		if !C.GetMonitorInfoW(C.MonitorFromWindow(hwnd, 2), voidptr(&mi)) {
			return
		}
		mw := mi.rc_monitor.right - mi.rc_monitor.left
		mh := mi.rc_monitor.bottom - mi.rc_monitor.top
		if mw <= 0 || mh <= 0 {
			return
		}
		ww := mw / 3
		wh := mh / 2
		wx := mi.rc_monitor.left + (mw - ww) / 2
		wy := mi.rc_monitor.top + (mh - wh) / 2
		// SWP_NOZORDER == 0x0004
		C.SetWindowPos(hwnd, unsafe { nil }, wx, wy, ww, wh, 0x0004)
	}
}

// Placement is a window rectangle (physical pixels) read back from the file
// written by placement_save().
$if windows {
	struct Placement {
	mut:
		x int
		y int
		w int
		h int
	}

	// saved_placement reads the saved rectangle ("left top width height"). It
	// fails when the file is missing or malformed, the size is not positive, or
	// the rectangle no longer intersects the desktop (for example a monitor was
	// disconnected since the last run), so the caller falls back to the default
	// startup placement.
	fn saved_placement() !Placement {
		txt := os.read_file(window_settings_path()) or {
			return error('no saved placement')
		}
		mut f := []string{}
		for part in txt.split(' ') {
			if part != '' {
				f << part
			}
		}
		if f.len != 4 {
			return error('malformed saved placement')
		}
		p := Placement{
			x: f[0].int()
			y: f[1].int()
			w: f[2].int()
			h: f[3].int()
		}
		if p.w <= 0 || p.h <= 0 {
			return error('non-positive saved size')
		}
		// SM_XVIRTUALSCREEN, SM_YVIRTUALSCREEN, SM_CXVIRTUALSCREEN,
		// SM_CYVIRTUALSCREEN: reject a rectangle entirely off the desktop.
		vx := C.GetSystemMetrics(76)
		vy := C.GetSystemMetrics(77)
		vw := C.GetSystemMetrics(78)
		vh := C.GetSystemMetrics(79)
		if p.x + p.w <= vx || p.y + p.h <= vy || p.x >= vx + vw || p.y >= vy + vh {
			return error('saved placement is off-screen')
		}
		return p
	}

	// apply_saved_placement moves/resizes the window to the saved rectangle.
	// Runs in app_init on every launch after the first.
	fn apply_saved_placement(x int, y int, w int, h int) {
		hwnd := sapp.win32_get_hwnd()
		if hwnd == unsafe { nil } {
			return
		}
		// SWP_NOZORDER == 0x0004
		C.SetWindowPos(hwnd, unsafe { nil }, x, y, w, h, 0x0004)
	}
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
	$if windows {
		if app.place_first {
			apply_first_launch_placement()
		} else if app.place_restore {
			apply_saved_placement(app.place_x, app.place_y, app.place_w, app.place_h)
		}
	}
	if app.scan_pending.len > 0 {
		// Command-line paths are scanned incrementally now that the window
		// (and its progress bar) exists.
		app.item_list.scan_begin(app.scan_pending)
	}
	if !app.item_list.scanning {
		app.load_current_image()
	}
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
		placement_save()
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
			placement_save()
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
		if ev.key_code == .w {
			app.show_warn_flag = !app.show_warn_flag
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

	// drag & drop: restart the scan with the dropped paths (dropping while a
	// scan is running cancels the previous one: its workers are stopped and
	// joined before the new scan starts). The list is then filled
	// incrementally by app_update, with the progress bar visible.
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
		app.item_list.scan_abort()
		app.item_list.scan_begin(paths)
	}
}

// run_scan_check implements `viewer --check <paths...>`: a headless scan
// that prints the drawable items found and every scan warning, then exits
// without opening a window. Used to verify a folder/zip tree (and to see
// exactly what was skipped) from a console.
fn run_scan_check(paths []string) {
	if paths.len == 0 {
		println('usage: viewer --check <file|folder|zip> ...')
		return
	}
	mut il := Item_list{}
	t0 := time.clock()
	il.get_items_list(paths)
	elapsed := time.clock() - t0
	println('--- scan report ---')
	println('drawable images: ${il.n_item}  (scan ${elapsed} ms)')
	for it in il.lst {
		if it.drawable {
			// Every branch interpolates so `src` is always an owned string
			// that can be freed under the no-GC build.
			src := if it.need_extract {
				'zip: ${it.path} :: ${it.name}'
			} else if it.path != '' {
				'${it.path}${il.path_sep}${it.name}'
			} else {
				'${it.name}'
			}
			println('  ${src}')
			unsafe { src.free() }
		}
	}
	for w in il.scan_warnings {
		println('warning: ${w}')
	}
	if il.last_error != '' {
		println('error: ${il.last_error}')
	}
}

fn main() {
	// Headless scan check mode: no window, no sokol context.
	check_idx := os.args.index('--check')
	if check_idx >= 0 {
		run_scan_check(os.args[check_idx + 1..])
		return
	}
	mut app := &App{ player: NullPlayer{} }
	// Command-line paths are scanned after the window opens (see app_init)
	// so the scan progress bar is visible for them too.
	app.scan_pending = os.args[1..]
	// First launch: 1/3 x 1/2 of the monitor, centered (applied in app_init).
	// Later launches: Windows restores the previous placement by itself, so
	// the window must not be touched.
	app.place_first = window_is_first_launch()
	mut win_w := 0
	mut win_h := 0
	$if windows {
		win_w, win_h = startup_window_size()
		// On a later launch, restore the rectangle saved on exit. Creating the
		// window at the saved size avoids a visible resize; the position is
		// applied in app_init (sokol cannot place the window itself).
		if !app.place_first {
			if sp := saved_placement() {
				app.place_restore = true
				app.place_x = sp.x
				app.place_y = sp.y
				app.place_w = sp.w
				app.place_h = sp.h
				win_w = sp.w
				win_h = sp.h
			}
		}
	} $else {
		win_w, win_h = startup_window_size()
	}

	app.gg = gg.new_context(
		width:             win_w
		height:            win_h
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
