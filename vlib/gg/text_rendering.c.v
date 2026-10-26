// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import fontstash
import sokol.gfx
import os
import os.font

pub struct FT {
pub:
	fons        &fontstash.Context = unsafe { nil }
	font_normal int
	font_bold   int
	font_mono   int
	font_italic int
pub mut:
	fonts_map map[string]int
	scale     f32               = 1.0
	rt        &FonsRenderTarget = unsafe { nil }
}

// FonsRenderTarget holds the atlas state used by the fontstash render callbacks.
// It is allocated *before* the fontstash context, because fontstash calls
// renderCreate synchronously from fonsCreateInternal, i.e. while the FT that
// will own this target does not exist yet.
struct FonsRenderTarget {
mut:
	ctx       &Context = unsafe { nil }
	atlas_img gfx.Image
	atlas_smp gfx.Sampler
	atlas_w   int
	atlas_h   int
}

pub enum HorizontalAlign {
	left   = C.FONS_ALIGN_LEFT
	center = C.FONS_ALIGN_CENTER
	right  = C.FONS_ALIGN_RIGHT
}

pub enum VerticalAlign {
	top      = C.FONS_ALIGN_TOP
	middle   = C.FONS_ALIGN_MIDDLE
	bottom   = C.FONS_ALIGN_BOTTOM
	baseline = C.FONS_ALIGN_BASELINE
}

const initial_text_atlas_size = int($d('gg_text_buff_size', 2048))

fn fons_render_create(uptr voidptr, width i32, height i32) i32 {
	mut rt := unsafe { &FonsRenderTarget(uptr) }
	rt.create_atlas(int(width), int(height))
	return 1
}

fn (mut rt FonsRenderTarget) create_atlas(width int, height int) {
	if width <= 0 || height <= 0 {
		return
	}
	if rt.atlas_img.id > 0 {
		gfx.destroy_image(rt.atlas_img)
	}
	mut img_desc := gfx.ImageDesc{
		type:         ._2d
		width:        width
		height:       height
		num_mipmaps:  1
		pixel_format: .r8
		usage:        .dynamic
	}
	rt.atlas_img = gfx.make_image(&img_desc)
	if rt.atlas_smp.id == 0 {
		mut smp_desc := gfx.SamplerDesc{
			min_filter: .linear
			mag_filter: .linear
			wrap_u:     .clamp_to_edge
			wrap_v:     .clamp_to_edge
		}
		rt.atlas_smp = gfx.make_sampler(&smp_desc)
	}
	rt.atlas_w = width
	rt.atlas_h = height
	if rt.ctx != unsafe { nil } && rt.ctx.r != unsafe { nil } {
		rt.ctx.r.register_image(int(rt.atlas_img.id), unsafe { nil }, width, height, 1, 0)
	}
}

fn fons_render_resize(uptr voidptr, width i32, height i32) i32 {
	mut rt := unsafe { &FonsRenderTarget(uptr) }
	rt.create_atlas(int(width), int(height))
	return 1
}

fn fons_render_update(uptr voidptr, rect &i32, data &u8) {
	mut rt := unsafe { &FonsRenderTarget(uptr) }
	if rt.atlas_img.id == 0 || rt.atlas_w <= 0 || rt.atlas_h <= 0 {
		return
	}
	mut img_data := gfx.ImageData{}
	img_data.subimage[0][0] = gfx.Range{
		ptr:  voidptr(data)
		size: usize(rt.atlas_w * rt.atlas_h)
	}
	gfx.update_image(rt.atlas_img, &img_data)
	if rt.ctx != unsafe { nil } && rt.ctx.r != unsafe { nil } {
		// rect is {x, y, width, height} of the dirty atlas region; only that
		// region is copied into the CPU texture registry.
		mut rx := 0
		mut ry := 0
		mut rw := 0
		mut rh := 0
		unsafe {
			rx = rect[0]
			ry = rect[1]
			rw = rect[2]
			rh = rect[3]
		}
		rt.ctx.r.update_image_region(int(rt.atlas_img.id), voidptr(data), rx, ry, rw, rh, 1)
	}
}

fn fons_render_draw(uptr voidptr, verts &f32, tcoords &f32, colors &u32, nverts i32) {
	mut rt := unsafe { &FonsRenderTarget(uptr) }
	if rt.ctx == unsafe { nil } || rt.ctx.r == unsafe { nil } {
		return
	}
	if rt.ctx.pipeline == unsafe { nil } {
		return
	}
	ctx := rt.ctx
	ctx.r.enable_texture()
	ctx.r.texture(rt.atlas_img, rt.atlas_smp)
	ctx.r.load_pipeline(ctx.pipeline.alpha)
	ctx.r.begin_quads()
	for i in 0 .. int(nverts) {
		x := unsafe { verts[i * 2] }
		y := unsafe { verts[i * 2 + 1] }
		u := unsafe { tcoords[i * 2] }
		vv := unsafe { tcoords[i * 2 + 1] }
		c := unsafe { colors[i] }
		ctx.r.c1i(c)
		ctx.r.t2f(u, vv)
		ctx.r.v2f(x, y)
	}
	ctx.r.end()
	ctx.r.disable_texture()
}

fn fons_render_delete(uptr voidptr) {
	mut rt := unsafe { &FonsRenderTarget(uptr) }
	if rt.atlas_img.id > 0 {
		gfx.destroy_image(rt.atlas_img)
		rt.atlas_img = gfx.Image{}
	}
	if rt.atlas_smp.id > 0 {
		gfx.destroy_sampler(rt.atlas_smp)
		rt.atlas_smp = gfx.Sampler{}
	}
}

const max_text_atlas_size = 8192

fn expand_atlas_callback(uptr voidptr, error i32, _val i32) {
	if error != C.FONS_ATLAS_FULL {
		return
	}
	fons := unsafe { &fontstash.Context(uptr) }
	width, height := fons.get_atlas_size()
	mut next_width := if width > 0 { width } else { initial_text_atlas_size }
	mut next_height := if height > 0 { height } else { initial_text_atlas_size }
	if next_width < max_text_atlas_size {
		next_width = if next_width * 2 > max_text_atlas_size {
			max_text_atlas_size
		} else {
			next_width * 2
		}
	}
	if next_height < max_text_atlas_size {
		next_height = if next_height * 2 > max_text_atlas_size {
			max_text_atlas_size
		} else {
			next_height * 2
		}
	}
	if next_width == width && next_height == height {
		return
	}
	fons.expand_atlas(next_width, next_height)
}

fn new_ft(ctx &Context, c FTConfig) ?&FT {
	if c.font_path == '' {
		if c.bytes_normal.len > 0 {
			mut params := C.FONSparams{}
			params.width = initial_text_atlas_size
			params.height = initial_text_atlas_size
			params.flags = 1
			params.renderCreate = fons_render_create
			params.renderResize = fons_render_resize
			params.renderUpdate = fons_render_update
			params.renderDraw = fons_render_draw
			params.renderDelete = fons_render_delete
			rt := &FonsRenderTarget{ ctx: ctx }
			params.userPtr = voidptr(rt)
			fons := fontstash.create_internal(&params)
			bytes_normal := c.bytes_normal
			bytes_bold := if c.bytes_bold.len > 0 {
				c.bytes_bold
			} else {
				debug_font_println('setting bold variant to normal')
				bytes_normal
			}
			bytes_mono := if c.bytes_mono.len > 0 {
				c.bytes_mono
			} else {
				debug_font_println('setting mono variant to normal')
				bytes_normal
			}
			bytes_italic := if c.bytes_italic.len > 0 {
				c.bytes_italic
			} else {
				debug_font_println('setting italic variant to normal')
				bytes_normal
			}
			fons.set_error_callback(expand_atlas_callback, fons)
			return &FT{
				fons:        fons
				font_normal: fons.add_font_mem('sans', bytes_normal.clone(), true)
				font_bold:   fons.add_font_mem('sans', bytes_bold.clone(), true)
				font_mono:   fons.add_font_mem('sans', bytes_mono.clone(), true)
				font_italic: fons.add_font_mem('sans', bytes_italic.clone(), true)
				scale:       c.scale
				rt:          rt
			}
		} else {
			// Load default font
		}
	}

	if c.font_path == '' || !os.exists(c.font_path) {
		$if !android {
			println('failed to load font "${c.font_path}"')
			return none
		}
	}

	mut normal_path := c.font_path
	mut bytes := []u8{}
	$if android {
		// First try any filesystem paths
		bytes = os.read_bytes(c.font_path) or { []u8{} }
		if bytes.len == 0 {
			// ... then try the APK asset path
			bytes = os.read_apk_asset(c.font_path) or {
				println('failed to load font "${c.font_path}"')
				return none
			}
		}
	} $else {
		bytes = os.read_bytes(c.font_path) or {
			println('failed to load font "${c.font_path}"')
			return none
		}
	}
	mut bold_path := if c.custom_bold_font_path != '' {
		c.custom_bold_font_path
	} else {
		font.get_path_variant(c.font_path, .bold)
	}
	bytes_bold := os.read_bytes(bold_path) or {
		debug_font_println('failed to load font "${bold_path}"')
		bold_path = c.font_path
		bytes
	}
	mut mono_path := font.get_path_variant(c.font_path, .mono)
	bytes_mono := os.read_bytes(mono_path) or {
		debug_font_println('failed to load font "${mono_path}"')
		mono_path = c.font_path
		bytes
	}
	mut italic_path := font.get_path_variant(c.font_path, .italic)
	bytes_italic := os.read_bytes(italic_path) or {
		debug_font_println('failed to load font "${italic_path}"')
		italic_path = c.font_path
		bytes
	}
	mut params := C.FONSparams{}
	params.width = initial_text_atlas_size
	params.height = initial_text_atlas_size
	params.flags = 1
	params.renderCreate = fons_render_create
	params.renderResize = fons_render_resize
	params.renderUpdate = fons_render_update
	params.renderDraw = fons_render_draw
	params.renderDelete = fons_render_delete
	rt := &FonsRenderTarget{ ctx: ctx }
	params.userPtr = voidptr(rt)
	fons := fontstash.create_internal(&params)
	debug_font_println('Font used for font_normal : ${normal_path}')
	debug_font_println('Font used for font_bold   : ${bold_path}')
	debug_font_println('Font used for font_mono   : ${mono_path}')
	debug_font_println('Font used for font_italic : ${italic_path}')
	fons.set_error_callback(expand_atlas_callback, fons)
	return &FT{
		fons:        fons
		font_normal: fons.add_font_mem('sans', bytes.clone(), true)
		font_bold:   fons.add_font_mem('sans', bytes_bold.clone(), true)
		font_mono:   fons.add_font_mem('sans', bytes_mono.clone(), true)
		font_italic: fons.add_font_mem('sans', bytes_italic.clone(), true)
		scale:       c.scale
		rt:          rt
	}
}

// set_text_cfg sets the current text configuration
pub fn (ctx &Context) set_text_cfg(cfg TextCfg) {
	if !ctx.font_inited {
		return
	}
	if cfg.family != '' {
		// println('set text cfg family=${cfg.family}')
		mut f := ctx.ft.fonts_map[cfg.family]
		if f == 0 {
			// No such font in the cache yet, create it
			bytes := os.read_bytes(cfg.family) or {
				debug_font_println('failed to load font "${cfg.family}"')
				return
			}
			f = ctx.ft.fons.add_font_mem(cfg.family, bytes.clone(), true)
			unsafe {
				ctx.ft.fonts_map[cfg.family] = f
			}
		}
		ctx.ft.fons.set_font(f)
	} else if cfg.bold {
		ctx.ft.fons.set_font(ctx.ft.font_bold)
	} else if cfg.mono {
		ctx.ft.fons.set_font(ctx.ft.font_mono)
	} else if cfg.italic {
		ctx.ft.fons.set_font(ctx.ft.font_italic)
	} else {
		ctx.ft.fons.set_font(ctx.ft.font_normal)
	}
	scale := if ctx.ft.scale == 0 { f32(1) } else { ctx.ft.scale }
	size := if cfg.mono { cfg.size - 2 } else { cfg.size }
	ctx.ft.fons.set_size(scale * f32(size))
	ctx.ft.fons.set_align(int(cfg.align) | int(cfg.vertical_align))
	color := u32(cfg.color.r) | (u32(cfg.color.g) << 8) | (u32(cfg.color.b) << 16) | (u32(cfg.color.a) << 24)
	if cfg.color.a != 255 {
		ctx.r.load_pipeline(ctx.pipeline.alpha)
	}
	ctx.ft.fons.set_color(color)
	ascender := f32(0.0)
	descender := f32(0.0)
	lh := f32(0.0)
	ctx.ft.fons.vert_metrics(&ascender, &descender, &lh)
}

@[params]
pub struct DrawTextParams {
pub:
	x    int
	y    int
	text string

	color          Color           = black
	size           int             = 16
	align          HorizontalAlign = .left
	vertical_align VerticalAlign   = .top
	max_width      int
	family         string
	bold           bool
	mono           bool
	italic         bool
}

pub fn (ctx &Context) draw_text2(p DrawTextParams) {
	ctx.draw_text(p.x, p.y, p.text, TextCfg{
		color:          p.color
		size:           p.size
		align:          p.align
		vertical_align: p.vertical_align
		max_width:      p.max_width
		family:         p.family
		bold:           p.bold
		mono:           p.mono
		italic:         p.italic
	}) // TODO: perf once it's the only function to draw text
}

// draw_text draws the string in `text_` starting at top-left position `x`,`y`.
// Text settings can be provided with `cfg`.
pub fn (ctx &Context) draw_text(x int, y int, text_ string, cfg TextCfg) {
	$if macos {
		if ctx.native_rendering {
			if cfg.align == align_right {
				width := darwin_text_width_with_cfg(text_, cfg)
				// println('draw text ctx.height = ${ctx.height}')
				darwin_draw_string(x - width, ctx.height - y, text_, cfg)
			} else {
				darwin_draw_string(x, ctx.height - y, text_, cfg)
			}
			return
		}
	}
	if !ctx.font_inited {
		eprintln('gg: draw_text(): font not initialized')
		return
	}
	// text := text_.trim_space() // TODO: remove/optimize
	// mut text := text_
	// if text.contains('\t') {
	// text = text.replace('\t', '    ')
	// }
	ctx.set_text_cfg(cfg)
	scale := if ctx.ft.scale == 0 { f32(1) } else { ctx.ft.scale }
	ctx.ft.fons.draw_text(x * scale, y * scale, text_) // TODO: check offsets/alignment
}

// draw_text draws the string in `text_` starting at top-left position `x`,`y` using
// default text settings.
pub fn (ctx &Context) draw_text_def(x int, y int, text string) {
	ctx.draw_text(x, y, text)
}

// flush prepares the font for use.
pub fn (ft &FT) flush() {
	mut dirty := [4]int{}
	_ = ft.fons.validate_texture(&dirty[0])
}

@[inline]
fn (ctx &Context) text_metrics(s string) (f32, [4]f32) {
	mut bounds := [4]f32{}
	advance := ctx.ft.fons.text_bounds(0, 0, s, &bounds[0])
	return advance, bounds
}

// text_width returns the width of the `string` `s` in pixels.
pub fn (ctx &Context) text_width(s string) int {
	$if macos {
		if ctx.native_rendering {
			return darwin_text_width(s)
		}
	}
	// ctx.set_text_cfg(cfg) TODO
	if !ctx.font_inited {
		return 0
	}
	advance, _ := ctx.text_metrics(s)
	return int(advance / ctx.scale)
}

// text_height returns the height of the `string` `s` in pixels.
pub fn (ctx &Context) text_height(s string) int {
	// ctx.set_text_cfg(cfg) TODO
	if !ctx.font_inited {
		return 0
	}
	_, bounds := ctx.text_metrics(s)
	return int((bounds[3] - bounds[1]) / ctx.scale)
}

// text_size returns the width and height of the `string` `s` in pixels.
pub fn (ctx &Context) text_size(s string) (int, int) {
	// ctx.set_text_cfg(cfg) TODO
	if !ctx.font_inited {
		return 0, 0
	}
	advance, bounds := ctx.text_metrics(s)
	return int(advance / ctx.scale), int((bounds[3] - bounds[1]) / ctx.scale)
}

// text_width returns the width of the `string` `s` in pixels.
pub fn (ctx &Context) text_width_f(s string) f32 {
	$if macos {
		if ctx.native_rendering {
			return darwin_text_width(s)
		}
	}
	// ctx.set_text_cfg(cfg) TODO
	if !ctx.font_inited {
		return 0
	}
	advance, _ := ctx.text_metrics(s)
	return advance / ctx.scale
}
