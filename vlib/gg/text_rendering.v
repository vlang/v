// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import sokol.sfons
import gx
import os

const (
	default_font_size = 16
)

struct FT {
pub:
	fons &C.FONScontext

	font_normal int
	scale f32 = 1.0
}

struct FTConfig {
	font_path string
	scale f32 = 1.0
	font_size int
}

fn new_ft(c FTConfig) ?&FT{
	if c.font_path == '' {
		// Load default font
	}
	if c.font_path == '' || !os.exists(c.font_path) {
		println('failed to load font "$c.font_path"')
		return none
	}
	bytes := os.read_bytes(c.font_path) or {
		println('failed to load font "$c.font_path"')
		return none
	}
	fons := sfons.create(512, 512, 1)
	return &FT{
		fons : fons
		font_normal: C.fonsAddFontMem(fons, 'sans', bytes.data, bytes.len, false)
		scale: c.scale
	}

}

pub fn (ctx &Context) draw_text(x, y int, text string, cfg gx.TextCfg) {
	if !ctx.font_inited {
		return
	}
	ctx.ft.fons.set_font(ctx.ft.font_normal)
	scale := if ctx.ft.scale == 0 { f32(1) } else { ctx.ft.scale }
	size := if cfg.size == 0 { gg.default_font_size } else { cfg.size }
	ctx.ft.fons.set_size(scale * f32(size))
	if cfg.align == gx.align_right {
		C.fonsSetAlign(ctx.ft.fons, C.FONS_ALIGN_RIGHT | C.FONS_ALIGN_TOP)
	}
	else {
		C.fonsSetAlign(ctx.ft.fons, C.FONS_ALIGN_LEFT | C.FONS_ALIGN_TOP)
	}
	color := C.sfons_rgba(cfg.color.r, cfg.color.g, cfg.color.b, 255)
	C.fonsSetColor(ctx.ft.fons, color)
	ascender := f32(0.0)
	descender := f32(0.0)
	lh := f32(0.0)
	ctx.ft.fons.vert_metrics(&ascender, &descender, &lh)
	C.fonsDrawText(ctx.ft.fons, x*scale, y*scale, text.str, 0) // TODO: check offsets/alignment
}

pub fn (ctx &Context) draw_text_def(x, y int, text string) {
	cfg := gx.TextCfg {
		color: gx.black
		size: default_font_size
		align: gx.align_left
	}
	ctx.draw_text(x, y, text, cfg)
}

/*
pub fn (mut gg FT) init_font() {
}
*/

pub fn (ft &FT) flush(){
	sfons.flush(ft.fons)
}

pub fn (ctx &Context) text_width(s string) int {
	if !ctx.font_inited {
		return 0
	}
	mut buf := [4]f32
	C.fonsTextBounds(ctx.ft.fons, 0, 0, s.str, 0, buf)
	return int((buf[2] - buf[0]) / ctx.scale)
}

pub fn (ctx &Context) text_height(s string) int {
	if !ctx.font_inited {
		return 0
	}
	mut buf := [4]f32
	C.fonsTextBounds(ctx.ft.fons, 0, 0, s.str, 0, buf)
	return int((buf[3] - buf[1]) / ctx.scale)
}

pub fn (ctx &Context) text_size(s string) (int, int) {
	if !ctx.font_inited {
		return 0,0
	}
	mut buf := [4]f32
	C.fonsTextBounds(ctx.ft.fons, 0, 0, s.str, 0, buf)
	return int((buf[2] - buf[0]) / ctx.scale), int((buf[3] - buf[1]) / ctx.scale)
}

