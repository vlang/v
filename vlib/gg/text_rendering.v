// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import sokol.sfons
import gx
import os

const (
	default_font_size = 20
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
	ctx.ft.fons.set_size(2.0 * ctx.ft.scale * f32(cfg.size))
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
	C.fonsDrawText(ctx.ft.fons, x*ctx.ft.scale, y*ctx.ft.scale, text.str, 0) // TODO: check offsets/alignment
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

pub fn (ft &Context) text_width(s string) int {
	return 0
}

pub fn (ft &Context) text_height(s string) int {
	return 0
}

pub fn (ft &Context) text_size(s string) (int, int) {
	return 0,0
}

