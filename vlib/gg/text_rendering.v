// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import sokol.sfons
import gx
import os

struct FT {
pub:
	fons &C.FONScontext

	font_normal int
	font_bold int
	font_mono int
	font_italic int
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
	$if !android {
		if c.font_path == '' || !os.exists(c.font_path) {
			println('failed to load font "$c.font_path"')
			return none
		}
	}

	mut bytes := []byte{}
	$if android {
		bytes = os.read_apk_asset(c.font_path) or {
			println('failed to load font "$c.font_path"')
			return none
		}
	} $else {
		bytes = os.read_bytes(c.font_path) or {
			println('failed to load font "$c.font_path"')
			return none
		}
	}

	bold_path := 'SFNS-bold.ttf'// c.font_path.replace('.ttf', '-bold.ttf')
	bytes_bold := os.read_bytes(bold_path) or {
		println('failed to load font "$bold_path"')
		bytes
	}
	mono_path := '/System/Library/Fonts/SFNSMono.ttf'// c.font_path.replace('.ttf', '-bold.ttf')
	bytes_mono:= os.read_bytes(mono_path) or {
		println('failed to load font "$mono_path"')
		bytes
	}
	italic_path := '/System/Library/Fonts/SFNSItalic.ttf'
	bytes_italic:= os.read_bytes(italic_path) or {
		println('failed to load font "$italic_path"')
		bytes
	}
	fons := sfons.create(512, 512, 1)
	return &FT{
		fons : fons
		font_normal: C.fonsAddFontMem(fons, 'sans', bytes.data, bytes.len, false)
		font_bold: C.fonsAddFontMem(fons, 'sans', bytes_bold.data, bytes_bold.len, false)
		font_mono: C.fonsAddFontMem(fons, 'sans', bytes_mono.data, bytes_mono.len, false)
		font_italic: C.fonsAddFontMem(fons, 'sans', bytes_italic.data, bytes_italic.len, false)
		scale: c.scale
	}

}

fn (ctx &Context) set_cfg(cfg gx.TextCfg) {
	if !ctx.font_inited {
		return
	}
	if cfg.bold {
		ctx.ft.fons.set_font(ctx.ft.font_bold)
	}
	else if cfg.mono {
		ctx.ft.fons.set_font(ctx.ft.font_mono)
	}
	else if cfg.italic {
		ctx.ft.fons.set_font(ctx.ft.font_italic)
	}
	else {
		ctx.ft.fons.set_font(ctx.ft.font_normal)
	}
	scale := if ctx.ft.scale == 0 { f32(1) } else { ctx.ft.scale }
	size := if cfg.mono { cfg.size - 2 } else { cfg.size }
	ctx.ft.fons.set_size(scale * f32(size))
	C.fonsSetAlign(ctx.ft.fons, int(cfg.align) | int(cfg.vertical_align))
	color := C.sfons_rgba(cfg.color.r, cfg.color.g, cfg.color.b, 255)
	C.fonsSetColor(ctx.ft.fons, color)
	ascender := f32(0.0)
	descender := f32(0.0)
	lh := f32(0.0)
	ctx.ft.fons.vert_metrics(&ascender, &descender, &lh)
}

pub fn (ctx &Context) draw_text(x, y int, text_ string, cfg gx.TextCfg) {
	if !ctx.font_inited {
		eprintln('gg: draw_text(): font not initialized')
		return
	}
	//text := text_.trim_space() // TODO remove/optimize
	mut text := text_
	if text.contains('\t') {
		text = text.replace('\t', '    ')
	}
	ctx.set_cfg(cfg)
	scale := if ctx.ft.scale == 0 { f32(1) } else { ctx.ft.scale }
	C.fonsDrawText(ctx.ft.fons, x * scale, y * scale, text.str, 0) // TODO: check offsets/alignment
}

pub fn (ctx &Context) draw_text_def(x, y int, text string) {
	ctx.draw_text(x, y, text, {})
}

/*
pub fn (mut gg FT) init_font() {
}
*/

pub fn (ft &FT) flush(){
	sfons.flush(ft.fons)
}

pub fn (ctx &Context) text_width(s string) int {
	// ctx.set_cfg(cfg) TODO
	if !ctx.font_inited {
		return 0
	}
	mut buf := [4]f32{}
	C.fonsTextBounds(ctx.ft.fons, 0, 0, s.str, 0, buf)
	if s.ends_with(' ') {
		return int((buf[2] - buf[0]) / ctx.scale) + ctx.text_width('i') // TODO fix this in fontstash?
	}
	return int((buf[2] - buf[0]) / ctx.scale)
}

pub fn (ctx &Context) text_height(s string) int {
	// ctx.set_cfg(cfg) TODO
	if !ctx.font_inited {
		return 0
	}
	mut buf := [4]f32{}
	C.fonsTextBounds(ctx.ft.fons, 0, 0, s.str, 0, buf)
	return int((buf[3] - buf[1]) / ctx.scale)
}

pub fn (ctx &Context) text_size(s string) (int, int) {
	// ctx.set_cfg(cfg) TODO
	if !ctx.font_inited {
		return 0,0
	}
	mut buf := [4]f32{}
	C.fonsTextBounds(ctx.ft.fons, 0, 0, s.str, 0, buf)
	return int((buf[2] - buf[0]) / ctx.scale), int((buf[3] - buf[1]) / ctx.scale)
}

