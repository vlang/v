module ft

import sokol.sfons
import gx
import os

const (
	default_font_size = 20
)
// TODO remove globals
/*
__global g_fons &C.FONScontext
__global g_font_normal int
__global g_font_path string
*/


pub struct FT   {
	pub:
	fons &C.FONScontext

	font_normal int
	scale f32 = 1.0
}

pub struct Config {
	font_path string
	scale f32 = 1.0
	font_size int
}

pub fn new(c Config) ?&FT{
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

pub fn (ft &FT) draw_text(x, y int, text string, cfg gx.TextCfg) {
	ft.fons.set_font(ft.font_normal)
	ft.fons.set_size(2.0 * ft.scale * f32(cfg.size))
	C.fonsSetAlign(ft.fons, C.FONS_ALIGN_LEFT | C.FONS_ALIGN_TOP)
	color := C.sfons_rgba(cfg.color.r, cfg.color.g, cfg.color.b, 255)
	C.fonsSetColor(ft.fons, color)
	ascender := f32(0.0)
	descender := f32(0.0)
	lh := f32(0.0)
	ft.fons.vert_metrics(&ascender, &descender, &lh)
	C.fonsDrawText(ft.fons, x, y, text.str, 0) // TODO: check offsets/alignment
}

pub fn (ft &FT) draw_text_def(x, y int, text string) {
	cfg := gx.TextCfg {
		color: gx.black
		size: default_font_size
		align: gx.align_left
	}
	ft.draw_text(x, y, text, cfg)
}

pub fn (mut gg FT) init_font() {
	// TODO
	////gg.fons =g_fons
	//gg.font_normal=g_font_normal
}

pub fn (ft &FT) flush(){
	sfons.flush(ft.fons)
}
