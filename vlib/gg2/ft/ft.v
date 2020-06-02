module ft

import sokol.sfons
import gx
import os

const (
	default_font_size = 24
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
}

pub struct Config {
	font_path string
}

pub fn new(c Config) ?&FT{
	if c.font_path == '' || !os.exists(c.font_path) {
		println('failed to load font "$c.font_path"')
		return none
	}
	bytes := os.read_bytes(c.font_path) or {
		println('failed to load font "$c.font_path"')
		return none
	}
	s := &C.sgl_desc_t{}
	C.sgl_setup(s)
	fons :=sfons.create(512, 512, 1)
	return &FT{
		fons : fons
		font_normal: C.fonsAddFontMem(fons, 'sans', bytes.data, bytes.len, false)
	}

}

pub fn (gg &FT) draw_text(x, y int, text string, cfg gx.TextCfg) {
	/*
	gg.fons.set_font(gg.font_normal)
	gg.fons.set_size(f32(cfg.size))
	ascender := f32(0.0)
	descender := f32(0.0)
	lh := f32(0.0)
	gg.fons.vert_metrics(&ascender, &descender, &lh)
	color:= C.sfons_rgba(cfg.color.r, cfg.color.g, cfg.color.b, 255)
	C.fonsSetColor(gg.fons, color)
	C.fonsDrawText(gg.fons, x, y, text.str, 0)
	*/
}

pub fn (ctx &FT) draw_text_def(x, y int, text string) {
	cfg := gx.TextCfg {
		color: gx.black
		size: default_font_size
		align: gx.align_left
	}
	ctx.draw_text(x, y, text, cfg)
}

pub fn (mut gg FT) init_font() {
	// TODO
	////gg.fons =g_fons
	//gg.font_normal=g_font_normal
}


