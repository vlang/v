// BEAM Backend: Graphics/GPU modules are not applicable on the BEAM VM.
// The BEAM backend focuses on server-side, networking, and concurrent workloads.
// All functions return safe defaults (empty structs, 0, false).
// For GUI on BEAM, consider using wx (Erlang's native GUI toolkit).
//
// Text rendering requires fontstash (C library) which is not available on BEAM.
// Text measurement functions return 0; draw_text functions are no-ops.
module gg

struct FT {
pub:
	font_normal int
	font_bold   int
	font_mono   int
	font_italic int
pub mut:
	fonts_map map[string]int
	scale     f32 = 1.0
}

fn (ft &FT) flush() {}

pub enum HorizontalAlign {
	left   = 0
	center = 1
	right  = 2
}

pub enum VerticalAlign {
	top      = 0
	middle   = 1
	bottom   = 2
	baseline = 3
}

fn new_ft(c FTConfig) ?&FT {
	return &FT{}
}

pub fn (ctx &Context) set_text_cfg(cfg TextCfg) {}

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

pub fn (ctx &Context) draw_text2(p DrawTextParams) {}

pub fn (ctx &Context) draw_text(x int, y int, text_ string, cfg TextCfg) {}

pub fn (ctx &Context) draw_text_def(x int, y int, text string) {}

pub fn (ctx &Context) text_width(s string) int {
	return 0
}

pub fn (ctx &Context) text_height(s string) int {
	return 0
}

pub fn (ctx &Context) text_size(s string) (int, int) {
	return 0, 0
}

pub fn (ctx &Context) text_width_f(s string) f32 {
	return 0.0
}
