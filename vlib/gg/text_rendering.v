// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

struct FTConfig {
	font_path             string
	custom_bold_font_path string
	scale                 f32 = 1.0
	font_size             int
	bytes_normal          []u8
	bytes_bold            []u8
	bytes_mono            []u8
	bytes_italic          []u8
}

struct StringToRender {
	x    int
	y    int
	text string
	cfg  TextCfg
}

@[if debug_font ?]
fn debug_font_println(s string) {
	println(s)
}

@[markused; params]
pub struct TextCfg {
pub:
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

// to_css_string returns a CSS compatible string of the TextCfg `cfg`.
// For example: `'mono 14px serif'`.
pub fn (cfg &TextCfg) to_css_string() string {
	mut font_style := ''
	if cfg.bold {
		font_style += 'bold '
	}
	if cfg.mono {
		font_style += 'mono '
	}
	if cfg.italic {
		font_style += 'italic '
	}
	return '${font_style} ${cfg.size}px ${cfg.family}'
}
