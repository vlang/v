module gx

import gg

// TODO: remove these, and use the enum everywhere
pub const align_left = HorizontalAlign.left
pub const align_right = HorizontalAlign.right

pub type TextCfg = gg.TextCfg

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
