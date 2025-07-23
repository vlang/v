module gx

// TODO: remove these, and use the enum everywhere
pub const align_left = HorizontalAlign.left
pub const align_right = HorizontalAlign.right

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
