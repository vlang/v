module gg

pub enum HorizontalAlign {
	left
	center
	right
}

pub enum VerticalAlign {
	top
	middle
	bottom
	baseline
}

pub fn (mut ctx Context) draw_text(x int, y int, text_ string, cfg TextCfg) {
	ctx.context.fillStyle = cfg.color.to_css_string().str
	ctx.context.font = cfg.to_css_string().str
	ctx.context.fillText(text_.str, x, y)
}
