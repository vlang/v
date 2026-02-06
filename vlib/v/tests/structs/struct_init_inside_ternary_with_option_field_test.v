pub struct Color {
pub mut:
	r u8
	g u8
	b u8
	a u8 = 255
}

pub const color_transparent = Color{0, 0, 0, 0}

pub struct MarkdownStyle {
pub:
	table_row_alt ?Color
}

pub struct MarkdownCfg {
pub:
	id    string
	style MarkdownStyle
}

struct AResult {
	x int
}

struct App {
	mode bool
}

fn (app App) method(params MarkdownCfg) AResult {
	return AResult{123}
}

fn another_fn() AResult {
	return AResult{456}
}

fn test_main() {
	app := App{}
	x := if app.mode {
		[app.method(id: 'abc')]
	} else {
		[another_fn()]
	}
	assert x == [AResult{456}]
}
