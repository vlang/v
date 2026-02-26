// vtest vflags: -autofree
module main

const no_style = 'no_style'
const no_string = ''

struct TextStyleParams {
	id             string = no_style
	font_name      string = no_string
	color          string
	size           int = -1
	align          int
	vertical_align int
}

fn draw_text(text string, ts TextStyleParams) string {
	return text + ts.color
}

fn test_autofree_if_expr_call_arg_with_struct_init() {
	files_dropped := true
	got := draw_text(if files_dropped {
		'Empty listbox. Drop files here ...'
	} else {
		''
	}, color: 'gray')
	assert got == 'Empty listbox. Drop files here ...gray'
}
