struct Button {
	width  int
	height int
}

enum Color {
	red
	blue
	yellow
}

struct ColoredButton {
	Button
	color Color
}

fn change_color(cb ColoredButton, color Color) ColoredButton {
	return ColoredButton{
		...cb
		color: color
	}
}

fn test_struct_update_with_embed_field() {
	red_button := ColoredButton{Button{100, 100}, .red}
	blue_button := change_color(red_button, .blue)

	println(red_button)
	println(blue_button)

	assert blue_button == ColoredButton{Button{100, 100}, .blue}
}
