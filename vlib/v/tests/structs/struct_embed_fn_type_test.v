type FnClick = fn () string

struct Widget {
mut:
	x     int
	y     int
	click FnClick = click
}

fn click() string {
	return 'click me'
}

struct Button {
	Widget
	title string
}

fn test_struct_embed_fn_type() {
	mut button := Button{
		title: 'Click me'
	}

	button.x = 3
	ret := button.click()

	println(ret)
	assert ret == 'click me'
}
