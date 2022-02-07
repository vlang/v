struct Window {
mut:
	widgets []Widget
}

struct Widget {
mut:
	parent &Window = voidptr(0)
}

fn test_circular_2() {
	mut window := &Window{}

	mut btn := &Widget{}
	btn.parent = window

	window.widgets << btn

	dump(window)
	assert '$window'.len < 100
}
