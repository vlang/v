@[heap]
struct Window {
mut:
	width int
}

struct Holder {
	window &Window
}

fn (h &Holder) get_window() &Window { return h.window }

fn test_pointer_field_return() {
	h := Holder{ window: &Window{ width: 1 } }
	mut win := h.get_window()
	win.width = 42
	assert h.window.width == 42
}
