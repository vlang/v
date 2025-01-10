import gg

pub type WindowResizeFn = fn (window &Window, w int, h int)

@[heap]
pub struct Window {
	id string = '_window_'
pub mut:
	resize_fn WindowResizeFn = unsafe { nil }
}

@[params]
pub struct WindowParams {
pub:
	on_resize WindowResizeFn = unsafe { nil }
}

fn window_resized(event gg.Event, mut w &Window) {
	window_width, window_height := 200, 100

	if w.resize_fn != WindowResizeFn(0) {
		w.resize_fn(w, window_width, window_height)
	}
}

fn on_event(e &gg.Event, mut w Window) {
	window_resized(e, mut w)
}

fn on_resize(window &Window, w int, h int) {
	assert w == 200
	assert h == 100
}

fn test_main() {
	e := gg.Event{}
	mut w := &Window{
		resize_fn: on_resize
	}

	on_event(e, mut w)
	assert true
}
