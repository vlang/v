interface Widget {
}

interface ResizableWidget {
	Widget
	resize(x int, y int) int
}

fn draw(w Widget) {
	// w.resize(10, 20) // <- this won't work, since all Widgets may not implement resize()

	// however, we can check if the underlying type of w implements a different interface:
	if w is ResizableWidget {
		assert w is WidgetB
		rw := w as ResizableWidget
		assert rw is WidgetB
		// if so, we can now safely call that extra method
		assert rw.resize(10, 20) == 200
	} else {
		assert w is WidgetA
	}
}

fn draw_ref(w &Widget) {
	if w is ResizableWidget {
		rw := w as ResizableWidget
		assert rw.resize(10, 20) == 200
	} else {
		assert false
	}
}

// implements Widget, but not ResizableWidget
struct WidgetA {
}

// implements both Widget and ResizableWidget
struct WidgetB {
}

fn (w WidgetB) resize(x int, y int) int {
	return x * y
}

fn test_interface_runtime_conversions() {
	draw(WidgetA{})
	draw(WidgetB{})
}
