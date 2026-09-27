interface Layout {
mut:
	id int
}

interface Widget {
mut:
	id int
}

struct Item {
mut:
	id int
}

fn (w &Widget) read_id() int { return w.id }

fn read(l Layout) int {
	p := l
	if p is Item { return Widget(p).read_id() }
	return 0
}

fn test_readonly_interface_cast() {
	assert read(Item{ id: 42 }) == 42
}
