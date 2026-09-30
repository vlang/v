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

fn update(mut l Layout) {
	if mut l is Widget {
		mut w := l as Widget
		w.id = 42
	}
}

fn test_mut_as_alias() {
	mut item := &Item{ id: 1 }
	mut l := Layout(item)
	update(mut l)
	assert item.id == 42
}
