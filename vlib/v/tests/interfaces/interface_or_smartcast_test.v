interface Widget {
mut:
	id int
}

struct First {
mut:
	id int
}

struct Second {
mut:
	id int
}

fn test_mut_interface_or_condition() {
	mut children := [Widget(First{ id: 1 }), Widget(Second{ id: 2 })]
	mut total := 0
	for mut child in children {
		if child is First || child is Second { total += child.id }
	}
	assert total == 3
}

fn test_negative_mut_interface_condition_without_else() {
	mut children := [Widget(First{ id: 1 }), Widget(Second{ id: 2 })]
	mut total := 0
	for mut child in children {
		if child !is First { total += child.id }
		if !(child is Second) { total += child.id }
		for child !is First {
			total += child.id
			break
		}
	}
	assert total == 5
}
