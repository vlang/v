module main

struct Todo {
	title string
}

type TodosStore = []Todo

fn (mut ts TodosStore) add_todo(td Todo) {
	ts << td
}

fn (ts TodosStore) get_todo(id int) ?Todo {
	if id > ts.len {
		return none
	}
	return ts[id - 1]
}

fn test_option_assert_eq_none_then_if_guard() {
	td := Todo{
		title: 'title1'
	}
	mut ts := TodosStore([]Todo{})
	assert ts.len == 0
	assert ts.get_todo(5) == none

	ts.add_todo(td)

	if gtd := ts.get_todo(1) {
		assert gtd.title == 'title1'
	} else {
		assert false
	}
}
