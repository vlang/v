type Item = []Item | int

fn (item Item) str() string {
	return match item {
		int { item.str() }
		[]Item { '[' + item.map(it.str()).join(',') + ']' }
	}
}

fn test_sumtype_array_map_string() {
	item := Item([Item(1), Item([Item(2), Item(3)])])
	assert item.str() == '[1,[2,3]]'
	match item {
		[]Item {
			assert item.map('x').join(',') == 'x,x'
			assert item.map(fn (value Item) string { return value.str() }).join(';') == '1;[2,3]'
		}
		else {
			assert false
		}
	}
}
