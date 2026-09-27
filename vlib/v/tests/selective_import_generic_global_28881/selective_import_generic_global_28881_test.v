module main

import foo28881
import bar28881

// https://github.com/vlang/v/issues/28881
fn test_iterate_selectively_imported_generic_global() {
	bar28881.fill()
	mut values := []int{}
	for item in bar_value.iter() {
		values << item.value
	}
	assert values == [1, 2, 3]
	assert bar28881.sum() == 6
}

fn test_selectively_imported_generic_global_in_containers() {
	bar28881.fill()
	mut values := []int{}
	for foo in bar_values {
		for item in foo.iter() {
			values << item.value
		}
	}
	for item in bar_by_name['a'].iter() {
		values << item.value
	}
	assert values == [10, 20, 30, 100, 200]
}

fn test_assign_selectively_imported_generic_global_from_another_module() {
	bar28881.fill()
	bar_value = foo28881.Foo[bar28881.Bar]{
		items: [bar28881.Bar{4}, bar28881.Bar{5}]
	}
	assert bar_value.items.len == 2
	assert bar28881.sum() == 9
	assert bar_plain.value == 7
	assert bar_plain.doubled() == 14
	assert bar_pair.key == 'k'
	assert bar_pair.val.value == 8
}
