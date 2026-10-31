@[has_globals]
module bar28881

import foo28881 { Foo, Pair, Plain }

pub struct Bar {
pub:
	value int
}

// Every global type below names its `foo28881` type only through the selective import.
__global bar_value Foo[Bar]
__global bar_values []Foo[Bar]
__global bar_by_name map[string]Foo[Bar]
__global bar_plain Plain
__global bar_pair Pair[string, Bar]

// fill initializes the module globals from inside the declaring module.
pub fn fill() {
	bar_value = Foo[Bar]{
		items: [Bar{1}, Bar{2}, Bar{3}]
	}
	bar_values = [Foo[Bar]{
		items: [Bar{10}]
	}, Foo[Bar]{
		items: [Bar{20}, Bar{30}]
	}]
	bar_by_name = {
		'a': Foo[Bar]{
			items: [Bar{100}, Bar{200}]
		}
	}
	bar_plain = Plain{
		value: 7
	}
	bar_pair = Pair[string, Bar]{
		key: 'k'
		val: Bar{8}
	}
}

// sum iterates the generic global inside its declaring module.
pub fn sum() int {
	mut total := 0
	for item in bar_value.iter() {
		total += item.value
	}
	return total
}
