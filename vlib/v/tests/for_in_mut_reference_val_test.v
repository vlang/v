pub struct AA {
	id string
}

pub struct BB {
pub mut:
	arr []&AA
}

fn test_for_in_mut_reference_selector_val() {
	bb := BB{
		arr: [&AA{
			id: 'Test1'
		}, &AA{
			id: 'Test2'
		}]
	}

	mut ret := []string{}
	for mut aa in bb.arr {
		println(aa.id)
		ret << aa.id
	}
	println(ret)
	assert ret == ['Test1', 'Test2']
}

struct Thing {}

struct Bag {
pub mut:
	things []&Thing
}

pub fn test_for_in_mut_array_of_reference_values() {
	mut bag := &Bag{}
	bag.things << &Thing{}

	for mut thing in bag.things {
		println(thing)
		assert '$thing' == '&Thing{}'
		mut fixed_thing := unsafe { &thing }
		println(fixed_thing)
		assert '$fixed_thing' == '&Thing{}'
	}
}
