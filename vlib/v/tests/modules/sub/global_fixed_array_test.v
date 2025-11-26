@[has_globals]
module main

import sub.foo

struct DummyStruct {
	dummy_item i32
}

// vfmt off
__global (
	d [foo.num_elements]DummyStruct
)
// vfmt on

const f = [foo.num_elements]DummyStruct{}

fn test_main() {
	assert dump(foo.num_elements) == 2
	assert dump(f) == [foo.num_elements]DummyStruct{}
	assert dump(d) == [foo.num_elements]DummyStruct{}
}

struct DummyStruct2 {
	dummy_item1 int
	dummy_item2 i32
	dummy_item3 u8
}

const m = [foo.num_elements]DummyStruct2{}

fn test_many_dummy_fields_with_diff_types() {
	assert dump(m) == [foo.num_elements]DummyStruct2{}
}
