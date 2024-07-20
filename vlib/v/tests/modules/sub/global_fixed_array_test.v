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
