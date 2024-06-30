@[has_globals]
module main

const num_elements = 10

struct DummyStruct {
}

__global (
	d [num_elements]DummyStruct
)

fn test_main() {
	a := dump(d)
	assert a.len == num_elements
}
