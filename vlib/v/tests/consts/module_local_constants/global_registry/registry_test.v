@[has_globals]
module registry

struct Record {
	value int
}

__global registry = Record{ value: 7 }

const value = 42

fn test_global_named_after_module_stays_a_value() {
	assert registry.value == 7
	assert value == 42
}
