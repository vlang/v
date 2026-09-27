module registry

struct Record {
	value int
}

const registry = Record{ value: 42 }

fn registry_value() int {
	return registry.value
}
