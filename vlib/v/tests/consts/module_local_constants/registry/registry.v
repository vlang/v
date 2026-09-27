module registry

struct Record {
	value int
}

const registry = Record{ value: 42 }
const answer = 44

fn registry_value() int {
	return registry.value
}

fn registry_answer() int {
	return registry.answer
}
