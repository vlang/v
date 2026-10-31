module registry

struct Record {
	value int
}

const registry = Record{ value: 42 }
const derived = registry.runtime_answer + 1
const runtime_answer = make_runtime_answer()
const answer = 44

fn make_runtime_answer() int {
	return 45
}

fn registry_value() int {
	return registry.value
}

fn registry_answer() int {
	return registry.answer
}

fn registry_derived() int {
	return registry.derived
}
