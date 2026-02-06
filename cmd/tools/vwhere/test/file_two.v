module test

const x = 10
const y = 100
const z = 1000

pub enum Public {
	importable
	shareable
}

enum Private {
	storable
	downloadable
}

interface Drinker {
	drink(int) string
}

pub fn sprint(dkr Drinker) {
	println(drk.drink(6))
}

fn some_function_name(foo string, bar int) string {
	return 'baz'
}
