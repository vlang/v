module test

const (
	x = 10
	y = 100
	z = 1000
)

pub enum Public {
	importable
	shareable
}

enum Private {
	storable
	donwloadable
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
