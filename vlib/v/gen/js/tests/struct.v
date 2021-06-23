module main

struct Int {
mut:
	value int
	test  map[string]int
	hello []int
}

fn (mut i Int) add(value int) {
	i.value += value
}

fn (i Int) get() int {
	return i.value
}

struct Config {
	foo int
	bar string
}

fn use_config(c Config) {
}

fn main() {
	mut a := Int{
		value: 10
	}
	a.add(5)
	println(a) // 15
	mut b := Int{}
	b.add(10)
	println(b.get()) // 10
	use_config(Config{2, 'bar'})
	use_config(
		foo: 2
		bar: 'bar'
	)
}
