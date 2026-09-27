module main

import encoding.csv
import genstream as gs
import lib

struct Counter {
mut:
	n   int
	max int
}

fn (mut c Counter) next() ?int {
	if c.n >= c.max {
		return none
	}
	c.n++
	return c.n
}

fn main() {
	mut rows := csv.new_reader('a,b\n')
	println(rows.read() or { []string{} })
	mut c := Counter{
		max: 3
	}
	println(lib.collect(mut c))
	mut r := gs.Reader[int](Counter{ max: 2 })
	println(lib.collect(mut r))
}
