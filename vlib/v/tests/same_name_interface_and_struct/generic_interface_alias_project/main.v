module main

import genstream as csv
import encoding.csv as ecsv

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

// `csv.Reader[T]` is the generic `genstream.Reader` interface here, while the
// `encoding.csv.Reader` struct is loaded too.
fn collect[T](mut r csv.Reader[T]) []T {
	mut out := []T{}
	for {
		v := r.next() or { break }
		out << v
	}
	return out
}

fn main() {
	mut rows := ecsv.new_reader('a,b\n')
	println(rows.read() or { []string{} })
	mut c := Counter{
		max: 3
	}
	println(collect(mut c))
}
