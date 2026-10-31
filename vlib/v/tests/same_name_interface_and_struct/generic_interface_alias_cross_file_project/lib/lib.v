module lib

import genstream as csv

// collect is declared where `csv` is the `genstream` module, while its caller
// binds `csv` to `encoding.csv`, whose `Reader` is a struct.
pub fn collect[T](mut r csv.Reader[T]) []T {
	mut out := []T{}
	for {
		v := r.next() or { break }
		out << v
	}
	return out
}
