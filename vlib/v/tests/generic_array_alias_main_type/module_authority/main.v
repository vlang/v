module main

import dep

type Box[T] = []T

fn main() {
	box := dep.box[int](7)
	assert box.value == 7
}
