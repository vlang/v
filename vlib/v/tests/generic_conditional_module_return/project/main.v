import dep

type Box[T] = []T
type Boxes[T] = []T

struct Point {
	id int
}

fn main() {
	for first in [true, false] {
		item := Point{ id: 7 }
		box := if first { dep.make(item) } else { dep.make(Point{ id: 9 }) }
		assert box.value.id == if first { 7 } else { 9 }
		assert box.marker == 41
		boxes := if first { dep.many(item) } else { dep.many(Point{ id: 9 }) }
		assert boxes[0].value.id == if first { 7 } else { 9 }
		assert boxes[0].marker == 41
		text := if first { dep.make('left') } else { dep.make('right') }
		assert text.value == if first { 'left' } else { 'right' }
	}
}
