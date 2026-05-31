struct Point {
	x int
}

type Primitive = []int | []string | []Point | bool | int | string | Point

fn wrap_first[T](values []T) Primitive {
	if values.len > 0 {
		first := values[0]
		return Primitive(first)
	}
	return Primitive(false)
}

fn check(value Primitive) {
	match value {
		[]int {
			int_value := wrap_first(value)
			if int_value is int {
				println('int:${int_value}')
			} else {
				println('int:bad')
			}
		}
		[]string {
			string_value := wrap_first(value)
			if string_value is string {
				println('string:${string_value}')
			} else {
				println('string:bad')
			}
		}
		[]Point {
			point_value := wrap_first(value)
			if point_value is Point {
				println('point:${point_value.x}')
			} else {
				println('point:bad')
			}
		}
		else {}
	}
}

fn main() {
	ints := []int{len: 1, init: 7}
	check(Primitive(ints))

	strings := []string{len: 1, init: 'abc'}
	check(Primitive(strings))

	points := []Point{len: 1, init: Point{
		x: 3
	}}
	check(Primitive(points))
}
