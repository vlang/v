module main

interface Point {
	coords() (int, int)
}

struct SPoint {
	x int
	y int
}

fn (s SPoint) coords() (int, int) {
	return s.x, s.y
}

fn new_spoints() []SPoint {
	return [
		SPoint{10, 10},
		SPoint{15, 11},
		SPoint{1, 22},
	]
}

fn sum_x(points []Point) int {
	mut total := 0
	for point in points {
		x, _ := point.coords()
		total += x
	}
	return total
}

fn test_pass_array_of_structs_to_array_of_interface_param() {
	spoints := new_spoints()
	assert sum_x(spoints) == 26
}

fn test_append_array_of_structs_to_array_of_interface() {
	spoints := new_spoints()
	mut points := []Point{}
	points << spoints
	assert sum_x(points) == 26
}

interface Cols {
	cols() string
}

struct Data implements Cols {}

fn (d Data) cols() string {
	return 'data'
}

fn print_first_last(cols []Cols) string {
	return '${cols[0].cols()}:${cols[cols.len - 1].cols()}'
}

fn test_pass_len_initialized_array_of_implements_struct_to_array_of_interface_param() {
	data := []Data{len: 3}
	assert print_first_last(data) == 'data:data'
}

fn test_pass_array_of_struct_pointers_to_array_of_interface_param() {
	data := []&Data{len: 3, init: &Data{}}
	assert print_first_last(data) == 'data:data'
}
