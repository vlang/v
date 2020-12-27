module main

fn test_autoprint_string_vargs() {
	add_s('a')
	assert true
	add_s('a', 'b', 'c')
	assert true
}

fn add_s(column string, other_columns ...string) {
	println(column)
	println(other_columns)
	eprintln(column)
	eprintln(other_columns)
}

//
fn test_autoprint_int_vargs() {
	add_i(1)
	assert true
	add_i(1, 2, 3)
	assert true
}

fn add_i(column int, other_columns ...int) {
	println(column)
	println(other_columns)
	eprintln(column)
	eprintln(other_columns)
}

//
struct Point {
	x int
	y int
}

fn test_autoprint_struct_vargs() {
	add_point(Point{1, 2})
	assert true
	add_point(Point{1, 2}, Point{3, 4}, Point{5, 6})
	assert true
}

fn add_point(column Point, other_columns ...Point) {
	println(column)
	println(other_columns)
	eprintln(column)
	eprintln(other_columns)
}
