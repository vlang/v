struct Point {
	x int
	y int
}

fn test_array_of_ptr_str() {
	point_1 := &Point{1, 2}
	point_2 := &Point{3, 4}
	point_3 := &Point{5, 6}
	points := [point_1, point_2, point_3]
	assert '${points}' == '[&Point{
    x: 1
    y: 2
}, &Point{
    x: 3
    y: 4
}, &Point{
    x: 5
    y: 6
}]
'.trim_indent()
}
