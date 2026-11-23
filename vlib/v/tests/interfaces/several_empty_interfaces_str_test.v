// vtest vflags: -memory-limit 1500
// Every type implements an empty interface, other interfaces among them. Printing a
// value of one used to expand the printers of all the others into each other, so
// the C code, and cgen's memory, grew factorially with the number of such
// interfaces: four took cgen past 6 GB.
interface AnyA {}

interface AnyB {}

interface AnyC {}

interface AnyD {}

struct Point {
	x int
}

fn test_values_of_several_empty_interfaces_print() {
	values := [AnyA(7), AnyA('text'), AnyA(Point{3})]
	assert '${values[0]}' == 'AnyA(7)'
	assert '${values}' == "[AnyA(7), AnyA('text'), AnyA(Point{\n    x: 3\n})]"
	b := AnyB(1.5)
	assert '${b}' == 'AnyB(1.5)'
	c := [AnyC(true)]
	assert c[0] == true
	d := AnyD(Point{4})
	assert d is Point
}
