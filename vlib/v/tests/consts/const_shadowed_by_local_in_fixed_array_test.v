// Params and locals must shadow same-named consts inside fixed array literals,
// both for consts of the current module and for consts of an imported module,
// even when that module is imported only as `_` (issue #28910).
import math as _

const scale = 3
const doubled = scale * 2
const pair = [scale, scale]!

struct Addr {
	octets [6]u8
}

struct Point {
	x f64
}

struct Points {
	items [1]Point
}

fn new_addr(a u8, b u8, c u8, d u8, e u8, f u8) Addr {
	return Addr{
		octets: [a, b, c, d, e, f]!
	}
}

fn fixed_from_params(e f64, pi f64) [2]f64 {
	return [e, pi]!
}

fn fixed_from_locals() [2]f64 {
	e := 4.0
	pi := 5.0
	return [e, pi]!
}

fn fixed_from_exprs(e f64, pi f64) [2]f64 {
	return [e + 1, -pi]!
}

fn nested_fixed(e f64) [1][2]f64 {
	return [[e, e]!]!
}

fn struct_in_fixed(e f64) Points {
	return Points{
		items: [Point{
			x: e
		}]!
	}
}

fn slice_of_param(pair [2]int) []int {
	return pair[..]
}

fn same_module_const(scale int) [2]int {
	return [scale, doubled]!
}

fn same_module_fixed_const(scale int) [2][2]int {
	return [pair, [scale, scale]!]!
}

fn test_imported_const_is_shadowed_by_param() {
	assert new_addr(0, 1, 2, 3, 4, 5).octets == [u8(0), 1, 2, 3, 4, 5]!
	assert fixed_from_params(4, 5) == [4.0, 5]!
	assert fixed_from_locals() == [4.0, 5]!
	assert fixed_from_exprs(4, 5) == [5.0, -5]!
	assert nested_fixed(4) == [[4.0, 4]!]!
	assert struct_in_fixed(4).items[0].x == 4.0
}

fn test_const_is_shadowed_by_param() {
	assert slice_of_param([1, 2]!) == [1, 2]
	assert same_module_const(1) == [1, 6]!
	assert same_module_fixed_const(1) == [[3, 3]!, [1, 1]!]!
}
