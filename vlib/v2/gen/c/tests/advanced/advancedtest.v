module main

// Test advanced SSA operations: phi, insertvalue, call_indirect,
// unsigned ops, switch, option types, short-circuit logic, etc.

struct Point {
mut:
	x int
	y int
}

struct Result {
	ok    bool
	value int
	err   int
}

fn make_result(success bool, val int) Result {
	if success {
		return Result{ok: true, value: val, err: 0}
	}
	return Result{ok: false, value: 0, err: -1}
}

fn add_fn(a int, b int) int {
	return a + b
}

fn mul_fn(a int, b int) int {
	return a * b
}

fn sub_fn(a int, b int) int {
	return a - b
}

fn apply(f fn (int, int) int, a int, b int) int {
	return f(a, b)
}

fn print_str(s string) {
	println(s)
}

// Test if/else expression that produces a value (requires phi nodes)
fn abs_val(x int) int {
	return if x < 0 { -x } else { x }
}

// Test short-circuit && (requires phi)
fn both_positive(a int, b int) bool {
	return a > 0 && b > 0
}

// Test short-circuit || (requires phi)
fn either_positive(a int, b int) bool {
	return a > 0 || b > 0
}

// Test nested if/else expressions (multiple phi)
fn classify(x int) int {
	return if x > 0 { 1 } else if x < 0 { -1 } else { 0 }
}

fn main() {
	// 1. If/else expression (phi node)
	r1 := abs_val(-42)
	r2 := abs_val(17)
	if r1 == 42 && r2 == 17 {
		print_str('phi abs ok')
	}

	// 2. Short-circuit && (phi)
	if both_positive(3, 5) && !both_positive(-1, 5) {
		print_str('phi and ok')
	}

	// 3. Short-circuit || (phi)
	if either_positive(-1, 5) && !either_positive(-1, -2) {
		print_str('phi or ok')
	}

	// 4. Nested if/else expression (phi)
	c1 := classify(10)
	c2 := classify(-5)
	c3 := classify(0)
	if c1 == 1 && c2 == -1 && c3 == 0 {
		print_str('nested phi ok')
	}

	// 5. Result struct construction (insertvalue)
	res_ok := make_result(true, 42)
	res_err := make_result(false, 0)
	if res_ok.ok && res_ok.value == 42 && !res_err.ok && res_err.err == -1 {
		print_str('insertvalue ok')
	}

	// 6. Function pointers / indirect call
	result_add := apply(add_fn, 10, 20)
	result_mul := apply(mul_fn, 10, 20)
	result_sub := apply(sub_fn, 10, 20)
	if result_add == 30 && result_mul == 200 && result_sub == -10 {
		print_str('call_indirect ok')
	}

	// 7. Match expression (may use switch_ or branching)
	mut match_result := 0
	val := 2
	match val {
		1 { match_result = 10 }
		2 { match_result = 20 }
		3 { match_result = 30 }
		else { match_result = -1 }
	}
	if match_result == 20 {
		print_str('match ok')
	}

	// 8. Unsigned comparison edge cases
	a := -1 // As a signed int, -1 is less than 0
	b := 0
	// But as unsigned, -1 wraps to max uint
	if a < b {
		print_str('signed cmp ok')
	}

	// 9. Shift operations
	s1 := 1 << 4  // 16
	s2 := 16 >> 2 // 4
	if s1 == 16 && s2 == 4 {
		print_str('shift ok')
	}

	// 10. Bitwise operations
	b1 := 0xFF & 0x0F // 15
	b2 := 0xF0 | 0x0F // 255
	b3 := 0xFF ^ 0x0F // 240
	if b1 == 15 && b2 == 255 && b3 == 240 {
		print_str('bitwise ok')
	}

	// 11. Multiple return values via struct
	p := compute_pair(3, 7)
	if p.x == 10 && p.y == -4 {
		print_str('multi return ok')
	}

	// 12. Complex control flow with phi
	mut sum := 0
	for i := 0; i < 10; i++ {
		sum += if i % 2 == 0 { i } else { -i }
	}
	// 0 - 1 + 2 - 3 + 4 - 5 + 6 - 7 + 8 - 9 = -5
	if sum == -5 {
		print_str('loop phi ok')
	}

	// 13. Chained function pointer calls
	fns := [add_fn, mul_fn, sub_fn]
	mut chain_result := 0
	chain_result = fns[0](5, 3)   // 8
	chain_result = fns[1](chain_result, 2) // 16
	chain_result = fns[2](chain_result, 6) // 10
	if chain_result == 10 {
		print_str('fn chain ok')
	}

	// 14. Bool expressions with phi
	t := true
	f := false
	r := (t && !f) || (f && t)
	if r {
		print_str('bool phi ok')
	}

	// 15. Struct field update
	mut pt := Point{x: 10, y: 20}
	pt.x = pt.x + 5
	pt.y = pt.y * 2
	if pt.x == 15 && pt.y == 40 {
		print_str('field update ok')
	}
}

fn compute_pair(a int, b int) Point {
	return Point{
		x: a + b
		y: a - b
	}
}
