// Regression test for https://github.com/vlang/v/issues/28000
// A `match` whose arms use `!`/`?` propagation, used as the value of an
// `if`-expression (or directly), used to emit invalid C (`_t = ;`) because
// the calls in the match arms were not marked as having their return used.

struct First {}

struct Second {}

type Node = First | Second

fn lower_first(_ First) !int {
	return 1
}

fn lower_second(_ Second) !int {
	return 2
}

fn opt_first(_ First) ?int {
	return 10
}

fn opt_second(_ Second) ?int {
	return 20
}

// match inside an if-guard, assigned to a variable (the original repro)
fn select_value(node ?Node) !int {
	result := if value := node {
		match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}
	} else {
		0
	}
	return result
}

// parenthesized match value: `( match .. { .. } )` keeps an ast.ParExpr wrapper
fn select_value_paren(node ?Node) !int {
	result := if value := node {
		(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

// unsafe-wrapped match value: `( unsafe { match .. { .. } } )` keeps an
// ast.UnsafeExpr (inside an ast.ParExpr) around the match
fn select_value_unsafe(node ?Node) !int {
	result := if value := node {
		(unsafe {
			match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
		})
	} else {
		0
	}
	return result
}

// cast-wrapped match value: `i64(match .. { .. })` keeps an ast.CastExpr
// around the match (which is also nested in a void-context if-branch)
fn select_value_cast(node ?Node) !i64 {
	result := if value := node {
		i64(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		i64(0)
	}
	return result
}

// composed wrappers: cast around unsafe around match, `i64(unsafe { match .. })`
fn select_value_cast_unsafe(node ?Node) !i64 {
	result := if value := node {
		i64(unsafe {
			match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
		})
	} else {
		i64(0)
	}
	return result
}

// match on the right of an infix expression: `1 + (match .. { .. })`
fn select_value_infix_right(node ?Node) !int {
	result := if value := node {
		1 + (match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

// match on the left of an infix expression: `(match .. { .. }) + 10`
fn select_value_infix_left(node ?Node) !int {
	result := if value := node {
		(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}) + 10
	} else {
		0
	}
	return result
}

fn wrap(x int) int {
	return x * 10
}

// match as a call argument: `wrap(match .. { .. })`
fn select_value_callarg(node ?Node) !int {
	result := if value := node {
		wrap(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

// match nested inside a call argument that is itself a call:
// `wrap(wrap(match .. { .. }))`
fn select_value_nested_callarg(node ?Node) !int {
	result := if value := node {
		wrap(wrap(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}))
	} else {
		0
	}
	return result
}

// match inside an infix expression inside a call argument:
// `wrap(1 + (match .. { .. }))`
fn select_value_callarg_infix(node ?Node) !int {
	result := if value := node {
		wrap(1 + (match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}))
	} else {
		0
	}
	return result
}

// match as an array-literal element: `[match .. { .. }]`
fn select_value_arraylit(node ?Node) ![]int {
	result := if value := node {
		[
			match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			},
		]
	} else {
		[0]
	}
	return result
}

struct Circle {
	r int
}

struct Square {
	s int
}

type Shape = Circle | Square

fn make_circle(r int) !Shape {
	return Circle{r}
}

// as-cast wrapped match value: `(match .. { .. }) as Circle` keeps an
// ast.AsCast (around an ast.ParExpr) with propagating arms
fn select_value_ascast(node ?int) !int {
	shape := if v := node {
		(match v {
			0 { make_circle(v)! }
			else { make_circle(v + 1)! }
		}) as Circle
	} else {
		Circle{99}
	}
	return shape.r
}

// composed wrappers: as-cast around unsafe around match,
// `(unsafe { match .. }) as Circle`
fn select_value_ascast_unsafe(node ?int) !int {
	shape := if v := node {
		(unsafe {
			match v {
				0 { make_circle(v)! }
				else { make_circle(v + 1)! }
			}
		}) as Circle
	} else {
		Circle{99}
	}
	return shape.r
}

// match with `?` option propagation
fn select_opt(node ?Node) ?int {
	result := if value := node {
		match value {
			First { opt_first(value)? }
			Second { opt_second(value)? }
		}
	} else {
		0
	}
	return result
}

// match used directly as the return value
fn direct_match(node Node) !int {
	return match node {
		First { lower_first(node)! }
		Second { lower_second(node)! }
	}
}

// match assigned directly to a variable
fn assign_match(node Node) !int {
	x := match node {
		First { lower_first(node)! }
		Second { lower_second(node)! }
	}
	return x
}

fn test_match_as_if_expr_value_with_propagation() {
	assert select_value(First{})! == 1
	assert select_value(Second{})! == 2
	assert select_value(none) or { -1 } == 0
}

fn test_parenthesized_match_as_if_expr_value_with_propagation() {
	assert select_value_paren(First{})! == 1
	assert select_value_paren(Second{})! == 2
	assert select_value_paren(none) or { -1 } == 0
}

fn test_unsafe_wrapped_match_as_if_expr_value_with_propagation() {
	assert select_value_unsafe(First{})! == 1
	assert select_value_unsafe(Second{})! == 2
	assert select_value_unsafe(none) or { -1 } == 0
}

fn test_cast_wrapped_match_as_if_expr_value_with_propagation() {
	assert select_value_cast(First{})! == i64(1)
	assert select_value_cast(Second{})! == i64(2)
	assert select_value_cast(none) or { i64(-1) } == i64(0)
}

fn test_cast_unsafe_wrapped_match_as_if_expr_value_with_propagation() {
	assert select_value_cast_unsafe(First{})! == i64(1)
	assert select_value_cast_unsafe(Second{})! == i64(2)
	assert select_value_cast_unsafe(none) or { i64(-1) } == i64(0)
}

fn test_as_cast_wrapped_match_as_if_expr_value_with_propagation() {
	assert select_value_ascast(0)! == 0
	assert select_value_ascast(5)! == 6
	assert select_value_ascast(none) or { -1 } == 99
}

fn test_as_cast_unsafe_wrapped_match_as_if_expr_value_with_propagation() {
	assert select_value_ascast_unsafe(0)! == 0
	assert select_value_ascast_unsafe(5)! == 6
	assert select_value_ascast_unsafe(none) or { -1 } == 99
}

fn test_infix_right_match_as_if_expr_value_with_propagation() {
	assert select_value_infix_right(First{})! == 2
	assert select_value_infix_right(Second{})! == 3
	assert select_value_infix_right(none) or { -1 } == 0
}

fn test_infix_left_match_as_if_expr_value_with_propagation() {
	assert select_value_infix_left(First{})! == 11
	assert select_value_infix_left(Second{})! == 12
	assert select_value_infix_left(none) or { -1 } == 0
}

fn test_call_argument_match_as_if_expr_value_with_propagation() {
	assert select_value_callarg(First{})! == 10
	assert select_value_callarg(Second{})! == 20
	assert select_value_callarg(none) or { -1 } == 0
}

fn test_nested_call_argument_match_as_if_expr_value_with_propagation() {
	assert select_value_nested_callarg(First{})! == 100
	assert select_value_nested_callarg(Second{})! == 200
	assert select_value_nested_callarg(none) or { -1 } == 0
}

fn test_call_argument_infix_match_as_if_expr_value_with_propagation() {
	assert select_value_callarg_infix(First{})! == 20
	assert select_value_callarg_infix(Second{})! == 30
	assert select_value_callarg_infix(none) or { -1 } == 0
}

fn test_array_literal_match_as_if_expr_value_with_propagation() {
	assert select_value_arraylit(First{})! == [1]
	assert select_value_arraylit(Second{})! == [2]
	assert select_value_arraylit(none) or { [-1] } == [0]
}

fn test_match_as_if_expr_value_with_option_propagation() {
	assert select_opt(First{})? == 10
	assert select_opt(Second{})? == 20
	assert select_opt(none) or { -1 } == 0
}

fn test_match_as_return_and_assign_value_with_propagation() {
	assert direct_match(First{})! == 1
	assert direct_match(Second{})! == 2
	assert assign_match(First{})! == 1
	assert assign_match(Second{})! == 2
}
