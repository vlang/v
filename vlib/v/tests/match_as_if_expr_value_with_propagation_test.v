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
