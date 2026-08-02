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

fn side_effect() int {
	return 5
}

// an array-element match whose arm contains a nested *statement* match/if (with a
// void/empty branch) before the propagated value. The value-element flag must not
// leak into the nested statement, which is valid as a statement, not an expression.
fn select_value_arraylit_nested_stmt(node ?Node, cond bool) ![]int {
	result := if value := node {
		[
			match value {
				First {
					match cond {
						true { side_effect() }
						else {}
					}
					if cond {
						side_effect()
					}
					lower_first(value)!
				}
				Second {
					lower_second(value)!
				}
			},
		]
	} else {
		[0]
	}
	return result
}

struct Holder {
	value int
	other int
}

// match as a struct-literal field value: `Holder{ value: match .. { .. } }`
fn select_value_structinit(node ?Node) !Holder {
	result := if value := node {
		Holder{
			value: match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
			other: 100
		}
	} else {
		Holder{}
	}
	return result
}

// match as a map-literal value: `{'value': match .. { .. }}`
fn select_value_mapinit(node ?Node) !map[string]int {
	result := if value := node {
		{
			'value': match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
		}
	} else {
		{
			'value': 0
		}
	}
	return result
}

// match as a prefix-expression operand: `-(match .. { .. })`
fn select_value_prefix(node ?Node) !int {
	result := if value := node {
		-(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

// match as an index-expression operand: `values[match .. { .. }]`
fn select_value_index(node ?Node) !string {
	values := ['a', 'b', 'c']
	result := if value := node {
		values[match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}]
	} else {
		'x'
	}
	return result
}

// match on the left of a membership operator: `(match .. { .. }) in [1, 2]`
fn select_value_membership(node ?Node) !bool {
	result := if value := node {
		(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}) in [1, 2]
	} else {
		false
	}
	return result
}

struct Boxed {
	value int
}

fn boxed(v int) Boxed {
	return Boxed{v}
}

// match as a selector receiver: `(match .. { .. }).value`
fn select_value_selector(node ?Node) !int {
	result := if value := node {
		(match value {
			First { boxed(lower_first(value)!) }
			Second { boxed(lower_second(value)!) }
		}).value
	} else {
		0
	}
	return result
}

// match as the first (parenthesized) key of an inferred map: `{(match ..): v}`
fn select_value_mapkey(node ?Node) !map[int]string {
	result := if value := node {
		{
			(match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}): 'x'
		}
	} else {
		{
			0: 'x'
		}
	}
	return result
}

fn lower_first_array(_ First) ![]int {
	return [1, 1]
}

fn lower_second_array(_ Second) ![]int {
	return [2, 2]
}

// match as an array spread operand: `[...(match .. { .. })]`
fn select_value_spread(node ?Node) ![]int {
	result := if value := node {
		[...(match value {
			First { lower_first_array(value)! }
			Second { lower_second_array(value)! }
		})]
	} else {
		[0]
	}
	return result
}

fn make_holder_first(_ First) !Holder {
	return Holder{
		value: 1
	}
}

fn make_holder_second(_ Second) !Holder {
	return Holder{
		value: 2
	}
}

// match as a struct update operand: `Holder{ ...(match .. { .. }), other: 9 }`
fn select_value_struct_update(node ?Node) !Holder {
	result := if value := node {
		Holder{
			...(match value {
				First { make_holder_first(value)! }
				Second { make_holder_second(value)! }
			})
			other: 9
		}
	} else {
		Holder{}
	}
	return result
}

fn map_first(_ First) !map[string]int {
	return {
		'a': 1
	}
}

fn map_second(_ Second) !map[string]int {
	return {
		'a': 2
	}
}

// match as a map update operand: `{ ...(match .. { .. }), 'b': 5 }`
fn select_value_map_update(node ?Node) !map[string]int {
	result := if value := node {
		{
			...(match value {
				First { map_first(value)! }
				Second { map_second(value)! }
			})
			'b': 5
		}
	} else {
		{
			'a': 0
		}
	}
	return result
}

// match as a string interpolation operand: `'x=${match .. { .. }}'`
fn select_value_interp(node ?Node) !string {
	result := if value := node {
		'x=${match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}}'
	} else {
		'x=0'
	}
	return result
}

// match as a `dump()` operand: `dump(match .. { .. })`
fn select_value_dump(node ?Node) !int {
	result := if value := node {
		dump(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

fn bool_first(_ First) !bool {
	return true
}

fn bool_second(_ Second) !bool {
	return false
}

// match as a `_likely_()` operand: `_likely_(match .. { .. })`
fn select_value_likely(node ?Node) !bool {
	result := if value := node {
		_likely_(match value {
			First { bool_first(value)! }
			Second { bool_second(value)! }
		})
	} else {
		false
	}
	return result
}

// match as one value of a multi-return value list: `match .. { .. }, 9`
fn select_value_multiret(node ?Node) !(int, int) {
	a, b := if value := node {
		match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}, 9
	} else {
		0, 0
	}
	return a, b
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

fn test_array_literal_match_with_nested_statement_match() {
	assert select_value_arraylit_nested_stmt(First{}, true)! == [1]
	assert select_value_arraylit_nested_stmt(Second{}, false)! == [2]
	assert select_value_arraylit_nested_stmt(none, true) or { [-1] } == [0]
}

fn test_struct_init_field_match_as_if_expr_value_with_propagation() {
	assert select_value_structinit(First{})!.value == 1
	assert select_value_structinit(Second{})!.value == 2
	assert select_value_structinit(none) or {
		Holder{
			value: -1
		}
	}.value == 0
}

fn test_map_init_value_match_as_if_expr_value_with_propagation() {
	assert select_value_mapinit(First{})!['value'] == 1
	assert select_value_mapinit(Second{})!['value'] == 2
	assert (select_value_mapinit(none) or {
		{
			'value': -1
		}
	})['value'] == 0
}

fn test_prefix_operand_match_as_if_expr_value_with_propagation() {
	assert select_value_prefix(First{})! == -1
	assert select_value_prefix(Second{})! == -2
	assert select_value_prefix(none) or { 42 } == 0
}

fn test_index_operand_match_as_if_expr_value_with_propagation() {
	assert select_value_index(First{})! == 'b'
	assert select_value_index(Second{})! == 'c'
	assert select_value_index(none) or { 'z' } == 'x'
}

fn test_membership_operand_match_as_if_expr_value_with_propagation() {
	assert select_value_membership(First{})! == true
	assert select_value_membership(Second{})! == true
	assert select_value_membership(none) or { true } == false
}

fn test_selector_receiver_match_as_if_expr_value_with_propagation() {
	assert select_value_selector(First{})! == 1
	assert select_value_selector(Second{})! == 2
	assert select_value_selector(none) or { -1 } == 0
}

fn test_map_key_match_as_if_expr_value_with_propagation() {
	assert select_value_mapkey(First{})![1] == 'x'
	assert select_value_mapkey(Second{})![2] == 'x'
	assert (select_value_mapkey(none) or {
		{
			9: 'z'
		}
	})[0] == 'x'
}

fn test_array_spread_match_as_if_expr_value_with_propagation() {
	assert select_value_spread(First{})! == [1, 1]
	assert select_value_spread(Second{})! == [2, 2]
	assert select_value_spread(none) or { [-1] } == [0]
}

fn test_struct_update_match_as_if_expr_value_with_propagation() {
	assert select_value_struct_update(First{})!.value == 1
	assert select_value_struct_update(First{})!.other == 9
	assert select_value_struct_update(Second{})!.value == 2
	assert select_value_struct_update(none) or {
		Holder{
			value: -1
		}
	}.value == 0
}

fn test_map_update_match_as_if_expr_value_with_propagation() {
	assert select_value_map_update(First{})!['a'] == 1
	assert select_value_map_update(First{})!['b'] == 5
	assert select_value_map_update(Second{})!['a'] == 2
	assert (select_value_map_update(none) or {
		{
			'a': -1
		}
	})['a'] == 0
}

fn test_string_interp_match_as_if_expr_value_with_propagation() {
	assert select_value_interp(First{})! == 'x=1'
	assert select_value_interp(Second{})! == 'x=2'
	assert select_value_interp(none) or { 'x=z' } == 'x=0'
}

fn test_dump_operand_match_as_if_expr_value_with_propagation() {
	assert select_value_dump(First{})! == 1
	assert select_value_dump(Second{})! == 2
	assert select_value_dump(none) or { -1 } == 0
}

fn test_likely_operand_match_as_if_expr_value_with_propagation() {
	assert select_value_likely(First{})! == true
	assert select_value_likely(Second{})! == false
	assert select_value_likely(none) or { true } == false
}

fn test_multi_return_value_match_as_if_expr_value_with_propagation() {
	a1, b1 := select_value_multiret(First{})!
	assert a1 == 1
	assert b1 == 9
	a2, b2 := select_value_multiret(Second{})!
	assert a2 == 2
	assert b2 == 9
	a3, b3 := select_value_multiret(none) or { -1, -1 }
	assert a3 == 0
	assert b3 == 0
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
