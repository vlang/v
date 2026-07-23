module parser

import os
import v3.flat
import v3.pref

// Parses `src` on its own and returns the flat AST plus the exact bytes the
// parser saw, so a node's [offset, end) span can be checked against the source.
fn parse_span_source(name string, src string) (&flat.FlatAst, string) {
	path := os.join_path(os.temp_dir(), 'v3_span_${name}_${os.getpid()}.v')
	os.write_file(path, src) or { panic(err) }
	mut p := Parser.new(pref.new_preferences())
	ast := p.parse_file(path)
	content := os.read_file(path) or { panic(err) }
	os.rm(path) or {}
	return ast, content
}

fn span_text(src string, node flat.Node) string {
	if !node.pos.is_valid() || node.pos.offset < 0 || node.pos.end > src.len
		|| node.pos.end < node.pos.offset {
		return ''
	}
	return src[node.pos.offset..node.pos.end]
}

// Literal nodes must carry their own span, not the span of the token that
// happens to follow them after p.next().
fn test_literal_nodes_span_their_own_source() {
	ast, src := parse_span_source('literals', 'fn main() {
	n := 12345
	c := `q`
	b := true
	_ = n
	_ = c
	_ = b
}
')
	mut saw_number := false
	mut saw_char := false
	mut saw_bool := false
	for node in ast.nodes {
		match node.kind {
			.int_literal {
				if node.value == '12345' {
					saw_number = true
					assert span_text(src, node) == '12345'
				}
			}
			.char_literal {
				saw_char = true
				assert span_text(src, node) == '`q`'
			}
			.bool_literal {
				if node.value == 'true' {
					saw_bool = true
					assert span_text(src, node) == 'true'
				}
			}
			else {}
		}
	}
	assert saw_number
	assert saw_char
	assert saw_bool
}

// Identifiers span the name, and postfix `++`/`--` span operand..operator, so a
// C-style for post clause and its wrapping block are positioned at the code, not
// at the following token.
fn test_c_style_for_post_clause_spans() {
	ast, src := parse_span_source('for_post', 'fn main() {
	n := 5
	for i, j := 0, 0; i < n; i++, j++ {
		println(i + j)
	}
}
')
	mut saw_ident := false
	mut saw_postfix := false
	mut saw_block := false
	mut stmt_spans := []string{}
	for node in ast.nodes {
		if node.kind == .ident && node.value == 'i' && span_text(src, node) == 'i' {
			saw_ident = true
		} else if node.kind == .postfix && node.op == .inc && span_text(src, node) == 'i++' {
			saw_postfix = true
		} else if node.kind == .expr_stmt {
			stmt_spans << span_text(src, node)
		} else if node.kind == .block && node.children_count == 2
			&& span_text(src, node) == 'i++, j++' {
			saw_block = true
		}
	}
	assert saw_ident
	assert saw_postfix
	assert saw_block
	// Each post expression is wrapped in its own statement spanning just that
	// expression, not the whole comma-separated clause.
	assert 'i++' in stmt_spans
	assert 'j++' in stmt_spans
	assert stmt_spans.filter(it == 'i++, j++').len == 0
}

// Array cast expressions (`[N]T(x)`, `[]T(x)`) are built after the `[N]T`/`[]T`
// prefix and `(x)` are consumed, so they must span from the opening `[` — the
// fixed-array variant builds its node directly on the flat AST and previously had
// no span at all.
fn test_array_cast_spans_from_bracket() {
	ast, src := parse_span_source('array_cast', 'fn main() {
	p := unsafe { nil }
	a := [3]int(p)
	b := []int(p)
	_ = a
	_ = b
}
')
	mut spans := []string{}
	for node in ast.nodes {
		if node.kind == .cast_expr && node.value.contains('int') {
			spans << span_text(src, node)
		}
	}
	assert '[3]int(p)' in spans
	assert '[]int(p)' in spans
}

fn test_mut_channel_element_type_is_preserved() {
	ast, _ := parse_span_source('mut_channel', 'struct Item {}
fn consume(chs []chan mut Item) {}
')
	mut found := false
	for node in ast.nodes {
		if node.kind == .param && node.value == 'chs' {
			found = true
			assert node.typ == '[]chan mut Item'
		}
	}
	assert found
}

// Dynamic array initializers (`[]T{...}`) are parsed after the `[]T` prefix is
// consumed, so the array_init node must span from the opening `[`, not the `{`.
fn test_dynamic_array_init_spans_from_bracket() {
	ast, src := parse_span_source('array_init', 'fn main() {
	a := []int{len: 3, init: 0}
	_ = a
}
')
	mut saw := false
	for node in ast.nodes {
		if node.kind == .array_init && node.value == 'int' {
			saw = true
			assert span_text(src, node) == '[]int{len: 3, init: 0}'
		}
	}
	assert saw
}

// Call nodes are completed after the closing `)` is consumed, so they must be
// anchored at the callee — the checker reports unknown-function diagnostics on
// the call node, which should point at the call, not the following token. For
// selector method calls (`recv.method()`) the callee is a `.selector`, which
// must itself span `recv.method` so the call covers the whole `recv.method()`.
fn test_call_node_spans_from_callee() {
	ast, src := parse_span_source('call', 'struct Obj { x int }
fn main() {
	missing()
	x := foo(1, 2)
	o := Obj{x: 1}
	y := o.missing(1, 2)
	_ = x
	_ = y
}
')
	mut call_spans := []string{}
	mut selector_spans := []string{}
	for node in ast.nodes {
		if node.kind == .call {
			call_spans << span_text(src, node)
		} else if node.kind == .selector {
			selector_spans << span_text(src, node)
		}
	}
	assert 'missing()' in call_spans
	assert 'foo(1, 2)' in call_spans
	assert 'o.missing(1, 2)' in call_spans
	assert 'o.missing' in selector_spans
}

fn test_keyword_identifiers_keep_declaration_names_and_spans() {
	ast, src := parse_span_source('keyword_ident', 'fn info(type int) int {
	return type
}
fn main() {
	type := 3
	for type in [1, 2] {
		println(type)
	}
	println(type)
}
')
	mut saw_param := false
	mut saw_decl := false
	mut saw_loop := false
	mut type_ident_count := 0
	for node in ast.nodes {
		if node.kind == .param && node.value == 'type' && node.typ == 'int' {
			saw_param = true
		}
		if node.kind == .ident && node.value == 'type' {
			type_ident_count++
			assert span_text(src, node) == 'type'
		}
		if node.kind == .decl_assign && node.children_count >= 2 {
			lhs := ast.nodes[int(ast.children[int(node.children_start)])]
			if lhs.kind == .ident && lhs.value == 'type' {
				saw_decl = true
			}
		}
		if node.kind == .for_in_stmt && node.children_count >= 3 {
			key := ast.nodes[int(ast.children[int(node.children_start)])]
			if key.kind == .ident && key.value == 'type' {
				saw_loop = true
			}
		}
	}
	assert saw_param
	assert saw_decl
	assert saw_loop
	assert type_ident_count >= 5
}

// Address-of expressions (`&Foo{}`, `&[]T{}`, `&T(x)`) span from the `&` through
// the whole operand; the pointer/array-init variants build their nodes directly
// on the flat AST, so they must still carry a valid, full span.
fn test_address_of_prefix_spans_from_ampersand() {
	ast, src := parse_span_source('addr_of', 'struct Foo { x int }
fn main() {
	a := &[]int{len: 1}
	b := &Foo{x: 1}
	c := &int(0)
	_ = a
	_ = b
	_ = c
}
')
	mut spans := []string{}
	for node in ast.nodes {
		if (node.kind == .prefix && node.op == .amp)
			|| (node.kind == .cast_expr && node.value.starts_with('&')) {
			spans << span_text(src, node)
		}
	}
	assert '&[]int{len: 1}' in spans
	assert '&Foo{x: 1}' in spans
	assert '&int(0)' in spans
}

// Prefix expressions span from the operator to the end of their operand.
fn test_prefix_expression_spans_operator_to_operand() {
	ast, src := parse_span_source('prefix', 'fn main() {
	m := -42
	_ = m
}
')
	mut saw_prefix := false
	for node in ast.nodes {
		if node.kind == .prefix {
			saw_prefix = true
			assert span_text(src, node) == '-42'
		}
	}
	assert saw_prefix
}

// Slice range bounds must span the whole `low..high`, including open-low ranges
// (`..high` / `..`) whose synthetic low bound has no span — those anchor at the
// `..` token rather than collapsing onto the high bound or the closing `]`.
fn test_slice_range_bounds_span_full_range() {
	ast, src := parse_span_source('ranges', 'fn main() {
	xs := [1, 2, 3, 4]
	a := xs[..2]
	b := xs[..]
	c := xs[1..3]
	d := xs[1..]
	_ = a
	_ = b
	_ = c
	_ = d
}
')
	mut ranges := []string{}
	for node in ast.nodes {
		if node.kind == .range {
			ranges << span_text(src, node)
		}
	}
	assert '..2' in ranges
	assert '..' in ranges
	assert '1..3' in ranges
	assert '1..' in ranges
}

// Inferred-size fixed array literals (`[..]T[...]`) build their array/postfix
// nodes only after the value list is consumed, so they must span from the outer
// opening `[` rather than the following token, and each inner row must span from
// its own opening `[`.
fn test_inferred_fixed_array_spans_from_opening_bracket() {
	ast, src := parse_span_source('inferred_fixed', 'fn main() {
	a := [..]int[1, 2, 3]
	b := [..][..]int[[1], [2, 3]]
	_ = a
	_ = b
}
')
	mut saw_a := false
	mut saw_b := false
	mut saw_row := false
	for node in ast.nodes {
		if node.kind != .postfix || !node.typ.starts_with('[') {
			continue
		}
		t := span_text(src, node)
		if t == '[..]int[1, 2, 3]' {
			saw_a = true
		} else if t == '[..][..]int[[1], [2, 3]]' {
			saw_b = true
		} else if t == '[2, 3]' {
			// an inner row of `b`, spanning from its own `[`
			saw_row = true
		}
	}
	assert saw_a
	assert saw_b
	assert saw_row
}

// Explicit fixed-array value literals (`[N]T[...]`) are parsed after the `[N]T`
// prefix is consumed, so the array_literal node must span from the outer opening
// `[` of the type prefix, not just the value list.
fn test_explicit_fixed_array_value_literal_spans_from_type_prefix() {
	ast, src := parse_span_source('fixed_value', 'fn main() {
	a := [3]int[1, 2, 3]
	_ = a
}
')
	mut saw := false
	for node in ast.nodes {
		if node.kind == .array_literal && node.typ == '[3]int' {
			saw = true
			assert span_text(src, node) == '[3]int[1, 2, 3]'
		}
	}
	assert saw
}

// A postfix `!` fixed-array literal (`[1, 2, 3]!`) must keep the `!` on the
// `.postfix` parent: the child `.array_literal` spans only `[1, 2, 3]`, while the
// postfix spans the whole `[1, 2, 3]!`.
fn test_postfix_fixed_array_literal_excludes_bang_from_child() {
	ast, src := parse_span_source('bang', 'fn main() {
	a := [1, 2, 3]!
	b := [42]!
	_ = a
	_ = b
}
')
	mut literals := []string{}
	mut postfixes := []string{}
	for node in ast.nodes {
		if node.kind == .array_literal {
			literals << span_text(src, node)
		} else if node.kind == .postfix && node.op == .not {
			postfixes << span_text(src, node)
		}
	}
	assert '[1, 2, 3]' in literals
	assert '[42]' in literals
	assert '[1, 2, 3]!' in postfixes
	assert '[42]!' in postfixes
}

// The `$compile_error(...)` sentinel call is a compiler-only `.call` node that
// the checker reports diagnostics against, so it must carry the directive's span
// (from the leading `$`) rather than a zero position.
fn test_compile_error_sentinel_call_has_directive_span() {
	ast, src := parse_span_source('compile_error', 'fn main() {
	\$compile_error("boom")
}
')
	mut saw := false
	for node in ast.nodes {
		if node.kind == .call && node.value == '__v_compile_error' {
			saw = true
			assert node.pos.is_valid()
			assert span_text(src, node) == '\$compile_error("boom")'
		}
	}
	assert saw
}

// Lambda nodes are built directly on the flat AST; they must still receive a
// valid span covering the whole lambda rather than a zero position.
fn test_lambda_nodes_have_valid_spans() {
	ast, src := parse_span_source('lambda', 'fn main() {
	f := |it| it + 7
	_ = f
}
')
	mut saw_lambda := false
	for node in ast.nodes {
		if node.kind == .lambda_expr {
			saw_lambda = true
			assert node.pos.is_valid()
			assert span_text(src, node) == '|it| it + 7'
		}
	}
	assert saw_lambda
}

// `$match` is valid in expression position. It must consume every branch before
// the enclosing declaration continues, otherwise later patterns are parsed as
// stray function-body statements and the real `return` is lost.
fn test_comptime_match_expression_consumes_all_branches() {
	ast, _ := parse_span_source('comptime_match_expr', 'fn choose[T]() int {
	value := \$match T.unaliased_typ {
		int { 1 }
		\$float { 2 }
		\$else { 3 }
	}
	return value
}
')
	mut saw := false
	for node in ast.nodes {
		if node.kind != .fn_decl || node.value != 'choose' {
			continue
		}
		saw = true
		assert node.children_count == 2
		decl := ast.child_node(&node, 0)
		ret := ast.child_node(&node, 1)
		assert decl.kind == .decl_assign
		assert ret.kind == .return_stmt
		assert decl.children_count == 2
		rhs := ast.child_node(decl, 1)
		assert rhs.kind == .comptime_if
	}
	assert saw
}

fn test_comptime_type_accessor_initializers_stay_single_expressions() {
	ast, _ := parse_span_source('comptime_type_init', 'struct Box[T] {}

fn make_zero[T](x ?T) {
	_ := typeof(x).payload_type{}
	_ := \$zero([]typeof(x).payload_type{})
	_ := \$zero([][]int{})
	_ := \$zero([]?int{})
	_ := \$zero([]Box[int]{})
	_ := \$new([]Box[string]{})
}
')
	mut saw_fn := false
	mut marker_count := 0
	mut new_marker_count := 0
	mut saw_array_target := false
	mut saw_accessor_array_target := false
	mut saw_nested_array_target := false
	mut saw_optional_array_target := false
	mut saw_generic_array_target := false
	mut saw_generic_new_array_target := false
	for node in ast.nodes {
		if node.kind == .fn_decl && node.value == 'make_zero' {
			saw_fn = true
			assert node.children_count == 7
			assert ast.child_node(&node, 0).kind == .param
			assert ast.child_node(&node, 1).kind == .decl_assign
			assert ast.child_node(&node, 2).kind == .decl_assign
		}
		if node.kind == .string_literal && node.value in ['__v3_comptime_zero', '__v3_comptime_new']
			&& node.children_count == 1 {
			if node.value == '__v3_comptime_zero' {
				marker_count++
			} else {
				new_marker_count++
			}
			target := ast.child_node(&node, 0)
			if target.kind == .array_init && target.value == '__v3_comptime_type_array' {
				saw_array_target = true
				if target.children_count == 1 {
					elem := ast.child_node(target, 0)
					if elem.kind == .selector && elem.value == 'payload_type' {
						saw_accessor_array_target = true
					}
					if elem.kind == .array_init && elem.value == '__v3_comptime_type_array' {
						saw_nested_array_target = true
						assert elem.children_count == 1
						leaf := ast.child_node(elem, 0)
						assert leaf.kind == .ident
						assert leaf.value == 'int'
					}
					if elem.kind == .ident && elem.value == '?int' {
						saw_optional_array_target = true
					}
					if elem.kind == .ident && elem.value == 'Box[int]' {
						saw_generic_array_target = true
					}
					if elem.kind == .ident && elem.value == 'Box[string]'
						&& node.value == '__v3_comptime_new' {
						saw_generic_new_array_target = true
					}
				}
			}
		}
	}
	assert saw_fn
	assert marker_count == 5
	assert new_marker_count == 1
	assert saw_array_target
	assert saw_accessor_array_target
	assert saw_nested_array_target
	assert saw_optional_array_target
	assert saw_generic_array_target
	assert saw_generic_new_array_target
}

fn test_comptime_fixed_array_targets_stay_type_nodes() {
	ast, _ := parse_span_source('comptime_fixed_array_type_arg', 'struct Box[T] {}

fn main() {
	_ := \$zero([2]int{})
	_ := \$new([3]Box[int]{})
}
')
	mut saw_zero := false
	mut saw_new := false
	for node in ast.nodes {
		if node.kind != .string_literal || node.children_count != 1 {
			continue
		}
		target := ast.child_node(&node, 0)
		if node.value == '__v3_comptime_zero' && target.kind == .ident && target.value == '[2]int' {
			saw_zero = true
		}
		if node.value == '__v3_comptime_new' && target.kind == .ident
			&& target.value == '[3]Box[int]' {
			saw_new = true
		}
	}
	assert saw_zero
	assert saw_new
}
