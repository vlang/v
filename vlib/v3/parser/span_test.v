module parser

import os
import flat
import pref

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
