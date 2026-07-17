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
