module parser

import os
import v.flat
import v.pref
import v.workers

fn test_inferred_types_relocate_every_nested_expression_link() {
	assert shift_inferred_type('![]&ui2.Signal[typeof(__expr_12)]', 100) == '![]&ui2.Signal[typeof(__expr_112)]'
	assert shift_inferred_type('fn(typeof(__expr_1)) typeof(__expr_22)', 10) == 'fn(typeof(__expr_11)) typeof(__expr_32)'
	assert shift_inferred_type('[]int', 10) == '[]int'
}

fn test_inferred_values_relocate_outside_literal_text() {
	assert shift_inferred_value(.array_init, 'typeof(__expr_4)', 10) == 'typeof(__expr_14)'
	assert shift_inferred_value(.ident, '[]typeof(__expr_4)', 10) == '[]typeof(__expr_14)'
	assert shift_inferred_value(.string_literal, 'typeof(__expr_4)', 10) == 'typeof(__expr_4)'
}

// inferred_type_links lists every `typeof(expr)` link of a merged AST, as the
// spelling's holder and the expression it names, so parses can be compared.
fn inferred_type_links(a &flat.FlatAst) []string {
	mut links := []string{}
	for id, node in a.nodes {
		for field, text in {
			'typ':   node.typ_text()
			'value': node.value_text()
		} {
			if node.kind in [.string_literal, .char_literal, .string_interp] {
				continue
			}
			mut offset := 0
			for {
				start := text[offset..].index('typeof(__expr_') or { break } + offset
				end := text[start..].index(')') or { break } + start
				target := text[start + 'typeof(__expr_'.len..end].int()
				offset = end + 1
				assert target >= 0 && target < a.nodes.len, '${node.kind}.${field} = ${text}'
				target_node := a.nodes[target]
				links << '${id} ${node.kind}.${field} -> ${target_node.kind}:${target_node.value}'
			}
		}
	}
	return links
}

fn test_parallel_merges_relocate_inferred_type_links_in_every_field() {
	root := os.join_path(os.vtmp_dir(), 'inferred_types_parallel_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut paths := []string{}
	for i in 0 .. 3 {
		path := os.join_path(root, 'padding_${i}.v')
		os.write_file(path, 'module main\n\nfn padding_${i}() int {\n\treturn ${i}\n}\n' +
			'// padding\n'.repeat(15000))!
		paths << path
	}
	use := os.join_path(root, 'use.v')
	os.write_file(use, "module main\n\nfn main() {\n\tx := 'hello'\n\tmut a := []typeof(x){}\n\ta << x\n\tb := []typeof(x.len + 1){}\n\tf := fn (s typeof(x)) typeof(s.len) {\n\t\treturn s.len\n\t}\n\t_ = b\n\t_ = f\n}\n")!
	paths << use
	mut serial := Parser.new(pref.new_preferences())
	_, serial_was_parallel := serial.parse_files_dispatch(paths, false)
	assert !serial_was_parallel
	expected := inferred_type_links(serial.a)
	assert expected.len >= 4, expected.str()
	assert expected.any(it.contains('array_init.value -> ident:x')), expected.str()
	for merge in ['pooled', 'ordered'] {
		if merge == 'ordered' {
			os.setenv('V3_NO_PAR_PARSE_MERGE', '1', true)
		}
		mut p := Parser.new(pref.new_preferences())
		p.a.worker_pool = workers.new(3)
		_, was_parallel := p.parse_files_dispatch(paths, true)
		os.unsetenv('V3_NO_PAR_PARSE_MERGE')
		assert was_parallel, merge
		assert inferred_type_links(p.a) == expected, merge
	}
}
