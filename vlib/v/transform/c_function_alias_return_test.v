module transform

import os
import v.parser
import v.pref
import v.types

fn test_c_function_alias_keeps_v_return_type_in_both_declaration_orders() {
	root := os.join_path(os.vtmp_dir(), 'c_function_alias_return_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	v_path := os.join_path(root, 'buffer.v')
	c_path := os.join_path(root, 'main.c.v')
	os.write_file(v_path, 'module builtin
pub fn buffer() &u8 { return unsafe { nil } }
')!
	os.write_file(c_path, 'module main
fn C.buffer() voidptr
fn main() {
	v_value := buffer()
	c_value := C.buffer()
	_ = v_value
	_ = c_value
}
')!
	for c_first in [false, true] {
		paths := if c_first { [c_path, v_path] } else { [v_path, c_path] }
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_files(paths)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.check_semantics()
		assert tc.errors.len == 0, tc.errors.str()
		transform(mut a, &tc)
		mut found := map[string]string{}
		for node in a.nodes {
			if node.kind == .decl_assign && node.children_count == 2 {
				lhs := a.child_node(&node, 0)
				if lhs.value in ['v_value', 'c_value'] {
					found[lhs.value] = node.typ_text()
				}
			}
		}
		assert found['v_value'] == '&u8', '${c_first}: ${found}'
		assert found['c_value'] == 'voidptr', '${c_first}: ${found}'
	}
}
