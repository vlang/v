module ssa

import os
import v.parser
import v.pref
import v.types

fn test_native_optional_and_result_tuples_preserve_payload_layout() {
	path := os.join_path(os.vtmp_dir(), 'native_tuple_option_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, 'fn optional_pair() ?(string, string) { return "receiver", "method" }
fn result_pair() !(string, int) { return "kept", 7 }
fn plain_pair() (string, string) { return "first", "second" }
fn main() {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	m := build_with_used(a, map[string]bool{}, tc)
	for name in ['optional_pair', 'result_pair', 'plain_pair'] {
		function := m.funcs.filter(it.name == name)[0]
		outer := m.type_store.types[function.typ]
		assert outer.kind == .struct_t
		payload_id := if name == 'plain_pair' { function.typ } else { outer.fields[1] }
		payload := m.type_store.types[payload_id]
		assert payload.kind == .struct_t, name
		assert payload.field_names == ['arg0', 'arg1'], name
		assert m.type_size(payload.fields[0]) == 16
		assert m.type_size(payload.fields[1]) == if name == 'result_pair' { 4 } else { 16 }
		if name == 'optional_pair' {
			assert m.type_size(function.typ) == 40
		}
		mut saw_tuple := false
		mut saw_return := false
		for block_id in function.blocks {
			for value_id in m.blocks[block_id].instrs {
				instruction := m.instrs[m.values[value_id].index]
				if instruction.op == .load && m.values[value_id].typ == payload_id {
					saw_tuple = true
				}
				if instruction.op == .ret {
					assert m.values[instruction.operands[0]].typ == function.typ
					saw_return = true
				}
			}
		}
		assert saw_tuple, name
		assert saw_return, name
	}
}
