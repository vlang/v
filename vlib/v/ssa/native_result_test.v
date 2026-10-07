module ssa

import os
import v.parser
import v.pref
import v.types

fn test_native_void_result_identity_does_not_match_an_ordinary_ok_field() {
	mut b := Builder{
		m: Module.new()
	}
	boolean := b.m.type_store.get_int(1)
	plain := b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      [boolean]
		field_names: ['ok']
	})
	optional := b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      [boolean]
		field_names: ['ok']
	})
	b.option_types['?void'] = optional
	assert !b.is_option_type(plain)
	assert b.is_option_type(optional)
}

fn test_native_void_results_terminate_with_a_success_value() {
	path := os.join_path(os.vtmp_dir(), 'native_result_void_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, 'fn implicit_return(enabled bool) ! {
	if enabled { return }
}
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
	function := m.funcs.filter(it.name == 'implicit_return')[0]
	mut returns := 0
	for block_id in function.blocks {
		block := m.blocks[block_id]
		assert block.instrs.len > 0
		last := m.instrs[m.values[block.instrs.last()].index]
		assert last.op in [.ret, .jmp, .br, .unreachable]
		if last.op == .ret {
			assert last.operands.len == 1
			assert m.values[last.operands[0]].typ == function.typ
			returns++
		}
	}
	assert returns == 2
}
