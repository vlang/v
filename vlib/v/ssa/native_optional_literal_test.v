module ssa

import os
import v.parser
import v.pref
import v.transform
import v.types

fn test_native_optional_pointer_literals_register_the_wrapper_before_the_payload() {
	for building_v in [false, true] {
		path := os.join_path(os.vtmp_dir(), 'ssa_optional_literal_${building_v}_${os.getpid()}.v')
		defer { os.rm(path) or {} }
		os.write_file(path, 'module main
struct Payload { roots []string count int }
fn main() {
    mut waited := ?&main.Payload(none)
    if value := waited { _ = value.roots }
}
') or { panic(err) }
		mut preferences := pref.new_preferences()
		preferences.backend = 'arm64'
		mut p := parser.Parser.new(preferences)
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.building_v_fast = building_v
		tc.collect(a)
		if !building_v { tc.annotate_types() }
		assert tc.errors.len == 0, tc.errors.str()
		if building_v {
			_, _, errors := transform.transform_with_used_opt_config_scoped_workers_checked(mut a, tc, map[string]bool{}, false, true, false, true)
			assert errors.len == 0, errors.str()
		} else {
			transform.transform(mut a, tc)
		}
		m := build_with_used(a, map[string]bool{}, tc)
		mut found := false
		for typ in m.type_store.types {
			if typ.field_names != ['ok', 'value'] || typ.fields.len != 2 { continue }
			payload_pointer := m.type_store.types[typ.fields[1]]
			if payload_pointer.kind != .ptr_t { continue }
			payload := m.type_store.types[payload_pointer.elem_type]
			if payload.field_names != ['roots', 'count'] { continue }
			assert m.type_size(typ.fields[1]) == 8
			assert m.type_size(payload_pointer.elem_type) == 40
			main_function := m.funcs.filter(it.name == 'main')[0]
			for block_id in main_function.blocks {
				for value_id in m.blocks[block_id].instrs {
					instruction := m.instrs[m.values[value_id].index]
					if instruction.op == .load {
						assert m.values[value_id].typ != payload_pointer.elem_type, 'none must not load the pointed payload as its optional value'
					}
				}
			}
			found = true
		}
		assert found, 'optional pointer wrapper was replaced by the payload struct'
	}
}
