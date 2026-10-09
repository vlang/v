module driver

import os
import v.flat
import v.parser
import v.pref
import v.types

// The wasm backend lowers dynamic arrays of primitive elements, so the
// driver gate must admit them while still rejecting maps, structs, fixed
// arrays and nested arrays. A rejection here is a hard error before any wasm
// is emitted; an admission it should not make would miscompile silently
// instead, so both directions are pinned.
fn wasm_gate_check(src string) string {
	dir := os.join_path(os.vtmp_dir(), 'wasm_gate_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'g.v')
	os.write_file(path, src) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	for d in p.diagnostics {
		assert false, 'DIAG ${d.severity} ${d.file}:${d.line}:${d.column} ${d.message}'
	}
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	tc.annotate_types()
	// Every aggregate node in the file, so a nested literal reports the outer
	// rejection too rather than only its (supported) inner row.
	mut msgs := []string{}
	for idx, node in a.nodes {
		if node.kind !in [.array_literal, .array_init, .map_init, .struct_init] {
			continue
		}
		mut visited := []bool{len: a.nodes.len}
		if msg := unsupported_backend_node_error(a, &tc, flat.NodeId(idx), 'wasm', true,
			'', mut visited) {
			msgs << msg
		}
	}
	return msgs.join(' | ')
}

fn test_wasm_gate_admits_primitive_arrays() {
	assert wasm_gate_check('module main\nfn main() {\n\ta := [1, 2, 3]\n\tprintln(a[0])\n}\n') == ''
	assert wasm_gate_check('module main\nfn main() {\n\tz := []int{len: 4, init: 7}\n\tprintln(z.len)\n}\n') == ''
}

// Struct literals are admitted when every field has a lowering, which is what
// lets lesson pages with structs run client-side. The rejection direction is
// still pinned: a struct holding a map has no field-wise wasm lowering, so it
// must be refused rather than miscompiled.
fn test_wasm_gate_admits_structs_with_supported_fields() {
	assert wasm_gate_check('module main\nstruct P {\n\tx int\n\ty int\n}\nfn main() {\n\tp := P{\n\t\tx: 1,\n\t\ty: 2\n\t}\n\tprintln(p.x)\n}\n') == ''
	assert wasm_gate_check("module main\nstruct Name {\n\ts string\n}\nstruct P {\n\tname Name\n\tn    int\n}\nfn main() {\n\tp := P{\n\t\tname: Name{\n\t\t\ts: 'a'\n\t\t},\n\t\tn:    2\n\t}\n\tprintln(p.n)\n}\n") == ''
	// A pointer field to a supported struct. V requires a reference field to
	// be initialized, so the literal names it explicitly.
	assert wasm_gate_check('module main\nstruct Leaf {\n\tid int\n}\nstruct P {\n\tleaf &Leaf\n\tn    int\n}\nfn main() {\n\tp := P{\n\t\tleaf: unsafe { nil }\n\t\tn:    2\n\t}\n\tprintln(p.n)\n}\n') == ''
}

fn test_wasm_gate_rejects_maps_structs_fixed_nested() {
	map_msg := wasm_gate_check("module main\nfn main() {\n\tm := {'a': 1}\n\tprintln(m)\n}\n")
	assert map_msg.contains('does not support type')
	// A struct field the backend cannot lower keeps the whole literal
	// rejected: admitting it would silently miscompile the field copy.
	bad_field := wasm_gate_check('module main\nstruct P {\n\tm map[string]int\n}\nfn main() {\n\tp := P{}\n\tprintln(p.m)\n}\n')
	assert bad_field.contains('does not support type'), bad_field
	fixed_msg := wasm_gate_check('module main\nfn main() {\n\ta := [2]int[1, 2]\n\tprintln(a[0])\n}\n')
	assert fixed_msg.contains('does not support type'), fixed_msg
	nested_msg := wasm_gate_check('module main\nfn main() {\n\tm := [[1, 2], [3]]\n\tprintln(m[0][0])\n}\n')
	assert nested_msg.contains('does not support type')
}

fn test_wasm_supported_array_elem() {
	for t in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'isize', 'usize', 'f32',
		'f64', 'bool', 'char', 'rune'] {
		assert wasm_supported_array_elem('[]' + t), t
	}
	for t in ['', '[]', 'int', 'map[string]int', 'Point', '[2]int', '[][]int', '[]Point', '[]string'] {
		assert !wasm_supported_array_elem(t), t
	}
}
