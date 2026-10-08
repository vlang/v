module ssa

import os
import v.parser
import v.pref

fn test_native_uname_layout_preserves_darwin_fields_without_changing_wasm32() {
	path := os.join_path(os.vtmp_dir(), 'ssa_utsname_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'pub struct C.utsname {
	sysname &char
	nodename &char
	release &char
	version &char
	machine &char
}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	for pointer_size in [4, 8] {
		m := build_with_options(a, map[string]bool{}, unsafe { nil }, BuildOptions{
			target: TargetData{ ptr_size: pointer_size }
		})
		mut found := false
		for id, typ in m.type_store.types {
			if typ.kind != .struct_t
				|| typ.field_names != ['sysname', 'nodename', 'release', 'version', 'machine'] {
				continue
			}
			stride := if pointer_size == 8 { 256 } else { 4 }
			assert m.type_size(TypeID(id)) == stride * 5
			for i in 0 .. 5 {
				assert m.struct_field_offset(TypeID(id), i) == stride * i
			}
			found = true
		}
		assert found
	}
}
