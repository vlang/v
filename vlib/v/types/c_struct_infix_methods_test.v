module types

import os

fn test_imported_c_infix_visibility_and_import_usage() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_infix_methods_${os.getpid()}')
	for name in ['provider', 'left', 'right'] { os.mkdir_all(os.join_path(root, name))! }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_infix_methods' }\n")!
	os.write_file(os.join_path(root, 'provider', 'counter.c.v'), 'module provider
pub struct C.Counter { mut: value int }
pub fn make() C.Counter { return C.Counter{} }
')!
	for visibility in ['pub ', ''] {
		os.write_file(os.join_path(root, 'left', 'counter.c.v'), 'module left
pub struct C.Counter { mut: value int }
pub fn used() {}
${visibility}fn (a C.Counter) + (b C.Counter) C.Counter { return C.Counter{value: a.value + b.value} }
')!
		for body in ['value := a + b; println(value.value)', 'a += b; println(a.value)'] {
			use := if visibility.len == 0 { 'extension.used();' } else { '' }
			os.write_file(os.join_path(root, 'main.v'), 'module main\nimport provider\nimport left as extension\nfn main() { ${use} mut a := provider.make(); b := provider.make(); ${body} }\n')!
			for flags in ['-W', '-W -no-parallel'] {
				result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check',
					root])
				if visibility.len > 0 {
					assert result.exit_code == 0, result.output
				} else {
					assert result.exit_code != 0, result.output
				}
			}
		}
	}
	for name in ['left', 'right'] {
		os.write_file(os.join_path(root, name, 'counter.c.v'), 'module ${name}
pub struct C.Counter { mut: value int }
pub fn used() {}
pub fn (a C.Counter) + (b C.Counter) C.Counter { return C.Counter{value: a.value + b.value} }
')!
	}
	for body in ['value := a + b; println(value.value)', 'a += b; println(a.value)'] {
		os.write_file(os.join_path(root, 'main.v'), 'module main\nimport provider\nimport left\nimport right\nfn main() { left.used(); right.used(); mut a := provider.make(); b := provider.make(); ${body} }\n')!
		result := os.exec([@VEXE, '-check', root])
		assert result.exit_code != 0, result.output
	}
}
