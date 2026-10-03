module types

import os

fn test_regular_v_storage_rules_remain_checked() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_storage_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	translated := os.join_path(root, 'translated.v')
	regular := os.join_path(root, 'main.v')
	os.write_file(translated, '@[translated]\nmodule main\nfn translated() {}\n')!
	os.write_file(regular, 'module main\n__global ordinary = int(0)\nfn write_pointer(p &int) { *p = 3 }\nfn main() { translated() }\n')!
	for flags in ['', '-no-parallel'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('enable globals'), result.output
		assert result.output.contains('modifying variables via dereferencing'), result.output
	}
}

fn test_ordinary_assignment_through_pointer_call_remains_rejected() {
	root := os.join_path(os.vtmp_dir(), 'v3_pointer_call_storage_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'module main\nfn ptr(value &int) &int { return value }\nfn main() { value := 0; unsafe { *ptr(&value) = 42 } }\n')!
	result := os.exec([@VEXE, '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot dereference a function call on the left side'), result.output
}

fn test_translated_shared_mutations_still_require_write_locks() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_shared_storage_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	prefix := '@[translated]\nmodule main\nstruct State { mut: count int }\nfn main() { shared state := State{}; '
	for mutation in ['= 1', '+= 1', '-= 1', '*= 1', '/= 1', '%= 1', '<<= 1', '>>= 1', '>>>= 1',
		'&= 1', '|= 1', '^= 1', '++', '--'] {
		for mode in ['', 'rlock', 'lock'] {
			body := if mode.len == 0 {
				'state.count ${mutation}'
			} else {
				'${mode} state { state.count ${mutation} }'
			}
			os.write_file(path, prefix + body + ' }\n')!
			result := os.exec([@VEXE, '-check', path])
			if mode == 'lock' {
				assert result.exit_code == 0, result.output
			} else {
				assert result.exit_code != 0, result.output
				assert result.output.contains('lock'), result.output
			}
		}
	}
}
