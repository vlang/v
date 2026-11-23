module main

import os
import v.cmdexec

// A generic `T{}` with `T = &&S` (and `&T{}` with `T = &S`) is a pointer value, so its
// heap slot has the pointer's C type. A `S*` slot stored in a `S**` is rejected by
// gcc 14 and later; `-Werror` makes older compilers and clang reject it too.
fn test_multi_pointer_struct_init_has_the_pointer_c_type() ! {
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'multi_pointer_struct_init_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(@VEXEROOT, 'vlib', 'v', 'tests',
		'generic_multi_pointer_struct_init_test.v')
	generated := os.join_path(root, 'probe.c')
	compiled := cmdexec.run(@VEXE, ['-gc', 'none', '-nocache', '-o', generated, source])
	assert compiled.exit_code == 0, compiled.output
	mut cc_args := ['-std=gnu11', '-fsyntax-only']
	if os.exec([cc, '--version']).output.contains('clang') {
		// clang counts dropped `const` qualifiers in this group; only the pointee
		// types matter here.
		cc_args << ['-Wno-everything', '-Werror=incompatible-pointer-types',
			'-Wno-error=incompatible-pointer-types-discards-qualifiers']
	} else {
		cc_args << '-Werror=incompatible-pointer-types'
	}
	checked := os.exec([cc, ...cc_args, generated])
	assert checked.exit_code == 0, checked.output
}
