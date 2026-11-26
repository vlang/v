import os

fn test_const_qualified_c_exports_forward_to_v_parameters() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'export_const_arguments_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(@VEXEROOT, 'vlib', 'v', 'compiler_tests', 'testdata', 'export_const_arguments', 'main.c.v')
	for role in ['debug', 'prod'] {
		program := os.join_path(work, role) + $if windows { '.exe' } $else { '' }
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', cc, '-cstrict', '-cflags',
			'-Werror=incompatible-pointer-types-discards-qualifiers']
		if role == 'prod' { command << '-prod' }
		command << ['-o', program, source]
		compiled := os.exec(command)
		assert compiled.exit_code == 0, '${role}: ${compiled.output}'
		result := os.exec([program])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == 'const-qualified C export arguments passed'
	}
}

fn test_const_qualified_c_exports_from_object_files() ! {
	$if windows {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'export_const_object_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	fixtures := os.join_path(@VEXEROOT, 'vlib', 'v', 'compiler_tests', 'testdata', 'export_const_arguments')
	object := os.join_path(work, 'library.o')
	compiled := os.exec([@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', cc, '-cstrict',
		'-cflags', '-Werror=incompatible-pointer-types-discards-qualifiers', '-is_o', '-d',
		'export_const_object', '-o', object, os.join_path(fixtures, 'main.c.v')])
	assert compiled.exit_code == 0, compiled.output
	host_source := os.join_path(work, 'host.c')
	os.write_file(host_source, '#include "sdk.h"\nint main(void) { return sdk_verify(); }\n')!
	host := os.join_path(work, 'host')
	linked := os.exec([cc, '-Wall', '-Wextra', '-Werror', '-I', fixtures, host_source, object,
		'-lm', '-lpthread', '-o', host])
	assert linked.exit_code == 0, linked.output
	result := os.exec([host])
	assert result.exit_code == 0, result.output
}

fn test_const_export_forwarding_does_not_hide_incompatible_pointees() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'export_const_bad_pointee_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	os.write_file(os.join_path(work, 'sdk.h'), '#include <stdint.h>\nuint64_t sdk_bad(const int32_t *source);\n')!
	source := os.join_path(work, 'main.c.v')
	os.write_file(source, '#include "@DIR/sdk.h"
fn C.sdk_bad(const_source &i32) u64
@[export: "sdk_bad"]
fn bad_reader(source &i64) u64 { return unsafe { u64(*source) } }
fn main() {}
')!
	compiled := os.exec([@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', cc, '-cstrict',
		'-cflags', '-Werror=incompatible-pointer-types', '-o', os.join_path(work, 'bad'), source])
	assert compiled.exit_code != 0
	assert compiled.output.contains('incompatible pointer types'), compiled.output
	assert compiled.output.contains('i64'), compiled.output
}
