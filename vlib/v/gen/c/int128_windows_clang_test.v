module c

import os

fn int128_clang_test_source(portable bool) string {
	mut g := FlatGen.new()
	if portable {
		g.compile_defines << 'v3_no_native_int128'
	}
	g.emit_int128_preamble()
	checks := $embed_file('testdata/int128_arithmetic.c').to_string()
	return 'typedef unsigned long long u64;\ntypedef long long i64;\ntypedef _Bool bool;\n' +
		g.sb.str() + checks
}

fn int128_clang_pe_link_command(target string, object string, output string) []string {
	if linker := os.find_abs_path_of_executable('lld-link') {
		return [linker, '/entry:entry', '/subsystem:console', '/nodefaultlib', '/out:${output}',
			object]
	}
	$if linux {
		if target.starts_with('x86_64-') {
			if linker := os.find_abs_path_of_executable('ld') {
				info := os.exec([linker, '-V'])
				if info.exit_code == 0 && info.output.contains('i386pep') {
					return [linker, '-mi386pep', '-e', 'entry', '--subsystem', 'console', '-o',
						output, object]
				}
			}
		}
	}
	return []string{}
}

fn test_int128_helpers_windows_clang_do_not_need_runtime_builtins() {
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'int128_windows_clang_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'check.c')
	os.write_file(source, int128_clang_test_source(false))!
	for target in ['x86_64-pc-windows-msvc', 'aarch64-pc-windows-msvc'] {
		assembly := os.join_path(root, '${target}.s')
		object := os.join_path(root, '${target}.obj')
		flags := [clang, '-target', target, '-std=c11', '-ffreestanding', '-fno-stack-protector',
			'-O0']
		assembled := os.exec([...flags, '-S', source, '-o', assembly])
		assert assembled.exit_code == 0, '${target}: ${assembled.output}'
		code := os.read_file(assembly)!
		// These calls link with libgcc/compiler-rt on GNU targets, but the MSVC
		// SDK does not provide them. Keep the probe independent of a Windows SDK.
		for helper in ['__udivti3', '__umodti3', '__divti3', '__modti3'] {
			assert !code.contains(helper), '${target} requires ${helper}'
		}
		compiled := os.exec([...flags, '-c', source, '-o', object])
		assert compiled.exit_code == 0, '${target}: ${compiled.output}'
		assert os.is_file(object)
		output := os.join_path(root, '${target}.exe')
		link_command := int128_clang_pe_link_command(target, object, output)
		if link_command.len > 0 {
			linked := os.exec(link_command)
			assert linked.exit_code == 0, '${target}: ${linked.output}'
			assert os.is_file(output)
		}
	}
	// MinGW Clang can use its compiler runtime and keeps the native C ABI.
	os.write_file(source, int128_clang_test_source(false) + '\n_Static_assert(__builtin_types_compatible_p(u128, unsigned __int128), "GNU u128 must stay native");\n')!
	gnu := os.exec([clang, '-target', 'x86_64-w64-windows-gnu', '-std=c11', '-ffreestanding', '-c',
		source, '-o', os.join_path(root, 'gnu.obj')])
	assert gnu.exit_code == 0, gnu.output
}

fn test_int128_helpers_native_and_portable_arithmetic_agree() {
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'int128_arithmetic_clang_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for mode in ['default', 'portable', 'msvc_selection'] {
		source := os.join_path(root, '${mode}.c')
		output := os.join_path(root, mode + $if windows { '.exe' } $else { '' })
		mut code := int128_clang_test_source(mode == 'portable')
		mut flags := [clang, '-std=c11', '-O0']
		if mode == 'msvc_selection' {
			code = '#ifndef _MSC_VER\n#define _MSC_VER 1930\n#endif\n' + code
			flags << '-fms-extensions'
		}
		os.write_file(source, code)!
		compiled := os.exec([...flags, source, '-o', output])
		assert compiled.exit_code == 0, '${mode}: ${compiled.output}'
		run := os.exec([output])
		assert run.exit_code == 0, '${mode}: check ${run.exit_code}: ${run.output}'
	}
}
