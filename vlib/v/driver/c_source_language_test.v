module driver

import os
import time
import v.cmdexec

// source_language_test_compiles_cpp reports whether `compiler` can compile C++. A C
// compiler driver such as gcc runs a separately installed C++ front end (cc1plus),
// which an image may lack.
fn source_language_test_compiles_cpp(compiler string, root string) bool {
	probe := os.join_path(root, 'cpp_probe.cpp')
	os.write_file(probe, 'template<int N> int probe() { return N; }\nint value() { return probe<0>(); }\n') or {
		return false
	}
	return cmdexec.run(compiler, ['-fsyntax-only', '-x', 'c++', probe]).exit_code == 0
}

fn test_joined_cpp_hashflag_selects_language_and_links_runtime() {
	// A Windows `clang` usually targets the MSVC ABI, which V cannot build Windows
	// programs with, so use the MinGW gcc there.
	compiler := os.find_abs_path_of_executable($if windows { 'gcc' } $else { 'clang' }) or {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v joined cpp flag ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	if !source_language_test_compiles_cpp(compiler, root) {
		return
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'joined_cpp_flag' }\n")!
	os.write_file(os.join_path(root, 'shim.c'), '#include <string>\nextern "C" int cpp_probe(void) { std::string value(128, \'v\'); return int(value.size()); }\n')!
	os.write_file(os.join_path(root, 'probe.h'), 'int cpp_probe(void);\n')!
	for form in ['-xc++', '-x c++'] {
		for input in ['shim.c', 'shim.o'] {
			source := os.join_path(root, 'main.c.v')
			os.write_file(source, 'module main\n#flag ${form} "@VMODROOT/${input}" -xnone\n#include "@VMODROOT/probe.h"\nfn C.cpp_probe() int\nfn main() { assert C.cpp_probe() == 128 }\n')!
			result := cmdexec.run(os.join_path(@VMODROOT, 'v'), ['-new-compiler', '-gc', 'none',
				'-nocache', '-no-retry-compilation', '-cc', compiler, 'run', source])
			assert result.exit_code == 0, '${form} ${input}: ${result.output}'
		}
	}
}

fn test_joined_explicit_cpp_language_compiles_c_named_source() {
	compiler := os.find_abs_path_of_executable('clang') or {
		os.find_abs_path_of_executable('gcc') or { return }
	}
	root := os.join_path(os.vtmp_dir(), 'v joined source language ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	if !source_language_test_compiles_cpp(compiler, root) {
		return
	}
	source := os.join_path(root, 'template.c')
	os.write_file(source, 'template<typename T> T identity(T value) { return value; }\nint main() { return identity(0); }\n')!
	for flags in [['-xc++', source], ['-x', 'c++', source]] {
		mut args := ['-fsyntax-only']
		args << c_source_language_flags(flags)
		result := cmdexec.run(compiler, args)
		assert result.exit_code == 0, result.output
	}
}

fn test_flag_c_sources_keep_c_language_beside_cpp_sources() {
	compiler := os.find_abs_path_of_executable('gcc') or {
		os.find_abs_path_of_executable('clang') or { return }
	}
	root := os.join_path(os.vtmp_dir(), 'v c source language ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	if !source_language_test_compiles_cpp(compiler, root) {
		return
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_language_probe' }\n")!
	os.write_file(os.join_path(root, 'my_test_cshim.c'), '#ifdef __cplusplus\n#error expected C language\n#endif\nint probe_is_c_mode(void) { return _Generic(0, int: 101, default: 0); }\n')!
	os.write_file(os.join_path(root, 'my_test_cppshim.C'), 'template<typename T> int cpp_probe(T) { return 202; }\nextern "C" int probe_is_cpp_mode(void) { return cpp_probe(0); }\n')!
	os.write_file(os.join_path(root, 'probe.h'), 'int probe_is_c_mode(void);\nint probe_is_cpp_mode(void);\n')!
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, 'module main\n#flag "@VMODROOT/my_test_cshim.c"\n#flag "@VMODROOT/my_test_cppshim.C"\n#include "@VMODROOT/probe.h"\nfn C.probe_is_c_mode() int\nfn C.probe_is_cpp_mode() int\nfn main() {\n\tassert C.probe_is_c_mode() == 101\n\tassert C.probe_is_cpp_mode() == 202\n}\n')!
	result := cmdexec.run(os.join_path(@VMODROOT, 'v'), ['-new-compiler', '-gc', 'none', '-nocache',
		'-no-retry-compilation', '-cc', compiler, 'run', source])
	assert result.exit_code == 0, result.output
	// Simulate a Windows 8.3 alias changing the extension to .C after language
	// selection. C11's _Generic must still reach the compiler in C mode.
	alias := os.join_path(root, 'MY_TES~1.C')
	c_source := os.join_path(root, 'my_test_cshim.c')
	os.cp(c_source, alias)!
	mut args := ['-std=c11', '-fsyntax-only']
	args << c_source_language_flags([c_source]).map(if it == c_source { alias } else { it })
	aliased := cmdexec.run(compiler, args)
	assert aliased.exit_code == 0, aliased.output
}
