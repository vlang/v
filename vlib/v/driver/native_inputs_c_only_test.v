module driver

import os
import time
import v.cmdexec

fn test_uncached_c_output_does_not_probe_native_headers() {
	$if windows {
		// The compiler probe is a shell script.
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v c only native inputs ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	probe_log := os.join_path(root, 'compiler_probes')
	c_compiler := os.join_path(root, 'cc')
	// Toolchain selection may query its identity. Preprocessing and compiling
	// are the expensive operations this C-emission regression guards against.
	os.write_file(c_compiler, '#!/bin/sh\nif [ "$1" = "--version" ]; then\n  echo "clang version test"\n  exit 0\nfi\necho called >> ${os.quoted_path(probe_log)}\nexit 1\n')!
	os.chmod(c_compiler, 0o755)!
	header := os.join_path(root, 'native.h')
	os.write_file(header, '#define V_TEST_NATIVE_VALUE 7\n')!
	source := os.join_path(root, 'main.c.v')
	output := os.join_path(root, 'main.c')
	for serial in [false, true] {
		for directive in ['include', 'insert', 'preinclude', 'postinclude'] {
			os.write_file(source, 'module main\nimport crypto.sha3\n#${directive} "@DIR/native.h"\nfn main() { println(sha3.sum512([]u8{})) }\n')!
			mut args := ['-new-compiler', '-nocache', '-cc', c_compiler, '-o', output]
			if serial {
				args << '-no-parallel'
			}
			args << source
			result := cmdexec.run(@VEXE, args)
			assert result.exit_code == 0, result.output
			assert !os.exists(probe_log), 'C emission unexpectedly invoked the native compiler'
			generated := os.read_file(output)!
			assert generated.contains('v3_')
			assert generated.contains('sha3__sum512')
			assert generated.contains(header)
		}
	}
}

fn test_native_typedef_binding_is_explicit_in_cached_and_uncached_builds() {
	root := os.join_path(os.vtmp_dir(), 'v explicit native typedef ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or { panic(err) } }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'native_typedef' }")!
	os.write_file(os.join_path(root, 'foo.h'), 'typedef struct { int x; } Foo;\nstatic int foo_get(Foo* f) { return f->x; }\n')!
	source := os.join_path(root, 'main.c.v')
	output := os.join_path(root, 'main')
	mut compilers := [[]string{}]
	if (os.find_abs_path_of_executable('clang') or { '' }) != '' { compilers << ['-cc', 'clang'] }
	for directive in ['include', 'insert'] {
		os.write_file(source, 'module main\n#${directive} "@VMODROOT/foo.h"\n@[typedef]\nstruct C.Foo { x int }\nfn C.foo_get(f &C.Foo) int\nfn main() { f := C.Foo{x: 3}; println(C.foo_get(&f)) }\n')!
		for uncached in [false, true] {
			for cc_flags in compilers {
				mut args := ['-new-compiler', '-o', output]
				if uncached { args << '-nocache' }
				args << cc_flags
				args << source
				compiled := cmdexec.run(@VEXE, args)
				assert compiled.exit_code == 0, compiled.output
				run := cmdexec.run(output, []string{})
				assert run.exit_code == 0, run.output
				assert run.output.trim_space() == '3'
			}
		}
	}
}
