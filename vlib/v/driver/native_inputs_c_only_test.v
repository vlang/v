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
	os.write_file(source, 'module main\nimport crypto.sha3\n#include "@DIR/native.h"\nfn main() { println(sha3.sum512([]u8{})) }\n')!
	output := os.join_path(root, 'main.c')
	for serial in [false, true] {
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
