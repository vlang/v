// vtest vflags: -compile-backend fastc

module fastc

import os
import time
import v.cmdexec
import v.pref

fn test_self_build_preserves_native_headers_for_the_c_compiler() {
	root := os.join_path(os.vtmp_dir(), 'fastc_native_header_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or { panic(err) } }
	header := os.join_path(root, 'native.h')
	os.write_file(header, '#define FASTC_HEADER_CONTENT_SENTINEL 42\n')!
	source_path := os.join_path(root, 'main.v')
	source := 'module main\n#include "@DIR/native.h"\nfn main() {}\n'
	for self_build in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.building_v = self_build
		generated := generate(source, source_path, prefs)!
		assert generated.contains('#include "${os.real_path(header)}"')
		assert !generated.contains('FASTC_HEADER_CONTENT_SENTINEL')
		if !self_build {
			// A self-build supplies its builtin types through the compiler sources.
			// This small standalone fixture exercises C compilation in ordinary mode.
			c_file := os.join_path(root, 'program.c')
			os.write_file(c_file, generated)!
			tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
			compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', os.join_path(root, 'program'), c_file])
			assert compiled.exit_code == 0, compiled.output
		}
	}
}
