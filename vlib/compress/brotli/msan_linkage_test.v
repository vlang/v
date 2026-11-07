module brotli

import os
import v.cmdexec

fn test_msan_unpoison_links_to_a_runtime_without_dynamic_exports() ! {
	$if !linux && !macos {
		return
	}
	if !is_available() {
		eprintln('skipping compress.brotli MSan linkage test; libbrotli is not available')
		return
	}
	compiler := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'brotli_msan_linkage_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	header := os.join_path(root, 'runtime.h')
	os.write_file(header, '#include <stddef.h>\nvoid __msan_unpoison(const volatile void *ptr, size_t len);\n' +
		'int brotli_msan_calls(void);\nint brotli_msan_exported(void);\n')!
	runtime := os.join_path(root, 'runtime.c')
	os.write_file(runtime, '#include "runtime.h"\n#include <dlfcn.h>\nstatic int calls;\n' +
		'__attribute__((visibility("hidden"))) void __msan_unpoison(const volatile void *ptr, size_t len) { if (ptr && len) calls++; }\n' +
		'int brotli_msan_calls(void) { return calls; }\n' +
		'int brotli_msan_exported(void) { return dlsym(RTLD_DEFAULT, "__msan_unpoison") != NULL; }\n')!
	source := os.join_path(root, 'probe.c.v')
	os.write_file(source, 'module main\nimport compress.brotli\n#flag "${runtime}"\n#include "${header}"\n' +
		'fn C.brotli_msan_calls() int\nfn C.brotli_msan_exported() int\n' +
		'fn main() {\n data := "unexported sanitizer runtime".bytes()\n' +
		' encoded := brotli.compress(data)!\n decoded := brotli.decompress(encoded)!\n' +
		' assert decoded == data\n assert C.brotli_msan_calls() >= 2\n' +
		' assert C.brotli_msan_exported() == 0\n}\n')!
	// Exercise the feature-selected linkage on hosts without a MemorySanitizer
	// runtime. The hidden mock deliberately cannot be found using dlsym.
	result := cmdexec.run_in_merged_with_env(@VEXE, ['-cc', compiler, '-nocache', '-gc', 'none',
		'-cflags', '-D__has_feature(x)=x -Dmemory_sanitizer=1 -Wno-builtin-macro-redefined', 'run',
		source], @VEXEROOT, {
		'VFLAGS':                 ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert result.exit_code == 0, result.output
}
