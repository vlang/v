import os

fn test_freestanding_with_target_headers_builds_and_runs() {
	$if !linux {
		return
	}
	$if !amd64 && !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_target_headers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	// The bare runtime exports qsort even when it is not called. Its callback type
	// must be declared alongside the target's libc headers, with the same C ABI.
	os.write_file(source, '#include <errno.h>
#include <stdlib.h>

fn main() {
	p := C.__errno_location()
	unsafe { *p = 22 }
	assert C.errno == 22
	C.errno = 2
	assert unsafe { *p } == 2
	println("freestanding target headers ok")
}
')!
	compiler := if @CCOMPILER in ['tinyc', 'tcc'] { 'cc' } else { @CCOMPILER }
	executable := os.join_path(root, 'main')
	build := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-freestanding',
		'-target-libc-headers', '-gc', 'none', '-cstrict', '-cc', compiler, '-o', executable, source])
	assert build.exit_code == 0, build.output
	run := os.exec([executable])
	assert run.exit_code == 0, run.output
	assert run.output == 'freestanding target headers ok\n', run.output
}
