module fastc

import os
import v.cmdexec
import v.pref

const c_errno_local_source = r'module main
#include <errno.h>

fn main() {
	C.errno = 23
	mut errno := C.errno
	C.errno = 0
	assert errno == 23
	errno += 1
	assert errno == 24
	C.errno = errno
	assert C.errno == 24
	println(errno)
}
'

fn test_fastc_local_errno_does_not_collide_with_c_library_macro() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate(c_errno_local_source, 'errno_local.c.v', prefs) or { panic(err) }
	assert c_source.contains(' __vf_keyword_errno = (errno);'), c_source
	assert c_source.contains('errno=__vf_keyword_errno;'), c_source
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_errno_local_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.c.v'), c_errno_local_source) or { panic(err) }
	program := os.join_path(root, 'program')
	compiled := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-nocache', '-o', program, root])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(program, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output == '24\n'
}
