module main

import os
import time
import v.cmdexec

fn testsuite_begin() {
	// This file checks Windows C with the MinGW gcc, which rejects the MSVC flavor
	// (intrinsics, CRT linkage) that `-cc msvc`, the VFLAGS of the MSVC CI jobs,
	// writes; let these generations use the default compiler.
	if os.getenv('VFLAGS').contains('msvc') {
		os.unsetenv('VFLAGS')
	}
}

fn test_windows_file_and_console_outputs_match_sdk_pointer_types() ! {
	cc_name := $if windows { 'gcc' } $else { 'x86_64-w64-mingw32-gcc' }
	cc := os.find_abs_path_of_executable(cc_name) or { return }
	root := os.join_path(os.vtmp_dir(), 'windows_sdk_outputs_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	v_args := ['-gc', 'none', '-nocache', '-os', 'windows', '-arch', 'amd64']
	cc_args := ['-std=gnu11', '-Werror=incompatible-pointer-types', '-fsyntax-only']
	// vmcp also captures compiler output through ReadFile.
	vmcp_c := os.join_path(root, 'vmcp.c')
	vmcp := cmdexec.run(@VEXE, [...v_args, '-o', vmcp_c,
		os.join_path(@VEXEROOT, 'cmd', 'tools', 'vmcp')])
	assert vmcp.exit_code == 0, vmcp.output
	vmcp_checked := os.exec([cc, ...cc_args, '-I',
		os.join_path(@VEXEROOT, 'thirdparty', 'mbedtls', 'include'), '-I',
		os.join_path(@VEXEROOT, 'thirdparty', 'vschannel'), vmcp_c])
	assert vmcp_checked.exit_code == 0, vmcp_checked.output

	source := os.join_path(root, 'probe.c.v')
	generated := os.join_path(root, 'probe.c')
	os.write_file(source, 'module main
#include <windows.h>
import os
import readline
import term.ui

fn main() {
	mut process := os.new_process("unused")
	process.set_redirect_stdio()
	process.stdin_write("x")

	mut reader := readline.Readline{}
	reader.enable_raw_mode()
	reader.disable_raw_mode()

	mut console := ui.init(ui.Config{})
	console.run() or {}

	mut fifo := os.fifo_client("unused")!
	fifo.write([u8(1)])!
	mut buffer := []u8{len: 1}
	fifo.read(mut buffer)!
	fifo.close()
}
')!
	compiled := cmdexec.run(@VEXE, [...v_args, '-o', generated, source])
	assert compiled.exit_code == 0, compiled.output
	checked := os.exec([cc, ...cc_args, generated])
	assert checked.exit_code == 0, checked.output
}
