module c

import os
import time
import v.cmdexec

fn test_windows_file_and_console_outputs_match_sdk_pointer_types() ! {
	cc_name := $if windows { 'gcc' } $else { 'x86_64-w64-mingw32-gcc' }
	cc := os.find_abs_path_of_executable(cc_name) or { return }
	root := os.join_path(os.vtmp_dir(), 'windows_sdk_outputs_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	programs := [
		'import os\nfn main() { mut p := os.new_process("unused"); p.set_redirect_stdio(); p.stdin_write("x") }',
		'import readline\nfn main() { mut r := readline.Readline{}; r.enable_raw_mode(); r.disable_raw_mode() }',
		'import term.ui\nfn main() { mut c := ui.init(ui.Config{}); c.run() or {} }',
		'import os\nfn main() { mut f := os.fifo_client("unused")!; f.write([u8(1)])!; mut b := []u8{len:1}; f.read(mut b)!; f.close() }',
	]
	// vmcp also captures compiler output through ReadFile.
	vmcp_c := os.join_path(root, 'vmcp.c')
	vmcp := cmdexec.run(@VEXE, ['-gc', 'none', '-nocache', '-os', 'windows', '-arch', 'amd64',
		'-o', vmcp_c, os.join_path(@VEXEROOT, 'cmd', 'tools', 'vmcp')])
	assert vmcp.exit_code == 0, vmcp.output
	vmcp_checked := os.exec([cc, '-std=gnu11', '-Werror=incompatible-pointer-types', '-fsyntax-only',
		'-I', os.join_path(@VEXEROOT, 'thirdparty', 'mbedtls', 'include'), '-I',
		os.join_path(@VEXEROOT, 'thirdparty', 'vschannel'), vmcp_c])
	assert vmcp_checked.exit_code == 0, vmcp_checked.output
	for index, program in programs {
		source := os.join_path(root, 'probe_${index}.c.v')
		generated := os.join_path(root, 'probe_${index}.c')
		os.write_file(source, 'module main\n#include <windows.h>\n${program}\n')!
		compiled := cmdexec.run(@VEXE, ['-gc', 'none', '-nocache', '-os', 'windows', '-arch', 'amd64',
			'-o', generated, source])
		assert compiled.exit_code == 0, compiled.output
		checked := os.exec([cc, '-std=gnu11', '-Werror=incompatible-pointer-types', '-fsyntax-only',
			generated])
		assert checked.exit_code == 0, checked.output
	}
}
