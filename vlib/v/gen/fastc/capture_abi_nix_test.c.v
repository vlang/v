module fastc

import os

fn test_capture_abi_with_both_os_and_fastc_bindings() ! {
	sh := os.find_abs_path_of_executable('sh')!
	for code in [0, 1, 23, 255] {
		args := ['-c', 'printf capture; exit ${code}']
		regular := os.exec([sh, ...args])
		fast := fastc_run_command(sh, args)
		assert regular.exit_code == code, regular.output
		assert fast.exit_code == code, fast.output
		assert regular.output == 'capture'
		assert fast.output == regular.output
	}
}
