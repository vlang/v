module main

import os

const exe = os.find_abs_path_of_executable('wasm-validate') or {
	println('skipping test, since wasm-validate could not be found')
	exit(0)
}

pub fn validate(mod []u8) ! {
	mut proc := os.new_process(exe)
	proc.set_args(['-'])
	proc.set_redirect_stdio()
	proc.run()
	{
		os.fd_write(proc.stdio_fd[0], mod.bytestr())
		os.fd_close(proc.stdio_fd[0])
	}
	proc.wait()
	if proc.status != .exited {
		return error('wasm-validate exited abormally')
	}
	if proc.code != 0 {
		return error('wasm-validate exited with a non zero exit code')
	}
	proc.close()
}
