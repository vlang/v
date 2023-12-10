import os
import time
import term

python_exe := os.find_abs_path_of_executable('python') or {
	eprintln('This example needs a python executable in your PATH. Please install Python to see it in action.')
	exit(1)
}
mut p := os.new_process(python_exe)
defer {
	dump(p.code)
	p.close()
	p.wait()
}
// The Python flags here, are needed to reduce clutter and buffering.
// See https://docs.python.org/3/using/cmdline.html
p.set_args(['-i', '-q', '-u'])
p.set_redirect_stdio()
p.run()
dump(p.pid)
println('This is a simple V wrapper/shell for the Python interpreter.')
println('Try typing some python code here, or type `bye` to end your session:')
for p.is_alive() {
	// check if there is any input from the user (it does not block, if there is not):
	if os.fd_is_pending(0) {
		cmd := os.get_raw_line()
		if cmd.len == 0 {
			println('closed stdin detected, perhaps due to Ctrl-D...exiting')
			break
		}
		if cmd.trim_space() == 'bye' {
			println('Goodbye...')
			break
		}
		p.stdin_write(cmd)
	}
	if oline := p.pipe_read(.stdout) {
		print(term.bright_yellow('python stdout: ') + term.bold(oline))
	}
	if eline := p.pipe_read(.stderr) {
		eprint(term.red('python stderr: ') + eline)
	}
	time.sleep(20 * time.millisecond)
}
