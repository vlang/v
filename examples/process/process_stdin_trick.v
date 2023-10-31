module main

import os

// this is a example script to show you stdin can be used and keep a process open

fn exec(cmd string) (string, int) {
	mut cmd2 := cmd
	mut out := ''
	mut line := ''
	mut rc := 0
	mut p := os.new_process('/bin/bash')

	// there are methods missing to know if stderr/stdout has data as such its better to redirect bot on same FD
	// not so nice trick to run bash in bash and redirect stderr, maybe someone has a better solution
	p.set_args(['-c', 'bash 2>&1'])
	p.set_redirect_stdio()
	p.run()

	p.stdin_write('${cmd2} && echo **OK**')
	os.fd_close(p.stdio_fd[0]) // important: close stdin so cmd can end by itself

	for p.is_alive() {
		line = p.stdout_read()
		println(line)
		// line_err = p.stderr_read() //IF WE CALL STDERR_READ will block
		// we need a mechanism which allows us to check if stderr/stdout has data or it should never block
		// is not a good way, need to use a string buffer, is slow like this
		out += line
		if line.ends_with('**OK**\n') {
			out = out[0..(out.len - 7)]
			break
		}
	}

	// println("read from stdout, should not block")
	// is not really needed but good test to see behaviour
	out += p.stdout_read()
	println('read done')

	println(p.stderr_read())
	p.close()
	p.wait()
	if p.code > 0 {
		rc = 1
		println('ERROR:')
		println(cmd2)
		print(out)
	}

	return out, rc
}

fn main() {
	mut out := ''
	mut rc := 0

	// find files from /tmp excluding files unlistable by current user

	out, rc = exec("find /tmp/ -user \$UID; echo '******'")
	println(out)
	assert out.ends_with('******\n')

	out, rc = exec('echo to stdout')
	assert out.contains('to stdout')

	out, rc = exec('echo to stderr 1>&2')
	assert out.contains('to stderr')

	out, rc = exec('ls /sssss')
	assert rc > 0 // THIS STILL GIVES AN ERROR !

	println('test ok stderr & stdout is indeed redirected')
}
