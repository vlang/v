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

	if !cmd2.ends_with('\n') {
		cmd2 += '\n'
	}

	p.stdin_write(cmd2)
	p.stdin_write('\necho **OK**\n')

	for {
		if !p.is_alive() {
			break
		}
		line = p.stdout_read()
		println(line)
		// line_err = p.stderr_read() //IF WE CALL STDERR_READ will block
		// we need a mechanism which allows us to check if stderr/stdout has data or it should never block
		// is not a good way, need to use a string buffer, is slow like this
		out += line
		if out.ends_with('**OK**\n') {
			out = out[0..(out.len - 7)]
			break
		}
	}

	// println("read from stdout, should not block")
	// is not really needed but good test to see behaviour
	// out += p.stdout_read()
	// println("read done")

	// println(cmd.stderr_read())

	if p.code > 0 {
		rc = 1
		println('ERROR:')
		println(cmd2)
		print(out)
	}
	// documentation says we need to call p.wait(), but this does not seem to work, will be process stop or become zombie?
	// p.wait()

	return out, rc
}

fn main() {
	mut out := ''
	mut rc := 0

	// the following does not work, not sure why not
	// out,rc = exec("find /tmp/ && echo '******'")

	out, rc = exec("find /tmp/ ; echo '******'")
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
