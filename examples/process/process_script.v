module main

import os

// a test where we execute a bash script but work around where we put script in bash inside bash

fn exec(path string, redirect bool) {
	mut line := ''
	mut line_err := ''
	mut cmd := os.new_process('/bin/bash')

	if redirect {
		cmd.set_args(['-c', '/bin/bash /tmp/test.sh 2>&1'])
	} else {
		cmd.set_args([path])
	}

	cmd.set_redirect_stdio()
	cmd.run()
	if cmd.is_alive() {
		for {
			line = cmd.stdout_read()
			println('STDOUT: ${line}')

			if !redirect {
				line_err = cmd.stderr_read()
				println('STDERR: ${line_err}')
			}

			if !cmd.is_alive() {
				break
			}
		}
	}
	if cmd.code > 0 {
		println('ERROR:')
		println(cmd)
		// println(cmd.stderr_read())
	}
}

fn main() {
	script := '
echo line 1
#will use some stderr now
echo redirect 1 to 2  1>&2
echo line 3
'

	os.write_file('/tmp/test.sh', script) or { panic(err) }
	// os.chmod("/tmp/test.sh",0o700) //make executable

	// this will work because stderr/stdout are smaller than 4096 chars, once larger there can be deadlocks
	// in other words this can never work reliably without being able to check if there is data on stderr or stdout
	exec('/tmp/test.sh', false)

	// this will always work
	exec('/tmp/test.sh', true)
}
