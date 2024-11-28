module main

// This example shows how to communicate with a child process (`bash` in this case), by sending
// commands to its stdin pipe, and reading responses from its stdout and stderr pipes.
// Note, you can use `if p.is_pending(.stdout) {` and `if p.is_pending(.stderr) {`, to check if
// there is available data in the pipes, without having to block in your main loop, if the data
// is missing or just not available yet.
import os
import time

const tmp_folder = os.join_path(os.temp_dir(), 'process_folder')

const max_txt_files = 20

fn exec(cmd string) (string, int, string) {
	mut out := []string{}
	mut er := []string{}
	mut rc := 0

	mut p := os.new_process('/bin/bash')
	p.set_redirect_stdio()
	p.run()

	p.stdin_write('echo "START         " && sleep 0.1 && ${cmd};\n')
	p.stdin_write('ECODE=\$?;\n')
	p.stdin_write('sleep 0.1;\n')
	p.stdin_write('exit \$ECODE;\n')

	// Note, that you can also ensure that `bash` will exit, when the command finishes,
	// by closing its stdin pipe. In the above example, that is not needed however, since
	// the last `exit` command, will make it quit as well.
	// os.fd_close(p.stdio_fd[0])

	for p.is_alive() {
		if data := p.pipe_read(.stderr) {
			eprintln('p.pipe_read .stderr, len: ${data.len:4} | data: `${data#[0..10]}`...')
			er << data
		}
		if data := p.pipe_read(.stdout) {
			eprintln('p.pipe_read .stdout, len: ${data.len:4} | data: `${data#[0..10]}`...')
			out << data
		}
		// avoid a busy loop, by sleeping a bit between each iteration
		time.sleep(2 * time.millisecond)
	}

	// the process finished, slurp all the remaining data in the pipes:
	out << p.stdout_slurp()
	er << p.stderr_slurp()
	p.close()
	p.wait()

	if p.code > 0 {
		eprintln('----------------------------------------------------------')
		eprintln('COMMAND: ${cmd}')
		eprintln('STDOUT:\n${out}')
		eprintln('STDERR:\n${er}')
		eprintln('----------------------------------------------------------')
		rc = 1
	}

	return out.join(''), rc, er.join('')
}

fn main() {
	mut out := ''
	mut er := ''
	mut ecode := 0

	// prepare some files in a temporary folder
	defer {
		os.rmdir_all(tmp_folder) or {}
	}
	os.mkdir_all(tmp_folder) or {}
	for i in 0 .. max_txt_files {
		os.write_file(os.join_path(tmp_folder, '${i}.txt'), '${i}\n${i}\n')!
	}

	out, ecode, er = exec("find ${os.quoted_path(tmp_folder)} ; sleep 0.1; find ${os.quoted_path(tmp_folder)} ; echo '******'")
	assert out.ends_with('******\n')
	assert er == ''

	out, ecode, er = exec('echo to stdout')
	assert out.contains('to stdout')
	assert er == ''

	out, ecode, er = exec('echo to stderr 1>&2')
	assert out.starts_with('START')
	assert er.contains('to stderr')

	out, ecode, er = exec('ls /sssss')
	assert out.starts_with('START')
	assert er != ''
	assert ecode > 0 // THIS STILL GIVES AN ERROR !

	println('test ok stderr & stdout is indeed redirected, ecode: ${ecode}')
}
