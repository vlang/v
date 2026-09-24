module diagserver

import os
import time
import v.workers

#include <errno.h>
#include <sys/wait.h>
#include <unistd.h>

fn C.waitpid(pid int, status &int, options int) int
fn C.dup2(oldfd int, newfd int) int
fn C._exit(code int)

// serve turns this compilation into a diagnostics server when the environment
// sets V_DIAGNOSTICS_SERVER, and returns '' at once otherwise. The work done so
// far - builtin, parsed - is kept for every request: each `check` line read on
// stdin is answered by a child created by fork(), which returns from here and
// finishes the compilation exactly as a one-shot run of the same command line
// would, printing its diagnostics and exiting with its exit code. Once that
// child is gone the server prints `v-diagnostics-server: end <code> <token>` on
// a line of its own, where the token is whatever followed `check` on the
// request line: a source line quoted in a diagnostic cannot then pass for the
// end of the answer. It stops when stdin closes or a `quit` line arrives.
//
// A `query <token> <file>:<line>:<code><column>` line asks a question of the
// mini-VLS protocol instead, as `-line-info` does: its child returns the
// question, and answers it in place of the diagnostics. The token comes first,
// as the path of the file may hold spaces.
//
// A request carries nothing else: the command line fixes the input, and with
// it every setting the driver derived from the input before this point.
// Between requests the client changes the files on disk, and it only shows the
// diagnostics of its own files: see V_CHECK_SELECTED_FILES_ONLY.
pub fn serve() string {
	if os.getenv('V_DIAGNOSTICS_SERVER') == '' {
		return ''
	}
	// fork() keeps only the calling thread. The child gives the worker pools new
	// threads, and no other thread may be running.
	others := threads_besides_pool_workers()
	if others > 0 {
		println('v-diagnostics-server: unavailable (${others} threads besides the worker pools; start it without -v and with -no-memory-limit)')
		flush_stdout()
		return ''
	}
	// The client shows the diagnostics of its own files only, so the library
	// bodies that cannot produce one for them are left unchecked.
	os.setenv('V_CHECK_SELECTED_FILES_ONLY', '1', true)
	// The client may rebuild the input directory between requests. The child
	// enters it again by name, or it would see the directory that was there
	// when the server started.
	work_dir := os.getwd()
	println('v-diagnostics-server: ready')
	flush_stdout()
	for {
		line := os.get_raw_line()
		if line == '' {
			exit(0)
		}
		request := line.trim_space()
		if request == '' {
			continue
		}
		if request == 'quit' {
			exit(0)
		}
		mut token := ''
		mut question := ''
		if request == 'check' || request.starts_with('check ') {
			token = request.all_after('check').trim_space()
		} else if request.starts_with('query ') && request.all_after('query ').trim_space().contains(' ') {
			rest := request.all_after('query ').trim_space()
			token = rest.all_before(' ')
			question = rest.all_after(' ').trim_space()
		} else {
			println('v-diagnostics-server: unknown request `${request}`')
			answered(2, '')
			continue
		}
		pid := os.fork()
		if pid == 0 {
			// The client reads a single stream: diagnostics go where the answer goes.
			C.dup2(1, 2)
			os.chdir(work_dir) or {
				eprintln('v-diagnostics-server: cannot enter ${work_dir}: ${err}')
				exit(2)
			}
			workers.note_fork()
			// The server runs without the compiler's memory watchdog, which is a
			// thread of its own; the child, which may start threads, keeps one.
			spawn watch_memory(memory_limit_kb())
			return question
		}
		if pid < 0 {
			println('v-diagnostics-server: fork failed')
			answered(2, token)
			continue
		}
		// A client that no longer wants this answer can stop the child.
		println('v-diagnostics-server: child ${pid} ${token}')
		flush_stdout()
		answered(wait_exit_code(pid), token)
	}
}

// threads_besides_pool_workers counts this process's threads other than the
// main one, which calls this, and the worker pool threads, named `v3-pool`.
fn threads_besides_pool_workers() int {
	main_task := os.getpid().str()
	mut others := 0
	for task in os.ls('/proc/self/task') or { return 1 } {
		if task == main_task {
			continue
		}
		name := os.read_file('/proc/self/task/${task}/comm') or { continue }
		if name.trim_space() != 'v3-pool' {
			others++
		}
	}
	return others
}

// memory_limit_kb is what a check may use before it is stopped: the limit of the
// compiler's own watchdog, or V_DIAGNOSTICS_MEMORY_LIMIT_MB.
fn memory_limit_kb() i64 {
	mb := os.getenv('V_DIAGNOSTICS_MEMORY_LIMIT_MB').i64()
	return if mb > 0 { mb * 1024 } else { i64(10176) * 1024 }
}

// watch_memory stops the check once its resident memory passes `limit_kb`.
fn watch_memory(limit_kb i64) {
	page_kb := i64(os.page_size()) / 1024
	for {
		statm := os.read_file('/proc/self/statm') or { return }
		fields := statm.fields()
		if fields.len > 1 && fields[1].i64() * page_kb > limit_kb {
			eprintln('v-diagnostics-server: the check passed its memory limit of ${limit_kb / 1024} MB and was stopped')
			flush_stderr()
			C._exit(137)
		}
		time.sleep(50 * time.millisecond)
	}
}

fn answered(code int, token string) {
	println('\nv-diagnostics-server: end ${code} ${token}')
	flush_stdout()
}

// wait_exit_code waits for the child `pid` and returns its exit code, or 128
// plus the number of the signal that killed it.
fn wait_exit_code(pid int) int {
	mut status := 0
	for C.waitpid(pid, &status, 0) < 0 {
		if C.errno != C.EINTR {
			return 2
		}
	}
	if status & 0x7f == 0 {
		return (status >> 8) & 0xff
	}
	return 128 + (status & 0x7f)
}
