module diagserver

import crypto.sha256
import hash
import os
import time
import v.workers

#include <errno.h>
#include <signal.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <unistd.h>

fn C.waitpid(pid int, status &int, options int) int
fn C.dup2(oldfd int, newfd int) int
fn C._exit(code int)
fn C.kill(pid int, sig int) int
fn C.prctl(option int, arg2 voidptr, arg3 u64, arg4 u64, arg5 u64) int

// Request is what a child of the diagnostics server was made for: answering
// `question`, the questions of a `query` line, or checking the program when it
// is empty. A child that answered can answer the next questions too, while the
// files it read hold what they held (see next_question).
pub struct Request {
pub:
	question string
mut:
	questions LineReader // the next questions from the server, and its `go`
	status_fd int = -1 // where the child tells the server what it does
	inputs    Inputs // the files the check read
	buffer    []u8   // where the child reads them again
	current   bool   // they held what the check read when the child looked
	base_kb   i64    // the child's resident memory after its first answer
}

// serve turns this compilation into a diagnostics server when the environment
// sets V_DIAGNOSTICS_SERVER, and returns an empty request at once otherwise. The
// work done so far - builtin, parsed - is kept for every request: each `check`
// line read on stdin is answered by a child created by fork(), which returns
// from here and finishes the compilation exactly as a one-shot run of the same
// command line would, printing its diagnostics and exiting with its exit code.
// Once that child is gone the server prints `v-diagnostics-server: end <code>
// <token>` on a line of its own, where the token is whatever followed `check`
// on the request line: a source line quoted in a diagnostic cannot then pass for
// the end of the answer. It stops when stdin closes or a `quit` line arrives.
//
// A `query <token> <file>:<line>:<code><column>` line asks a question of the
// mini-VLS protocol instead, as `-line-info` does: its child returns the
// question, and answers it in place of the diagnostics. The token comes first,
// as the path of the file may hold spaces. The child that answered a query
// stays, with the program it checked: the next query goes to it while every
// file it read holds the same bytes, and no file was added next to one of them
// or removed, and to a new child that checks the program otherwise.
//
// A request carries nothing else: the command line fixes the input, and with
// it every setting the driver derived from the input before this point.
// Between requests the client changes the files on disk, and it only shows the
// diagnostics of its own files: see V_CHECK_SELECTED_FILES_ONLY.
pub fn serve() Request {
	if os.getenv('V_DIAGNOSTICS_SERVER') == '' {
		return Request{}
	}
	// fork() keeps only the calling thread. The child gives the worker pools new
	// threads, and no other thread may be running.
	others := threads_besides_pool_workers()
	if others > 0 {
		println('v-diagnostics-server: unavailable (${others} threads besides the worker pools; start it without -v and with -no-memory-limit)')
		flush_stdout()
		return Request{}
	}
	// The client shows the diagnostics of its own files only, so the library
	// bodies that cannot produce one for them are left unchecked.
	os.setenv('V_CHECK_SELECTED_FILES_ONLY', '1', true)
	// The client may rebuild the input directory between requests. The child
	// enters it again by name, or it would see the directory that was there
	// when the server started.
	work_dir := os.getwd()
	// A child that ended between two queries closed the pipe the server sends
	// the next one to: writing there must not end the server.
	C.signal(C.SIGPIPE, C.SIG_IGN)
	println('v-diagnostics-server: ready')
	flush_stdout()
	mut warm := Warm{}
	for {
		line := os.get_raw_line()
		if line == '' {
			warm.stop()
			exit(0)
		}
		request := line.trim_space()
		if request == '' {
			continue
		}
		if request == 'quit' {
			warm.stop()
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
		if question != '' && warm.pid > 0 {
			if warm.accepts(question) {
				println('v-diagnostics-server: child ${warm.pid} ${token}')
				flush_stdout()
				answered(warm.answer(), token)
				continue
			}
			warm.stop()
		}
		// The child of a query reads its next questions from one pipe and tells
		// the server on another that it answered.
		mut channel := Channel{}
		if question != '' {
			channel = new_channel()
		}
		pid := os.fork()
		if pid == 0 {
			// The client reads a single stream: diagnostics go where the answer goes.
			C.dup2(1, 2)
			C.signal(C.SIGPIPE, C.SIG_DFL)
			// Nobody reads what a child prints once the server is gone.
			C.prctl(C.PR_SET_PDEATHSIG, voidptr(usize(C.SIGKILL)), 0, 0, 0)
			warm.close_server_ends()
			os.chdir(work_dir) or {
				eprintln('v-diagnostics-server: cannot enter ${work_dir}: ${err}')
				exit(2)
			}
			workers.note_fork()
			// The server runs without the compiler's memory watchdog, which is a
			// thread of its own; the child, which may start threads, keeps one.
			spawn watch_memory(memory_limit_kb())
			return channel.child_request(question)
		}
		if pid < 0 {
			channel.close_all()
			println('v-diagnostics-server: fork failed')
			answered(2, token)
			continue
		}
		channel.close_child_ends()
		// A client that no longer wants this answer can stop the child.
		println('v-diagnostics-server: child ${pid} ${token}')
		flush_stdout()
		if channel.questions_fd >= 0 {
			mut child := Warm{
				pid:          pid
				questions_fd: channel.questions_fd
				status:       LineReader{
					fd: channel.status_fd
				}
			}
			// The child tells that it answered, and whether it can answer
			// again, or ends.
			if code := child.read_first_report() {
				if child.answers_again {
					warm = child
				} else {
					child.stop()
				}
				answered(code, token)
				continue
			}
			child.close_server_ends()
		}
		answered(wait_exit_code(pid), token)
	}
	return Request{}
}

// answers_again reports whether this child can answer more questions after
// its first ones: the server gave it a way to receive them.
pub fn (r &Request) answers_again() bool {
	return r.status_fd >= 0
}

// keep_inputs takes the files the check read, by absolute path, with the
// SHA-256 of what it read in each, in hexadecimal: the child answers a next
// question only while they hold it, and no file was added next to one of them
// or removed. It reads them again already, as one may have changed meanwhile.
pub fn (mut r Request) keep_inputs(digests map[string]string) {
	r.current = r.inputs.prepare(digests, mut r.buffer)
}

// next_question tells the server that the answer is complete, with the exit
// code a one-shot run would have, and returns the next question it sends, or
// none once the server wants no more from this child. The memory a child
// allocates for its answers is never freed: after it has grown by
// V_DIAGNOSTICS_RETIRE_MB (1024 by default) since its first answer, it answers
// no more, and a new child checks the program for the next question.
pub fn (mut r Request) next_question(code int) ?string {
	flush_stdout()
	flush_stderr()
	resident := resident_kb()
	mut last := !r.current
	if r.base_kb == 0 {
		r.base_kb = resident
	} else if resident - r.base_kb >= retire_growth_kb() {
		last = true
	}
	if last {
		os.fd_write(r.status_fd, 'last ${code}\n')
		return none
	}
	os.fd_write(r.status_fd, 'done ${code}\n')
	question := r.questions.read_line()?
	// The child answers only from the program on disk. The server names it
	// before its answer: it waits for `go`.
	if r.inputs.changed(mut r.buffer) {
		os.fd_write(r.status_fd, 'stale\n')
		return none
	}
	os.fd_write(r.status_fd, 'current\n')
	if r.questions.read_line()? != 'go' {
		return none
	}
	return question
}

fn retire_growth_kb() i64 {
	mb := os.getenv_opt('V_DIAGNOSTICS_RETIRE_MB') or { '1024' }
	return mb.i64() * 1024
}

fn resident_kb() i64 {
	statm := os.read_file('/proc/self/statm') or { return 0 }
	fields := statm.fields()
	if fields.len < 2 {
		return 0
	}
	return fields[1].i64() * (i64(os.page_size()) / 1024)
}

// Channel holds the pipes of a query child: the server writes its next
// questions into one, and reads from the other that the child answered.
struct Channel {
mut:
	questions_fd       int = -1 // the server's end
	status_fd          int = -1 // the server's end
	child_questions_fd int = -1
	child_status_fd    int = -1
}

fn new_channel() Channel {
	questions := os.pipe() or { return Channel{} }
	status := os.pipe() or {
		os.fd_close(questions.read_fd)
		os.fd_close(questions.write_fd)
		return Channel{}
	}
	return Channel{
		questions_fd:       questions.write_fd
		status_fd:          status.read_fd
		child_questions_fd: questions.read_fd
		child_status_fd:    status.write_fd
	}
}

// child_request closes the server's ends in the child, which keeps its own.
fn (mut c Channel) child_request(question string) Request {
	os.fd_close(c.questions_fd)
	os.fd_close(c.status_fd)
	return Request{
		question:  question
		questions: LineReader{
			fd: c.child_questions_fd
		}
		status_fd: c.child_status_fd
	}
}

fn (mut c Channel) close_child_ends() {
	os.fd_close(c.child_questions_fd)
	os.fd_close(c.child_status_fd)
	c.child_questions_fd = -1
	c.child_status_fd = -1
}

fn (mut c Channel) close_all() {
	c.close_child_ends()
	os.fd_close(c.questions_fd)
	os.fd_close(c.status_fd)
	c.questions_fd = -1
	c.status_fd = -1
}

// Warm is the child that answered the last query, kept with the program it
// checked. What it keeps is its own: the server, built without a garbage
// collector and running for hours, allocates next to nothing for it.
struct Warm {
mut:
	pid           int = -1
	questions_fd  int = -1
	status        LineReader
	answers_again bool // its last answer was not its last one
}

// read_first_report reads the exit code of the child's first answer, or none
// when it ends instead.
fn (mut w Warm) read_first_report() ?int {
	return w.answered(w.status.read_line()?)
}

// accepts sends the child `question` and reports whether it answers it: it is
// still there, and every file its check read holds what it read.
fn (mut w Warm) accepts(question string) bool {
	mut status := 0
	if C.waitpid(w.pid, &status, C.WNOHANG) != 0 {
		// It ended, or cannot be waited for.
		w.close_server_ends()
		w.pid = -1
		return false
	}
	write_line(w.questions_fd, question)
	return (w.status.read_line() or { return false }) == 'current'
}

// answer lets the child answer the question it accepted, and returns the exit
// code of its answer.
fn (mut w Warm) answer() int {
	os.fd_write(w.questions_fd, 'go\n')
	if line := w.status.read_line() {
		if code := w.answered(line) {
			if !w.answers_again {
				w.stop()
			}
			return code
		}
	}
	// It ended while answering: a client stopped it, or it failed.
	return w.stop()
}

// stop ends the child and returns its exit code.
fn (mut w Warm) stop() int {
	if w.pid <= 0 {
		return 0
	}
	w.close_server_ends()
	C.kill(w.pid, C.SIGKILL)
	code := wait_exit_code(w.pid)
	w = Warm{}
	return code
}

fn (mut w Warm) close_server_ends() {
	os.fd_close(w.questions_fd)
	os.fd_close(w.status.fd)
	w.questions_fd = -1
	w.status.fd = -1
}

// answered reads the line with which the child ends an answer: `done <code>`,
// or `last <code>` when it will answer no more.
fn (mut w Warm) answered(line string) ?int {
	if line.starts_with('done ') {
		w.answers_again = true
	} else if line.starts_with('last ') {
		w.answers_again = false
	} else {
		return none
	}
	return line[5..].int()
}

// Inputs are the files a check read, with a quick sum of what it read in each,
// and a sum of the names in each of their directories. Comparing them again
// allocates no memory.
struct Inputs {
mut:
	files []InputFile
	dirs  []InputDir
}

struct InputFile {
	path string
	size int
	sum  u64
}

struct InputDir {
	path    string
	entries int
	sum     u64
}

// prepare reads every file of `digests` again, and keeps a quick sum of those
// that hold what the check read, and of the names in their directories. False
// when one holds something else already, or when there is no file to watch.
fn (mut i Inputs) prepare(digests map[string]string, mut buffer []u8) bool {
	if digests.len == 0 {
		return false
	}
	mut dirs := map[string]bool{}
	for path, digest in digests {
		size := read_whole(path, mut buffer) or { return false }
		if sha256.sum(buffer[..size]).hex() != digest {
			return false
		}
		i.files << InputFile{
			path: path
			size: size
			sum:  quick_sum(buffer, size)
		}
		dirs[os.dir(path)] = true
	}
	for dir, _ in dirs {
		entries, sum := names_sum(dir) or { return false }
		i.dirs << InputDir{
			path:    dir
			entries: entries
			sum:     sum
		}
		// A v.mod can change the program too. A new one changes the names of
		// its directory.
		vmod := os.join_path_single(dir, 'v.mod')
		if vmod !in digests {
			if size := read_whole(vmod, mut buffer) {
				i.files << InputFile{
					path: vmod
					size: size
					sum:  quick_sum(buffer, size)
				}
			}
		}
	}
	return true
}

// changed reports whether a file holds other bytes than those read, or a
// directory other names.
fn (i &Inputs) changed(mut buffer []u8) bool {
	for file in i.files {
		// One byte more than was read tells a file that grew.
		if buffer.len <= file.size {
			return true
		}
		size := read_into(file.path, mut buffer) or { return true }
		if size != file.size || quick_sum(buffer, size) != file.sum {
			return true
		}
	}
	for dir in i.dirs {
		entries, sum := names_sum(dir.path) or { return true }
		if entries != dir.entries || sum != dir.sum {
			return true
		}
	}
	return false
}

fn quick_sum(buffer []u8, size int) u64 {
	return hash.wyhash_c(unsafe { &u8(buffer.data) }, u64(size), 0)
}

// read_whole reads the file `path` into `buffer`, which it makes larger than
// the file when needed, and returns its size.
fn read_whole(path string, mut buffer []u8) ?int {
	want := int((os.stat(path) or { return none }).size)
	if buffer.len <= want {
		buffer = []u8{len: want * 2 + 4096}
	}
	size := read_into(path, mut buffer)?
	if size != want {
		// It changed while it was being read.
		return none
	}
	return size
}

// read_into reads the file `path` into `buffer`, as much of it as fits, and
// returns how many bytes it read.
fn read_into(path string, mut buffer []u8) ?int {
	fd := C.open(&char(path.str), C.O_RDONLY)
	if fd < 0 {
		return none
	}
	defer {
		C.close(fd)
	}
	mut size := 0
	for size < buffer.len {
		got := C.read(fd, unsafe { &u8(buffer.data) + size }, usize(buffer.len - size))
		if got < 0 {
			if C.errno == C.EINTR {
				continue
			}
			return none
		}
		if got == 0 {
			break
		}
		size += int(got)
	}
	return size
}

// names_sum returns how many entries the directory `path` holds, and a sum of
// their names that does not depend on their order.
fn names_sum(path string) ?(int, u64) {
	dir := C.opendir(&char(path.str))
	if isnil(dir) {
		return none
	}
	defer {
		C.closedir(dir)
	}
	mut entries := 0
	mut sum := u64(0)
	for {
		ent := C.readdir(dir)
		if isnil(ent) {
			break
		}
		name := unsafe { &u8(&ent.d_name[0]) }
		len := unsafe { vstrlen(name) }
		if unsafe { (len == 1 && name[0] == `.`) || (len == 2 && name[0] == `.` && name[1] == `.`) } {
			continue
		}
		entries++
		sum += hash.wyhash_c(name, u64(len), 0)
	}
	return entries, sum
}

// LineReader reads the lines of a pipe, through one small buffer it keeps: the
// server reads a line or two for each query, for hours.
struct LineReader {
mut:
	fd    int = -1
	chunk []u8
	start int // where the bytes read and not returned yet begin in `chunk`
	end   int
}

// read_line returns the next line, without its newline, or none at the end.
fn (mut r LineReader) read_line() ?string {
	if r.chunk.len == 0 {
		r.chunk = []u8{len: 4096}
	}
	mut line := []u8{}
	for {
		for r.start < r.end {
			c := r.chunk[r.start]
			r.start++
			if c == `\n` {
				return line.bytestr()
			}
			line << c
		}
		got := C.read(r.fd, unsafe { &u8(r.chunk.data) }, usize(r.chunk.len))
		if got < 0 && C.errno == C.EINTR {
			continue
		}
		if got <= 0 {
			return none
		}
		r.start = 0
		r.end = int(got)
	}
	return none
}

// write_line writes `line` and a newline to `fd`, without joining them.
fn write_line(fd int, line string) {
	os.fd_write(fd, line)
	os.fd_write(fd, '\n')
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
