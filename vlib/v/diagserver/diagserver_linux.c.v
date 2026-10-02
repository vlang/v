module diagserver

import crypto.sha256
import hash
import os
import time
// The requests carry tokens of their own.
import v.token as vtoken
import v.workers

#include <errno.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <unistd.h>

// `-os cross` compiles this file into vc/v.c for every Unix, and only `$if`
// guards reach the C preprocessor there: prctl and the memfd_create syscall are
// Linux's.
$if linux {
	#include <sys/prctl.h>
	#include <sys/syscall.h>
}

fn C.waitpid(pid int, status &int, options int) int
fn C.dup2(oldfd int, newfd int) int
fn C._exit(code int)
fn C.kill(pid int, sig int) int
fn C.prctl(option int, arg2 voidptr, arg3 u64, arg4 u64, arg5 u64) int
fn C.lseek(fd int, offset i64, whence int) i64
fn C.flock(i32, i32) i32
fn C.pread(fd int, buf voidptr, count usize, offset i64) isize
fn C.pwrite(fd int, buf voidptr, count usize, offset i64) isize

// Request is what a child of the diagnostics server was made for: answering
// `question`, the questions of a `query` line, or checking the program when it
// is empty. A child that answered can answer the next questions too, while the
// files it read hold what they held (see next_question). In a server that
// shares its children, it answers the next checks too.
pub struct Request {
pub:
	question string
	// from_server tells a child of the server from a one-shot compilation.
	from_server bool
mut:
	questions LineReader // the next questions from the server, and its `go`
	status_fd int = -1 // where the child tells the server what it does
	inputs    Inputs // the files the check read
	buffer    []u8   // where the child reads them again
	current   bool   // they held what the check read when the child looked
	// digests are what the check read in each file, which the child reads
	// again once its first answer is out (see keep_inputs).
	digests  map[string]string
	base_kb  i64    // the child's resident memory after its first answer
	work_dir string // the input directory, which the child enters by name
	// shares_children is set in a server whose checks and questions share their
	// children.
	shares_children bool
	diagnostics     Diagnostics
	// token is the one of the check the child answers, which ends its partial
	// answer (see print_diagnostics), when the client takes partial answers
	// (V_DIAGNOSTICS_PARTIAL).
	token    string
	partials bool
	// partial prints what the check found before the grandchild went on, and
	// returns the exit code it gives.
	partial fn () int = unsafe { nil }
	// record_fd holds what the last check that recorded one left for the next
	// check of the program (see keep_incremental_record).
	record_fd int = -1
	// idle does part of the work the child can do while it waits for a question,
	// and reports whether some remains (see keep_busy_with).
	idle fn () bool = unsafe { nil }
}

// check_question is what the server sends a child that answers checks, for
// the diagnostics of its program, with the token of the check after it. A
// question names a file and a line.
const check_question = 'check'

// partial_answer_ms is how long a check waits for its grandchild before it
// sends the diagnostics it found so far (see print_diagnostics).
const partial_answer_ms = 3

// Diagnostics are what a grandchild of a shared child printed for the program:
// the rest of the check, which rewrites the tree that the child answers its
// questions from (see diagnose_in_grandchild).
struct Diagnostics {
mut:
	pid    int = -1 // the grandchild, until its output is read
	fd     int = -1 // the file it prints into
	ready  bool // output and code hold what it printed, and its exit code
	failed bool // no grandchild could be made
	output string
	code   int
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
// file it read holds the same bytes, no file was added next to one of them or
// removed, and each import resolves to the directory it was read from, and to a
// new child that checks the program otherwise.
//
// With V_DIAGNOSTICS_SHARED set, checks and queries share their children: the
// child of a check stays too, and the next check or query goes to the child
// that checked the program, while its files hold (see diagnose_in_grandchild).
// A client that asks both of each version of a program checks it once.
//
// Such a check checks again only the bodies of the functions whose code changed
// since the last check of the program, and takes what the others reported from
// what that check left in a file of the server (see
// types.start_incremental_check). The child checks the bodies it left out while
// it waits for a question, as a question reads their types. It leaves out no
// body unless those it would leave out hold V_DIAGNOSTICS_INCREMENTAL_MIN_NODES
// nodes (2048 by default). V_DIAGNOSTICS_INCREMENTAL=0 checks every body; with
// V_DIAGNOSTICS_INCREMENTAL_VERIFY set, the child checks them before it tells
// the server it answered, and traces each body that reports otherwise than what
// was put back for it (see V_DIAGNOSTICS_TRACE).
//
// The children of the last versions of the program stay, up to
// V_DIAGNOSTICS_WARM_CHILDREN (3 by default): a child whose files changed waits
// for them to hold what it read again, as when an edit is undone, and answers
// then at once. Each request goes to the child that holds the version on disk,
// which each of them looks for at once, and to a new child when none does.
//
// A request carries nothing else: the command line fixes the input, and with
// it every setting the driver derived from the input before this point.
// Between requests the client changes the files on disk, and it only shows the
// diagnostics of its own files: see V_CHECK_SELECTED_FILES_ONLY.
pub fn serve() Request {
	if os.getenv('V_DIAGNOSTICS_SERVER') == '' {
		return Request{}
	}
	shares_children := os.getenv('V_DIAGNOSTICS_SHARED') != ''
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
	// What a check leaves for the next check of the program, in a file that
	// every child sees and that ends with the server.
	record_fd := if shares_children {
		memory_file(c'v-diagnostics-record')
	} else {
		-1
	}
	println('v-diagnostics-server: ready')
	flush_stdout()
	// The children that stay, the one that answered last first.
	mut warm := []Warm{}
	warm_limit := warm_children_limit()
	for {
		line := os.get_raw_line()
		if line == '' {
			stop_all(mut warm)
			exit(0)
		}
		request := line.trim_space()
		if request == '' {
			continue
		}
		if request == 'quit' {
			stop_all(mut warm)
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
		if (question != '' || shares_children) && warm.len > 0 {
			if i := holding_child(mut warm, if question != '' {
				question
			} else {
				'${check_question} ${token}'
			})
			{
				mut child := warm[i]
				warm.delete(i)
				println('v-diagnostics-server: child ${child.pid} ${token}')
				flush_stdout()
				code := child.answer()
				if child.pid > 0 {
					warm.prepend(child)
				}
				answered(code, token)
				continue
			}
		}
		// The child of a query reads its next questions from one pipe and tells
		// the server on another that it answered.
		mut channel := Channel{}
		if question != '' || shares_children {
			channel = new_channel()
		}
		server_pid := os.getpid()
		pid := os.fork()
		if pid == 0 {
			// A test holds the child here, before it asks to end with the server,
			// until something writes to the FIFO V_DIAGNOSTICS_CHILD_PAUSE names.
			if pause := os.getenv_opt('V_DIAGNOSTICS_CHILD_PAUSE') {
				os.read_file(pause) or {}
			}
			// The client reads a single stream: diagnostics go where the answer goes.
			C.dup2(1, 2)
			C.signal(C.SIGPIPE, C.SIG_DFL)
			// Nobody reads what a child prints once the server is gone. The kernel
			// ends the child with the server only if the server is still there when
			// the child asks: one that ended since the fork left the child to another
			// parent, and the child ends at once.
			$if linux {
				if C.prctl(C.PR_SET_PDEATHSIG, voidptr(usize(C.SIGKILL)), 0, 0, 0) != 0 {
					eprintln('v-diagnostics-server: a child cannot follow the server')
					exit(2)
				}
			}
			if os.getppid() != server_pid {
				C._exit(2)
			}
			for mut other in warm {
				other.close_server_ends()
			}
			os.chdir(work_dir) or {
				eprintln('v-diagnostics-server: cannot enter ${work_dir}: ${err}')
				exit(2)
			}
			workers.note_fork()
			// The server runs without the compiler's memory watchdog, which is a
			// thread of its own; the child, which may start threads, keeps one.
			spawn watch_memory(memory_limit_kb())
			return channel.child_request(question, work_dir, shares_children, token, record_fd)
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
					warm.prepend(child)
					for warm.len > warm_limit {
						mut oldest := warm.pop()
						oldest.stop()
					}
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

// shares_checks reports whether this child answers the checks of its program
// too, besides its questions.
pub fn (r &Request) shares_checks() bool {
	return r.shares_children && r.status_fd >= 0
}

// print_partial_with sets what prints the diagnostics the check found before
// the grandchild went on, and returns the exit code they give: a check whose
// grandchild takes long sends them first (see print_diagnostics).
pub fn (mut r Request) print_partial_with(print fn () int) {
	r.partial = print
}

// asks_for_diagnostics reports whether `question`, which next_question
// returned, asks for the diagnostics of the program (see print_diagnostics).
pub fn (r &Request) asks_for_diagnostics(question string) bool {
	return question == check_question || question.starts_with('${check_question} ')
}

// diagnose_in_grandchild makes a grandchild that goes on with the check, and
// prints the diagnostics of the program into a file, as a one-shot check prints
// them: the rest of the check rewrites the tree that the child answers its
// questions from. It returns true in the grandchild, which ends with the check.
// In the child it returns false, unless no grandchild can be made and the child
// was made for a check: it then goes on with the check itself, answers nothing
// more, and ends with it.
pub fn (mut r Request) diagnose_in_grandchild() bool {
	flush_stdout()
	flush_stderr()
	fd := memory_file(c'v-diagnostics')
	child_pid := os.getpid()
	pid := if fd >= 0 { os.fork() } else { -1 }
	if pid == 0 {
		// What it prints goes to its file only: a grandchild that outlives the
		// child cannot write into an answer. It does not outlive it for long,
		// and a child that ended since the fork leaves it to end at once, as
		// serve's children do with the server.
		$if linux {
			if C.prctl(C.PR_SET_PDEATHSIG, voidptr(usize(C.SIGKILL)), 0, 0, 0) != 0 {
				C._exit(2)
			}
		}
		if os.getppid() != child_pid {
			C._exit(2)
		}
		C.dup2(fd, 1)
		C.dup2(fd, 2)
		os.fd_close(fd)
		r.close_channel()
		workers.note_fork()
		spawn watch_memory(memory_limit_kb())
		return true
	}
	if pid < 0 {
		if fd >= 0 {
			os.fd_close(fd)
		}
		r.diagnostics = Diagnostics{
			failed: true
		}
		if r.question == '' {
			r.close_channel()
			return true
		}
		return false
	}
	r.diagnostics = Diagnostics{
		pid: pid
		fd:  fd
	}
	return false
}

// memory_file makes a file that lives in memory and ends with its last
// descriptor, and returns that descriptor, or -1 where it cannot be made.
fn memory_file(name &char) int {
	$if linux {
		return unsafe { int(C.syscall(C.SYS_memfd_create, name, voidptr(0))) }
	} $else {
		return -1
	}
}

// keep_busy_with has the child call `step` while it waits for its next
// question, until `step` returns false or a question comes: the work its next
// answers need, done before they are asked.
pub fn (mut r Request) keep_busy_with(step fn () bool) {
	r.idle = step
}

// print_diagnostics prints what the grandchild printed for the program, once it
// ended, and returns its exit code, that of a one-shot check. For a client that
// takes partial answers (V_DIAGNOSTICS_PARTIAL), a grandchild that takes longer
// than partial_answer_ms has the child print the diagnostics it found before
// first (see print_partial_with), and end them on a line of their own,
// `v-diagnostics-server: partial <code> <token>`: the client can show the
// errors of the program while the rest of the check runs, the instances of its
// generic functions above all.
pub fn (mut r Request) print_diagnostics() int {
	if !r.diagnostics.ready {
		mut code := 0
		if r.partials && r.partial != unsafe { nil } && r.token != '' {
			code = exit_code_within(r.diagnostics.pid, partial_answer_ms) or {
				partial_code := r.partial()
				flush_stdout()
				flush_stderr()
				os.fd_write(1, '\nv-diagnostics-server: partial ${partial_code} ${r.token}\n')
				wait_exit_code(r.diagnostics.pid)
			}
		} else {
			code = wait_exit_code(r.diagnostics.pid)
		}
		output := read_from_start(r.diagnostics.fd)
		os.fd_close(r.diagnostics.fd)
		r.diagnostics = Diagnostics{
			ready:  true
			output: output
			code:   code
		}
	}
	flush_stdout()
	os.fd_write(1, r.diagnostics.output)
	return r.diagnostics.code
}

// incremental_record returns what the last check of the program that recorded
// something left for the next one (see keep_incremental_record), or ''.
pub fn (r &Request) incremental_record() string {
	if r.record_fd < 0 {
		return ''
	}
	C.flock(r.record_fd, C.LOCK_SH)
	mut out := []u8{}
	mut chunk := []u8{len: 65536}
	for {
		got := C.pread(r.record_fd, unsafe { &u8(chunk.data) }, usize(chunk.len), i64(out.len))
		if got < 0 && C.errno == C.EINTR {
			continue
		}
		if got <= 0 {
			break
		}
		out << chunk[..int(got)]
	}
	C.flock(r.record_fd, C.LOCK_UN)
	return out.bytestr()
}

// keep_incremental_record keeps `text` for the next check of the program, in
// place of what an earlier check kept: a check that checks again only the
// function bodies that changed reads it (see types.start_incremental_check).
pub fn (r &Request) keep_incremental_record(text string) {
	if r.record_fd < 0 || text == '' {
		return
	}
	C.flock(r.record_fd, C.LOCK_EX)
	C.ftruncate(r.record_fd, u64(0))
	mut written := 0
	for written < text.len {
		wrote := C.pwrite(r.record_fd, unsafe { text.str + written }, usize(text.len - written),
			i64(written))
		if wrote < 0 && C.errno == C.EINTR {
			continue
		}
		if wrote <= 0 {
			// A record cut short reads as none.
			C.ftruncate(r.record_fd, u64(0))
			break
		}
		written += int(wrote)
	}
	C.flock(r.record_fd, C.LOCK_UN)
}

// exit_code_within waits `ms` milliseconds at most for the child `pid`, and
// returns its exit code as wait_exit_code does, or none when it still runs.
fn exit_code_within(pid int, ms int) ?int {
	for _ in 0 .. ms * 4 {
		mut status := 0
		got := C.waitpid(pid, &status, C.WNOHANG)
		if got == pid {
			return exit_code_of(status)
		}
		if got < 0 && C.errno != C.EINTR {
			return 2
		}
		time.sleep(250 * time.microsecond)
	}
	return none
}

fn (mut r Request) close_channel() {
	os.fd_close(r.questions.fd)
	os.fd_close(r.status_fd)
	r.questions.fd = -1
	r.status_fd = -1
}

// read_from_start reads the file open as `fd` from its start to its end.
fn read_from_start(fd int) string {
	if C.lseek(fd, 0, C.SEEK_SET) != 0 {
		return ''
	}
	mut out := []u8{}
	mut chunk := []u8{len: 65536}
	for {
		got := C.read(fd, unsafe { &u8(chunk.data) }, usize(chunk.len))
		if got < 0 && C.errno == C.EINTR {
			continue
		}
		if got <= 0 {
			break
		}
		out << chunk[..int(got)]
	}
	return out.bytestr()
}

// keep_inputs takes the files the check read, by absolute path, with the
// SHA-256 of what it read in each, in hexadecimal, or its quick_sum_digest:
// the child answers a next question only while they hold it, no file was added
// next to one of them or removed, and `imports_hold` finds each import
// resolving to the directory it was read from. It notes the names in their
// directories already. A quick sum is what the check read: the file is read
// again only for the next question. A file known by its SHA-256 is read again
// once the first answer is out, as the client does not wait for that.
pub fn (mut r Request) keep_inputs(digests map[string]string, imports_hold fn () bool) {
	r.inputs.imports_hold = imports_hold
	r.current = r.inputs.note_dirs(digests, mut r.buffer)
	for path, digest in digests {
		if sum, size := quick_sum_of(digest) {
			r.inputs.files << InputFile{
				path: path
				size: size
				sum:  sum
			}
			// Reading a file again takes one byte more than it held.
			if r.buffer.len <= size {
				r.buffer = []u8{len: size * 2 + 4096}
			}
		} else {
			r.digests[path] = digest
		}
	}
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
	mut last := false
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
	// The answer is out: the files the check read are read again now. One that
	// the client is writing again, with what it held or not, is read again when
	// the next question comes.
	if r.current && r.digests.len > 0 && r.inputs.note_files(r.digests, mut r.buffer) {
		r.digests = map[string]string{}
	}
	for r.idle != unsafe { nil } && !r.questions.has_input() {
		if !r.idle() {
			r.idle = unsafe { nil }
		}
	}
	for {
		question := r.questions.read_line()?
		if r.current && r.digests.len > 0 {
			r.current = r.inputs.note_files(r.digests, mut r.buffer)
			r.digests = map[string]string{}
		}
		// A child whose files held something else already answers no more, nor
		// does one whose grandchild could not take the diagnostics, for a check.
		if !r.current || (r.asks_for_diagnostics(question) && (!r.shares_children || r.diagnostics.failed)) {
			os.fd_write(r.status_fd, 'stale\n')
			return none
		}
		// The client may have written the input directory again, with the same
		// files: the child enters it by name again, as a new child does, or it
		// would read the relative paths of its next answer from the directory it
		// replaced. The child answers only from the program on disk: for another
		// version of it, it waits for the next question, which may find its own.
		if r.work_dir != '' {
			os.chdir(r.work_dir) or {
				os.fd_write(r.status_fd, 'other\n')
				continue
			}
		}
		if r.inputs.changed(mut r.buffer) {
			os.fd_write(r.status_fd, 'other\n')
			continue
		}
		// The server names the child that answers before its answer: it waits
		// for `go`, or another child answers.
		os.fd_write(r.status_fd, 'current\n')
		if r.questions.read_line()? == 'go' {
			if r.asks_for_diagnostics(question) {
				r.token = question.all_after(' ').trim_space()
			}
			return question
		}
	}
	return none
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
fn (mut c Channel) child_request(question string, work_dir string, shares_children bool, token string, record_fd int) Request {
	os.fd_close(c.questions_fd)
	os.fd_close(c.status_fd)
	return Request{
		question:        question
		from_server:     true
		questions:       LineReader{
			fd: c.child_questions_fd
		}
		status_fd:       c.child_status_fd
		work_dir:        work_dir
		shares_children: shares_children
		token:           token
		partials:        os.getenv('V_DIAGNOSTICS_PARTIAL') != ''
		record_fd:       record_fd
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

// Warm is a child that stays, with the program it checked. What it keeps is its
// own: the server, built without a garbage collector and running for hours,
// allocates next to nothing for it.
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

// offer sends the child `question`, which it answers if every file its check
// read holds what it read (see holding_child). False when it ended.
fn (mut w Warm) offer(question string) bool {
	mut status := 0
	if C.waitpid(w.pid, &status, C.WNOHANG) != 0 {
		// It ended, or cannot be waited for.
		w.close_server_ends()
		w.pid = -1
		return false
	}
	write_line(w.questions_fd, question)
	return true
}

// holding_child offers `question` to every child that stays, which each read
// their files again at once, and returns the index of the first one that holds
// the version of the program on disk. The others stay for their versions, but
// those that ended, or that answer no more, which leave.
fn holding_child(mut warm []Warm, question string) ?int {
	mut offered := []bool{len: warm.len}
	for i, mut child in warm {
		offered[i] = child.offer(question)
	}
	mut holding := -1
	mut leaving := []int{}
	for i, mut child in warm {
		reply := if offered[i] { child.status.read_line() or { '' } } else { '' }
		if reply == 'current' && holding < 0 {
			holding = i
		} else if reply == 'current' {
			// Two children of one version: the first answers.
			write_line(child.questions_fd, 'no')
		} else if reply != 'other' {
			leaving << i
		}
	}
	for j := leaving.len - 1; j >= 0; j-- {
		i := leaving[j]
		warm[i].stop()
		warm.delete(i)
		if holding > i {
			holding--
		}
	}
	return if holding >= 0 { holding } else { none }
}

// stop_all ends every child that stays.
fn stop_all(mut warm []Warm) {
	for mut child in warm {
		child.stop()
	}
	warm.clear()
}

// warm_children_limit is how many children may stay: V_DIAGNOSTICS_WARM_CHILDREN,
// or 3.
fn warm_children_limit() int {
	limit := os.getenv('V_DIAGNOSTICS_WARM_CHILDREN').int()
	return if limit > 0 { limit } else { 3 }
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
// allocates no memory. And a check that each import still resolves to the
// directory it was read from: a module added to a directory searched before
// that one changes no file that was read, nor any of their directories.
struct Inputs {
mut:
	files        []InputFile
	dirs         []InputDir
	imports_hold fn () bool = unsafe { nil }
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

// note_dirs keeps a sum of the names in the directories of the files of
// `digests`. False when there is no file to watch.
fn (mut i Inputs) note_dirs(digests map[string]string, mut buffer []u8) bool {
	if digests.len == 0 {
		return false
	}
	mut dirs := map[string]bool{}
	for path, _ in digests {
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

// note_files reads every file of `digests` again, and keeps a quick sum of
// them when they all hold what the check read. False when one holds something
// else, or cannot be read.
fn (mut i Inputs) note_files(digests map[string]string, mut buffer []u8) bool {
	mut files := []InputFile{cap: digests.len}
	for path, digest in digests {
		size := read_whole(path, mut buffer) or { return false }
		held := if digest.starts_with(quick_digest_prefix) {
			quick_sum_digest(quick_sum(buffer, size), size)
		} else {
			sha256.sum(buffer[..size]).hex()
		}
		if held != digest {
			return false
		}
		files << InputFile{
			path: path
			size: size
			sum:  quick_sum(buffer, size)
		}
	}
	i.files << files
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
	return i.imports_hold != unsafe { nil } && !i.imports_hold()
}

fn quick_sum(buffer []u8, size int) u64 {
	return vtoken.quick_sum(unsafe { &u8(buffer.data) }, size)
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

// has_input reports whether a line, or a part of one, can be read at once.
fn (r &LineReader) has_input() bool {
	return r.start < r.end || os.fd_is_pending(r.fd)
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
	return exit_code_of(status)
}

// exit_code_of is the exit code of a wait status, or 128 plus the number of
// the signal that killed the child.
fn exit_code_of(status int) int {
	if status & 0x7f == 0 {
		return (status >> 8) & 0xff
	}
	return 128 + (status & 0x7f)
}
