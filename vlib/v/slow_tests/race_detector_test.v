// vtest build: !windows
// Builds and runs programs with `v -race`, the data race detector.
import os

const vexe = os.quoted_path(@VEXE)
const tdir = os.join_path(os.vtmp_dir(), 'race_detector_test_${os.getpid()}')

const racy_source = 'struct Counter {
mut:
	n int
}

fn inc(mut c Counter) {
	for _ in 0 .. 1000 {
		c.n++
	}
}

fn main() {
	mut c := &Counter{}
	t1 := spawn inc(mut c)
	t2 := spawn inc(mut c)
	t1.wait()
	t2.wait()
	println("joined \${c.n > 0}")
}
'

const synchronized_source = 'import sync

struct Data {
mut:
	vals []int
}

fn producer(ch chan &Data) {
	for i in 0 .. 100 {
		mut d := &Data{}
		d.vals << i
		ch <- d
	}
	ch.close()
}

fn main() {
	ch := chan &Data{cap: 4}
	spawn producer(ch)
	mut total := 0
	for {
		d := <-ch or { break }
		total += d.vals[0]
	}
	mut wg := sync.new_waitgroup()
	mut mu := sync.new_mutex()
	mut shared_data := &Data{}
	for _ in 0 .. 4 {
		wg.add(1)
		spawn fn (mut wg sync.WaitGroup, mut mu sync.Mutex, mut d Data) {
			for k in 0 .. 100 {
				mu.lock()
				d.vals << k
				mu.unlock()
			}
			wg.done()
		}(mut wg, mut mu, mut shared_data)
	}
	wg.wait()
	shared s := []int{}
	threads := [spawn fn (shared s []int) {
		lock s {
			s << 1
		}
	}(shared s), spawn fn (shared s []int) {
		lock s {
			s << 2
		}
	}(shared s)]
	threads.wait()
	rlock s {
		println("total: \${total} vals: \${shared_data.vals.len} shared: \${s.len}")
	}
	println("race: \${is_race_build()}")
}

fn is_race_build() bool {
	\$if race ? {
		return true
	}
	return false
}
'

// Changing the value of an existing map entry in place, like `m[k].x += 1`, writes the map,
// like any insertion does, so it races with an unsynchronized `m.len`, even though it does
// not change the count.
const map_value_update_source = 'struct P {
mut:
	x int
}

struct Cell {
mut:
	m map[int]P
}

fn main() {
	mut c := &Cell{}
	c.m[1] = P{}
	t := spawn fn (mut c Cell) {
		for _ in 0 .. 100 {
			c.m[1].x += 1
		}
	}(mut c)
	mut n := 0
	for _ in 0 .. 100 {
		n += c.m.len
	}
	t.wait()
	println("len \${n}")
}
'

// `_ = value` reads the value, like in Go: in an optimized build too, which drops unused
// copies and values computed without side effects, and for a fixed array or a `mut`
// parameter, which C accesses through a pointer.
const blank_read_source = 'struct Cell {
mut:
	s string
	a [4]int
}

fn writer(mut c Cell) {
	for i in 0 .. 1000 {
		c.s = i.str()
		c.a[1] = i
	}
}

fn read_mut_param(mut c Cell) {
	_ = c
}

fn read_mut_param_in_parens(mut a [4]int) {
	_ = ((a))
}

fn main() {
	mut c := &Cell{}
	t := spawn writer(mut c)
	read := \$d("read", "string")
	for _ in 0 .. 1000 {
		match read {
			"string" { _ = c.s }
			"fixed_array" { _ = c.a }
			"computed" { _ = c.a[1] * 2 + 1 }
			"array_literal" { _ = [c.a[1], 0]! }
			"mut_param" { read_mut_param(mut c) }
			else { read_mut_param_in_parens(mut c.a) }
		}
	}
	t.wait()
	println("done")
}
'

// Reading a line of a file, which V does with `getc`, happens after the write of that line
// in another thread, like every file read.
const file_line_source = 'import os
import time

struct Data {
mut:
	x int
}

fn main() {
	path := os.join_path(os.vtmp_dir(), "race_file_line_\${os.getpid()}.txt")
	os.write_file(path, "")!
	mut r := os.open(path)!
	mut d := &Data{}
	t := spawn fn (mut d Data, path string) {
		d.x = 42
		mut w := os.open_append(path) or { panic(err) }
		w.write_string("ready\n") or { panic(err) }
		w.close()
	}(mut d, path)
	for os.file_size(path) == 0 {
		time.sleep(time.millisecond)
	}
	mut buf := []u8{len: 16}
	n := r.read_bytes_with_newline(mut buf)!
	println("read \${n} \${d.x}")
	t.wait()
	r.close()
	os.rm(path) or {}
}
'

// Every done() of a WaitGroup happens before wait() returns, and every file write happens
// before a later read, also when nothing that the race detector sees orders the done() calls
// or the writes: racerelease merges the clocks, a later release does not replace them.
const release_merge_source = 'import os
import sync
import time

struct Slots {
mut:
	a int
	b int
	c int
}

fn main() {
	mut s := &Slots{}
	mut wg := sync.new_waitgroup()
	wg.add(3)
	spawn fn (mut s Slots, mut wg sync.WaitGroup) {
		s.a = 1
		wg.done()
	}(mut s, mut wg)
	spawn fn (mut s Slots, mut wg sync.WaitGroup) {
		time.sleep(30 * time.millisecond)
		s.b = 2
		wg.done()
	}(mut s, mut wg)
	spawn fn (mut s Slots, mut wg sync.WaitGroup) {
		time.sleep(60 * time.millisecond)
		s.c = 3
		wg.done()
	}(mut s, mut wg)
	wg.wait()
	println("wg \${s.a + s.b + s.c}")

	path := os.join_path(os.vtmp_dir(), "race_release_merge_\${os.getpid()}.txt")
	os.write_file(path, "")!
	mut r := os.open(path)!
	mut d := &Slots{}
	t1 := spawn fn (mut d Slots, path string) {
		d.a = 1
		mut f := os.open_append(path) or { panic(err) }
		f.write_string("a\n") or { panic(err) }
		f.close()
	}(mut d, path)
	t2 := spawn fn (mut d Slots, path string) {
		time.sleep(30 * time.millisecond)
		d.b = 2
		mut f := os.open_append(path) or { panic(err) }
		f.write_string("b\n") or { panic(err) }
		f.close()
	}(mut d, path)
	for os.file_size(path) < 4 {
		time.sleep(time.millisecond)
	}
	mut buf := []u8{len: 16}
	n := r.read(mut buf)!
	println("io \${n} \${d.a + d.b}")
	t1.wait()
	t2.wait()
	r.close()
	os.rm(path) or {}
}
'

// A read that fails does not happen after earlier file writes: reading what the writer
// published before its write is still a race.
const failed_read_source = 'import os
import time

struct Data {
mut:
	x int
}

fn main() {
	path := os.join_path(os.vtmp_dir(), "race_failed_read_\${os.getpid()}.txt")
	os.write_file(path, "")!
	// Opened before the writer starts: opening a file after the write would synchronize.
	mut dir := os.open(os.vtmp_dir())!
	mut d := &Data{}
	t := spawn fn (mut d Data, path string) {
		d.x = 42
		mut w := os.open_append(path) or { panic(err) }
		w.write_string("ready\n") or { panic(err) }
		w.close()
	}(mut d, path)
	for os.file_size(path) == 0 {
		time.sleep(time.millisecond)
	}
	mut buf := []u8{len: 16}
	if _ := dir.read(mut buf) {
		println("the read of a directory did not fail")
	} else {
		println("read failed")
	}
	println("x \${d.x}")
	t.wait()
	dir.close()
	os.rm(path) or {}
}
'

// A compiler build (`-building-v`, or the `cmd/v` input) defaults to the arena allocator,
// but a race build keeps the C allocator.
const allocator_source = 'fn main() {
	xs := [1, 2, 3]
	\$if prealloc {
		println("allocator: prealloc \${xs.len}")
	} \$else {
		println("allocator: c \${xs.len}")
	}
}
'

// The end of a command's output, where `read_line` stops, happens after earlier file writes
// too, like every read that does not fail.
const command_eof_source = 'import os
import time

struct Data {
mut:
	x int
}

fn main() {
	path := os.join_path(os.vtmp_dir(), "race_command_eof_\${os.getpid()}.txt")
	os.write_file(path, "")!
	// Started before the writer: starting a process after the write would synchronize.
	mut cmd := os.Command{
		path: "sleep 0.3"
	}
	cmd.start()!
	mut d := &Data{}
	t := spawn fn (mut d Data, path string) {
		d.x = 42
		mut w := os.open_append(path) or { panic(err) }
		w.write_string("ready\n") or { panic(err) }
		w.close()
	}(mut d, path)
	for os.file_size(path) == 0 {
		time.sleep(time.millisecond)
	}
	line := cmd.read_line()
	println("eof \${cmd.eof} \${line.len} x \${d.x}")
	cmd.close()!
	t.wait()
	os.rm(path) or {}
}
'

// A select that finds all its receive channels closed (-2) happens after their close, like a
// receive from a closed channel; a write after the close still races.
const select_closed_source = 'import sync

struct Data {
mut:
	x int
}

fn main() {
	mut d := &Data{}
	mut ch := sync.new_channel[int](0)
	mut closed := sync.new_channel[int](1)
	closed.close()
	spawn fn (mut d Data, mut ch sync.Channel) {
		\$if write_after_close ? {
			ch.close()
			d.x = 42
		} \$else {
			d.x = 42
			ch.close()
		}
	}(mut d, mut ch)
	mut a := 0
	mut b := 0
	mut chans := [ch, closed]
	mut objs := [voidptr(&a), voidptr(&b)]
	idx := sync.channel_select(mut chans, [sync.Direction.pop, .pop], mut objs, max_i64)
	println("select \${idx} x \${d.x}")
}
'

// `println` to a redirected stdout happens before a later read of that output, like a file
// write does.
const stdout_write_source = 'import os
import time

struct Data {
mut:
	x int
}

fn main() {
	// stdout is redirected to this file. It is opened before the writer starts: opening a
	// file after the write would synchronize.
	path := os.getenv("RACE_STDOUT_FILE")
	mut r := os.open(path)!
	mut d := &Data{}
	t := spawn fn (mut d Data) {
		d.x = 42
		println("ready")
	}(mut d)
	for os.file_size(path) == 0 {
		time.sleep(time.millisecond)
	}
	mut buf := []u8{len: 16}
	n := r.read(mut buf)!
	eprintln("read \${n} x \${d.x}")
	t.wait()
	r.close()
}
'

// The channel implementation is hidden from the race detector, but the values that `push`
// reads and that `pop`, `try_pop` and `select` write are the caller's memory; a handoff
// through the channel still orders the sender before the receiver.
const channel_value_source = 'import sync

struct Cell {
mut:
	v int
}

fn main() {
	mode := \$d("mode", "pop")
	mut ch := sync.new_channel[int](1000)
	mut c := &Cell{}
	if mode == "handoff" {
		// No race: the receiver writes the sent variable only after the receive.
		mut x := &Cell{
			v: 1
		}
		mut ch2 := sync.new_channel[int](0)
		t := spawn fn (mut x Cell, mut ch2 sync.Channel) {
			ch2.push(&x.v)
		}(mut x, mut ch2)
		mut y := 0
		ch2.pop(&y)
		x.v = 2
		t.wait()
		println("handoff \${y} \${x.v}")
		return
	}
	if mode != "push" {
		for i in 0 .. 1000 {
			ch.push(&i)
		}
	}
	t := spawn fn (mut c Cell, ch &sync.Channel, mode string) {
		mut ch_ := unsafe { ch }
		for _ in 0 .. 1000 {
			match mode {
				"pop" {
					ch_.pop(&c.v)
				}
				"try_pop" {
					ch_.try_pop(&c.v)
				}
				"push" {
					ch_.push(&c.v)
				}
				else {
					mut chans := [ch_]
					mut objs := [voidptr(&c.v)]
					sync.channel_select(mut chans, [sync.Direction.pop], mut objs, max_i64)
				}
			}
		}
	}(mut c, ch, mode)
	mut s := 0
	for i in 0 .. 1000 {
		if mode == "push" {
			c.v = i
		} else {
			s += c.v
		}
	}
	t.wait()
	println("\${mode} done")
}
'

// `input_character` reads stdin after the write of that input in another thread, and so does
// reaching the end of the input.
const stdin_read_source = 'import os
import time

struct Data {
mut:
	x int
}

fn main() {
	// With `-d eof`, stdin is empty and the writer writes another file.
	path := os.getenv("RACE_WRITE_FILE")
	mut d := &Data{}
	t := spawn fn (mut d Data, path string) {
		d.x = 42
		mut w := os.open_append(path) or { panic(err) }
		w.write_string("!") or { panic(err) }
		w.close()
	}(mut d, path)
	for os.file_size(path) == 0 {
		time.sleep(time.millisecond)
	}
	c := input_character()
	println("char \${c} x \${d.x}")
	t.wait()
}
'

// close() stores its error before the release that a receive of the closed channel acquires,
// so the receive reads the error without a race.
const close_error_source = 'fn main() {
	ch := chan int{}
	t := spawn fn (ch chan int) {
		ch.close(error("custom close error"))
	}(ch)
	mut msg := ""
	_ := <-ch or {
		msg = err.msg()
		0
	}
	t.wait()
	println("received: \${msg}")
}
'

fn testsuite_begin() {
	os.mkdir_all(tdir) or {}
}

fn testsuite_end() {
	os.rmdir_all(tdir) or {}
}

// race_c_compiler returns the C compiler that `v -race` uses here: the one that VFLAGS
// selects with `-cc`, or else clang when it is installed, or else the platform default.
fn race_c_compiler() string {
	flags := os.getenv('VFLAGS').fields()
	i := flags.index('-cc')
	if i >= 0 && i + 1 < flags.len {
		return flags[i + 1]
	}
	$if !macos {
		if _ := os.find_abs_path_of_executable('clang') {
			return 'clang'
		}
	}
	return 'cc'
}

// thread_sanitizer_runs reports whether the C compiler that `-race` uses can build and run
// a ThreadSanitizer program here. It cannot on Windows, with compilers or distributions
// without the TSan runtime, or on Linux kernels whose ASLR layout old TSan runtimes reject.
fn thread_sanitizer_runs() bool {
	probe_c := os.join_path(tdir, 'tsan_probe.c')
	probe_exe := os.join_path(tdir, 'tsan_probe')
	os.write_file(probe_c, 'int main(void) { return 0; }\n') or { return false }
	cc := race_c_compiler()
	compiled := os.exec([cc, '-fsanitize=thread', '${probe_c}', '-o', probe_exe])
	if compiled.exit_code != 0 {
		eprintln('skipping: `${cc} -fsanitize=thread` does not work here:\n${compiled.output}')
		return false
	}
	ran := os.exec([probe_exe])
	if ran.exit_code != 0 {
		eprintln('skipping: ThreadSanitizer programs do not run here:\n${ran.output}')
		return false
	}
	return true
}

fn build_race_program(name string, source string, flags ...string) string {
	source_path := os.join_path(tdir, '${name}.v')
	exe_path := os.join_path(tdir, name)
	os.write_file(source_path, source) or { panic(err) }
	res := os.exec([@VEXE, '-race', ...flags, '-o', exe_path, source_path])
	assert res.exit_code == 0, res.output
	return exe_path
}

fn test_race_detector() {
	if !thread_sanitizer_runs() {
		return
	}
	racy := build_race_program('racy', racy_source)
	racy_run := os.exec([racy])
	assert racy_run.exit_code == 66, racy_run.output
	assert racy_run.output.contains('WARNING: ThreadSanitizer: data race'), racy_run.output
	// The unsynchronized increments can lose updates, so only check that both threads ended.
	assert racy_run.output.contains('joined true'), racy_run.output
	summary := racy_run.output.all_after('SUMMARY: ThreadSanitizer: data race').all_before('\n')
	if summary.contains('.c:') || summary.contains('.v:') {
		// The symbolizer found line info: it must point at the V source, not the generated C.
		assert summary.contains('racy.v:8'), racy_run.output
	}

	// VRACE passes options to the race detector, like GORACE does for Go.
	vrace_run := os.exec(['env', 'VRACE=exitcode=7', '${racy}'])
	assert vrace_run.exit_code == 7, vrace_run.output

	// The race runtime support is compiled on its own, and must build in strict C99 mode too.
	racy_c99 := build_race_program('racy_c99', racy_source, '-c99')
	racy_c99_run := os.exec(['env', 'VRACE=exitcode=7', '${racy_c99}'])
	assert racy_c99_run.exit_code == 7, racy_c99_run.output
	assert racy_c99_run.output.contains('WARNING: ThreadSanitizer: data race'), racy_c99_run.output

	synchronized := build_race_program('synchronized', synchronized_source)
	synchronized_run := os.exec([synchronized])
	assert synchronized_run.exit_code == 0, synchronized_run.output
	assert !synchronized_run.output.contains('ThreadSanitizer'), synchronized_run.output
	assert synchronized_run.output.contains('total: 4950 vals: 400 shared: 2'), synchronized_run.output
	assert synchronized_run.output.contains('race: true'), synchronized_run.output
}

fn test_race_rejects_a_garbage_collector() {
	source_path := os.join_path(tdir, 'gc.v')
	os.write_file(source_path, 'fn main() {}\n')!
	res := os.exec([@VEXE, '-race', '-gc', 'boehm', '-o', os.join_path(tdir, 'gc'), source_path])
	assert res.exit_code != 0, res.output
	assert res.output.contains('`-race` cannot be combined with `-gc boehm`'), res.output
}

fn test_race_map_value_update_is_a_write() {
	if !thread_sanitizer_runs() {
		return
	}
	exe := build_race_program('map_value_update', map_value_update_source)
	res := os.exec(['env', 'VRACE=exitcode=7', exe])
	assert res.exit_code == 7, res.output
	assert res.output.contains('WARNING: ThreadSanitizer: data race'), res.output
}

fn test_race_rejects_the_arena_allocator_and_a_plain_race_define() {
	source_path := os.join_path(tdir, 'plain.v')
	os.write_file(source_path, 'fn main() {}\n')!
	out := os.quoted_path(os.join_path(tdir, 'plain'))
	prealloc := os.exec([@VEXE, '-race', '-prealloc', '-o', os.join_path(tdir, 'plain'), source_path])
	assert prealloc.exit_code != 0, prealloc.output
	assert prealloc.output.contains('`-race` cannot be combined with `-prealloc`'), prealloc.output
	define := os.exec([@VEXE, '-d', 'race', '-o', os.join_path(tdir, 'plain'), source_path])
	assert define.exit_code != 0, define.output
	assert define.output.contains('`-d race` is reserved for race builds'), define.output
	assert !define.output.contains('retrying with'), define.output
}

fn test_race_blank_reads_are_reads() {
	if !thread_sanitizer_runs() {
		return
	}
	for read in ['string', 'fixed_array', 'computed', 'array_literal', 'mut_param',
		'mut_param_in_parens'] {
		mut flags := ['-d', 'read=${read}']
		if read in ['string', 'fixed_array', 'computed', 'array_literal'] {
			flags << '-prod'
		}
		exe := build_race_program('blank_read_${read}', blank_read_source, ...flags)
		res := os.exec(['env', 'VRACE=exitcode=7', exe])
		assert res.exit_code == 7, '${read}: ${res.output}'
		assert res.output.contains('WARNING: ThreadSanitizer: data race'), '${read}: ${res.output}'
	}
}

fn test_race_file_line_read_happens_after_the_write() {
	if !thread_sanitizer_runs() {
		return
	}
	exe := build_race_program('file_line', file_line_source)
	res := os.exec([exe])
	assert res.exit_code == 0, res.output
	assert !res.output.contains('ThreadSanitizer'), res.output
	assert res.output.contains('read 6 42'), res.output
}

fn test_race_every_release_happens_before_the_acquire() {
	if !thread_sanitizer_runs() {
		return
	}
	exe := build_race_program('release_merge', release_merge_source)
	res := os.exec([exe])
	assert res.exit_code == 0, res.output
	assert !res.output.contains('ThreadSanitizer'), res.output
	assert res.output.contains('wg 6'), res.output
	assert res.output.contains('io 4 3'), res.output
}

fn test_race_failed_file_read_does_not_synchronize() {
	if !thread_sanitizer_runs() {
		return
	}
	exe := build_race_program('failed_read', failed_read_source)
	res := os.exec(['env', 'VRACE=exitcode=7', exe])
	assert res.output.contains('read failed'), res.output
	assert res.exit_code == 7, res.output
	assert res.output.contains('WARNING: ThreadSanitizer: data race'), res.output
}

fn test_race_compiler_builds_keep_the_c_allocator() {
	if !thread_sanitizer_runs() {
		return
	}
	exe := build_race_program('allocator', allocator_source, '-building-v')
	res := os.exec([exe])
	assert res.exit_code == 0, res.output
	assert res.output.contains('allocator: c 3'), res.output
}

fn test_race_command_output_eof_happens_after_the_write() {
	if !thread_sanitizer_runs() {
		return
	}
	exe := build_race_program('command_eof', command_eof_source)
	res := os.exec([exe])
	assert res.exit_code == 0, res.output
	assert !res.output.contains('ThreadSanitizer'), res.output
	assert res.output.contains('eof true 0 x 42'), res.output
}

fn test_race_select_of_closed_channels_happens_after_the_close() {
	if !thread_sanitizer_runs() {
		return
	}
	exe := build_race_program('select_closed', select_closed_source)
	res := os.exec([exe])
	assert res.exit_code == 0, res.output
	assert !res.output.contains('ThreadSanitizer'), res.output
	assert res.output.contains('select -2 x 42'), res.output
	racy := build_race_program('select_closed_racy', select_closed_source, '-d', 'write_after_close')
	racy_res := os.exec(['env', 'VRACE=exitcode=7', '${racy}'])
	assert racy_res.exit_code == 7, racy_res.output
	assert racy_res.output.contains('WARNING: ThreadSanitizer: data race'), racy_res.output
}

fn test_race_stdout_write_happens_before_reading_the_output() {
	if !thread_sanitizer_runs() {
		return
	}
	exe := build_race_program('stdout_write', stdout_write_source)
	out := os.join_path(tdir, 'stdout_write.out')
	err := os.join_path(tdir, 'stdout_write.err')
	res := os.exec(['sh', '-c', 'RACE_STDOUT_FILE="\${1}" "\${2}" > "\${3}" 2> "\${4}"', 'v', '${out}',
		'${exe}', '${out}', '${err}'])
	stderr := os.read_file(err) or { '' }
	assert res.exit_code == 0, stderr
	assert !stderr.contains('ThreadSanitizer'), stderr
	assert stderr.contains('read 6 x 42'), stderr
}

fn test_race_channel_values_are_the_caller_memory() {
	if !thread_sanitizer_runs() {
		return
	}
	for mode in ['pop', 'try_pop', 'push', 'select'] {
		exe := build_race_program('channel_value_${mode}', channel_value_source, '-d', 'mode=${mode}')
		res := os.exec(['env', 'VRACE=exitcode=7', exe])
		assert res.exit_code == 7, '${mode}: ${res.output}'
		assert res.output.contains('WARNING: ThreadSanitizer: data race'), '${mode}: ${res.output}'
	}
	exe := build_race_program('channel_value_handoff', channel_value_source, '-d', 'mode=handoff')
	res := os.exec([exe])
	assert res.exit_code == 0, res.output
	assert !res.output.contains('ThreadSanitizer'), res.output
	assert res.output.contains('handoff 1 2'), res.output
}

fn test_race_stdin_read_happens_after_the_write() {
	if !thread_sanitizer_runs() {
		return
	}
	exe := os.quoted_path(build_race_program('stdin_read', stdin_read_source))
	input := os.join_path(tdir, 'stdin_read.in')
	other := os.join_path(tdir, 'stdin_read.other')
	os.write_file(input, '')!
	res := os.exec(['sh', '-c', 'RACE_WRITE_FILE="\${1}" "\${2}" < "\${3}"', 'v', '${input}',
		'${build_race_program('stdin_read', stdin_read_source)}', '${input}'])
	assert res.exit_code == 0, res.output
	assert !res.output.contains('ThreadSanitizer'), res.output
	assert res.output.contains('char 33 x 42'), res.output
	os.write_file(other, '')!
	eof := os.exec(['sh', '-c', 'RACE_WRITE_FILE="\${1}" "\${2}" < /dev/null', 'v', '${other}',
		'${build_race_program('stdin_read', stdin_read_source)}'])
	assert eof.exit_code == 0, eof.output
	assert !eof.output.contains('ThreadSanitizer'), eof.output
	assert eof.output.contains('char -1 x 42'), eof.output
}

fn test_race_close_error_is_published_with_the_close() {
	if !thread_sanitizer_runs() {
		return
	}
	exe := build_race_program('close_error', close_error_source)
	res := os.exec([exe])
	assert res.exit_code == 0, res.output
	assert !res.output.contains('ThreadSanitizer'), res.output
	assert res.output.contains('received: custom close error'), res.output
}
