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
	compiled := os.execute('${os.quoted_path(cc)} -fsanitize=thread ${os.quoted_path(probe_c)} -o ${os.quoted_path(probe_exe)}')
	if compiled.exit_code != 0 {
		eprintln('skipping: `${cc} -fsanitize=thread` does not work here:\n${compiled.output}')
		return false
	}
	ran := os.execute(os.quoted_path(probe_exe))
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
	res := os.execute('${vexe} -race ${flags.join(' ')} -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}')
	assert res.exit_code == 0, res.output
	return exe_path
}

fn test_race_detector() {
	if !thread_sanitizer_runs() {
		return
	}
	racy := build_race_program('racy', racy_source)
	racy_run := os.execute(os.quoted_path(racy))
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
	vrace_run := os.execute('VRACE="exitcode=7" ${os.quoted_path(racy)}')
	assert vrace_run.exit_code == 7, vrace_run.output

	// The race runtime support is compiled on its own, and must build in strict C99 mode too.
	racy_c99 := build_race_program('racy_c99', racy_source, '-c99')
	racy_c99_run := os.execute('VRACE="exitcode=7" ${os.quoted_path(racy_c99)}')
	assert racy_c99_run.exit_code == 7, racy_c99_run.output
	assert racy_c99_run.output.contains('WARNING: ThreadSanitizer: data race'), racy_c99_run.output

	synchronized := build_race_program('synchronized', synchronized_source)
	synchronized_run := os.execute(os.quoted_path(synchronized))
	assert synchronized_run.exit_code == 0, synchronized_run.output
	assert !synchronized_run.output.contains('ThreadSanitizer'), synchronized_run.output
	assert synchronized_run.output.contains('total: 4950 vals: 400 shared: 2'), synchronized_run.output
	assert synchronized_run.output.contains('race: true'), synchronized_run.output
}

fn test_race_rejects_a_garbage_collector() {
	source_path := os.join_path(tdir, 'gc.v')
	os.write_file(source_path, 'fn main() {}\n')!
	res := os.execute('${vexe} -race -gc boehm -o ${os.quoted_path(os.join_path(tdir, 'gc'))} ${os.quoted_path(source_path)}')
	assert res.exit_code != 0, res.output
	assert res.output.contains('`-race` cannot be combined with `-gc boehm`'), res.output
}

fn test_race_map_value_update_is_a_write() {
	if !thread_sanitizer_runs() {
		return
	}
	exe := build_race_program('map_value_update', map_value_update_source)
	res := os.execute('VRACE="exitcode=7" ${os.quoted_path(exe)}')
	assert res.exit_code == 7, res.output
	assert res.output.contains('WARNING: ThreadSanitizer: data race'), res.output
}

fn test_race_rejects_the_arena_allocator_and_a_plain_race_define() {
	source_path := os.join_path(tdir, 'plain.v')
	os.write_file(source_path, 'fn main() {}\n')!
	out := os.quoted_path(os.join_path(tdir, 'plain'))
	prealloc := os.execute('${vexe} -race -prealloc -o ${out} ${os.quoted_path(source_path)}')
	assert prealloc.exit_code != 0, prealloc.output
	assert prealloc.output.contains('`-race` cannot be combined with `-prealloc`'), prealloc.output
	define := os.execute('${vexe} -d race -o ${out} ${os.quoted_path(source_path)}')
	assert define.exit_code != 0, define.output
	assert define.output.contains('`-d race` is reserved for race builds'), define.output
	assert !define.output.contains('retrying with'), define.output
}
