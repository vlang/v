import os

const closure_tls_vexe = @VEXE
const closure_tls_tests_dir = os.dir(@FILE)
const closure_tls_v3_dir = os.dir(closure_tls_tests_dir)
const closure_tls_vlib_dir = os.dir(closure_tls_v3_dir)
const closure_tls_v3_src = os.join_path(closure_tls_v3_dir, 'v3.v')

fn test_capturing_callbacks_use_thread_local_capture_slots() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_closure_tls_${pid}')
	src := os.join_path(os.temp_dir(), 'v3_closure_tls_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_closure_tls_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	build :=
		os.execute('${closure_tls_vexe} -gc none -path "${closure_tls_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${closure_tls_v3_src}')
	assert build.exit_code == 0, build.output
	os.write_file(src, "module main

import time

fn invoke(value int, callback fn (int) int) int {
	time.sleep(time.microsecond)
	return callback(value)
}

fn worker(base int, results chan int) {
	for i in 0 .. 100 {
		result := invoke(i, fn [base] (value int) int {
			time.sleep(time.microsecond)
			return base + value
		})
		results <- result
	}
}

fn main() {
	results := chan int{cap: 400}
	mut threads := []thread{}
	for base in [0, 1000, 2000, 3000] {
		threads << spawn worker(base, results)
	}
	for thread in threads {
		thread.wait()
	}
	mut counts := [0, 0, 0, 0]
	for _ in 0 .. 400 {
		value := <-results
		bucket := value / 1000
		assert bucket >= 0 && bucket < 4
		counts[bucket]++
	}
	assert counts == [100, 100, 100, 100]
	println('ok')
}
") or {
		panic(err)
	}
	compile := os.execute('${v3_bin} ${src} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(out + '.c') or { panic(err) }
	assert c_source.contains('_key_init(void) __attribute__((constructor))')
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_spawned_capturing_literals_use_thread_local_capture_slots() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_spawn_capture_tls_${pid}')
	src := os.join_path(os.temp_dir(), 'v3_spawn_capture_tls_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_spawn_capture_tls_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	build :=
		os.execute('${closure_tls_vexe} -gc none -path "${closure_tls_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${closure_tls_v3_src}')
	assert build.exit_code == 0, build.output
	os.write_file(src, "module main

import time

fn main() {
	count := 32
	results := chan int{cap: count}
	mut threads := []thread int{cap: count}
	for i in 0 .. count {
		mut value := i
		threads << spawn fn [mut value, results] () int {
			time.sleep(time.millisecond)
			results <- value
			return value
		}()
	}
	mut seen := []bool{len: count}
	for _ in 0 .. count {
		value := <-results
		assert value >= 0 && value < count
		seen[value] = true
	}
	for thread in threads {
		_ := thread.wait()
	}
	for i in 0 .. count {
		assert seen[i]
	}
	println('ok')
}
") or {
		panic(err)
	}
	compile := os.execute('${v3_bin} ${src} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(out + '.c') or { panic(err) }
	assert c_source.contains('_key_init(void) __attribute__((constructor))'), c_source
	assert c_source.contains('_Thread_local int __anon_fn_'), c_source
	assert c_source.contains('_args_thread_wrapper'), c_source
	assert c_source.contains(' = p->c'), c_source
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
