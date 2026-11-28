import os
import v.tempname

fn capturing_callback_c_body(c_source string, name string) string {
	_, definition := c_source.rsplit_once('\nvoid ${name}(') or {
		panic('missing generated function ${name}')
	}
	signature, body := definition.split_once('\n') or { panic('missing body for ${name}') }
	assert signature.ends_with(') {'), signature
	return body.all_before('\n}')
}

fn capturing_callback_spawn_wrapper(c_source string, caller string) string {
	// Follow the call site so a wrapper emitted for another case cannot satisfy the check.
	_, call_args := caller.split_once('__v_thread_spawn(') or {
		panic('missing spawn call in\n${caller}')
	}
	wrapper := call_args.all_before(',')
	assert wrapper.starts_with('fn_value_args_thread_wrapper_'), caller
	_, definition := c_source.split_once('static void* ${wrapper}(void* arg) {') or {
		panic('missing generated spawn wrapper ${wrapper}')
	}
	return definition.all_before('\n')
}

fn test_capturing_callbacks_use_owned_capture_contexts() {
	dir := os.join_path(os.vtmp_dir(), 'v3_capturing_callbacks_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	src := os.join_path(dir, 'main.v')
	out := os.join_path(dir, 'program')
	os.write_file(src, "module main

import os
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

fn check_worker_callbacks() {
	results := chan int{cap: 400}
	mut threads := []thread{}
	for base in [0, 1000, 2000, 3000] {
		threads << spawn worker(base, results)
	}
	for worker_thread in threads {
		worker_thread.wait()
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

fn check_spawned_literal() {
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
	for worker_thread in threads {
		_ := worker_thread.wait()
	}
	for i in 0 .. count {
		assert seen[i]
	}
	println('ok')
}

fn check_named_callback() {
	value := 41
	callback := fn [value] () int {
		return value
	}
	worker := spawn (callback)()
	assert worker.wait() == 41
	assert callback() == 41
	println('ok')
}

fn main() {
	match os.args[1] {
		'worker' { check_worker_callbacks() }
		'literal' { check_spawned_literal() }
		'named' { check_named_callback() }
		else { panic('unknown callback case') }
	}
}
")!
	compile := os.exec([@VEXE, '-b', 'c', '-o', out, src])
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(out + '.c') or { panic(err) }
	worker := capturing_callback_c_body(c_source, 'worker')
	assert worker.contains('closure__closure_create_with_data('), worker
	literal := capturing_callback_c_body(c_source, 'check_spawned_literal')
	assert literal.contains('closure__closure_create_with_data(__anon_fn_'), literal
	assert literal.contains('_Ctx'), literal
	literal_wrapper := capturing_callback_spawn_wrapper(c_source, literal)
	named := capturing_callback_c_body(c_source, 'check_named_callback')
	named_wrapper := capturing_callback_spawn_wrapper(c_source, named)
	destroy := 'closure__closure_try_destroy((void*)__v3_spawn_args->f);'
	assert literal_wrapper.contains(' = __v3_spawn_args->f'), literal_wrapper
	assert literal_wrapper.contains(destroy), literal_wrapper
	assert named_wrapper.contains(' = __v3_spawn_args->f'), named_wrapper
	assert !named_wrapper.contains(destroy), named_wrapper

	for name in ['worker', 'literal', 'named'] {
		run := os.exec([out, name])
		assert run.exit_code == 0, '${name}: ${run.output}'
		assert run.output.trim_space() == 'ok', '${name}: ${run.output}'
	}
}
