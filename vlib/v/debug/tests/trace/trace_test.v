@[has_globals]
module main

import time
import v.debug

__global counter = HookCounter{}

struct HookCounter {
pub mut:
	after  int = 0
	before int = 0
}

@[heap]
struct Profiler {
mut:
	fn_map map[string]time.StopWatch
}

fn (mut p Profiler) trace_after(fn_name string) {
	counter.after++
	println('>>> called after ${fn_name} - ${p.fn_map[fn_name].elapsed()}')
}

fn (mut p Profiler) trace_before(fn_name string) {
	p.fn_map[fn_name] = time.new_stopwatch()
	println('>>> called before ${fn_name}')
	counter.before++
}

fn test_call() {
	println('${@FN} got called')
	time.sleep(100)
}

fn test_main() {
	counter = HookCounter{}
	mut myprofiler := Profiler{}
	id_callback := debug.add_after_call(myprofiler.trace_after)
	id_callback2 := debug.add_before_call(myprofiler.trace_before)
	test_call()
	debug.remove_after_call(id_callback)
	debug.remove_before_call(id_callback2)
	test_call()
	$if trace ? {
		assert counter.before == 2
		assert counter.after == 2
	} $else {
		assert counter.before == 0
		assert counter.after == 0
	}
}
