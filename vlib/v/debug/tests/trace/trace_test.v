@[has_globals]
module main

import time
import v.debug

__global counter = HookCounter{}

struct HookCounter {
pub mut:
	after  int
	before int
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

fn hook_before(fn_name string) {
	counter.before++
}

fn hook_after(fn_name string) {
	counter.after++
}

fn test_main() {
	counter = HookCounter{}
	mut myprofiler := Profiler{}
	hook1 := debug.add_after_call(myprofiler.trace_after)
	hook2 := debug.add_before_call(myprofiler.trace_before)
	hook3 := debug.add_before_call(hook_before)
	hook4 := debug.add_after_call(hook_after)
	test_call()
	debug.remove_after_call(hook1)
	debug.remove_before_call(hook2)
	debug.remove_before_call(hook3)
	debug.remove_after_call(hook4)
	test_call()
	$if trace ? {
		assert counter.before == 4
		assert counter.after == 4
	} $else {
		assert counter.before == 0
		assert counter.after == 0
	}
}
