// The goal of this program, is to test V's garbage collector interaction with gc_disable(), gc_enable(), gc_collect() and `-gc none`.
// Note that this program is intended to be run on Linux, where /proc/PID/status exists.
// It should run on other platforms too, but the last columns of the output will be empty.
// Example invocation: `MAX_ITERATIONS=100 BLOCK_SIZE=1_000_000 v run gc.v` .
import os

const block_size = os.getenv_opt('BLOCK_SIZE') or { '1_000' }.int()
const max_iterations = os.getenv_opt('MAX_ITERATIONS') or { '40' }.int()

fn do_some_work_and_allocate_memory() u64 {
	a := []u8{len: block_size, init: u8(index)}
	mut s := u64(0)
	for x in a {
		s += x
	}
	return s
}

fn process_mem_stats(pid int) string {
	lines := os.read_lines('/proc/${pid}/status') or { [] }
	mut vals := map[string]string{}
	for line in lines {
		x := line.split(':')
		vals[x[0]] = x[1]
	}
	return 'VmSize: ${vals['VmSize']} | VmRSS: ${vals['VmRSS']}'
}

fn main() {
	println('BLOCK_SIZE: ${block_size:15}, MAX_ITERATIONS: ${max_iterations:5}, gc_is_enabled: ${gc_is_enabled()}')
	gc_disable()
	mypid := os.getpid()
	for c := 0; c < max_iterations; c++ {
		if c % 15 == 0 {
			gc_enable()
		}
		gc_collect()
		s := do_some_work_and_allocate_memory()
		println('gc_is_enabled: ${gc_is_enabled():6}, c: ${c:5}, s: ${s:10}, gc_memory_use: ${gc_memory_use():10}, ${process_mem_stats(mypid):30}')
		if c % 15 == 0 {
			gc_disable()
		}
	}
}
