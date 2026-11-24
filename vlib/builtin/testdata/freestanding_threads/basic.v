module main

import arena
import sync.stdatomic

__global basic_completed u64

fn require_basic_thread(condition bool) {
	if !condition { exit(101) }
}

fn number(value int) int {
	require_basic_thread(!arena.active())
	return value * 3 + 1
}

fn text(value string) string { return value + ' result' }

fn detached(value int) int {
	stdatomic.add_u64(&basic_completed, 1)
	return value
}

fn main() {
	mut region := arena.new()
	region.push()
	task := spawn number(12)
	require_basic_thread(task.wait() == 37)
	require_basic_thread(region.is_current())
	parent_number := 10 + int(stdatomic.load_u64(&basic_completed))
	borrowed := 'parent ' + parent_number.str()
	require_basic_thread(region.owns(borrowed.str))
	borrowed_task := spawn text(borrowed)
	retained := borrowed_task.wait()
	require_basic_thread(retained == 'parent 10 result')
	require_basic_thread(!region.owns(retained.str))
	region.pop()
	region.free()
	require_basic_thread(retained == 'parent 10 result')
	text_task := spawn text('thread')
	require_basic_thread(text_task.wait() == 'thread result')
	mut tasks := []thread int{}
	for i in 0 .. 16 { tasks << spawn number(i) }
	values := tasks.wait()
	for i, value in values { require_basic_thread(value == i * 3 + 1) }
	for i in 0 .. 128 { spawn detached(i) }
	for stdatomic.load_u64(&basic_completed) != 128 {
	}
	println('freestanding basic V threads passed')
}
