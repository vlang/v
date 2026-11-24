module main

import arena
import context
import os
import sync
import sync.stdatomic
import time

__global thread_completed u64

fn require_thread(condition bool) {
	if !condition { exit(95) }
}

fn number(value int) int {
	require_thread(!arena.active())
	return value * 3 + 1
}

fn join_text(value string) string { return value + ' result' }

fn detached(value int) int {
	stdatomic.add_u64(&thread_completed, 1)
	return value
}

fn channel_worker(input chan int, output chan int) {
	value := <-input
	output <- number(value)
	output.close()
}

fn mapped_bytes() u64 {
	mut total := u64(0)
	text := os.read_file('/proc/self/maps') or { exit(96) }
	for line in text.split_into_lines() {
		field := line.all_before(' ')
		ends := field.split('-')
		if ends.len != 2 { exit(96) }
		start := ends[0].parse_uint(16, 64) or { exit(96) }
		stop := ends[1].parse_uint(16, 64) or { exit(96) }
		total += stop - start
	}
	return total
}

fn test_detached_reclamation() {
	// Warm up heap/thread machinery before retaining the process mapping count.
	previous_warmup := stdatomic.load_u64(&thread_completed)
	spawn detached(0)
	for stdatomic.load_u64(&thread_completed) != previous_warmup + 1 {
		time.sleep(time.millisecond)
	}
	time.sleep(5 * time.millisecond)
	before := mapped_bytes()
	previous := stdatomic.load_u64(&thread_completed)
	for i in 0 .. 128 { spawn detached(i) }
	for stdatomic.load_u64(&thread_completed) != previous + 128 { time.sleep(time.millisecond) }
	// A completion happens just before the runtime reclaims the worker stack.
	// Wait for that tail, allowing ordinary heap growth but not leaked stacks.
	mut reclaimed := false
	for _ in 0 .. 1000 {
		if mapped_bytes() <= before + 2 * 1024 * 1024 {
			reclaimed = true
			break
		}
		time.sleep(time.millisecond)
	}
	require_thread(reclaimed)
}

fn main() {
	mut scope := arena.new()
	scope.push()
	task := spawn number(12)
	require_thread(task.wait() == 37)
	require_thread(scope.is_current())
	scope.pop()
	scope.free()
	string_task := spawn join_text('thread')
	require_thread(string_task.wait() == 'thread result')
	mut tasks := []thread int{}
	for i in 0 .. 16 { tasks << spawn number(i) }
	values := tasks.wait()
	for i, value in values { require_thread(value == i * 3 + 1) }
	input := chan int{cap: 1}
	output := chan int{cap: 1}
	channel_task := spawn channel_worker(input, output)
	input <- 7
	input.close()
	require_thread(<-output == 22)
	channel_task.wait()
	mut background := context.background()
	mut ctx, cancel := context.with_timeout(mut background, 20 * time.millisecond)
	done := ctx.done()
	select {
		_ := <-done {
		}
		2 * time.second {
			exit(97)
		}
	}
	require_thread(ctx.err().msg() == 'context deadline exceeded')
	cancel()
	timer := sync.new_timer(5 * time.millisecond)
	_ = <-timer.c
	timer.stop()
	$if linux {
		for i in 0 .. 64 { require_thread((spawn detached(i)) != (spawn detached(i + 1))) }
		for stdatomic.load_u64(&thread_completed) != 128 { time.sleep(time.millisecond) }
		test_detached_reclamation()
	}
	println('freestanding V threads passed')
}
