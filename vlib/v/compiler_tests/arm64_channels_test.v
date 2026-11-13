module main

import os
import v.cmdexec

// Channels and `select` go through vlib/sync on arm64, as they do through C. The
// program also passes a local function value to a C callback parameter.

const channels_program = "import time

fn C.alarm(u32) u32
fn C.qsort(voidptr, uint, uint, voidptr)

struct Job {
	value int
	stop  bool
}

struct Queue {
mut:
	jobs chan Job
}

fn work(q &Queue, done chan int) {
	for {
		job := <-q.jobs
		if job.stop {
			break
		}
		done <- job.value * 2
	}
}

fn take(c chan int) int {
	return <-c
}

fn less(a &int, b &int) int {
	return *a - *b
}

fn sort_with(mut values []int, compare fn (&int, &int) int) {
	C.qsort(values.data, uint(values.len), uint(sizeof(int)), compare)
}

fn main() {
	C.alarm(20)
	mut q := &Queue{
		jobs: chan Job{cap: 8}
	}
	done := chan int{cap: 8}
	mut workers := []thread{}
	for _ in 0 .. 3 {
		workers << spawn work(q, done)
	}
	for i in 1 .. 7 {
		q.jobs <- Job{
			value: i
		}
	}
	mut sum := 0
	for _ in 1 .. 7 {
		sum += <-done
	}
	for _ in 0 .. 3 {
		q.jobs <- Job{
			stop: true
		}
	}
	workers.wait()
	println('sum \${sum}')

	rendezvous := chan int{}
	taker := spawn take(rendezvous)
	select {
		rendezvous <- 7 {
			println('sent')
		}
	}
	println('took \${taker.wait()}')

	numbers := chan int{cap: 1}
	texts := chan string{cap: 1}
	numbers <- 5
	select {
		x := <-numbers {
			println('number \${x}')
		}
		s := <-texts {
			println('text \${s}')
		}
	}
	texts <- 'word'
	mut target := ''
	select {
		x := <-numbers {
			println('number \${x}')
		}
		target = <-texts {
			println('assigned \${target}')
		}
	}
	select {
		x := <-numbers {
			println('number \${x}')
		}
		else {
			println('empty')
		}
	}
	select {
		x := <-numbers {
			println('number \${x}')
		}
		1 * time.millisecond {
			println('timeout')
		}
	}
	println('len \${numbers.len} cap \${numbers.cap} closed \${numbers.closed}')
	numbers <- 9
	println('len \${numbers.len}')
	numbers.close()
	println('closed \${numbers.closed}')

	mut values := [3, 1, 2]
	sort_with(mut values, less)
	println(values)
}
"

const channels_output = 'sum 42
sent
took 7
number 5
assigned word
empty
timeout
len 0 cap 1 closed false
len 1
closed true
[1, 2, 3]'

fn test_channels_and_select_work_on_arm64() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_channels_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, channels_program) or { panic(err) }
		for index, flags in [['-b', 'arm64'], ['-prod', '-b', 'arm64']] {
			output := os.join_path(root, 'main_${index}')
			mut build := flags.clone()
			build << ['-w', '-nocache', '-o', output, source]
			compiled := cmdexec.run(@VEXE, build)
			assert compiled.exit_code == 0, compiled.output
			run := cmdexec.run(output, []string{})
			assert run.exit_code == 0, run.output
			assert run.output.trim_space() == channels_output
		}
	}
}
