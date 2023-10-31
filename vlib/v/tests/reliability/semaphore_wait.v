/*
This program can be used to test waiting for a semaphore
in the presence of signals that might interrupt the `wait()` call.

In particular the effect of Boehm-GC can be investigated.

To do so compile this program with `v -gc boehm semaphore_wait.v`
and run is as `./semaphore_wait > /dev/null` to test `sem.wait()`
or `./semaphore_wait -t > /dev/null` to test `sem.timedwait()`
on Windows: `.\semaphore_wait > NUL` or `.\semaphore_wait -t > NUL`

On success (no interrupted `wait`) the output (from stderr) should like like:

time: 524.228 result: `true`
time: 521.122 result: `true`
time: 523.714 result: `true`
time: 527.001 result: `true`
time: 521.696 result: `true`
...
Finished

(The "result" is only printed with `-t`.)
*/
import os
import time
import rand
import math
import sync

struct DataObj {
mut:
	data []f64
}

struct PtrObj {
mut:
	nxt []&DataObj
}

struct PtrPtrObj {
mut:
	nxt []&PtrObj
}

const (
	log2n = 9
	n     = 1 << log2n
	n4    = f64(u64(1) << (4 * log2n))
)

fn waste_mem() {
	mut objs := PtrPtrObj{
		nxt: []&PtrObj{len: n}
	}
	for {
		sz := rand.int_in_range(10, 1000)
		mut new_obj := &PtrObj{
			nxt: []&DataObj{len: sz}
		}
		sz2 := rand.int_in_range(10, 500000)
		new_obj2 := &DataObj{
			data: []f64{len: sz2}
		}
		idx2 := rand.int_in_range(0, sz)
		new_obj.nxt[idx2] = new_obj2
		// non-equally distributed random index
		idx := int(math.sqrt(math.sqrt(rand.f64n(n4))))
		objs.nxt[idx] = new_obj
	}
}

fn do_rec(mut sem sync.Semaphore, timed bool) {
	mut start := time.sys_mono_now()
	for {
		r := if timed {
			sem.timed_wait(600 * time.millisecond)
		} else {
			sem.wait()
			false
		}
		end := time.sys_mono_now()
		dur := f64(end - start) / f64(time.millisecond)
		res_str := if timed { ' result: `${r}`' } else { '' }
		if dur < 450.0 || dur > 550.0 {
			eprintln('Problem: time: ${dur:.3f}${res_str}')
		} else {
			eprintln('time: ${dur:.3f}${res_str}')
		}
		start = end
	}
}

fn do_send(mut sem sync.Semaphore) {
	for {
		time.sleep(500 * time.millisecond)
		sem.post()
	}
}

fn usage() {
	eprintln('usage:\n\t${os.args[0]} [-t] [num_iterations]')
	exit(1)
}

fn main() {
	mut n_iterations := 5_000_000
	mut timed := false
	if os.args.len > 3 {
		usage()
	}
	for i in 1 .. os.args.len {
		if os.args[i][0].is_digit() {
			if i > 1 && !timed {
				usage()
			}
			n_iterations = os.args[1].int()
		} else if os.args[i] == '-t' {
			timed = true
		} else {
			usage()
		}
	}
	if os.args.len > 3 || n_iterations <= 0 {
		eprintln('usage:\n\t${os.args[0]} [num_iterations]')
		exit(1)
	}
	mut sem := sync.new_semaphore()
	spawn do_rec(mut sem, timed)
	spawn do_send(mut sem)
	for _ in 0 .. 4 {
		spawn waste_mem()
	}
	mut last := time.sys_mono_now()
	for _ in 0 .. n_iterations {
		now := time.sys_mono_now()
		interval := now - last
		println(f64(interval) / f64(time.millisecond))
		last = now
	}
	sem.destroy()
	eprintln('Finished')
}
