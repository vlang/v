// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Goroutine runtime initialization.
// Translated from Go's schedinit() and procresize() in proc.go.
module goroutines

import runtime

// init initializes the goroutine scheduler.
// Creates P's (one per CPU core by default) and the initial M.
// Translated from Go's schedinit() + procresize().
fn init() {
	// Determine number of processors
	n := runtime.nr_cpus()
	if n < 1 {
		gomaxprocs = 1
	} else if n > 256 {
		gomaxprocs = 256
	} else {
		gomaxprocs = i32(n)
	}

	// Initialize the scheduler
	gsched.maxmcount = 10000
	gsched.mnext = 1

	// Create all P's (translated from procresize in proc.go)
	gsched.allp = []&Processor{cap: int(gomaxprocs)}
	for i in 0 .. gomaxprocs {
		mut pp := &Processor{
			id:     i
			status: .idle
		}
		// Initialize the local run queue
		for j in 0 .. local_queue_size {
			pp.runq[j] = unsafe { nil }
		}
		gsched.allp << pp
	}

	// Create the initial M (M0) for the main thread
	mut m0 := &Machine{
		id: 0
		g0: &Goroutine{
			id:     0
			status: .running
		}
	}

	// Create a "main goroutine" so chan_send/recv work from the main thread
	mut main_g := &Goroutine{
		id:     0
		status: .running
	}
	m0.curg = main_g
	main_g.m = m0

	// Wire M0 to P0
	mut p0 := gsched.allp[0]
	wire_p(mut m0, mut p0)

	// Set the current thread's M
	set_current_m(m0)

	// Put remaining P's on the idle list
	for i in 1 .. gomaxprocs {
		pid_put(gsched.allp[i])
	}
}

// set_max_procs changes the number of active processors.
// Returns the previous value. Translated from Go's GOMAXPROCS().
pub fn set_max_procs(n int) int {
	old := int(gomaxprocs)
	if n < 1 || n == old {
		return old
	}

	// TODO: implement full procresize like Go does
	// For now, just update the count
	gomaxprocs = i32(n)
	return old
}

// num_goroutine returns the number of goroutines that currently exist.
pub fn num_goroutine() int {
	allgs_mu.acquire()
	mut count := 0
	for g in allgs {
		if g.status != .dead {
			count++
		}
	}
	allgs_mu.release()
	return count
}

// shutdown gracefully shuts down the goroutine scheduler.
pub fn shutdown() {
	gsched.stopped = true
	// Wake all idle M's so they can exit
	gsched.mu.acquire()
	mut mp := gsched.midle
	for mp != unsafe { nil } {
		mp.park.post()
		mp = mp.sched_link
	}
	gsched.mu.release()
}
