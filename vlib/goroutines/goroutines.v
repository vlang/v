// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Go-style goroutine runtime for V.
// Implements the GMP (Goroutine-Machine-Processor) scheduling model
// translated from the Go runtime (src/runtime/proc.go, runtime2.go).
//
// Goroutine - goroutine: lightweight unit of execution with its own stack
// Machine - machine: OS thread that executes goroutines
// Processor - processor: logical processor that manages a local run queue
//
// Design doc: https://golang.org/s/go11sched
module goroutines

import sync
import runtime

// GoFn is the type for a goroutine function: takes a voidptr argument.
pub type GoFn = fn (voidptr)

// Default goroutine stack size (8KB, matching Go's minimum)
const default_stack_size = 8 * 1024

// Maximum number of goroutines in a Processor's local run queue (matches Go's 256)
const local_queue_size = 256

// How often to check the global queue for fairness (matches Go's 61)
const global_queue_check_interval = 61

// GStatus represents goroutine states (matches Go's runtime2.go)
pub enum GStatus {
	idle      // just allocated, not yet initialized
	runnable  // on a run queue, not currently executing
	running   // executing user code on an Machine
	waiting   // blocked (channel, mutex, etc.)
	dead      // finished execution, available for reuse
	copystack // stack is being moved (not used yet)
}

// PStatus represents processor states
pub enum PStatus {
	idle    // not being used, available on idle list
	running // owned by an Machine and executing code
	stopped // halted
	dead    // no longer used
}

// Goroutine represents a goroutine - the fundamental unit of concurrent execution.
// Translated from Go's `type g struct` in runtime2.go.
pub struct Goroutine {
pub mut:
	id          u64 // unique goroutine id
	status      GStatus = .idle // current state
	stack       voidptr // stack memory (allocated)
	stack_size  int = default_stack_size // stack allocation size
	context     Context // saved CPU context for switching (ucontext_t or similar)
	fn_ptr      voidptr // function to execute
	fn_arg      voidptr // argument to the function
	sched_link  &Goroutine = unsafe { nil } // linked list link for run queues
	m           &Machine   = unsafe { nil } // current Machine executing this Goroutine (nil if not running)
	parent_id   u64    // goroutine id of creator
	wait_reason string // if status==waiting, why
	preempt     bool   // preemption signal
}

// Machine represents a machine (OS thread) that executes goroutines.
// Translated from Go's `type m struct` in runtime2.go.
pub struct Machine {
pub mut:
	id         i64
	g0         &Goroutine = unsafe { nil } // goroutine with scheduling stack
	curg       &Goroutine = unsafe { nil } // current running goroutine
	p          &Processor = unsafe { nil } // attached processor (nil if not executing Go code)
	spinning   bool           // looking for work
	blocked    bool           // blocked on a note
	park       sync.Semaphore // for parking/unparking
	sched_link &Machine = unsafe { nil } // linked list for idle Machine list
	thread     thread // underlying OS thread handle
}

// Processor represents a processor - a resource required to execute goroutines.
// Each Processor has a local run queue. Translated from Go's `type p struct` in runtime2.go.
pub struct Processor {
pub mut:
	id         i32
	status     PStatus  = .idle
	m          &Machine = unsafe { nil } // back-link to associated Machine
	sched_tick u32 // incremented on every scheduler call

	// Local run queue - lock-free SPMC ring buffer (matches Go's design)
	runq_head u32 // consumer index (atomic)
	runq_tail u32 // producer index (atomic)
	runq      [local_queue_size]&Goroutine // circular buffer
	runnext   &Goroutine = unsafe { nil } // next Goroutine to run (fast path, like Go's runnext)

	// Free Goroutine list for reuse
	g_free GoroutineList

	// ID cache to avoid contention on the global counter
	goid_cache     u64
	goid_cache_end u64

	link &Processor = unsafe { nil } // linked list for idle Processor list
}

// Sched is the global scheduler state.
// Translated from Go's `type schedt struct` in runtime2.go.
pub struct Sched {
pub mut:
	goid_gen u64 // global goroutine ID generator (atomic)

	mu sync.Mutex // protects idle lists, global queue

	// Idle Machine's waiting for work
	midle  &Machine = unsafe { nil }
	nmidle i32

	// Idle Processor's
	pidle  &Processor = unsafe { nil }
	npidle i32

	// Global run queue
	runq       GoroutineQueue
	nmspinning i32 // number of spinning Machine's (atomic)

	// All Processor's (indexed by id)
	allp []&Processor

	// Total Machine count
	mnext     i64
	maxmcount i32 = 10000

	// Global Goroutine free list
	g_free_mu    sync.Mutex
	g_free       GoroutineList
	g_free_count i32

	// Shutdown
	stopped bool
}

// GoroutineQueue is a simple linked-list queue of Goroutine's (matches Go's gQueue).
pub struct GoroutineQueue {
pub mut:
	head &Goroutine = unsafe { nil }
	tail &Goroutine = unsafe { nil }
	size i32
}

// GoroutineList is a list of Goroutine's.
pub struct GoroutineList {
pub mut:
	head  &Goroutine = unsafe { nil }
	count i32
}

fn (mut q GoroutineQueue) push_back(gp &Goroutine) {
	mut g := unsafe { gp }
	g.sched_link = unsafe { nil }
	if q.tail != unsafe { nil } {
		q.tail.sched_link = g
	} else {
		q.head = g
	}
	q.tail = g
	q.size++
}

fn (mut q GoroutineQueue) push(gp &Goroutine) {
	mut g := unsafe { gp }
	g.sched_link = q.head
	q.head = g
	if q.tail == unsafe { nil } {
		q.tail = g
	}
	q.size++
}

fn (mut q GoroutineQueue) pop() &Goroutine {
	if q.head == unsafe { nil } {
		return unsafe { nil }
	}
	gp := q.head
	q.head = gp.sched_link
	if q.head == unsafe { nil } {
		q.tail = unsafe { nil }
	}
	q.size--
	return gp
}

fn (q &GoroutineQueue) empty() bool {
	return q.head == unsafe { nil }
}

fn (mut l GoroutineList) push(gp &Goroutine) {
	mut g := unsafe { gp }
	g.sched_link = l.head
	l.head = g
	l.count++
}

fn (mut l GoroutineList) pop() &Goroutine {
	if l.head == unsafe { nil } {
		return unsafe { nil }
	}
	gp := l.head
	l.head = gp.sched_link
	l.count--
	return gp
}

fn (l &GoroutineList) empty() bool {
	return l.head == unsafe { nil }
}

// Global scheduler instance
__global gsched = Sched{}

// Number of processors (defaults to number of CPU cores)
__global gomaxprocs = i32(0)

// All goroutines ever created (for debugging)
__global allgs_mu = sync.Mutex{}
__global allgs = []&Goroutine{}
