module main

#flag -I @DIR
#include "deny.h"

fn C.test_deny_allocations(int)

struct Outcome {
mut:
	text     string
	cleanups int
}

@[noinline]
fn fail_allocation() {
	// Keep the allocation observable even when the C compiler optimizes this function.
	unsafe {
		p := malloc(16)
		if p == nil { exit(9) }
		free(p)
	}
}

fn recover_allocation(mut outcome Outcome) {
	defer {
		if msg := recover() { outcome.text = msg }
	}
	defer { outcome.cleanups++ }
	fail_allocation()
}

fn recover_ordinary(mut outcome Outcome) {
	defer {
		if msg := recover() { outcome.text = msg }
	}
	panic('ordinary')
}

fn memory_inside_ordinary(mut outer Outcome, mut inner Outcome) {
	defer {
		if msg := recover() { outer.text = msg }
	}
	defer {
		C.test_deny_allocations(1)
		recover_allocation(mut inner)
		C.test_deny_allocations(0)
	}
	panic('outer')
}

fn ordinary_inside_memory(mut outer Outcome, mut inner Outcome) {
	defer {
		if msg := recover() { outer.text = msg }
	}
	defer {
		C.test_deny_allocations(0)
		recover_ordinary(mut inner)
	}
	C.test_deny_allocations(1)
	fail_allocation()
}

fn main() {
	mut outcome := Outcome{}
	C.test_deny_allocations(1)
	recover_allocation(mut outcome)
	C.test_deny_allocations(0)
	if outcome.text != 'memory allocation failure' || outcome.cleanups != 1 { exit(7) }
	mut a := Outcome{}
	mut b := Outcome{}
	memory_inside_ordinary(mut a, mut b)
	if a.text != 'outer' || b.text != outcome.text || b.cleanups != 1 { exit(8) }
	ordinary_inside_memory(mut a, mut b)
	if a.text != outcome.text || b.text != 'ordinary' { exit(10) }
}
