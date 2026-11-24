module main

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include "@DIR/probe.h"
#flag @DIR/probe.c

fn C.trace_note()
fn C.trace_gate() i32
fn C.trace_notes() i32

@[export: 'trace_reenter']
fn reentrant_callback() {
	C.trace_note()
}

fn observed() {
	assert C.errno == 71
}

fn work() {
	C.errno = 71
	observed()
	assert C.errno == 71
}

fn main() {
	C.errno = 71
	observed()
	first := spawn work()
	second := spawn work()
	first.wait()
	second.wait()
	assert C.trace_gate() == 2
	assert C.trace_notes() > 0
	C.errno = 71
	assert C.close(2) == 0
	observed()
	assert C.errno == 71
	println('trace guard ok')
}
