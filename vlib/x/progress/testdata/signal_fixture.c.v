module main

// Used by signal_test.v: starts a live display, delivers a signal to itself,
// and shows what happens for each way an application may treat that signal.
//
// Modes: default-int, default-term, handler-int, handler-term, ignore-int,
// ignore-term.
import os
import time
import x.progress

#include <signal.h>

fn C.raise(i32) i32

fn app_handler(_ os.Signal) {
	exit(42)
}

fn main() {
	mode := os.args[1]
	sig := if mode.ends_with('term') { os.Signal.term } else { os.Signal.int }
	if mode.starts_with('handler') {
		os.signal_opt(sig, app_handler) or { panic(err) }
	} else if mode.starts_with('ignore') {
		os.signal_ignore(sig)
	}
	mut mb := progress.MultiBar.new(force: true, delay: 5 * time.millisecond)
	mut sp := mb.add_spinner(text: 'work')
	mb.start()
	time.sleep(30 * time.millisecond)
	C.raise(i32(sig))
	time.sleep(30 * time.millisecond)
	sp.finish()
	mb.wait()
}
