module main

#flag @DIR/guard.c
#include "@DIR/guard.h"

fn C.trace_test_forbid(i32)
fn C.trace_test_sink(const_text &u8, length uint)
fn C.trace_test_reset()
fn C.trace_test_expected(u32, i64, i64, const_name &u8)
fn C.trace_test_matches() i32
fn C.trace_test_contains(const_text &u8) i32
fn C.trace_test_set_nested(fn ())
fn C.trace_test_fail_writes(i32)

struct TraceSample {
	tid   u32
	ns    i64
	stack i64
	name  string
}

fn require_trace(ok bool, detail string) {
	if !ok {
		eprintln('raw trace: ' + detail)
		exit(93)
	}
}

fn test_eprint(text string) { C.trace_test_sink(text.str, uint(text.len)) }

fn test_eprintln(text string) {
	test_eprint(text)
	test_eprint('\n')
}

fn nested_trace() { on_call('must_not_be_traced') }

fn main() {
	long_name := 'λ'.repeat(4096)
	samples := [TraceSample{0, 0, 0, 'main.main'}, TraceSample{42, 12, -1, 'short'},
		TraceSample{u32(0xffffffff), max_i64, min_i64, 'full width'},
		TraceSample{1, min_i64, max_i64, long_name}, TraceSample{8, -1, -42, 'signed bits'}]
	C.trace_test_set_nested(nested_trace)
	for sample in samples {
		C.trace_test_reset()
		C.trace_test_expected(sample.tid, sample.ns, sample.stack, sample.name.str)
		g_trace_in_hook = true
		C.trace_test_forbid(1)
		write_raw_trace_row(sample.tid, sample.ns, sample.stack, sample.name)
		C.trace_test_forbid(0)
		require_trace(g_trace_in_hook, 'nested hook preserves outer guard')
		g_trace_in_hook = false
		require_trace(C.trace_test_matches() == 1, 'decimal fields, padding and complete name')
	}
	C.trace_test_reset()
	C.errno = 77
	C.trace_test_forbid(2)
	on_call('heap_lock_is_already_held')
	C.trace_test_forbid(0)
	require_trace(C.errno == 77 && !g_trace_in_hook, 'hook restores errno and guard')
	require_trace(C.trace_test_contains(c'heap_lock_is_already_held') == 1, 'complete hook reached the sink')
	C.trace_test_reset()
	C.errno = 81
	C.trace_test_forbid(3)
	on_c_main(true)
	C.trace_test_forbid(0)
	require_trace(C.errno == 81 && !g_trace_in_hook, 'startup restores errno and guard')
	require_trace(C.trace_test_contains(c'#          tid       ns      ssize name\n') == 1, 'trace header')
	require_trace(C.trace_test_contains(c'C.main\n') == 1, 'entry trace')
	C.trace_test_fail_writes(1)
	C.errno = 83
	C.trace_test_forbid(4)
	on_call('closed_stream')
	C.trace_test_forbid(0)
	require_trace(C.errno == 83 && !g_trace_in_hook, 'failed write restores errno and guard')
	C.trace_test_fail_writes(0)
	println('allocation-free raw tracing passed')
}
