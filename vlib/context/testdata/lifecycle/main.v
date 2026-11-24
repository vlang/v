module main

import context
import os
import time

struct ExternalParent {
mut:
	signal chan int
}

fn (p &ExternalParent) deadline() ?time.Time { return none }

fn (p &ExternalParent) value(_ context.Key) ?context.Any { return none }

fn (mut p ExternalParent) done() chan int { return p.signal }

fn (mut p ExternalParent) err() IError {
	if p.signal.closed { return error('external cancellation: ' + 'payload'.repeat(1024)) }
	return none
}

fn require_lifecycle(ok bool, label string) {
	if !ok {
		eprintln(label)
		exit(90)
	}
}

fn process_status() (int, u64) {
	status := os.read_file('/proc/self/status') or { panic(err) }
	mut threads := 0
	mut virtual_bytes := u64(0)
	for line in status.split_into_lines() {
		fields := line.fields()
		if fields.len < 2 { continue }
		if fields[0] == 'Threads:' { threads = fields[1].int() }
		if fields[0] == 'VmSize:' { virtual_bytes = fields[1].u64() * 1024 }
	}
	require_lifecycle(threads > 0 && virtual_bytes > 0, 'invalid process status')
	return threads, virtual_bytes
}

fn wait_for_workers(baseline int) {
	deadline := time.sys_mono_now() + u64(5 * time.second)
	for time.sys_mono_now() < deadline {
		threads, _ := process_status()
		if threads <= baseline { return }
		time.sleep(time.millisecond)
	}
	threads, _ := process_status()
	require_lifecycle(false, 'context workers retained: ${threads}, baseline ${baseline}')
}

fn wait_for_cancel(mut ctx context.Context) {
	done := ctx.done()
	if done.closed { return }
	mut ready := false
	select {
		_ := <-done {
			ready = true
		}
		2 * time.second {
		}
	}
	require_lifecycle(ready, 'context deadline did not cancel')
}

fn repeat_cancellation(count int) {
	mut bg := context.background()
	wrapped := context.with_value(context.with_value(bg, context.Key('one'), 1), context.Key('two'), 2)
	mut custom := context.Context(&ExternalParent{ signal: chan int{} })
	for _ in 0 .. count {
		mut value_parent := wrapped
		mut plain, cancel_plain := context.with_cancel(mut value_parent)
		cancel_plain()
		require_lifecycle(plain.err().msg() == 'context canceled', 'plain cancellation')
		mut external, cancel_external := context.with_cancel(mut custom)
		cancel_external()
		require_lifecycle(external.err().msg() == 'context canceled', 'external cancellation')
		mut parent, cancel_parent := context.with_cancel_cause(mut bg)
		mut child, cancel_child := context.with_cancel_cause(mut parent)
		cancel_child(error('child first'))
		cancel_parent(error('parent later'))
		require_lifecycle(context.cause(child).msg() == 'child first', 'first child cause')
		mut pending, cancel_pending := context.with_timeout(mut bg, time.hour)
		cancel_pending()
		require_lifecycle(pending.err().msg() == 'context canceled', 'pending deadline cancellation')
		mut pending_cause, cancel_pending_cause := context.with_timeout_cause(mut bg, time.hour, error('unused deadline'))
		cancel_pending_cause()
		require_lifecycle(context.cause(pending_cause).msg() == 'context canceled', 'pending deadline cause')
	}
	// Leave the custom parent open: its canceled children must stop their own
	// propagation workers without relying on parent cancellation.
}

fn main() {
	baseline, _ := process_status()
	repeat_cancellation(8)
	wait_for_workers(baseline)
	_, before := process_status()
	repeat_cancellation(128)
	wait_for_workers(baseline)
	threads, after := process_status()
	$if !context_lifecycle_qemu ? {
		// Allow ordinary heap/page-pool growth, but not a retained stack per
		// canceled context. Native bare threads own and unmap their own stacks.
		require_lifecycle(after <= before + 32 * 1024 * 1024, 'retained worker mappings: ${before} -> ${after}')
	}
	mut bg := context.background()
	for _ in 0 .. 4 {
		mut expired, cancel := context.with_timeout_cause(mut bg, time.millisecond, error('actual deadline'))
		wait_for_cancel(mut expired)
		require_lifecycle(expired.err().msg() == 'context deadline exceeded', 'deadline error')
		require_lifecycle(context.cause(expired).msg() == 'actual deadline', 'deadline cause')
		cancel()
	}
	wait_for_workers(baseline)
	_, final_bytes := process_status()
	$if !context_lifecycle_qemu ? {
		require_lifecycle(final_bytes <= before + 32 * 1024 * 1024, 'retained deadline mappings: ${before} -> ${final_bytes}')
	}
	// Parent.err() is user code and can allocate an error on its worker.
	// That value must survive the temporary select storage and worker exit.
	parent := &ExternalParent{ signal: chan int{} }
	mut external_parent := context.Context(parent)
	mut escaped, cancel_escaped := context.with_cancel(mut external_parent)
	defer { cancel_escaped() }
	parent.signal.close()
	wait_for_cancel(mut escaped)
	wait_for_workers(baseline)
	expected := 'external cancellation: ' + 'payload'.repeat(1024)
	require_lifecycle(escaped.err().msg() == expected, 'worker error lifetime')
	require_lifecycle(context.cause(escaped).msg() == expected, 'worker cause lifetime')
	println('threads ${baseline} -> ${threads}; virtual bytes ${before} -> ${after} -> ${final_bytes}')
	println('freestanding context lifecycle passed')
}
