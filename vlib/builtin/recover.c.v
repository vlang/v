module builtin

// Panic recovery, the way Go does it.
//
// Once a program calls `recover()`, V3 cgen gives every `defer` a frame. The
// `defer` statement links its frame into `g_panic_state` and `setjmp()`s into
// it, and the frame is unlinked again right before the deferred block runs
// normally. A panic jumps from frame to frame, newest first, and the landing
// pad of each frame runs its deferred block. `recover()` called directly in
// such a block stops the panic: the function that deferred the block runs its
// remaining deferred blocks and returns the zero value of its result type.
// A panic without any frame to unwind prints its message and exits, as before.

// PanicFrame is the head of the C `v_unwind_frame`, which cgen declares for each
// `defer`; the rest of it holds the `jmp_buf` of the landing pad.
struct PanicFrame {
mut:
	prev &PanicFrame = unsafe { nil }
	// owner identifies the function call that deferred the block.
	owner voidptr
	// jump longjmp()s to the landing pad of the frame; it does not return.
	jump fn (voidptr) = unsafe { nil }
}

struct PanicRecord {
mut:
	// msg lives on the C heap: the only reference to it is in thread-local
	// storage, which the GC does not scan.
	msg string
	// debug holds what panic_debug() prints for the panic, when it raised it.
	debug PanicDebugInfo
	// frame is the frame whose deferred block runs because of this panic.
	frame &PanicFrame = unsafe { nil }
	// resume is where the unwinding goes on once that block is done.
	resume    &PanicFrame = unsafe { nil }
	recovered bool
	// aborted is set when a newer panic left the block that ran for this one.
	aborted bool
}

// PanicDebugInfo only holds string literals, which panic_debug() calls pass.
struct PanicDebugInfo {
	line_no int
	file    string
	mod     string
	fn_name string
}

struct PanicState {
mut:
	top &PanicFrame = unsafe { nil }
	// records are the panics in flight, oldest first, on the C heap.
	records &PanicRecord = unsafe { nil }
	len     int
	cap     int
}

// V3 cgen emits this as thread-local storage: each thread unwinds its own stack.
__global g_panic_state = PanicState{}

// recover stops the panic that is unwinding the stack and returns its message.
// It only does so when it is called directly in a `defer` block that runs
// because of that panic. The function that deferred the block then returns
// normally, with the zero value of its result type, once its other deferred
// blocks ran. Called anywhere else, or when there is no panic, recover returns
// `none` and has no effect.
// Example:
// ```v
// fn safe_div(a int, b int) int {
// 	defer {
// 		if msg := recover() {
// 			eprintln('recovered: ${msg}')
// 		}
// 	}
// 	return a / b
// }
// ```
pub fn recover() ?string {
	return none
}

// panic_frame_push links the frame of a `defer` that was just reached.
@[markused]
fn panic_frame_push(frame voidptr) {
	mut f := unsafe { &PanicFrame(frame) }
	f.prev = g_panic_state.top
	g_panic_state.top = f
}

// panic_frame_pop unlinks the frame of a deferred block that is about to run
// normally; for a `defer(fn)` block, that is its last pending run. A `defer(fn)`
// frame lives until its function returns, so a block deferred before it inside
// a loop can be unlinked from below it.
@[markused]
fn panic_frame_pop(frame voidptr) {
	f := unsafe { &PanicFrame(frame) }
	if g_panic_state.top == f {
		g_panic_state.top = f.prev
		return
	}
	mut cur := g_panic_state.top
	for cur != unsafe { nil } {
		if cur.prev == f {
			cur.prev = f.prev
			return
		}
		cur = cur.prev
	}
}

// panic_frame_relink links the frame of a `defer(fn)` block again, while its
// landing pad runs one of the block's pending runs and others are left. A panic
// in that run then still runs the others, and so does the panic that landed.
@[markused]
fn panic_frame_relink(frame voidptr) {
	panic_frame_push(frame)
	if g_panic_state.len > 0 {
		mut rec := panic_record(g_panic_state.len - 1)
		if voidptr(rec.frame) == frame {
			rec.resume = unsafe { &PanicFrame(frame) }
		}
	}
}

@[inline]
fn panic_record(i int) &PanicRecord {
	return unsafe { &g_panic_state.records[i] }
}

// panic_recover_frame is what `recover()` in the deferred block of `frame`
// compiles to.
@[markused]
fn panic_recover_frame(frame voidptr) ?string {
	if g_panic_state.len == 0 {
		return none
	}
	mut rec := panic_record(g_panic_state.len - 1)
	if rec.recovered || rec.aborted || voidptr(rec.frame) != frame {
		return none
	}
	rec.recovered = true
	return rec.msg.clone()
}

// panic_frame_done is called by the landing pad of `frame` after its deferred
// block ran. It only returns once the panic was recovered and no other frame of
// the same function call is left; the landing pad then returns the zero value.
@[markused]
fn panic_frame_done(frame voidptr) {
	f := unsafe { &PanicFrame(frame) }
	mut rec := panic_record(g_panic_state.len - 1)
	if !rec.recovered {
		panic_jump_next()
	}
	next := g_panic_state.top
	if next != unsafe { nil } && next.owner == f.owner {
		g_panic_state.top = next.prev
		rec.frame = next
		rec.resume = next.prev
		next.jump(voidptr(next))
	}
	// The panics that this one aborted end with it.
	panic_record_drop()
	for g_panic_state.len > 0 && panic_record(g_panic_state.len - 1).aborted {
		panic_record_drop()
	}
}

fn panic_record_drop() {
	g_panic_state.len--
	mut rec := panic_record(g_panic_state.len)
	unsafe {
		C.free(rec.msg.str)
		*rec = PanicRecord{}
	}
	// Nothing frees the records when their thread ends, so they go with the
	// last panic in flight.
	if g_panic_state.len == 0 {
		unsafe { C.free(g_panic_state.records) }
		g_panic_state.records = unsafe { nil }
		g_panic_state.cap = 0
	}
}

// panic_frames_reset forgets all frames and panics of the thread. The test
// runner calls it after each test, since a failed assert jumps out of the test
// without running its deferred blocks.
@[markused]
fn panic_frames_reset() {
	g_panic_state.top = unsafe { nil }
	for g_panic_state.len > 0 {
		panic_record_drop()
	}
}

// panic_unwind starts unwinding the stack for a panic, when there are frames.
@[noreturn]
fn panic_unwind(msg string, debug PanicDebugInfo) {
	if g_panic_state.len == g_panic_state.cap {
		new_cap := if g_panic_state.cap == 0 { 4 } else { g_panic_state.cap * 2 }
		records := unsafe {
			&PanicRecord(C.realloc(g_panic_state.records, usize(new_cap) * usize(sizeof(PanicRecord))))
		}
		if records == unsafe { nil } {
			panic_frames_reset()
			panic(msg)
		}
		g_panic_state.records = records
		g_panic_state.cap = new_cap
	}
	msg_copy := unsafe { &u8(C.malloc(usize(msg.len + 1))) }
	if msg_copy == unsafe { nil } {
		panic_frames_reset()
		panic(msg)
	}
	unsafe {
		C.memcpy(msg_copy, msg.str, usize(msg.len))
		msg_copy[msg.len] = 0
	}
	mut rec := panic_record(g_panic_state.len)
	unsafe {
		*rec = PanicRecord{
			msg:   string{
				str:    msg_copy
				len:    msg.len
				is_lit: 1
			}
			debug: debug
		}
	}
	g_panic_state.len++
	panic_jump_next()
}

// panic_jump_next runs the next deferred block for the newest panic.
@[noreturn]
fn panic_jump_next() {
	frame := g_panic_state.top
	// Leaving the block that runs for an older panic aborts that panic, like in Go.
	mut i := g_panic_state.len - 2
	for i >= 0 && panic_record(i).resume == frame {
		panic_record(i).aborted = true
		i--
	}
	if frame == unsafe { nil } {
		panic_fatal()
	}
	g_panic_state.top = frame.prev
	mut rec := panic_record(g_panic_state.len - 1)
	rec.frame = frame
	rec.resume = frame.prev
	frame.jump(voidptr(frame))
	for {}
}

// panic_fatal ends the program for a panic that no deferred block recovered,
// once all deferred blocks ran. Like Go, it lists the panics that the last one
// interrupted before it.
@[noreturn]
fn panic_fatal() {
	mut msg := ''
	for i in 0 .. g_panic_state.len {
		rec := panic_record(i)
		if i > 0 {
			msg += '\n\tpanic: '
		}
		msg += rec.msg.clone()
		if rec.recovered {
			msg += ' [recovered]'
		}
	}
	debug := panic_record(g_panic_state.len - 1).debug
	panic_frames_reset()
	// The native backends build panic_debug() in, so it has no source to call.
	$if !native ? {
		if debug.file.len > 0 {
			panic_debug(debug.line_no, debug.file, debug.mod, debug.fn_name, msg)
		}
	}
	panic(msg)
}
