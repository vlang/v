// A bare function name used as a fn value refers to the function, even when a
// method with the same name is declared earlier.
struct Document {
	id int
}

type LifecycleCallback = fn (target voidptr, doc &Document) int

struct Hook {
	target     voidptr
	did_change LifecycleCallback = unsafe { nil }
}

struct EventLoop {
mut:
	changes int
}

fn (mut event_loop EventLoop) did_change() {
	event_loop.changes++
}

fn did_change(target voidptr, doc &Document) int {
	mut event_loop := unsafe { &EventLoop(target) }
	event_loop.did_change()
	return doc.id
}

fn make_hook(event_loop &EventLoop) Hook {
	return Hook{
		target:     voidptr(event_loop)
		did_change: did_change
	}
}

fn test_fn_value_named_like_method() {
	mut event_loop := &EventLoop{}
	hook := make_hook(event_loop)
	assert hook.did_change(hook.target, &Document{
		id: 7
	}) == 7
	assert event_loop.changes == 1
}
