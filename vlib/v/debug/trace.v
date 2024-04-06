@[has_globals]
module debug

@[markused]
__global g_trace = TraceHook{}

type HookFnCall = fn (string)

@[noinit]
pub struct TraceHook {
mut:
	in_hook bool
pub mut:
	trace_after_call  []HookFnCall
	trace_before_call []HookFnCall
}

// after_call_hook calls the registered hook fns
@[markused]
fn after_call_hook(fn_name string) {
	g_trace.in_hook = true
	for func in g_trace.trace_after_call {
		func(fn_name)
	}
	g_trace.in_hook = false
}

// before_call_hook calls the registered hook fns
@[markused]
fn before_call_hook(fn_name string) {
	g_trace.in_hook = true
	for func in g_trace.trace_before_call {
		func(fn_name)
	}
	g_trace.in_hook = false
}

// add_after_call adds a fn hook to after hook list and returns its id
@[inline; markused]
pub fn add_after_call(func HookFnCall) HookFnCall {
	g_trace.trace_after_call << func
	return func
}

// add_before_call adds a fn hook to before hook list and return its id
@[inline; markused]
pub fn add_before_call(func HookFnCall) HookFnCall {
	g_trace.trace_before_call << func
	return func
}

// remove_after_call removes a fn hook from after hook list by its idx
@[inline; markused]
pub fn remove_after_call(func HookFnCall) {
	idx := g_trace.trace_after_call.index(func)
	if idx != -1 {
		g_trace.trace_after_call.delete(idx)
	}
}

// remove_before_call removes a fn hook from before hook list by its idx
@[inline; markused]
pub fn remove_before_call(func HookFnCall) {
	idx := g_trace.trace_before_call.index(func)
	if idx != -1 {
		g_trace.trace_before_call.delete(idx)
	}
}
