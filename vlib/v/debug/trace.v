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

pub fn after_call_hook(fn_name string) {
	g_trace.in_hook = true
	for func in g_trace.trace_after_call {
		func(fn_name)
	}
	g_trace.in_hook = false
}

pub fn before_call_hook(fn_name string) {
	g_trace.in_hook = true
	for func in g_trace.trace_before_call {
		func(fn_name)
	}
	g_trace.in_hook = false
}

pub fn add_after_call(func HookFnCall) {
	g_trace.trace_after_call << func
}

pub fn add_before_call(func HookFnCall) {
	g_trace.trace_before_call << func
}
