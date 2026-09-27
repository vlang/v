module c

import v.flat

// set_trace_calls configures V1-compatible function-entry tracing. This is
// separate from the user-provided call-site hooks enabled with `-d trace`.
pub fn (mut g FlatGen) set_trace_calls(enabled bool, patterns []string) {
	g.is_trace_calls = enabled
	g.trace_fns = patterns.clone()
}

fn trace_fn_matches(patterns []string, module_name string, fn_name string) bool {
	if patterns.len == 0 {
		return true
	}
	for pattern in patterns {
		if fn_name.match_glob(pattern)
			|| (module_name.len > 0 && (module_name.match_glob(pattern)
				|| fn_name.all_after_last('.').match_glob(pattern))) {
			return true
		}
	}
	return false
}

fn trace_qualified_fn_name(module_name string, fn_name string) string {
	if module_name == 'builtin' {
		return fn_name.trim_string_left('builtin.')
	}
	mod := if module_name.len == 0 { 'main' } else { module_name }
	return if fn_name.starts_with('${mod}.') { fn_name } else { '${mod}.${fn_name}' }
}

// A user module named trace_calls makes the loader retain the canonical name
// v.trace_calls for the tracing runtime instead of its declared short name.
fn (g &FlatGen) trace_runtime_module_name() string {
	if 'v.trace_calls.g_stack_base' in g.global_types {
		return 'v.trace_calls'
	}
	return 'trace_calls'
}

fn (mut g FlatGen) gen_trace_fn_begin(node flat.Node, module_name string) {
	if !g.is_trace_calls || module_name == g.trace_runtime_module_name() {
		return
	}
	mut arity := 0
	for i in 0 .. node.children_count {
		if g.a.child_node(&node, i).kind == .param {
			arity++
		}
	}
	mod := if module_name.len == 0 { 'main' } else { module_name }
	name := trace_qualified_fn_name(mod, node.value)
	g.gen_trace_call('${mod} ${name}/${arity}', mod, name)
}

fn (mut g FlatGen) gen_trace_call(label string, module_name string, fn_name string) {
	if !g.is_trace_calls || !trace_fn_matches(g.trace_fns, module_name, fn_name) {
		return
	}
	runtime := g.fn_c_name_in_module(g.trace_runtime_module_name(), 'on_call')
	// Lifecycle functions are emitted after the literal table. Use a literal
	// compound value rather than interning a string too late (also fork-safe).
	g.writeln('${runtime}((string){(u8*)"${c_escape(label)}", ${label.len}, 1});')
}

fn (mut g FlatGen) gen_trace_startup() {
	if !g.is_trace_calls {
		return
	}
	mod := g.trace_runtime_module_name()
	stack_base := g.global_c_name('${mod}.g_stack_base')
	start_time := g.global_c_name('${mod}.g_start_time')
	on_main := g.fn_c_name_in_module(mod, 'on_c_main')
	trace_main := trace_fn_matches(g.trace_fns, '', 'C.main')
	// Keep the anchor in the entry frame, not in on_c_main's temporary frame.
	// The guard also avoids a second header for exported callbacks initialized
	// before the synthesized main function.
	g.writeln('volatile u8 _v_trace_stack_base = 0;')
	g.writeln('if (!${start_time}) {')
	g.writeln('${stack_base} = (u8*)&_v_trace_stack_base;')
	g.writeln('${on_main}(${trace_main});')
	g.writeln('}')
}
