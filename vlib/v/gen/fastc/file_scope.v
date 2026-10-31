module fastc

// Each worker reuses a bounded parser arena. Only published output survives
// a file: all parser maps, tokens, and intermediate renderings are disposable.
const fastc_file_arena_keep_bytes = int(8) * 1024 * 1024

struct FastcFileScratch {
mut:
	scope voidptr
}

@[unsafe]
fn (mut scratch FastcFileScratch) generate(ctx &FastcFileGenContext, source FastcSourceFile) FastcFileGenOutput {
	$if prealloc {
		if scratch.scope == unsafe { nil }
			|| !unsafe { prealloc_scope_reenter(scratch.scope, fastc_file_arena_keep_bytes) } {
			scratch.free()
			scratch.scope = unsafe { prealloc_scope_begin() }
		}
		output := fastc_generate_single_file(ctx, source)
		unsafe { prealloc_scope_leave(scratch.scope) }
		return fastc_clone_file_gen_output(&output)
	}
	return fastc_generate_single_file(ctx, source)
}

@[unsafe]
fn (mut scratch FastcFileScratch) free() {
	$if prealloc {
		if scratch.scope != unsafe { nil } {
			unsafe { prealloc_scope_free_after(scratch.scope) }
			scratch.scope = unsafe { nil }
		}
	}
}

fn fastc_clone_output_string_map(values map[string]string) map[string]string {
	mut cloned := map[string]string{}
	cloned.reserve(u32(values.len))
	for key, value in values {
		cloned[key.clone()] = value.clone()
	}
	return cloned
}

fn fastc_clone_output_bool_map(values map[string]bool) map[string]bool {
	mut cloned := map[string]bool{}
	cloned.reserve(u32(values.len))
	for key, value in values {
		cloned[key.clone()] = value
	}
	return cloned
}

fn fastc_clone_output_strings(values []string) []string {
	mut cloned := []string{cap: values.len}
	for value in values {
		cloned << value.clone()
	}
	return cloned
}

// The caller leaves the parser's disposable arena before cloning. Every
// pointer in the returned output must then belong to the enclosing arena.
fn fastc_clone_file_gen_output(output &FastcFileGenOutput) FastcFileGenOutput {
	return FastcFileGenOutput{
		prototypes:        output.prototypes.clone()
		body:              output.body.clone()
		directive_lines:   output.directive_lines.clone()
		function_ids:      output.function_ids.clone()
		function_spans:    output.function_spans.clone()
		proto_spans:       output.proto_spans.clone()
		ref_starts:        output.ref_starts.clone()
		refs:              output.refs.clone()
		root_refs:         output.root_refs.clone()
		has_main_entry:    output.has_main_entry
		fixed_array_types: output.fixed_array_types.clone_owned()
		composite_types:   fastc_clone_output_bool_map(output.composite_types)
		spawn_typedefs:    fastc_clone_output_string_map(output.spawn_typedefs)
		spawn_helpers:     fastc_clone_output_string_map(output.spawn_helpers)
		mono_definitions:  fastc_clone_output_string_map(output.mono_definitions)
		c_flags:           fastc_clone_output_strings(output.c_flags)
		failed:            output.failed
		error_message:     output.error_message.clone()
	}
}
