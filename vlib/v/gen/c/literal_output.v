module c

import strings
import v.flat

struct LiteralOutputCall {
	fd   int
	text string
	line bool
}

// generated_literal_output_only reports that the emitted program has no V
// runtime or atomic operations, so the driver need not link atomic support.
pub fn (g &FlatGen) generated_literal_output_only() bool {
	return g.literal_output_only
}

// literal_output_program uses the driver's checked literal-output proof to omit
// all V runtime support, including GC, for an allocation-free executable.
fn (mut g FlatGen) literal_output_program() ?string {
	if !g.skip_enum_autostr || g.tc.diagnostic_files.len != 1 || g.test_files.len > 0
		|| g.initial_c_flags.len > 0 || g.output_cross_c
		|| g.cache_split || g.is_shared || g.object_file_mode || g.suppress_main
		|| g.program_body_only || g.race || g.track_heap || g.is_trace_calls
		|| g.profile_file.len > 0 || g.coverage_dir.len > 0 || g.print_fn_names.len > 0
		|| g.target.os !in ['macos', 'linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly', 'solaris',
			'haiku'] {
		return none
	}
	for define in g.compile_defines {
		if define in ['freestanding', 'noprintln', 'noeprintln', 'builtin_print_use_fprintf'] {
			return none
		}
	}
	mut calls := []LiteralOutputCall{}
	for id, node in g.a.nodes {
		if node.kind == .file && node.value in g.tc.diagnostic_files {
			if !g.collect_literal_output_calls(flat.NodeId(id), mut calls) {
				return none
			}
		}
	}
	if calls.len == 0 {
		return none
	}
	mut out := strings.new_builder(256)
	defer { unsafe { out.free() } }
	// libc handles the complete write and adds the newline. No V string value,
	// allocator, initialization function, or intermediate buffer is needed.
	if calls.len == 1 && calls[0].fd == 1 && calls[0].line
		&& !calls[0].text.contains('\x00') {
		out.writeln(g.literal_output_c_declaration('puts')?)
		out.writeln('int main(void) {')
		out.writeln('\tputs("${c_byte_string_escape(calls[0].text)}");')
		out.writeln('\treturn 0;')
		out.writeln('}')
		return out.str()
	}
	mut writes := []LiteralOutputCall{}
	for call in calls {
		text := if call.line { call.text + '\n' } else { call.text }
		if text.len > 0 {
			writes << LiteralOutputCall{ fd: call.fd, text: text }
		}
	}
	if writes.len > 0 {
		out.writeln(g.literal_output_c_declaration('write')?)
	}
	out.writeln('int main(void) {')
	for call in writes {
		out.writeln('\t{')
		out.writeln('\t\tconst char* p = "${c_byte_string_escape(call.text)}";')
		out.writeln('\t\t__SIZE_TYPE__ n = ${call.text.len};')
		out.writeln('\t\twhile (n) {')
		out.writeln('\t\t\t__PTRDIFF_TYPE__ written = write(${call.fd}, p, n);')
		out.writeln('\t\t\tif (written <= 0) break;')
		out.writeln('\t\t\tp += written;')
		out.writeln('\t\t\tn -= written;')
		out.writeln('\t\t}')
		out.writeln('\t}')
	}
	out.writeln('\treturn 0;')
	out.writeln('}')
	return out.str()
}

fn (g &FlatGen) collect_literal_output_calls(id flat.NodeId, mut calls []LiteralOutputCall) bool {
	node := g.a.node(id)
	match node.kind {
		.file, .block, .expr_stmt {}
		.module_decl { return node.value == 'main' }
		.fn_decl {
			if node.value != 'main' { return false }
		}
		.call {
			if node.children_count != 2 { return false }
			callee := g.a.child_node(node, 0)
			arg := g.a.child_node(node, 1)
			if callee.kind != .ident || arg.kind != .string_literal
				|| callee.value !in ['print', 'println', 'eprint', 'eprintln'] {
				return false
			}
			calls << LiteralOutputCall{
				fd:   if callee.value.starts_with('e') { 2 } else { 1 }
				text: arg.value
				line: callee.value.ends_with('ln')
			}
			return true
		}
		else { return false }
	}
	for i in 0 .. node.children_count {
		if !g.collect_literal_output_calls(g.a.child(node, i), mut calls) { return false }
	}
	return true
}

fn (mut g FlatGen) literal_output_c_declaration(name string) ?string {
	// Keep the V binding as the ABI authority, without introducing typedefs
	// needed only by the full preamble.
	for id in g.a.nodes {
		if id.kind == .c_fn_decl && id.value.trim_string_left('C.') == name {
			return g.c_extern_decl_line(id, name).replace_each(['i32', 'int', 'ptrdiff_t',
				'__PTRDIFF_TYPE__', 'size_t', '__SIZE_TYPE__'])
		}
	}
	return none
}
