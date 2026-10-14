module c

import strings
import os
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
			'haiku', 'windows', 'termux', 'qnx', 'serenity', 'wasm32_emscripten', 'android', 'ios'] {
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
	if g.target.os == 'android' {
		return g.literal_android_output(calls)
	}
	if g.target.os == 'ios' {
		return g.literal_ios_output(calls)
	}
	windows := g.target.os == 'windows'
	if windows {
		for call in calls {
			for byte in call.text {
				if byte >= 128 {
					return none
				}
			}
		}
	}
	mut out := strings.new_builder(256)
	defer { unsafe { out.free() } }
	// libc handles the complete write and adds the newline. No V string value,
	// allocator, initialization function, or intermediate buffer is needed.
	if calls.len == 1 && calls[0].fd == 1 && calls[0].line
		&& !calls[0].text.contains('\x00') {
		out.writeln(g.literal_output_c_declaration('puts')?)
		if windows {
			out.writeln(g.literal_output_c_declaration('_setmode')?)
		}
		out.writeln('int main(void) {')
		if windows {
			out.writeln('\t_setmode(1, 0x8000);')
		}
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
		if windows {
			out.writeln(g.literal_output_c_declaration('_setmode')?)
		}
	}
	out.writeln('int main(void) {')
	if windows {
		for fd in [1, 2] {
			if writes.any(it.fd == fd) {
				out.writeln('\t_setmode(${fd}, 0x8000);')
			}
		}
	}
	for call in writes {
		out.writeln('\t{')
		out.writeln('\t\tconst char* p = "${c_byte_string_escape(call.text)}";')
		size_type := if windows { 'unsigned long long' } else { '__SIZE_TYPE__' }
		out.writeln('\t\t${size_type} n = ${call.text.len};')
		out.writeln('\t\twhile (n) {')
		if windows {
			// The CRT count is unsigned int, but its signed int result limits one
			// call to INT_MAX bytes. Keep large literals and short writes lossless.
			out.writeln('\t\t\tunsigned int count = n > 2147483647U ? 2147483647U : (unsigned int)n;')
			out.writeln('\t\t\tint written = _write(${call.fd}, p, count);')
		} else {
			out.writeln('\t\t\t__PTRDIFF_TYPE__ written = write(${call.fd}, p, n);')
		}
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

fn (mut g FlatGen) literal_output_c_flags() []string {
	mut flags := g.initial_c_flags.clone()
	if g.target.os == 'android' {
		flags << '-llog'
	} else if g.target.os == 'ios' {
		// The literal path returns before collect_gen_info infers VROOT.
		// Resolve the logger binding's original source, including cached builtin
		// headers, so its native object is independent of the working directory.
		if g.compiler_vroot.len == 0 && g.a != unsafe { nil } {
			mut source_file := ''
			for node in g.a.nodes {
				if node.kind == .file {
					source_file = node.value
				} else if node.kind == .c_fn_decl
					&& node.value.trim_string_left('C.') == 'WrappedNSLog' {
					g.note_compiler_source_file(g.a.cached_header_sources[source_file] or {
						source_file
					})
					if g.compiler_vroot.len > 0 {
						break
					}
				}
			}
		}
		flags << [os.join_path(g.compiler_vroot, 'thirdparty', 'ios', 'ios.o'), '-framework',
			'Foundation']
	}
	return flags
}

fn (mut g FlatGen) literal_android_output(calls []LiteralOutputCall) ?string {
	mut out := strings.new_builder(512)
	defer { unsafe { out.free() } }
	out.writeln(g.literal_output_c_declaration('__android_log_print')?)
	// Match the existing Android adapter's configurable tag and stringification.
	out.writeln('#define V_ANDROID_LOG_STR_VALUE(arg) #arg')
	out.writeln('#define V_ANDROID_LOG_NAME(tag_name) V_ANDROID_LOG_STR_VALUE(tag_name)')
	out.writeln('#ifndef V_ANDROID_LOG_TAG')
	out.writeln('#if defined(APPNAME)')
	out.writeln('#define V_ANDROID_LOG_TAG APPNAME')
	out.writeln('#else')
	out.writeln('#define V_ANDROID_LOG_TAG "V"')
	out.writeln('#endif')
	out.writeln('#endif')
	out.writeln('#define V_ANDROID_LOG_TAG_NAME V_ANDROID_LOG_NAME(V_ANDROID_LOG_TAG)')
	out.writeln('int main(void) {')
	for call in calls {
		priority := if call.fd == 1 { 4 } else { 6 }
		// Android's print and println both append a newline on stdout. Stderr
		// appends one only for eprintln; printf precision preserves its NUL handling.
		format := if call.fd == 1 || call.line { '%.*s\\n' } else { '%.*s' }
		out.writeln('\t__android_log_print(${priority}, V_ANDROID_LOG_TAG_NAME, "${format}", ${call.text.len}, "${c_byte_string_escape(call.text)}");')
	}
	out.writeln('\treturn 0;')
	out.writeln('}')
	return out.str()
}

fn (mut g FlatGen) literal_ios_output(calls []LiteralOutputCall) ?string {
	for call in calls {
		// WrappedNSLog treats its first argument as an NSString format string.
		// Preserve the regular runtime's formatting path for those literals.
		if call.text.contains('%') {
			return none
		}
	}
	mut out := strings.new_builder(256)
	defer { unsafe { out.free() } }
	out.writeln(g.literal_output_c_declaration('WrappedNSLog')?)
	out.writeln('int main(void) {')
	for call in calls {
		out.writeln('\tWrappedNSLog("${c_byte_string_escape(call.text)}");')
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
			abi_name := if name == 'write' && g.target.os == 'windows' { '_write' } else { name }
			return g.c_extern_decl_line(id, abi_name).replace_each(['i32', 'int', 'u32', 'unsigned int',
				'ptrdiff_t', '__PTRDIFF_TYPE__', 'size_t', '__SIZE_TYPE__'])
		}
	}
	return none
}
