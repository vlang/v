module fastc

import os
import v3.cmdexec
import v3.gen.c.naming
import v3.pref
import v3.token
import v3.scanner

fn (mut g Parser) run() !string {
	g.next()
	g.parse_top_level_items(false)!
	generated := fastc_take_string(mut g.out)
	g.drain_pending_mono()!
	return generated
}

// drain_pending_mono generates every on-demand generic instance queued during the main
// parse (and any nested instances their own bodies queue via the `$for` unroll), appending
// each one's C to `g.out` / `g.protos` by re-parsing it as an ordinary function or method.
fn (mut g Parser) drain_pending_mono() ! {
	for g.pending_mono.len > 0 {
		req := g.pending_mono[0]
		g.pending_mono = g.pending_mono[1..].clone()
		src := g.generic_method_sources[req.source_key] or { continue }
		mut instance := g.render_mono_method(src, req.concrete)
		if instance == '' {
			continue
		}
		instance = g.erase_mono_generic_type_arguments(instance, src)
		if fastc_is_json2_voidptr_element_check(req, src) {
			if body_open := instance.index('{') {
				instance = instance[..body_open + 1] + '\nreturn false\n}'
			}
		}
		start := g.out.len
		g.parse_mono_instance(instance, src) or {
			return error('generic instance `${req.source_key}[${req.concrete}]`: ${err.msg()}')
		}
		definition := g.out.after(start)
		if definition != '' {
			mono_name := fastc_monomorphized_name(src.name, req.concrete)
			c_name := if src.receiver_type == '' {
				g.c_function_name_for_key(fastc_function_key(src.module_name, mono_name))
			} else {
				fastc_method_c_name(src.module_name, src.receiver_type, mono_name)
			}
			g.generated_mono[c_name] = true
			g.mono_definitions[c_name] = definition
		}
	}
}

// fastc_is_json2_voidptr_element_check scopes the erased-placeholder fallback to
// json2's internal decoder helper. A user method with the same ordinary name must
// retain its specialized body.
fn fastc_is_json2_voidptr_element_check(req FastcMonoRequest, src FastcGenericMethodSource) bool {
	normalized_path := src.path.replace('\\', '/')
	return req.concrete == 'voidptr' && src.name == 'check_element_type_valid' && (src.receiver_type == 'json2__Decoder' || src.receiver_type.ends_with('__json2__Decoder')) && normalized_path.ends_with('/vlib/json2/decode_sumtype.v')
}

// reset_lookup_memos discards the per-file name lookup memos. They are keyed
// by the bare name only, so they are valid for exactly one module and import
// context and must be reset whenever the parser switches to another one.
fn (mut g Parser) reset_lookup_memos() {
	g.unqualified_key_memo = map[string]string{}
	g.nonlocal_name_type_memo = map[string]string{}
	g.resolved_name_memo = map[string]string{}
	g.declared_type_key_memo = map[string]FastcMemoEntry{}
	g.type_memo = map[i64]string{}
	g.method_key_memo = map[string]map[string]string{}
	g.field_memo = map[string]map[string]FastcStructField{}
	g.last_field_known = false
}

// parse_mono_instance re-parses one concrete instance in its defining module, so its body
// (including any `$for`/`$if`) resolves unqualified functions and imported types correctly.
fn (mut g Parser) parse_mono_instance(instance string, source FastcGenericMethodSource) ! {
	file := token.File.unindexed(source.path, instance.len)
	g.path = source.path
	g.module_name = source.module_name
	g.imports = source.imports.clone()
	// The lookup memos are keyed by bare name and answer for the current
	// module and imports, so they must not carry over into another module.
	g.reset_lookup_memos()
	g.s = scanner.new_scanner(g.prefs, .normal)
	g.s.init(file, instance)
	g.next()
	if g.tok in [.key_pub, .key_static] {
		g.next()
	}
	if g.tok == .key_fn {
		previous_drain := g.in_mono_drain
		g.in_mono_drain = true
		g.parse_function(true)!
		g.in_mono_drain = previous_drain
	}
}

fn (mut g Parser) parse_top_level_items(stop_at_block_end bool) ! {
	for g.tok != .eof {
		g.skip_semicolons()
		if stop_at_block_end && g.tok == .rcbr {
			g.next()
			g.skip_semicolons()
			return
		}
		if g.tok == .eof {
			break
		}
		if g.selfhost && g.tok == .rcbr {
			g.next()
			continue
		}
		mut item_enabled := true
		g.pending_direct_array_access = false
		g.next_declaration_is_unsafe = false
		for g.tok == .attribute {
			item_enabled = g.skip_attribute()! && item_enabled
			g.skip_semicolons()
		}
		if g.tok == .key_module {
			g.parse_module()!
			continue
		}
		if g.tok == .key_import {
			g.skip_import()!
			continue
		}
		if g.tok == .hash {
			g.parse_c_directive()!
			continue
		}
		if g.selfhost && g.tok == .dollar {
			g.parse_top_level_comptime_if()!
			continue
		}
		if g.tok == .key_pub || g.tok == .key_static {
			g.next()
		}
		if g.tok in [.key_struct, .key_union, .key_enum, .key_interface, .key_type, .key_const,
			.key_global] {
			g.skip_top_level_declaration()!
			continue
		}
		if g.tok == .key_fn {
			out_start := g.out.len
			proto_start := g.protos.len
			g.parse_function(item_enabled)!
			g.record_function_span(out_start, proto_start)
			continue
		}
		if g.has_main {
			return g.unsupported('top-level `${g.token_source()}` after `main`')
		}
		if g.selfhost {
			return g.unsupported('unexpected top-level `${g.token_source()}` token `${g.tok.str()}`')
		}
		g.parse_script()!
		break
	}
	if stop_at_block_end {
		return g.unsupported('unfinished top-level compile-time block')
	}
}

fn (mut g Parser) parse_c_directive() ! {
	directive := g.lit.trim_space()
	g.next()
	if directive == 'flag' || directive.starts_with('flag ') || directive == 'pkgconfig' || directive.starts_with('pkgconfig ') {
		// FastC's compiler invocation supplies its own target flags. The bootstrap
		// dependency set only uses these directives for optional libraries.
		if g.prefs.selfhost {
			return
		}
		if directive.starts_with('flag ') {
			mut rest := directive['flag '.len..].trim_space()
			// A platform-qualified `#flag <os> ...` that names a different target is
			// inert for this build; skip it instead of rejecting the whole program,
			// mirroring the `#include <os>` target filtering below.
			qualifier := rest.all_before(' ')
			if qualifier in ['windows', 'macos', 'darwin', 'ios', 'mac', 'linux', 'freebsd', 'openbsd',
				'netbsd', 'dragonfly', 'solaris', 'android'] {
				if !pref.comptime_flag_value(g.prefs, qualifier) {
					return
				}
				rest = rest.all_after(qualifier).trim_space()
			}
			// FastC drives its own tcc link line (it already links pthread, libm and
			// the host system libraries), so a bare `#flag -lfoo`/`-L`/`-framework`
			// only names extra link libraries, and `-I<path>` only adds a header search
			// path (FastC's bundled tcc already resolves the system headers these C
			// files include). Skip such flags instead of rejecting the whole program.
			if fastc_flag_is_skippable(rest) {
				g.c_flags << fastc_c_flag_args(rest, g.prefs.vroot, g.path)!
				return
			}
			// A `#flag -DNAME` / `-DNAME=value` preprocessor define affects the C that
			// follows (e.g. the sqlite3 header/binding); emit it as a `#define` so the
			// definition is in effect, skipping any link/header flags mixed alongside.
			if defines := fastc_flag_defines(rest) {
				g.c_flags << fastc_c_flag_args(rest, g.prefs.vroot, g.path)!
				for define in defines {
					g.out.writeln('#define ${define}')
				}
				return
			}
		}
		if directive == 'pkgconfig' || directive.starts_with('pkgconfig ') {
			packages := directive.all_after('pkgconfig').trim_space()
			g.c_flags << fastc_pkgconfig_flags(packages)!
			return
		}
		return g.unsupported('C build directive `#${directive}`')
	}
	mut c_directive := fastc_resolve_c_pseudo_paths(directive, g.prefs.vroot, g.path)
	if c_directive.starts_with('insert ') {
		c_directive = 'include ' + c_directive['insert '.len..]
	}
	if c_directive.starts_with('include ') {
		remainder := c_directive['include '.len..]
		qualifier := remainder.all_before(' ')
		if qualifier in ['windows', 'macos', 'linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly',
			'solaris', 'android'] {
			if !pref.comptime_flag_value(g.prefs, qualifier) {
				return
			}
			c_directive = 'include ' + remainder.all_after(' ')
		}
	}
	if c_directive == '' {
		return g.unsupported('empty C directive')
	}
	g.out.writeln('#${c_directive}')
}

fn fastc_c_flag_args(raw string, vroot string, source_file string) ![]string {
	expanded := fastc_resolve_c_pseudo_paths(raw, vroot, source_file)
	mut args := cmdexec.split_args(expanded) or {
		return error('fastc parser cannot split C build flag `${raw}`')
	}
	base_dir := if source_file.len > 0 { os.real_path(os.dir(source_file)) } else { os.getwd() }
	mut resolve_next_path := false
	for i, arg in args {
		if resolve_next_path {
			if !os.is_abs_path(arg) {
				args[i] = os.join_path(base_dir, arg)
			}
			resolve_next_path = false
			continue
		}
		if arg in ['-I', '-L', '-F', '-isystem', '-iquote', '-idirafter'] {
			resolve_next_path = true
			continue
		}
		for prefix in ['-I', '-L', '-F'] {
			if arg.starts_with(prefix) && arg.len > prefix.len {
				path := arg[prefix.len..]
				if !os.is_abs_path(path) {
					args[i] = prefix + os.join_path(base_dir, path)
				}
				break
			}
		}
		if (arg.ends_with('.o') || arg.ends_with('.a') || arg.ends_with('.obj') || arg.ends_with('.lib')) && !os.is_abs_path(arg) {
			args[i] = os.join_path(base_dir, arg)
		}
		resolved_arg := args[i]
		if resolved_arg.ends_with('.o') && !os.is_file(resolved_arg) {
			c_source := resolved_arg[..resolved_arg.len - 2] + '.c'
			if os.is_file(c_source) {
				args[i] = c_source
			}
		}
	}
	return args
}

fn fastc_pkgconfig_flags(raw string) ![]string {
	packages := cmdexec.split_args(raw) or {
		return error('fastc parser cannot split `#pkgconfig ${raw}`')
	}
	if packages.len == 0 {
		return error('fastc parser requires a package name after `#pkgconfig`')
	}
	mut args := ['--cflags', '--libs']
	args << packages
	result := cmdexec.run('pkg-config', args)
	if result.exit_code != 0 {
		return error('fastc parser cannot resolve `#pkgconfig ${raw}`: ${result.output.trim_space()}')
	}
	return cmdexec.split_args(result.output.trim_space()) or {
		return error('fastc parser cannot split flags for `#pkgconfig ${raw}`')
	}
}

// fastc_flag_is_skippable reports whether a `#flag` payload only names link
// libraries/search paths/frameworks or header include paths. Such flags affect
// linking or header lookup, not C generation, so FastC can safely skip them (it
// manages its own tcc link line and ships the system headers these C files include).
// Anything else (`-D`, `-std=...`) still reaches the unsupported path.
fn fastc_flag_is_skippable(flag string) bool {
	tokens := flag.fields()
	if tokens.len == 0 {
		return false
	}
	mut i := 0
	for i < tokens.len {
		part := tokens[i]
		// Search-path / include flags whose operand can be a SEPARATE token
		// (`-I <path>`): skip the flag and its path operand.
		if part in ['-I', '-L', '-F', '-rpath', '-isystem', '-iquote', '-idirafter'] {
			i += 2
			continue
		}
		if part.starts_with('-l') || part.starts_with('-L') || part.starts_with('-Wl') || part.starts_with('-rpath') || part.starts_with('-I') || part.starts_with('-F') || part == '-pthread' {
			i++
			continue
		}
		if part in ['-framework', '-weak_framework', '-weak_library', '-Xlinker'] {
			// Skip the operand that names the framework/library too.
			i += 2
			continue
		}
		// A bare object / static-library link input (`@VEXEROOT/.../cJSON.o`): FastC drives
		// its own tcc link line and does not add these, so skip it. If a live reference to
		// its symbols survives reachability, the link fails explicitly instead.
		if part.ends_with('.o') || part.ends_with('.a') || part.ends_with('.obj') || part.ends_with('.lib') {
			i++
			continue
		}
		return false
	}
	return true
}

// fastc_flag_defines returns the `#define` bodies for a `#flag` payload made up of
// `-DNAME` / `-DNAME=value` preprocessor defines (link/header flags mixed in are
// skipped). Returns none if the payload has no define or contains anything else.
fn fastc_flag_defines(flag string) ?[]string {
	tokens := flag.fields()
	if tokens.len == 0 {
		return none
	}
	mut defines := []string{}
	mut has_define := false
	for part in tokens {
		if part.starts_with('-D') {
			body := part[2..]
			if body == '' {
				return none
			}
			has_define = true
			if body.contains('=') {
				defines << '${body.all_before('=')} ${body.all_after('=')}'
			} else {
				defines << '${body} 1'
			}
			continue
		}
		if part.starts_with('-l') || part.starts_with('-L') || part.starts_with('-I') || part.starts_with('-Wl') || part.starts_with('-rpath') || part == '-pthread' {
			continue
		}
		return none
	}
	if !has_define {
		return none
	}
	return defines
}

fn (mut g Parser) skip_attribute() !bool {
	if g.tok != .attribute {
		return true
	}
	g.next()
	mut depth := 1
	mut has_condition := false
	mut condition_value := true
	mut at_item_start := true
	for depth > 0 {
		if g.tok == .eof {
			return g.unsupported('unfinished attribute')
		}
		if depth == 1 && at_item_start && g.tok == .key_if {
			if has_condition {
				return g.unsupported('multiple conditional attributes')
			}
			has_condition = true
			g.next()
			condition_value = g.parse_comptime_or()!
			if g.tok !in [.semicolon, .rsbr] {
				return g.unsupported('conditional attribute expression near `${g.token_source()}`')
			}
			continue
		}
		if depth == 1 && g.tok == .name && g.lit == 'direct_array_access' {
			g.pending_direct_array_access = true
		}
		if depth == 1 && at_item_start && g.tok == .key_unsafe {
			// `@[unsafe]` marks the whole function body as an unsafe region.
			g.next_declaration_is_unsafe = true
		}
		if depth == 1 && g.tok == .semicolon {
			at_item_start = true
		} else if depth == 1 {
			at_item_start = false
		}
		if g.tok == .lsbr {
			depth++
		} else if g.tok == .rsbr {
			depth--
		}
		g.next()
	}
	return if has_condition { condition_value } else { true }
}

fn (mut g Parser) skip_top_level_declaration() ! {
	body_declaration := g.tok in [.key_struct, .key_union, .key_enum, .key_interface]
	type_declaration := g.tok == .key_type
	mut brace_depth := 0
	mut paren_depth := 0
	mut bracket_depth := 0
	for g.tok != .eof {
		if g.tok == .lcbr {
			brace_depth++
		} else if g.tok == .rcbr {
			if brace_depth == 0 {
				return
			}
			brace_depth--
			g.next()
			if body_declaration && brace_depth == 0 {
				g.skip_semicolons()
				return
			}
			continue
		} else if g.tok == .lpar {
			paren_depth++
		} else if g.tok == .rpar && paren_depth > 0 {
			paren_depth--
		} else if g.tok == .lsbr {
			bracket_depth++
		} else if g.tok == .rsbr && bracket_depth > 0 {
			bracket_depth--
		} else if g.tok == .semicolon && brace_depth == 0 && paren_depth == 0 && bracket_depth == 0 {
			g.next()
			if type_declaration && g.tok == .pipe {
				continue
			}
			return
		}
		g.next()
	}
}

fn (mut g Parser) next() {
	g.tok = g.s.scan()
	g.lit = g.s.lit
}

fn (mut g Parser) temporary_name(kind string) string {
	name := '__v_fastc_${kind}_${g.temp_id}'
	g.temp_id++
	return name
}

fn (g &Parser) temporary_namespace(kind string) string {
	mut index := 0
	for {
		namespace := '__v_fastc_${kind}' + if index == 0 { '' } else { '_${index}' }
		prefix := namespace + '_'
		mut collision := false
		for local_name in g.locals.keys() {
			if fastc_c_identifier(local_name).starts_with(prefix) {
				collision = true
				break
			}
		}
		if !collision {
			// Every candidate prefix starts with `__v_fastc_`, so only the
			// program's few `__v_fastc_`-prefixed function and global C names
			// can collide (see fastc_reserved_temporary_c_names); rescanning
			// every function key here made each temporary O(program).
			for c_name in g.fastc_prefixed_c_names {
				if c_name.starts_with(prefix) {
					collision = true
					break
				}
			}
		}
		if !collision {
			return namespace
		}
		index++
	}
	return ''
}

// fastc_reserved_temporary_c_names collects the generated function and global
// C names that begin with the `__v_fastc_` temporary namespace prefix. Only
// these can collide with a temporary_namespace candidate.
fn fastc_reserved_temporary_c_names(functions map[string]FastcFunctionSignature, globals map[string]string) []string {
	mut names := []string{}
	for function_key in functions.keys() {
		// Only an unqualified key can collide with a C keyword or libc name and
		// so be renamed into the `__v_fastc_` namespace: a module-qualified key
		// keeps its `module__name` spelling, and `C.` keys are emitted verbatim.
		if function_key.contains('.') {
			continue
		}
		function_c_name := fastc_c_function_name_for_key(function_key)
		if function_c_name.starts_with('__v_fastc_') {
			names << function_c_name
		}
	}
	for c_name in globals.values() {
		if c_name.starts_with('__v_fastc_') {
			names << c_name
		}
	}
	return names
}

fn (mut g Parser) skip_semicolons() {
	for g.tok == .semicolon {
		g.next()
	}
}

fn (g &Parser) unsupported(feature string) IError {
	return error('fastc parser does not support ${feature} at byte ${g.s.pos + g.source_offset} in ${g.path}')
}

fn (mut g Parser) expect(expected token.Token) ! {
	if g.tok != expected {
		return g.unsupported('`${expected.str()}` after `${g.token_source()}`')
	}
	g.next()
}

fn (mut g Parser) parse_module() ! {
	g.next()
	if g.tok != .name {
		return g.unsupported('module declaration')
	}
	if g.lit != g.module_name.all_after_last('.') {
		return g.unsupported('module `${g.lit}` in `${g.module_name}` source')
	}
	g.next()
	g.skip_semicolons()
}

fn (mut g Parser) skip_import() ! {
	g.next()
	if g.tok == .lpar {
		mut depth := 1
		g.next()
		for depth > 0 {
			if g.tok == .eof {
				return g.unsupported('unfinished import group')
			}
			if g.tok == .lpar {
				depth++
			} else if g.tok == .rpar {
				depth--
			}
			g.next()
		}
		g.skip_semicolons()
		return
	}
	mut selective_depth := 0
	for g.tok != .eof {
		if selective_depth == 0 && g.tok == .semicolon {
			g.next()
			return
		}
		if g.tok == .lcbr {
			selective_depth++
		} else if g.tok == .rcbr {
			if selective_depth == 0 {
				return
			}
			selective_depth--
		}
		g.next()
	}
}

fn (mut g Parser) parse_function(enabled bool) ! {
	g.last_function_c_name = ''
	g.type_memo.clear()
	g.locals = map[string]FastcLocal{}
	g.temp_id = 0
	g.direct_array_access = g.pending_direct_array_access
	g.pending_direct_array_access = false
	g.next()
	mut receiver_type := ''
	mut receiver_key := ''
	mut receiver_name := ''
	mut receiver_is_mut := false
	mut params := []string{}
	mut is_generic := false
	mut type_params := []string{}
	if g.tok == .lpar {
		// Detect a generic method receiver (`(x Foo[T])`): a `[` inside the receiver
		// clause names type parameters. Such a body is generated with `T` as a
		// `voidptr` placeholder, but if that lowering fails it is stubbed (below).
		mut receiver_look := g.s
		mut receiver_depth := 1
		mut in_type_args := false
		for receiver_depth > 0 {
			receiver_tok := receiver_look.scan()
			if receiver_tok == .eof {
				break
			}
			if receiver_tok == .lpar {
				receiver_depth++
			} else if receiver_tok == .rpar {
				receiver_depth--
			} else if receiver_tok == .lsbr && receiver_depth == 1 {
				is_generic = true
				in_type_args = true
			} else if receiver_tok == .rsbr && receiver_depth == 1 {
				in_type_args = false
			} else if receiver_tok == .name && in_type_args {
				type_params << receiver_look.lit
			}
		}
		g.next()
		if g.tok == .key_mut {
			receiver_is_mut = true
			g.next()
		}
		if g.tok != .name {
			return g.unsupported('method receiver')
		}
		receiver_name = g.lit
		g.next()
		if g.tok == .name && g.lit != 'C' {
			receiver_key = fastc_type_key(g.module_name, g.lit)
		} else if g.tok == .key_none {
			receiver_key = 'none'
		}
		receiver_type = g.parse_type()!
		if receiver_key == '' {
			receiver_key = g.semantic_type_key(receiver_type)
		}
		g.expect(.rpar)!
		receiver_is_reference := receiver_is_mut && !receiver_type.ends_with('*')
		receiver_parameter_type := if receiver_is_reference {
			receiver_type + '*'
		} else {
			receiver_type
		}
		params << '${fastc_output_c_type(receiver_parameter_type)} ${fastc_c_identifier(receiver_name)}'
		g.type_memo.clear()
		g.locals[receiver_name] = FastcLocal{
			is_mut: receiver_is_mut
			is_reference: receiver_is_reference
			typ: receiver_parameter_type
		}
	}
	if g.tok != .name && !(g.tok.is_overloadable() || g.tok.is_keyword()) {
		return g.unsupported('function declaration')
	}
	mut name := if g.tok == .name || g.tok.is_keyword() { g.lit } else { g.tok.str() }
	g.next()
	mut is_c_function := false
	mut is_static_method := false
	if receiver_type == '' && name == 'C' && g.tok == .dot {
		is_c_function = true
		g.next()
		if g.tok != .name && !g.tok.is_keyword() {
			return g.unsupported('C function declaration')
		}
		name = g.lit
		g.next()
	} else if receiver_type == '' && g.tok == .dot {
		type_key := fastc_type_key(g.module_name, name)
		if type_key !in g.declared_types {
			return g.unsupported('static method owner `${name}`')
		}
		receiver_type = fastc_c_declared_type_name(type_key)
		receiver_key = type_key
		is_static_method = true
		g.next()
		if g.tok != .name && !g.tok.is_keyword() {
			return g.unsupported('static method declaration')
		}
		name = g.lit
		g.next()
	}
	if g.tok == .lsbr {
		// Type parameters on a free function (`fn f[T](...)`): also generic. Emitted
		// with each `T` as a `voidptr` placeholder; `typeof(T).name`/`$if T is`
		// resolve against that. If the placeholder lowering fails the body is stubbed.
		is_generic = true
		g.next()
		for g.tok != .rsbr && g.tok != .eof {
			if g.tok == .name {
				type_params << g.lit
			}
			g.next()
		}
		g.expect(.rsbr)!
	}
	if is_c_function {
		g.skip_c_function_declaration()!
		return
	}
	g.expect(.lpar)!
	params << g.parse_parameters()!
	mut return_type := 'void'
	mut return_types := []string{}
	mut option_return_type := ''
	if g.tok != .lcbr && g.tok != .semicolon {
		if g.tok in [.not, .question] {
			g.next()
			return_type = 'Option'
			if g.tok in [.lcbr, .semicolon] {
				option_return_type = 'void'
			} else if g.tok == .lpar {
				return_types = g.parse_multi_return_types()!
				option_return_type = 'MultiReturn'
			} else {
				option_return_type = g.parse_type()!
			}
		} else if g.tok == .lpar {
			return_types = g.parse_multi_return_types()!
			return_type = 'MultiReturn'
		} else {
			return_type = g.parse_type()!
		}
	}
	function_key := if receiver_type == '' {
		fastc_function_key(g.module_name, name)
	} else {
		'${receiver_key}.${name}'
	}
	is_main := !is_static_method && receiver_type == '' && g.module_name in ['', 'main'] && name == 'main'
	is_module_init := !is_static_method && receiver_type == '' && name == 'init'
	is_module_cleanup := !is_static_method && receiver_type == '' && name == 'cleanup'
	if is_main {
		if params.len > 0 {
			return g.unsupported('main function with parameters')
		}
	}
	if is_module_init && params.len > 0 {
		return g.unsupported('module `init` with parameters')
	}
	if is_module_cleanup && params.len > 0 {
		return g.unsupported('module `cleanup` with parameters')
	}
	if g.tok == .semicolon {
		g.next()
		return
	}
	is_fastc_source := name.starts_with('fastc_') || g.path.ends_with('/fastc/fastc.v') || g.module_name.ends_with('fastc')
	if g.selfhost && name != 'fastc_collect_referenced_function_names' && !is_fastc_source && name !in [
		'main',
		'init',
		'cleanup',
	] && name !in g.used_function_names && name.len > 0 && (name[0].is_letter() || name[0] == `_`) && !g.in_mono_drain {
		g.skip_balanced(.lcbr, .rcbr)!
		return
	}
	if signature := g.functions[function_key] {
		if signature.path != g.path && name != 'fastc_collect_referenced_function_names' && !is_fastc_source {
			g.skip_balanced(.lcbr, .rcbr)!
			return
		}
	}
	if g.selfhost && !is_fastc_source && receiver_type != '' && g.method_uses_undefined_receiver() {
		// A method reaching an undefined identifier as a method-call receiver is broken
		// dead code, kept only because its name collides with a genuinely-used method on
		// another type (name-grouped reachability). The mainline compiler drops it via
		// `-skip-unused`; do the same. A live reference would fail C validation instead.
		g.skip_balanced(.lcbr, .rcbr)!
		return
	}
	c_name := if receiver_type == '' {
		fastc_c_function_name(g.module_name, name)
	} else {
		fastc_method_c_name(g.module_name, fastc_c_declared_type_name(receiver_key), name)
	}
	g.last_function_c_name = c_name
	c_return_type := if is_main { 'int' } else { fastc_output_c_type(return_type) }
	c_params := if is_main && g.selfhost {
		'int argc, char **argv'
	} else if params.len == 0 {
		'void'
	} else {
		params.join(', ')
	}
	g.protos.writeln('${c_return_type} ${c_name}(${c_params});')
	if g.selfhost && g.open_block_contains_select_statement() {
		// `select { … }` (channel multiplexing) has no C lowering. Emit a trivial stub rather
		// than skipping the function, so a genuinely referenced select-using function (the
		// parallel `Pool.run`) still links. FastC channels are erased, so the select path is
		// dead and a caller reaching this falls back to its synchronous branch.
		g.write_line('${c_return_type} ${c_name}(${c_params}) {')
		g.indent++
		if return_type != 'void' {
			g.write_line('return (${return_type}){0};')
		}
		g.indent--
		g.write_line('}')
		g.out.writeln('')
		g.skip_balanced(.lcbr, .rcbr)!
		return
	}
	if !enabled {
		g.write_line('${c_return_type} ${c_name}(${c_params}) {')
		g.indent++
		if return_type != 'void' {
			g.write_line('return (${fastc_output_c_type(return_type)}){0};')
		}
		g.indent--
		g.write_line('}')
		g.out.writeln('')
		g.skip_balanced(.lcbr, .rcbr)!
		return
	}
	g.expect(.lcbr)!
	if is_main {
		g.has_main = true
	}
	g.write_line('${c_return_type} ${c_name}(${c_params}) {')
	g.indent++
	if is_main {
		g.write_line('setvbuf(stdout, NULL, _IONBF, 0);')
		if g.selfhost {
			g.write_line('g_main_argc = argc;')
			g.write_line('g_main_argv = argv;')
		}
		if g.has_startup_inits {
			g.write_line('v_fastc_init_globals();')
		}
		if g.has_cleanup_hooks {
			g.write_line('atexit(v_fastc_cleanup_modules);')
		}
	}
	previous_in_main := g.in_main
	previous_return_type := g.return_type
	previous_return_types := g.return_types.clone()
	previous_option_return_type := g.option_return_type
	previous_function := g.current_function
	previous_receiver := g.current_receiver
	previous_method_is_static := g.current_method_is_static
	previous_deferred_lines := g.deferred_lines.clone()
	previous_deferred_block_starts := g.deferred_block_starts.clone()
	previous_function_defer_blocks := g.function_defer_blocks.clone()
	previous_function_defer_declarations := g.function_defer_declarations.clone()
	previous_loop_defer_block_starts := g.loop_defer_block_starts.clone()
	previous_loop_has_breaks := g.loop_has_breaks.clone()
	previous_statement_reachable := g.statement_reachable
	previous_unsafe_depth := g.unsafe_depth
	// Defer-capture state must never span function boundaries; a render path that reads a
	// block value speculatively (and swallows an error) can otherwise leave
	// `capturing_defer` set, which would misroute a later function's statements.
	previous_capturing_defer := g.capturing_defer
	previous_captured_defer_lines := g.captured_defer_lines.clone()
	previous_defer_depth := g.defer_depth
	if g.next_declaration_is_unsafe {
		// `@[unsafe]` makes the entire body an unsafe region (`nil`, pointer
		// arithmetic, ...), matching V's semantics for the attribute.
		g.unsafe_depth = 1
	}
	g.in_main = is_main
	g.return_type = return_type
	g.return_types = return_types.clone()
	g.option_return_type = option_return_type
	g.current_function = name
	g.current_receiver = receiver_key
	g.current_method_is_static = is_static_method
	g.deferred_lines.clear()
	g.deferred_block_starts.clear()
	g.function_defer_blocks.clear()
	g.function_defer_declarations.clear()
	g.loop_defer_block_starts.clear()
	g.loop_has_breaks.clear()
	g.capturing_defer = false
	g.captured_defer_lines.clear()
	g.defer_depth = 0
	g.statement_reachable = true
	function_body_start := g.out.len
	mut terminates := false
	if is_generic {
		// Locate the end of the body (the matching `}`) so a failed placeholder
		// lowering can skip past it, then try to generate the body.
		out_checkpoint := g.out.len
		saved_indent := g.indent
		mut body_end := g.s
		mut body_depth := 1
		// A generic instantiation `Name[T]` on an unbound type parameter (as in orm's
		// `new_query[T]` → `struct_meta[T]()`/`QueryBuilder[T]{}`) leaks `T` into the C
		// without raising an error, so detect a type-parameter token right after `[`
		// and force the body to be stubbed.
		mut force_stub := false
		mut prev_was_lsbr := false
		mut prev_was_type_param := false
		if g.tok == .lcbr {
			body_depth++
		} else if g.tok == .rcbr {
			body_depth--
		}
		for body_depth > 0 {
			body_tok := body_end.scan()
			if body_tok == .eof {
				break
			}
			if body_tok == .lcbr {
				body_depth++
			} else if body_tok == .rcbr {
				body_depth--
			}
			if prev_was_lsbr && body_tok == .name && body_end.lit in type_params {
				force_stub = true
			}
			// A struct literal on an unbound type parameter (`T{...}`, as in orm's
			// `map_row`'s `mut instance := T{}`): the placeholder lowering can miscount
			// the literal's `}` and end the body early, leaking a stray statement to the
			// top level. Type-parameter reflection (`T.name`, `T.fields`, ...) and a cast
			// or construction on the type parameter (`T(x)`, as in sync.pool's
			// `*(&T(items[idx]))`) likewise have no valid C spelling until this function is
			// monomorphized. Force the whole placeholder body to stub in every case.
			if prev_was_type_param && body_tok in [.lcbr, .dot, .lpar] {
				force_stub = true
			}
			prev_was_lsbr = body_tok == .lsbr
			prev_was_type_param = body_tok == .name && body_end.lit in type_params
		}
		previous_placeholder := g.in_generic_placeholder
		g.in_generic_placeholder = true
		terminates = g.parse_generic_body_or_stub(return_type, out_checkpoint, saved_indent, body_end, force_stub)
		g.in_generic_placeholder = previous_placeholder
	} else {
		terminates = g.parse_block_body()!
	}
	if !terminates {
		g.write_function_deferred_blocks()
	}
	if return_type != 'void' && !terminates {
		if !g.selfhost {
			return g.unsupported('non-void function `${name}` that can fall through')
		}
		// Self-host input was already accepted by the bootstrap compiler. Keep C's
		// control-flow rules satisfied when the streaming parser cannot prove that
		// every nested source branch terminates.
		g.write_line('return (${fastc_output_c_type(return_type)}){0};')
	}
	if is_main {
		g.write_line('return 0;')
	}
	if g.function_defer_declarations.len > 0 {
		function_body := g.out.cut_to(function_body_start)
		for declaration in g.function_defer_declarations {
			g.write_line(declaration)
		}
		g.out.write_string(function_body)
	}
	g.in_main = previous_in_main
	g.return_type = previous_return_type
	g.return_types = previous_return_types.clone()
	g.option_return_type = previous_option_return_type
	g.current_function = previous_function
	g.current_receiver = previous_receiver
	g.current_method_is_static = previous_method_is_static
	g.deferred_lines = previous_deferred_lines.clone()
	g.deferred_block_starts = previous_deferred_block_starts.clone()
	g.function_defer_blocks = previous_function_defer_blocks.clone()
	g.function_defer_declarations = previous_function_defer_declarations.clone()
	g.loop_defer_block_starts = previous_loop_defer_block_starts.clone()
	g.loop_has_breaks = previous_loop_has_breaks.clone()
	g.capturing_defer = previous_capturing_defer
	g.captured_defer_lines = previous_captured_defer_lines.clone()
	g.defer_depth = previous_defer_depth
	g.statement_reachable = previous_statement_reachable
	g.unsafe_depth = previous_unsafe_depth
	g.indent--
	g.write_line('}')
	g.out.writeln('')
}

// parse_generic_body_or_stub emits a generic (un-monomorphized) function body with
// each `T` treated as a `voidptr` placeholder. Most generic bodies lower fine that
// way (e.g. `typeof(T).name` reflection). When one cannot (e.g. orm's
// `QueryBuilder[T]`, which interpolates a `T`-typed struct), the partial output is
// discarded and a trivial stub is emitted instead — a generic body is never called
// un-monomorphized, so a stub is safe and keeps the surrounding module compiling.
// `body_end` is a scanner positioned just past the body's closing `}`, used to
// resume after a discarded body. Returns whether the emitted body terminates.
fn (mut g Parser) parse_generic_body_or_stub(return_type string, out_checkpoint int, saved_indent int, body_end scanner.Scanner, force_stub bool) bool {
	if force_stub {
		g.emit_generic_body_stub(out_checkpoint, saved_indent, body_end, return_type)
		return true
	}
	local_scope_start := g.local_scope_changes.len
	local_scope_depth := g.local_scope_depth
	statement_reachable := g.statement_reachable
	terminates := g.parse_block_body() or {
		g.restore_local_scope(local_scope_start)
		g.local_scope_depth = local_scope_depth
		g.statement_reachable = statement_reachable
		g.emit_generic_body_stub(out_checkpoint, saved_indent, body_end, return_type)
		return true
	}
	return terminates
}

// emit_generic_body_stub discards any partially-generated body, repositions the
// scanner just past the body, and emits a trivial `return (T){0};` stub.
fn (mut g Parser) emit_generic_body_stub(out_checkpoint int, saved_indent int, body_end scanner.Scanner, return_type string) {
	discard := g.out.len - out_checkpoint
	if discard > 0 {
		g.out.go_back(discard)
	}
	g.indent = saved_indent
	g.capturing_defer = false
	g.function_defer_blocks.clear()
	g.function_defer_declarations.clear()
	g.s = body_end
	g.next()
	if return_type != 'void' {
		g.write_line('return (${fastc_output_c_type(return_type)}){0};')
	}
}

// record_function_span notes the C definition the last parse_function wrote
// to `out` (and its prototype in `protos`) for the reachability prune.
fn (mut g Parser) record_function_span(out_start int, proto_start int) {
	if g.last_function_c_name == '' || g.in_mono_drain {
		return
	}
	g.function_ids << g.function_id_table[g.last_function_c_name] or { -1 }
	g.function_spans << out_start
	g.function_spans << g.out.len
	g.proto_spans << proto_start
	g.proto_spans << g.protos.len
}

fn fastc_method_c_name(module_name string, receiver_type string, name string) string {
	module_prefix := if module_name in ['', 'main'] {
		''
	} else {
		module_name.replace('.', '__') + '__'
	}
	// A composite receiver (`[]Any`/`map[string]Any` → `Array_json2__Any`/`Map_..._Any`)
	// must keep its `Array_`/`Map_` prefix, otherwise `all_after_last('__')` collapses it to
	// the element type (`Any`) and its method C name collides with the element type's own
	// method (`json2__Any_str` defined for both `Any` and `[]Any` → C redefinition).
	mut receiver_base := receiver_type.trim_right('*')
	mut composite_prefix := ''
	for {
		if receiver_base.starts_with('Array_') {
			composite_prefix += 'Array_'
			receiver_base = receiver_base['Array_'.len..]
		} else if receiver_base.starts_with('Map_') {
			composite_prefix += 'Map_'
			receiver_base = receiver_base['Map_'.len..]
		} else {
			break
		}
	}
	receiver := composite_prefix + receiver_base.all_after_last('__')
	method := match name {
		'+' { 'plus' }
		'-' { 'minus' }
		'*' { 'mul' }
		'/' { 'div' }
		'%' { 'mod' }
		'&' { 'and' }
		'|' { 'or' }
		'^' { 'xor' }
		'<<' { 'left_shift' }
		'>>' { 'right_shift' }
		'[]' { 'op_index' }
		'[]=' { 'op_index_set' }
		'==' { 'eq' }
		'!=' { 'ne' }
		'<' { 'lt' }
		'<=' { 'le' }
		'>' { 'gt' }
		'>=' { 'ge' }
		else { naming.sanitize(name) }
	}
	return '${module_prefix}${receiver}_${method}'
}

fn (mut g Parser) skip_balanced(open token.Token, close token.Token) ! {
	if g.tok != open {
		return g.unsupported('`${open.str()}` group')
	}
	mut depth := 0
	for {
		if g.tok == open {
			depth++
		} else if g.tok == close {
			depth--
			g.next()
			if depth == 0 {
				return
			}
			continue
		} else if g.tok == .eof {
			return g.unsupported('unfinished `${open.str()}` group')
		}
		g.next()
	}
}

fn (mut g Parser) skip_c_function_declaration() ! {
	mut parens := 0
	for g.tok != .eof {
		if g.tok == .lpar {
			parens++
		} else if g.tok == .rpar {
			parens--
		} else if g.tok == .semicolon && parens == 0 {
			g.next()
			return
		} else if g.tok == .lcbr && parens == 0 {
			g.skip_balanced(.lcbr, .rcbr)!
			return
		}
		g.next()
	}
}

fn (mut g Parser) parse_script() ! {
	g.type_memo.clear()
	g.locals = map[string]FastcLocal{}
	g.has_main = true
	g.protos.writeln('int main(void);')
	g.write_line('int main(void) {')
	g.indent++
	g.write_line('setvbuf(stdout, NULL, _IONBF, 0);')
	if g.has_startup_inits {
		g.write_line('v_fastc_init_globals();')
	}
	if g.has_cleanup_hooks {
		g.write_line('atexit(v_fastc_cleanup_modules);')
	}
	g.in_main = true
	g.skip_semicolons()
	for g.tok != .eof {
		if g.tok in [.key_module, .key_pub, .key_static, .key_fn] {
			return g.unsupported('declaration after top-level statements')
		}
		_ = g.parse_statement()!
		g.skip_semicolons()
	}
	g.write_line('return 0;')
	g.indent--
	g.write_line('}')
	g.out.writeln('')
}

fn (mut g Parser) parse_parameters() ![]string {
	mut params := []string{}
	mut blank_index := 0
	g.skip_semicolons()
	for g.tok != .rpar {
		mut is_mut := false
		if g.tok == .key_mut || (g.tok == .key_shared && !fastc_shared_parameter_is_name(g.s, g.path, g.module_name, g.imports, g.declared_types, g.prefs.building_v)) {
			is_mut = true
			g.next()
		}
		if g.tok !in [.name, .key_shared] {
			return g.unsupported('function parameters')
		}
		name := g.lit
		g.next()
		mut names := [name]
		for g.tok == .comma {
			g.next()
			if !fastc_token_can_be_decl_name(g.tok) {
				return g.unsupported('grouped parameter names')
			}
			names << g.lit
			g.next()
		}
		mut type_name := g.parse_type()!
		option_value_type := g.pending_option_value_type
		is_fn_pointer := g.pending_fn_pointer
		fn_return_type := g.pending_fn_return_type
		fn_option_value_type := g.pending_fn_option_value_type
		is_reference := is_mut && !type_name.ends_with('*')
		if is_reference {
			type_name += '*'
		}
		for parameter_name in names {
			// Two blank `_` parameters (`fn f(_ A, _ B)`) would both render as the C name
			// `_`, which C rejects as a redeclaration. `_` is never referenced in the body,
			// so give each a distinct throwaway name.
			c_name := if parameter_name == '_' {
				blank_index++
				'_fastc_unused_${blank_index}'
			} else {
				fastc_c_identifier(parameter_name)
			}
			if is_fn_pointer {
				// Declare a real function pointer with unspecified args so `f(x)`
				// compiles as a direct call; the return C type is recovered above.
				params << '${fastc_output_c_type(fn_return_type)} (*${c_name})()'
				g.type_memo.clear()
				g.locals[parameter_name] = FastcLocal{
					is_mut: is_mut
					typ: type_name
					fn_return_type: fn_return_type
					fn_option_value_type: fn_option_value_type
				}
			} else {
				params << '${fastc_output_c_type(type_name)} ${c_name}'
				g.type_memo.clear()
				g.locals[parameter_name] = FastcLocal{
					is_mut: is_mut
					is_reference: is_reference
					typ: type_name
					option_value_type: option_value_type
				}
			}
		}
		if g.tok == .comma {
			g.next()
			g.skip_semicolons()
			continue
		}
		if g.tok != .rpar {
			return g.unsupported('function parameter separator')
		}
	}
	g.next()
	return params
}

fn (mut g Parser) parse_type() !string {
	first_lit := g.lit
	g.pending_option_value_type = ''
	g.pending_fn_pointer = false
	g.pending_fn_return_type = ''
	g.pending_fn_option_value_type = ''
	if g.tok == .question {
		// Peek the wrapped value type of an option type `?T` before the main scan
		// erases it to `Option`, so a caller can record it on the declared local.
		mut look := g.s
		value_tok := look.scan()
		if value_tok !in [.lcbr, .eof] {
			g.pending_option_value_type = g.peek_option_value_type(mut look, value_tok)
		}
	}
	if g.tok == .key_fn {
		// Peek a function type's return (`fn (params) R`) before the main scan erases
		// it to `voidptr`, so a fn-typed parameter can be declared as a real function
		// pointer and its call results inferred. Parameters are skipped (the pointer is
		// declared with unspecified args), so only the return type is recovered.
		g.peek_fn_pointer_signature()
	}
	type_name, next_token := fastc_scan_type(mut g.s, g.tok, g.path, g.module_name, g.imports, g.declared_types, g.selfhost) or { return g.unsupported(err.msg()) }
	g.tok = next_token
	g.lit = g.s.lit
	if !g.selfhost && (first_lit in ['charptr', 'rune'] || type_name == 'char*') {
		return g.unsupported('type `${first_lit}`')
	}
	return type_name
}

// peek_option_value_type scans an option type's wrapped value type from a lookahead
// scanner, returning '' if it cannot be determined.
fn (g &Parser) peek_option_value_type(mut look scanner.Scanner, first token.Token) string {
	value_type, _ := fastc_scan_type(mut look, first, g.path, g.module_name, g.imports, g.declared_types, g.selfhost) or { return '' }
	return value_type
}

// peek_fn_pointer_signature records a function type's C return type (and, for an
// `!`/`?R` return, its wrapped value type) into the `pending_fn_*` fields. Called with
// `g.tok == .key_fn`; it works on a scanner copy and does not consume the real stream.
fn (mut g Parser) peek_fn_pointer_signature() {
	mut look := g.s
	mut tok := look.scan()
	if tok != .lpar {
		return
	}
	mut depth := 1
	for depth > 0 {
		tok = look.scan()
		if tok == .eof {
			return
		} else if tok == .lpar {
			depth++
		} else if tok == .rpar {
			depth--
		}
	}
	tok = look.scan()
	mut return_type := 'void'
	if tok in [.not, .question] {
		return_type = 'Option'
		value_tok := look.scan()
		if value_tok !in [.lcbr, .semicolon, .comma, .rpar, .eof] {
			g.pending_fn_option_value_type = g.peek_option_value_type(mut look, value_tok)
		}
	} else if tok !in [.lcbr, .semicolon, .comma, .rpar, .eof] {
		scanned, _ := fastc_scan_type(mut look, tok, g.path, g.module_name, g.imports, g.declared_types, g.selfhost) or { return }
		return_type = scanned
	}
	g.pending_fn_pointer = true
	g.pending_fn_return_type = return_type
}

fn (mut g Parser) parse_multi_return_types() ![]string {
	g.expect(.lpar)!
	mut types := []string{}
	for g.tok != .rpar {
		g.skip_semicolons()
		if g.tok == .rpar {
			break
		}
		types << g.parse_type()!
		if g.tok == .comma {
			g.next()
		} else if g.tok != .rpar {
			return g.unsupported('multi-return type separator')
		}
	}
	g.expect(.rpar)!
	return types
}

// Primitive scalar spellings that may appear as sum-type variants and therefore
// need a stable `__v_typeid_<name>`. `string` is intentionally excluded: in the
// self-host runtime it is a declared struct and already gets a type id. The C
// spelling equals the V spelling for each of these, so it doubles as the smart-
// cast type in a `match` branch.
const fastc_boxed_primitive_types = ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64',
	'f32', 'f64', 'bool', 'rune', 'isize', 'usize', 'char', 'voidptr']

// fastc_output_c_type maps a semantic FastC type string to the C spelling that
// is physically written into a declaration or cast. Only the platform-width
// `int` differs from its semantic key: it is emitted as `i64`/`i32` per the
// target, while staying `int` for method-name mangling and type inference (so
// `int` and `i64` keep distinct methods). Pointer suffixes are preserved.
fn fastc_output_c_type(t string) string {
	base := t.trim_right('*')
	if base == 'int' {
		return fastc_platform_int_c_type + t[base.len..]
	}
	return t
}

fn fastc_primitive_c_type(raw_type string) ?string {
	return match raw_type {
		'bool' { 'bool' }
		'byte' { 'byte' }
		'char' { 'char' }
		'f32' { 'f32' }
		'f64' { 'f64' }
		'float_literal' { 'f64' }
		'i8' { 'i8' }
		'i16' { 'i16' }
		'i32' { 'i32' }
		'i64' { 'i64' }
		'int' { 'int' }
		'int_literal' { 'i64' }
		'isize' { 'isize' }
		'rune' { 'rune' }
		'string' { 'string' }
		'u8' { 'u8' }
		'u16' { 'u16' }
		'u32' { 'u32' }
		'u64' { 'u64' }
		'uint' { 'unsigned int' }
		'usize' { 'usize' }
		'voidptr' { 'voidptr' }
		'byteptr' { 'byteptr' }
		'charptr' { 'charptr' }
		'chan' { 'chan' }
		'array' { 'array' }
		'map' { 'map' }
		'Option' { 'Option' }
		'any' { 'voidptr' }
		else { none }
	}
}

fn fastc_expression_tokens_contain(tokens []FastcExpressionToken, wanted token.Token) bool {
	for item in tokens {
		if item.tok == wanted {
			return true
		}
	}
	return false
}

fn fastc_expression_tokens_contain_boolean_operator(tokens []FastcExpressionToken) bool {
	for item in tokens {
		if item.tok in [.eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .not] {
			return true
		}
	}
	return false
}

fn fastc_expression_tokens_contain_assignment_or_mutation(tokens []FastcExpressionToken) bool {
	for item in tokens {
		if item.tok.is_assignment() || item.tok in [.inc, .dec] {
			return true
		}
	}
	return false
}

fn fastc_expression_tokens_contain_statement_method(tokens []FastcExpressionToken) bool {
	for item in tokens {
		if item.tok == .name && item.lit in ['set', 'clear'] {
			return true
		}
	}
	return false
}

fn fastc_expression_tokens_debug(tokens []FastcExpressionToken) string {
	mut details := []string{cap: tokens.len}
	for item in tokens {
		details << '${item.tok.str()}:${item.lit}'
	}
	return details.join(',')
}

fn fastc_all_true(values []bool) bool {
	for value in values {
		if !value {
			return false
		}
	}
	return true
}
