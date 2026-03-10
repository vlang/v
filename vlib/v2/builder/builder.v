// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v2.ast
import v2.abi
import v2.eval
import v2.gen.arm64
import v2.gen.c
import v2.gen.cleanc
import v2.gen.v as gen_v
import v2.gen.x64
import v2.insel
import v2.markused
import v2.parser
import v2.mir
import v2.pref
import v2.ssa
import v2.ssa.optimize as ssa_optimize
import v2.token
import v2.transformer
import v2.types
import time

const staged_c_file = '/tmp/v2_codegen.tmp.c'
const staged_main_obj_file = '/tmp/v2_codegen.tmp.main.o'

struct Builder {
	pref &pref.Preferences
mut:
	files               []ast.File
	user_files          []string // original user-provided files (for output name)
	file_set            &token.FileSet     = token.FileSet.new()
	env                 &types.Environment = unsafe { nil } // Type checker environment
	parsed_full_files_n int
	parsed_vh_files_n   int
	entry_v_lines_n     int
	parsed_v_lines_n    int
	parsed_full_files   []string
	parsed_vh_files     []string
	used_fn_keys        map[string]bool
}

pub fn new_builder(prefs &pref.Preferences) &Builder {
	unsafe {
		return &Builder{
			pref:         prefs
			used_fn_keys: map[string]bool{}
		}
	}
}

// exec_build_c_file returns the staging C file path for executable builds.
// A stable temp path avoids output-name-derived string construction in
// self-hosted arm64 binaries, where that path is currently unstable.
fn (b &Builder) exec_build_c_file(output_name string) string {
	if b.pref.keep_c {
		return output_name + '.c'
	}
	return staged_c_file
}

fn sanitize_staged_c_source(c_source string) string {
	mut source := c_source
	source = source.replace('struct IError {\n\tvoid* _object;\n\tint _type_id;\n\tstring (*type_name)(void*);\n\tvoid* msg;\n\tvoid* code;\n};',
		'struct IError {\n\tvoid* _object;\n\tint _type_id;\n\tstring (*type_name)(void*);\n\tstring (*msg)(void*);\n\tint (*code)(void*);\n};')
	source = source.replace('((u8)(ary_clone.data))', '((u8*)(ary_clone.data))')
	source = source.replace('((u8)(a.data))', '((u8*)(a.data))')
	source = source.replace('((u8)(arr.data))', '((u8*)(arr.data))')
	source = source.replace('((u8)(dst->data))', '((u8*)(dst->data))')
	source = source.replace('((u8)(const_s))', '((u8*)(const_s))')
	source = source.replace('((u8)(m->metas))', '((u8*)(m->metas))')
	source = source.replace('((u8)(pvalue))', '((u8*)(pvalue))')
	source = source.replace('(_idx_s < ((a)).len)', '(_idx_s < (*(a)).len)')
	source = source.replace('&((string*)((a)).data)', '&((string*)(*(a)).data)')
	source = source.replace('return err.msg(err._object);', 'return ((string (*)(void*))err.msg)(err._object);')
	source = ensure_result_struct_decl(source, '_result_int', 'int')
	source = ensure_result_struct_decl(source, '_result_rune', 'rune')
	source = replace_generated_c_fn(source, 'string u64_to_hex(u64 nn, u8 len)', sanitized_u64_to_hex_fn())
	source = replace_generated_c_fn(source, 'string u64_to_hex_no_leading_zeros(u64 nn, u8 len)',
		sanitized_u64_to_hex_no_leading_zeros_fn())
	source = replace_generated_c_fn(source, 'string u8__str_escaped(u8 b)', sanitized_u8_str_escaped_fn())
	source = replace_generated_c_fn(source, 'int int_min(int a, int b)', sanitized_int_min_fn())
	source = replace_generated_c_fn(source, 'int int_max(int a, int b)', sanitized_int_max_fn())
	source = replace_generated_c_fn(source, 'int string__utf32_code(string _rune)', sanitized_string_utf32_code_fn())
	source = replace_generated_c_fn(source, '_result_rune Array_u8__utf8_to_utf32(Array_u8 _bytes)',
		sanitized_array_u8_utf8_to_utf32_fn())
	source = replace_generated_c_fn(source, 'rune impl_utf8_to_utf32(u8* _bytes, int _bytes_len)',
		sanitized_impl_utf8_to_utf32_fn())
	source = replace_generated_c_fn(source, 'int utf8_str_visible_length(string s)', sanitized_utf8_str_visible_length_fn())
	source = source.replace('array__write_rune(', 'strings__Builder__write_rune(')
	source = source.replace('array__write_u8(', 'strings__Builder__write_u8(')
	source = source.replace('array__write_string(', 'strings__Builder__write_string(')
	source = source.replace('array__write_ptr(', 'strings__Builder__write_ptr(')
	source = source.replace('array__write_repeated_rune(', 'strings__Builder__write_repeated_rune(')
	source = source.replace('array__spart(', 'strings__Builder__spart(')
	source = source.replace('array__cut_last(', 'strings__Builder__cut_last(')
	source = source.replace('array__str(sb)', 'strings__Builder__str(&sb)')
	source = source.replace('strings__Builder__write_rune(sb,', 'strings__Builder__write_rune(&sb,')
	source = source.replace("Array_u8_contains(res, '.')", "array__contains(res, &(u8[1]){'.'})")
	source = source.replace('(state ? strings__IndentState__normal)', '(state == strings__IndentState__normal)')
	source = source.replace('(state ? strings__IndentState__in_string)', '(state == strings__IndentState__in_string)')
	source = source.replace('(c ? \'"\')', '(c == \'"\')')
	source = source.replace("(c ? '\\'')", "(c == '\\'')")
	source = source.replace('(c ? param.block_start)', '(c == param.block_start)')
	source = source.replace('(c ? param.block_end)', '(c == param.block_end)')
	source = source.replace("(c ? ' ')", "(c == ' ')")
	source = source.replace("(c ? '\\t')", "(c == '\\t')")
	source = source.replace("(c ? '\\r')", "(c == '\\r')")
	source = source.replace("(c ? '\\n')", "(c == '\\n')")
	source = source.replace('return tos(((u8)(&((u8*)buf.data)[((int)(0))])), i);', 'return tos(&((u8*)buf.data)[((int)(0))], i);')
	source = ensure_string_eq_impl(source)
	return source
}

fn ensure_result_struct_decl(source string, struct_name string, elem_c_type string) string {
	if source.contains('struct ${struct_name} {') {
		return source
	}
	anchor := 'struct _result_string { bool is_error; IError err; u8 data[sizeof(string) > 1 ? sizeof(string) : 1]; };'
	replacement := anchor +
		'\nstruct ${struct_name} { bool is_error; IError err; u8 data[sizeof(${elem_c_type}) > 1 ? sizeof(${elem_c_type}) : 1]; };'
	return source.replace(anchor, replacement)
}

fn ensure_string_eq_impl(source string) string {
	if source.contains('bool string__eq(string a, string b) {') {
		return source
	}
	return source + '\n' +
		['bool string__eq(string a, string b) {', '\tif ((a.str == 0)) {', '\t\treturn ((b.str == 0) || (b.len == 0));', '\t}', '\tif ((a.len != b.len)) {', '\t\treturn false;', '\t}', '\treturn (vmemcmp(a.str, b.str, b.len) == 0);', '}'].join('\n') +
		'\n'
}

fn replace_generated_c_fn(source string, signature string, replacement string) string {
	needle := signature + ' {'
	start := source.index(needle) or { return source }
	body_start := start + needle.len - 1
	if body_start < 0 || body_start >= source.len || source[body_start] != `{` {
		return source
	}
	mut depth := 0
	mut body_end := -1
	for i := body_start; i < source.len; i++ {
		if source[i] == `{` {
			depth++
		} else if source[i] == `}` {
			depth--
			if depth == 0 {
				body_end = i
				break
			}
		}
	}
	if body_end == -1 {
		return source
	}
	return source[..start] + replacement + '\n' + source[body_end + 1..]
}

fn sanitized_u64_to_hex_fn() string {
	return [
		'string u64_to_hex(u64 nn, u8 len) {',
		'\tu64 n = nn;',
		'\tu8 buf[17] = {0};',
		'\tbuf[((int)(len))] = 0;',
		'\tfor (int i = (((int)(len)) - 1); (i >= 0); i--) {',
		'\t\tu8 d = ((u8)((n & 0xF)));',
		"\t\tbuf[i] = (d < 10) ? ((u8)(d + '0')) : ((u8)(d + 87));",
		'\t\tn = (n >> 4);',
		'\t}',
		'\treturn tos(memdup(&buf[0], (((int)(len)) + 1)), len);',
		'}',
	].join('\n')
}

fn sanitized_u64_to_hex_no_leading_zeros_fn() string {
	return [
		'string u64_to_hex_no_leading_zeros(u64 nn, u8 len) {',
		'\tu64 n = nn;',
		'\tu8 buf[17] = {0};',
		'\tbuf[((int)(len))] = 0;',
		'\tint i = 0;',
		'\tfor (i = (((int)(len)) - 1); (i >= 0); i--) {',
		'\t\tu8 d = ((u8)((n & 0xF)));',
		"\t\tbuf[i] = (d < 10) ? ((u8)(d + '0')) : ((u8)(d + 87));",
		'\t\tn = (n >> 4);',
		'\t\tif ((n == 0)) {',
		'\t\t\tbreak;',
		'\t\t}',
		'\t}',
		'\tint res_len = (((int)(len)) - i);',
		'\treturn tos(memdup(&buf[i], (res_len + 1)), res_len);',
		'}',
	].join('\n')
}

fn sanitized_u8_str_escaped_fn() string {
	return [
		'string u8__str_escaped(u8 b) {',
		'\tif ((b == 0)) {',
		'\t\treturn (string){.str = "`\\0`", .len = sizeof("`\\0`") - 1, .is_lit = 1};',
		'\t}',
		'\tif ((b == 7)) {',
		'\t\treturn (string){.str = "`\\a`", .len = sizeof("`\\a`") - 1, .is_lit = 1};',
		'\t}',
		'\tif ((b == 8)) {',
		'\t\treturn (string){.str = "`\\b`", .len = sizeof("`\\b`") - 1, .is_lit = 1};',
		'\t}',
		'\tif ((b == 9)) {',
		'\t\treturn (string){.str = "`\\t`", .len = sizeof("`\\t`") - 1, .is_lit = 1};',
		'\t}',
		'\tif ((b == 10)) {',
		'\t\treturn (string){.str = "`\\n`", .len = sizeof("`\\n`") - 1, .is_lit = 1};',
		'\t}',
		'\tif ((b == 11)) {',
		'\t\treturn (string){.str = "`\\v`", .len = sizeof("`\\v`") - 1, .is_lit = 1};',
		'\t}',
		'\tif ((b == 12)) {',
		'\t\treturn (string){.str = "`\\f`", .len = sizeof("`\\f`") - 1, .is_lit = 1};',
		'\t}',
		'\tif ((b == 13)) {',
		'\t\treturn (string){.str = "`\\r`", .len = sizeof("`\\r`") - 1, .is_lit = 1};',
		'\t}',
		'\tif ((b == 27)) {',
		'\t\treturn (string){.str = "`\\e`", .len = sizeof("`\\e`") - 1, .is_lit = 1};',
		'\t}',
		'\tif (((b >= 32) && (b <= 126))) {',
		'\t\treturn u8__ascii_str(b);',
		'\t}',
		'\tstring xx = u8__hex(b);',
		'\tstring yy = string__plus((string){.str = "0x", .len = sizeof("0x") - 1, .is_lit = 1}, xx);',
		'\tstring__free(&xx);',
		'\treturn yy;',
		'}',
	].join('\n')
}

fn sanitized_int_min_fn() string {
	return [
		'int int_min(int a, int b) {',
		'\treturn (a < b) ? a : b;',
		'}',
	].join('\n')
}

fn sanitized_int_max_fn() string {
	return [
		'int int_max(int a, int b) {',
		'\treturn (a > b) ? a : b;',
		'}',
	].join('\n')
}

fn sanitized_string_utf32_code_fn() string {
	return [
		'int string__utf32_code(string _rune) {',
		'\tif ((_rune.len > 4)) {',
		'\t\treturn 0;',
		'\t}',
		'\treturn ((int)(impl_utf8_to_utf32((u8*)(_rune.str), _rune.len)));',
		'}',
	].join('\n')
}

fn sanitized_array_u8_utf8_to_utf32_fn() string {
	return [
		'_result_rune Array_u8__utf8_to_utf32(Array_u8 _bytes) {',
		'\tif ((_bytes.len > 4)) {',
		'\t\treturn (_result_rune){ .is_error=true, .err=error((string){.str = "attempted to decode too many bytes, utf-8 is limited to four bytes maximum", .len = sizeof("attempted to decode too many bytes, utf-8 is limited to four bytes maximum") - 1, .is_lit = 1}) };',
		'\t}',
		'\treturn ({ _result_rune _res = (_result_rune){0}; rune _val = impl_utf8_to_utf32((u8*)(_bytes.data), _bytes.len); _result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; });',
		'}',
	].join('\n')
}

fn sanitized_impl_utf8_to_utf32_fn() string {
	return [
		'rune impl_utf8_to_utf32(u8* _bytes, int _bytes_len) {',
		'\tif (((_bytes_len == 0) || (_bytes_len > 4))) {',
		'\t\treturn 0;',
		'\t}',
		'\tif ((_bytes_len == 1)) {',
		'\t\treturn ((rune)(_bytes[0]));',
		'\t}',
		'\tif ((_bytes_len == 2)) {',
		'\t\trune b0 = ((rune)(_bytes[0]));',
		'\t\trune b1 = ((rune)(_bytes[1]));',
		'\t\treturn ((((b0 & 0x1F)) << 6) | ((b1 & 0x3F)));',
		'\t}',
		'\tif ((_bytes_len == 3)) {',
		'\t\trune b0 = ((rune)(_bytes[0]));',
		'\t\trune b1 = ((rune)(_bytes[1]));',
		'\t\trune b2 = ((rune)(_bytes[2]));',
		'\t\treturn (((((b0 & 0x0F)) << 12) | (((b1 & 0x3F)) << 6)) | ((b2 & 0x3F)));',
		'\t}',
		'\tif ((_bytes_len == 4)) {',
		'\t\trune b0 = ((rune)(_bytes[0]));',
		'\t\trune b1 = ((rune)(_bytes[1]));',
		'\t\trune b2 = ((rune)(_bytes[2]));',
		'\t\trune b3 = ((rune)(_bytes[3]));',
		'\t\treturn ((((((b0 & 0x07)) << 18) | (((b1 & 0x3F)) << 12)) | (((b2 & 0x3F)) << 6)) | ((b3 & 0x3F)));',
		'\t}',
		'\treturn 0;',
		'}',
	].join('\n')
}

fn sanitized_utf8_str_visible_length_fn() string {
	return [
		'int utf8_str_visible_length(string s) {',
		'\tint l = 0;',
		'\tint ul = 1;',
		'\tfor (int i = 0; (i < s.len); i += ul) {',
		'\t\tu8 c = s.str[i];',
		'\t\tul = (((0xe5000000 >> (((s.str[i] >> 3) & 0x1e))) & 3) + 1);',
		'\t\tif (((i + ul) > s.len)) {',
		'\t\t\treturn l;',
		'\t\t}',
		'\t\tl++;',
		'\t\tif ((ul == 1)) {',
		'\t\t\tcontinue;',
		'\t\t}',
		'\t\tif ((ul == 2)) {',
		'\t\t\tu64 r = ((u64)((((u16)(c)) << 8) | s.str[(i + 1)]));',
		'\t\t\tif (((r >= 0xcc80) && (r < 0xcdb0))) {',
		'\t\t\t\tl--;',
		'\t\t\t}',
		'\t\t} else if ((ul == 3)) {',
		'\t\t\tu64 r = ((u64)((((u32)(c)) << 16) | ((((u32)(s.str[(i + 1)])) << 8) | s.str[(i + 2)])));',
		'\t\t\tif (((((((r >= 0xe1aab0) && (r <= 0xe1ac7f))) || (((r >= 0xe1b780) && (r <= 0xe1b87f)))) || (((r >= 0xe28390) && (r <= 0xe2847f)))) || (((r >= 0xefb8a0) && (r <= 0xefb8af))))) {',
		'\t\t\t\tl--;',
		'\t\t\t} else if (((((((((((r >= 0xe18480) && (r <= 0xe1859f))) || (((r >= 0xe2ba80) && (r <= 0xe2bf95)))) || (((r >= 0xe38080) && (r <= 0xe4b77f)))) || (((r >= 0xe4b880) && (r <= 0xea807f)))) || (((r >= 0xeaa5a0) && (r <= 0xeaa79f)))) || (((r >= 0xeab080) && (r <= 0xed9eaf)))) || (((r >= 0xefa480) && (r <= 0xefac7f)))) || (((r >= 0xefb8b8) && (r <= 0xefb9af))))) {',
		'\t\t\t\tl++;',
		'\t\t\t}',
		'\t\t} else if ((ul == 4)) {',
		'\t\t\tu64 r = ((u64)((((u32)(c)) << 24) | (((((u32)(s.str[(i + 1)])) << 16) | (((u32)(s.str[(i + 2)])) << 8)) | s.str[(i + 3)])));',
		'\t\t\tif (((((((r >= 0xf09f8880) && (r <= 0xf09f8a8f))) || (((r >= 0xf09f8c80) && (r <= 0xf09f9c90)))) || (((r >= 0xf09fa490) && (r <= 0xf09fa7af)))) || (((r >= 0xf0a08080) && (r <= 0xf180807f))))) {',
		'\t\t\t\tl++;',
		'\t\t\t}',
		'\t\t}',
		'\t}',
		'\treturn l;',
		'}',
	].join('\n')
}

fn (mut b Builder) compile_cleanc_executable(output_name string, cc string, cc_flags string, error_limit_flag string, mut sw time.StopWatch) {
	cc_start := sw.elapsed()
	cc_cmd := '${cc} ${cc_flags} -w "${staged_c_file}" -o "${output_name}"${error_limit_flag}'
	run_cc_cmd_or_exit(cc_cmd, 'C compilation', b.pref.show_cc)
	print_time('CC', time.Duration(sw.elapsed() - cc_start))

	println('[*] Compiled ${output_name}')
}

pub fn (mut b Builder) build(files []string) {
	b.user_files = files
	mut sw := time.new_stopwatch()
	$if parallel ? {
		b.files = if b.pref.no_parallel {
			b.parse_files(files)
		} else {
			b.parse_files_parallel(files)
		}
	} $else {
		b.files = b.parse_files(files)
	}
	parse_time := sw.elapsed()
	print_time('Scan & Parse', parse_time)
	b.update_parse_summary_counts()
	print_parse_summary(b.parsed_full_files_n, b.parsed_vh_files_n, b.entry_v_lines_n,
		b.parsed_v_lines_n, b.pref.stats, b.pref.print_parsed_files, b.parsed_full_files,
		b.parsed_vh_files)
	if b.pref.stats {
		// b.print_flat_ast_summary()
	}

	if b.pref.skip_type_check {
		b.env = types.Environment.new()
	} else {
		$if parallel ? {
			b.env = if b.pref.no_parallel {
				b.type_check_files()
			} else {
				b.type_check_files_parallel()
			}
		} $else {
			b.env = b.type_check_files()
		}
	}
	type_check_time := time.Duration(sw.elapsed() - parse_time)
	print_time('Type Check', type_check_time)

	// Transform AST (flag enum desugaring, etc.)
	transform_start := sw.elapsed()
	mut trans := transformer.Transformer.new_with_pref(b.files, b.env, b.pref)
	trans.set_file_set(b.file_set)
	b.files = trans.transform_files(b.files)
	transform_time := time.Duration(sw.elapsed() - transform_start)
	print_time('Transform', transform_time)

	// Mark used functions/methods for backend pruning.
	if b.pref.no_markused {
		b.used_fn_keys = map[string]bool{}
	} else {
		mark_used_start := sw.elapsed()
		b.used_fn_keys = markused.mark_used(b.files, b.env)
		mark_used_time := time.Duration(sw.elapsed() - mark_used_start)
		print_time('Mark Used', mark_used_time)
	}

	// Generate output based on backend
	match b.pref.backend {
		.v {
			if !b.pref.skip_genv {
				b.gen_v_files()
			}
		}
		.cleanc {
			b.gen_cleanc()
		}
		.c {
			b.gen_ssa_c()
		}
		.x64 {
			b.gen_native(.x64)
		}
		.arm64 {
			b.gen_native(.arm64)
		}
		.eval {
			mut runner := eval.new(b.pref)
			runner.run_files(b.files) or {
				eprintln('error: ${err.msg()}')
				exit(1)
			}
		}
	}

	print_time('Total', sw.elapsed())
}

fn (mut b Builder) gen_v_files() {
	mut gen := gen_v.new_gen(b.pref)
	for file in b.files {
		gen.gen(file)
		if b.pref.debug {
			gen.print_output()
		}
	}
}

fn (mut b Builder) gen_cleanc() {
	// Clean C Backend (AST -> C)
	mut sw := time.new_stopwatch()

	// The cached-core split is currently unstable in some module builds
	// (including cmd/v2 self-host and directory-style user module builds).
	// Force single-unit cleanc generation there.
	force_no_cache := b.should_disable_cleanc_cache()
	use_cache := !b.pref.no_cache && !force_no_cache
	if os.getenv('V2_TRACE_CACHE') != '' {
		eprintln('TRACE_CACHE use_cache=${use_cache} no_cache=${b.pref.no_cache} force_no_cache=${force_no_cache} self_build=${b.is_cmd_v2_self_build()} files=${b.user_files}')
	}

	// Determine output name
	output_name := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		b.default_output_name()
	} else {
		'out'
	}

	cc := configured_cc(b.pref.vroot)
	directive_flags := b.collect_cflags_from_sources()
	mut cc_flag_parts := []string{}
	env_flags := configured_cflags()
	if env_flags.trim_space() != '' {
		cc_flag_parts << env_flags.trim_space()
	}
	if directive_flags.trim_space() != '' {
		cc_flag_parts << directive_flags.trim_space()
	}
	tcc_extra := tcc_flags(cc, b.pref.vroot)
	if tcc_extra.trim_space() != '' {
		cc_flag_parts << tcc_extra.trim_space()
	}
	cc_flags := cc_flag_parts.join(' ')
	mut error_limit_flag := ''
	if !cc.contains('tcc') {
		version_res := os.execute('${cc} --version')
		if version_res.exit_code == 0 && version_res.output.contains('clang') {
			error_limit_flag = ' -ferror-limit=0'
		}
	}

	// If output ends with .c, just write the C file
	if output_name.ends_with('.c') {
		mut c_source := ''
		// For .c output, prefer the same cached-core split used by normal
		// build+link flow, when the cache is valid.
		if use_cache && !b.pref.skip_builtin && b.has_module('builtin') && b.has_module('strconv')
			&& b.can_use_cached_core_headers() {
			main_modules := b.collect_modules_excluding(core_cached_module_names)
			if main_modules.len > 0 {
				b.ensure_core_module_headers()
				c_source = b.gen_cleanc_source(main_modules)
			}
		}
		if c_source == '' {
			c_source = b.gen_cleanc_source([]string{})
		}
		print_time('C Gen', sw.elapsed())
		if c_source == '' {
			eprintln('error: cleanc backend is not fully functional (compiled with stubbed functions)')
			eprintln('hint: use v2 compiled with v1 for proper C code generation')
			return
		}
		os.write_file(output_name, c_source) or { panic(err) }
		println('[*] Wrote ${output_name}')
		return
	}

	// Fast path: cache one core object (builtin+strconv), compile/link only the rest.
	if use_cache && !b.pref.skip_builtin && b.has_module('builtin') && b.has_module('strconv') {
		if b.gen_cleanc_with_cached_core(output_name, cc, cc_flags, error_limit_flag, mut
			sw)
		{
			return
		}
	}

	// Fallback: compile one full C translation unit.
	c_source := b.gen_cleanc_source([]string{})
	print_time('C Gen', sw.elapsed())
	if c_source == '' {
		eprintln('error: cleanc backend is not fully functional (compiled with stubbed functions)')
		eprintln('hint: use v2 compiled with v1 for proper C code generation')
		return
	}
	os.write_file(staged_c_file, c_source) or { panic(err) }
	println('[*] Wrote ${staged_c_file}')
	b.compile_cleanc_executable(output_name, cc, cc_flags, error_limit_flag, mut sw)
}

fn (b &Builder) is_cmd_v2_self_build() bool {
	if b.user_files.len != 1 {
		return false
	}
	// Avoid path normalization here: during bootstraps, some intermediate
	// compilers can still have unstable path helpers.
	path := b.user_files[0].replace('\\', '/')
	if path == 'v2.v' || path.ends_with('/v2.v') {
		return true
	}
	return path.ends_with('/cmd/v2/v2.v') || path.ends_with('cmd/v2/v2.v')
}

fn (b &Builder) should_disable_cleanc_cache() bool {
	$if @BACKEND == 'arm64' {
		// The arm64 self-hosted compiler still mis-generates cached-core
		// boundaries for cleanc builds, which drops required runtime helpers.
		// Force a single translation unit until the cache path is stable there.
		return true
	}
	if b.is_cmd_v2_self_build() {
		return true
	}
	for raw_input in b.user_files {
		input := raw_input.trim_right('/\\')
		if input.len == 0 {
			continue
		}
		if input.ends_with('.v') || input.ends_with('.vv') || input.ends_with('.vsh')
			|| input.ends_with('.vh') {
			continue
		}
		if os.is_dir(input) {
			return true
		}
	}
	return false
}

fn (b &Builder) default_output_name() string {
	if b.user_files.len == 0 {
		return 'out'
	}
	last_input := b.user_files.last().trim_right('/\\')
	if last_input.len == 0 || last_input == '.' {
		cwd := os.getwd()
		base := os.file_name(cwd)
		return if base.len > 0 { base } else { 'out' }
	}
	if os.is_dir(last_input) {
		base := os.file_name(last_input)
		return if base.len > 0 { base } else { 'out' }
	}
	base := os.file_name(last_input).all_before_last('.v')
	return if base.len > 0 { base } else { os.file_name(last_input) }
}

fn (mut b Builder) gen_ssa_c() {
	// SSA -> C backend.
	mut sw := time.new_stopwatch()

	mut mod := ssa.Module.new('main')
	if mod == unsafe { nil } {
		eprintln('error: ssa c backend not available (compiled with stubbed ssa module)')
		eprintln('hint: use v2 compiled with v1 for ssa c code generation')
		return
	}
	mut ssa_builder := ssa.Builder.new_with_env(mod, b.env)

	mut stage_start := sw.elapsed()
	ssa_builder.build_all(b.files)
	print_time('SSA Build', time.Duration(sw.elapsed() - stage_start))

	// TODO: re-enable SSA optimization once the new builder is mature
	// stage_start = sw.elapsed()
	// optimize.optimize(mut mod)
	// print_time('SSA Optimize', time.Duration(sw.elapsed() - stage_start))

	cc := configured_cc(b.pref.vroot)
	directive_flags := b.collect_cflags_from_sources()
	mut cc_flag_parts := []string{}
	env_flags := configured_cflags()
	if env_flags.trim_space() != '' {
		cc_flag_parts << env_flags.trim_space()
	}
	if directive_flags.trim_space() != '' {
		cc_flag_parts << directive_flags.trim_space()
	}
	tcc_extra := tcc_flags(cc, b.pref.vroot)
	if tcc_extra.trim_space() != '' {
		cc_flag_parts << tcc_extra.trim_space()
	}
	cc_flags := cc_flag_parts.join(' ')
	mut error_limit_flag := ''
	if !cc.contains('tcc') {
		version_res := os.execute('${cc} --version')
		if version_res.exit_code == 0 && version_res.output.contains('clang') {
			error_limit_flag = ' -ferror-limit=0'
		}
	}

	// Try to get pre-compiled builtin.o and vlib.o from the cleanc cache
	mut builtin_obj := ''
	mut vlib_obj := ''
	if !b.pref.skip_builtin && b.has_module('builtin') && b.has_module('strconv')
		&& b.ensure_core_cache_dir() {
		cache_dir := b.core_cache_dir()
		builtin_obj = b.ensure_cached_module_object(cache_dir, builtin_cache_name, builtin_cached_module_paths,
			builtin_cached_module_names, cc, cc_flags, error_limit_flag) or { '' }
		if builtin_obj.len > 0 && vlib_cached_module_paths.len > 0 {
			vlib_obj = b.ensure_cached_module_object(cache_dir, vlib_cache_name, vlib_cached_module_paths,
				vlib_cached_module_names, cc, cc_flags, error_limit_flag) or { '' }
		}
	}

	stage_start = sw.elapsed()
	mut gen := c.Gen.new(mod)
	gen.link_builtin = builtin_obj.len > 0
	c_source := gen.gen()
	print_time('C Gen', time.Duration(sw.elapsed() - stage_start))
	if c_source == '' {
		eprintln('error: ssa c backend failed to generate C source')
		return
	}

	output_name := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		b.default_output_name()
	} else {
		'out'
	}

	if output_name.ends_with('.c') {
		os.write_file(output_name, c_source) or { panic(err) }
		println('[*] Wrote ${output_name}')
		return
	}

	c_file := b.exec_build_c_file(output_name)
	os.write_file(c_file, c_source) or { panic(err) }
	println('[*] Wrote ${c_file}')

	cc_start := sw.elapsed()
	mut cc_cmd := ''
	if builtin_obj.len > 0 {
		// Compile SSA main.c and link against pre-compiled builtin.o
		main_obj := staged_main_obj_file
		compile_cmd := '${cc} ${cc_flags} -w -c "${c_file}" -o "${main_obj}"${error_limit_flag}'
		if b.pref.show_cc {
			println(compile_cmd)
		}
		compile_res := os.execute(compile_cmd)
		if compile_res.exit_code != 0 {
			eprintln('error: ssa c backend compilation failed')
			lines := compile_res.output.split_into_lines()
			limit := if lines.len < 20 { lines.len } else { 20 }
			for line in lines[..limit] {
				eprintln(line)
			}
			exit(1)
		}
		mut link_objects := '"${main_obj}" "${builtin_obj}"'
		if vlib_obj.len > 0 {
			link_objects += ' "${vlib_obj}"'
		}
		cc_cmd = '${cc} ${cc_flags} -w ${link_objects} -o "${output_name}"'
		if b.pref.show_cc {
			println(cc_cmd)
		}
		cc_res := os.execute(cc_cmd)
		if cc_res.exit_code != 0 {
			eprintln('error: ssa c backend linking failed')
			lines := cc_res.output.split_into_lines()
			limit := if lines.len < 20 { lines.len } else { 20 }
			for line in lines[..limit] {
				eprintln(line)
			}
			exit(1)
		}
		os.rm(main_obj) or {}
	} else {
		// Single-file compilation (no builtin linking)
		cc_cmd = '${cc} ${cc_flags} -w "${c_file}" -o "${output_name}"${error_limit_flag}'
		if b.pref.show_cc {
			println(cc_cmd)
		} else if os.getenv('V2VERBOSE') != '' {
			dump(cc_cmd)
		}
		cc_res := os.execute(cc_cmd)
		if cc_res.exit_code != 0 {
			eprintln('error: ssa c backend compilation failed')
			lines := cc_res.output.split_into_lines()
			limit := if lines.len < 20 { lines.len } else { 20 }
			for line in lines[..limit] {
				eprintln(line)
			}
			exit(1)
		}
	}
	print_time('CC', time.Duration(sw.elapsed() - cc_start))

	if !b.pref.keep_c {
		os.rm(c_file) or {}
	}
	println('[*] Compiled ${output_name}')
}

fn (mut b Builder) gen_cleanc_source(modules []string) string {
	return b.gen_cleanc_source_with_options(modules, false, '', []string{}, true)
}

fn (mut b Builder) gen_cleanc_source_for_cache(modules []string, cache_bundle_name string) string {
	return b.gen_cleanc_source_with_options(modules, true, cache_bundle_name, []string{},
		false)
}

fn (mut b Builder) gen_cleanc_source_with_cache_init_calls(modules []string, cached_init_calls []string) string {
	return b.gen_cleanc_source_with_options(modules, false, '', cached_init_calls, true)
}

fn (mut b Builder) gen_cleanc_source_with_options(modules []string, export_const_symbols bool, cache_bundle_name string, cached_init_calls []string, use_markused bool) string {
	mut gen_files := b.files.clone()
	if cached_init_calls.len > 0 && b.can_use_cached_core_headers() {
		mut p := parser.Parser.new(b.pref)
		header_files := p.parse_files(b.core_cached_parse_paths(), mut b.file_set)
		gen_files << header_files
	}
	mut gen := cleanc.Gen.new_with_env_and_pref(gen_files, b.env, b.pref)
	if modules.len > 0 {
		gen.set_emit_modules(modules)
	}
	if use_markused && b.used_fn_keys.len > 0 {
		gen.set_used_fn_keys(b.used_fn_keys)
	}
	gen.set_export_const_symbols(export_const_symbols)
	if cache_bundle_name.len > 0 {
		gen.set_cache_bundle_name(cache_bundle_name)
	}
	if cached_init_calls.len > 0 {
		gen.set_cached_init_calls(cached_init_calls)
	}
	source := gen.gen()
	if os.getenv('V2TRACE_CLEANC') != '' {
		eprintln('TRACE_CLEANC builder_files=${b.files.len} gen_files=${gen_files.len} source_len=${source.len}')
	}
	return source
}

fn (mut b Builder) gen_cleanc_with_cached_core(output_name string, cc string, cc_flags string, error_limit_flag string, mut sw time.StopWatch) bool {
	main_modules := b.collect_modules_excluding(core_cached_module_names)
	if main_modules.len == 0 {
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=no_main_modules')
		}
		return false
	}

	cache_dir := b.core_cache_dir()
	if !b.ensure_core_cache_dir() {
		// If we cannot create a readable/writable cache dir, fall back to full compilation.
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=cache_dir_unusable')
		}
		return false
	}

	builtin_obj := b.ensure_cached_module_object(cache_dir, builtin_cache_name, builtin_cached_module_paths,
		builtin_cached_module_names, cc, cc_flags, error_limit_flag) or {
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=builtin_obj_failed')
		}
		return false
	}
	mut vlib_obj := ''
	if vlib_cached_module_paths.len > 0 {
		vlib_obj = b.ensure_cached_module_object(cache_dir, vlib_cache_name, vlib_cached_module_paths,
			vlib_cached_module_names, cc, cc_flags, error_limit_flag) or {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE cached_core=false reason=vlib_obj_failed')
			}
			return false
		}
	}
	b.ensure_core_module_headers()

	// When TCC is the default compiler but fell back to cc for cache
	// compilation (e.g. due to TCC not supporting certain C constructs),
	// the cached .o files are Mach-O (from cc) while TCC would produce ELF.
	// Detect this mismatch and use cc for main compilation and linking too.
	mut main_cc := cc
	mut main_cc_flags := cc_flags
	if cc.contains('tcc') && os.exists(builtin_obj) {
		bytes := os.read_bytes(builtin_obj) or { []u8{} }
		is_elf := bytes.len >= 4 && bytes[0] == 0x7f && bytes[1] == 0x45 && bytes[2] == 0x4c
			&& bytes[3] == 0x46
		if !is_elf {
			// Cached .o was compiled by cc (via TCC fallback), not TCC.
			// Use cc for main compilation and linking to match formats.
			main_cc = 'cc'
			main_cc_flags = configured_cflags()
		}
	}

	mut cached_init_calls := []string{}
	cached_init_calls << '__v2_cached_init_${builtin_cache_name}'
	if vlib_obj.len > 0 {
		cached_init_calls << '__v2_cached_init_${vlib_cache_name}'
	}
	mut main_source := b.gen_cleanc_source_with_cache_init_calls(main_modules, cached_init_calls)
	main_source = b.inject_cached_core_forward_decls(main_source)
	print_time('C Gen', sw.elapsed())
	if main_source == '' {
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=empty_main_source')
		}
		return false
	}

	main_c_file := b.exec_build_c_file(output_name)
	os.write_file(main_c_file, main_source) or { return false }
	println('[*] Wrote ${main_c_file}')

	cc_start := sw.elapsed()
	main_obj := staged_main_obj_file
	compile_main_cmd := '${main_cc} ${main_cc_flags} -w -c "${main_c_file}" -o "${main_obj}"${error_limit_flag}'
	main_fell_back := run_cc_cmd_or_exit(compile_main_cmd, 'C compilation', b.pref.show_cc)
	if main_fell_back && main_cc.contains('tcc') {
		// TCC failed on main.c but cached .o files are ELF (from TCC).
		// Fallback produced Mach-O main.o — can't link with ELF cache.
		// Fall back to non-cached full compilation.
		os.rm(main_obj) or {}
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=main_compile_fell_back')
		}
		return false
	}
	mut link_cmd := '${main_cc} ${main_cc_flags} -w "${main_obj}" "${builtin_obj}"'
	if vlib_obj.len > 0 {
		link_cmd += ' "${vlib_obj}"'
	}
	link_cmd += ' -o "${output_name}"'
	run_cc_cmd_or_exit(link_cmd, 'Linking', b.pref.show_cc)
	print_time('CC', time.Duration(sw.elapsed() - cc_start))

	os.rm(main_obj) or {}
	if !b.pref.keep_c {
		os.rm(main_c_file) or {}
	}
	if os.getenv('V2_TRACE_CACHE') != '' {
		eprintln('TRACE_CACHE cached_core=true')
	}
	println('[*] Compiled ${output_name}')
	return true
}

fn (b &Builder) inject_cached_core_forward_decls(source string) string {
	decls := b.cached_core_forward_decls()
	if decls == '' {
		return source
	}
	mut lines := source.split_into_lines()
	mut out := []string{cap: lines.len + decls.count('\n') + 2}
	mut saw_cached_decl := false
	mut inserted := false
	for line in lines {
		out << line
		if line.starts_with('void __v2_cached_init_') {
			saw_cached_decl = true
			continue
		}
		if saw_cached_decl && line == '' && !inserted {
			for decl_line in decls.split_into_lines() {
				if decl_line != '' {
					out << decl_line
				}
			}
			out << ''
			inserted = true
		}
	}
	if !inserted {
		return decls + '\n' + source
	}
	return out.join('\n')
}

fn (b &Builder) cached_core_forward_decls() string {
	cache_dir := b.core_cache_dir()
	mut seen := map[string]bool{}
	mut decls := []string{}
	for cache_name in [builtin_cache_name, vlib_cache_name] {
		c_path := os.join_path(cache_dir, '${cache_name}.c')
		if !os.exists(c_path) {
			continue
		}
		for decl in top_level_c_forward_decls(c_path) {
			if decl in seen {
				continue
			}
			seen[decl] = true
			decls << decl
		}
	}
	return decls.join('\n')
}

fn top_level_c_forward_decls(c_path string) []string {
	lines := os.read_lines(c_path) or { return []string{} }
	mut decls := []string{}
	for raw_line in lines {
		if raw_line.len == 0 || raw_line[0].is_space() {
			continue
		}
		line := raw_line.trim_space()
		if !line.ends_with(');') || !line.contains('(') {
			continue
		}
		if line.starts_with('#') || line.starts_with('typedef ') || line.starts_with('struct ')
			|| line.starts_with('union ') || line.starts_with('enum ')
			|| line.starts_with('return ') || line.starts_with('if ') || line.starts_with('for ')
			|| line.starts_with('while ') || line.starts_with('switch ') {
			continue
		}
		// Skip expression fragments that start with '(' or '*'
		if line[0] == `(` || line[0] == `*` {
			continue
		}
		// Skip global variable definitions (contain '=') - only extract function declarations.
		if line.contains('=') {
			continue
		}
		// A forward declaration has at least a return type and function name before '('.
		// Reject bare function calls like 'memset(...)' or 'tos(...)'.
		paren_idx := line.index_u8(`(`)
		if paren_idx > 0 {
			before_paren := line[..paren_idx].trim_space()
			if !before_paren.contains(' ') && !before_paren.contains('*') {
				continue
			}
		}
		decls << line
	}
	return decls
}

fn (mut b Builder) ensure_cached_module_object(cache_dir string, cache_name string, module_paths []string, emit_modules []string, cc string, cc_flags string, error_limit_flag string) !string {
	obj_path := cache_path_join(cache_dir, '${cache_name}.o')
	stamp_path := cache_path_join(cache_dir, '${cache_name}.stamp')
	c_path := cache_path_join(cache_dir, '${cache_name}.c')
	expected_stamp := b.cache_stamp_for_modules(cache_name, module_paths, cc, cc_flags)
	if os.exists(obj_path) && os.exists(stamp_path) {
		if current_stamp := os.read_file(stamp_path) {
			if current_stamp == expected_stamp {
				if os.getenv('V2VERBOSE') != '' {
					println('[*] Reusing ${obj_path}')
				}
				return obj_path
			}
		}
	}

	module_source := b.gen_cleanc_source_for_cache(emit_modules, cache_name)
	if module_source == '' {
		return error('failed to generate C source for ${cache_name}')
	}
	os.write_file(c_path, module_source)!

	compile_cmd := '${cc} ${cc_flags} -w -c "${c_path}" -o "${obj_path}"${error_limit_flag}'
	run_cc_cmd_or_exit(compile_cmd, 'C compilation', b.pref.show_cc)
	os.write_file(stamp_path, expected_stamp)!
	return obj_path
}

fn (b &Builder) has_module(module_name string) bool {
	for file in b.files {
		if file_module_name(file) == module_name {
			return true
		}
	}
	return false
}

fn (b &Builder) collect_modules_excluding(excluded []string) []string {
	mut excluded_set := map[string]bool{}
	for module_name in excluded {
		excluded_set[module_name] = true
	}
	mut modules_set := map[string]bool{}
	for file in b.files {
		module_name := file_module_name(file)
		if module_name in excluded_set {
			continue
		}
		modules_set[module_name] = true
	}
	mut modules := modules_set.keys()
	modules.sort()
	return modules
}

fn file_module_name(file ast.File) string {
	for stmt in file.stmts {
		if stmt is ast.ModuleStmt {
			return stmt.name.replace('.', '_')
		}
	}
	return 'main'
}

fn flag_os_matches(cond string) bool {
	current := os.user_os().to_lower()
	return match cond.to_lower() {
		'darwin', 'macos', 'mac' { current == 'macos' || current == 'darwin' }
		'linux' { current == 'linux' }
		'windows' { current == 'windows' }
		'freebsd' { current == 'freebsd' }
		'openbsd' { current == 'openbsd' }
		'netbsd' { current == 'netbsd' }
		'dragonfly' { current == 'dragonfly' }
		'android' { current == 'android' }
		else { false }
	}
}

fn find_vmod_root_for_file(file_path string) string {
	mut dir := os.dir(file_path)
	for _ in 0 .. 12 {
		if os.exists(os.join_path(dir, 'v.mod')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir || parent == '' {
			break
		}
		dir = parent
	}
	return os.dir(file_path)
}

fn resolve_flag_path(path string, file_dir string, vmod_root string) string {
	mut resolved := path.replace('@VMODROOT', vmod_root)
	if os.is_abs_path(resolved) {
		return resolved
	}
	if resolved.starts_with('./') || resolved.starts_with('../') {
		return os.norm_path(os.join_path(file_dir, resolved))
	}
	return resolved
}

fn normalize_flag_value_for_file(flag_value string, file_path string) string {
	file_dir := os.dir(file_path)
	vmod_root := find_vmod_root_for_file(file_path)
	mut tokens := flag_value.fields()
	mut out := []string{}
	mut i := 0
	for i < tokens.len {
		tok := tokens[i]
		if tok in ['-I', '-L', '-F'] && i + 1 < tokens.len {
			out << tok
			out << resolve_flag_path(tokens[i + 1], file_dir, vmod_root)
			i += 2
			continue
		}
		if tok.starts_with('-I') && tok.len > 2 {
			out << '-I' + resolve_flag_path(tok[2..], file_dir, vmod_root)
			i++
			continue
		}
		if tok.starts_with('-L') && tok.len > 2 {
			out << '-L' + resolve_flag_path(tok[2..], file_dir, vmod_root)
			i++
			continue
		}
		if tok.starts_with('-F') && tok.len > 2 {
			out << '-F' + resolve_flag_path(tok[2..], file_dir, vmod_root)
			i++
			continue
		}
		if tok.contains('@VMODROOT') || tok.starts_with('./') || tok.starts_with('../')
			|| tok.ends_with('.c') || tok.ends_with('.m') || tok.ends_with('.o') {
			out << resolve_flag_path(tok, file_dir, vmod_root)
			i++
			continue
		}
		out << tok
		i++
	}
	return out.join(' ')
}

fn parse_flag_directive_line(line string, file_path string) ?string {
	trimmed := line.trim_space()
	if !trimmed.starts_with('#flag') {
		return none
	}
	mut rest := trimmed['#flag'.len..].trim_space()
	if rest == '' {
		return none
	}
	if comment_idx := rest.index('//') {
		rest = rest[..comment_idx].trim_space()
		if rest == '' {
			return none
		}
	}
	parts := rest.fields()
	if parts.len == 0 {
		return none
	}
	if !parts[0].starts_with('-') && !parts[0].starts_with('@') && parts.len > 1 {
		if !flag_os_matches(parts[0]) {
			return none
		}
		rest = rest[parts[0].len..].trim_space()
	}
	if rest == '' {
		return none
	}
	return normalize_flag_value_for_file(rest, file_path)
}

fn flag_references_missing_file(flag string) bool {
	for tok in flag.fields() {
		clean := tok.trim('"').trim("'")
		if clean.len == 0 {
			continue
		}
		if clean.ends_with('.o') || clean.ends_with('.a') || clean.ends_with('.so')
			|| clean.ends_with('.dylib') || clean.ends_with('.m') || clean.ends_with('.c') {
			if os.is_abs_path(clean) || clean.starts_with('./') || clean.starts_with('../') {
				if !os.exists(clean) {
					return true
				}
			}
		}
	}
	return false
}

fn (b &Builder) collect_cflags_from_sources() string {
	mut flags := []string{}
	mut seen := map[string]bool{}
	for file in b.files {
		if file.name == '' {
			continue
		}
		lines := os.read_lines(file.name) or { continue }
		for line in lines {
			mut flag := parse_flag_directive_line(line, file.name) or { continue }
			flag = flag.replace('@VEXEROOT', b.pref.vroot).replace('VEXEROOT', b.pref.vroot)
			if flag_references_missing_file(flag) {
				continue
			}
			if flag == '' || flag in seen {
				continue
			}
			seen[flag] = true
			flags << flag
		}
	}
	return flags.join(' ')
}

fn default_cc(vroot string) string {
	// Try to use tcc by default, like v1 does.
	tcc_path := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	if os.exists(tcc_path) {
		return tcc_path
	}
	return 'cc'
}

fn configured_cc(vroot string) string {
	cc := (os.getenv_opt('V2CC') or { '' }).trim_space()
	if cc != '' {
		return cc
	}
	return default_cc(vroot)
}

fn configured_cflags() string {
	return (os.getenv_opt('V2CFLAGS') or { '' }).trim_space()
}

fn tcc_flags(cc string, vroot string) string {
	if !cc.contains('tcc') {
		return ''
	}
	tcc_dir := os.join_path(vroot, 'thirdparty', 'tcc')
	return '-I "${os.join_path(tcc_dir, 'lib', 'include')}" -L "${os.join_path(tcc_dir,
		'lib')}"'
}

// run_cc_cmd_or_exit runs a C compiler command, falling back from tcc to cc
// if needed. Returns true if tcc fell back to cc.
fn run_cc_cmd_or_exit(cmd string, stage string, show_cc bool) bool {
	if show_cc {
		println(cmd)
	} else if os.getenv('V2VERBOSE') != '' {
		dump(cmd)
	}
	result := os.execute(cmd)
	if result.exit_code != 0 {
		// If tcc failed, fall back to cc.
		// Check only the compiler binary (before the first space), not the full
		// command string which contains tcc in include/library flag paths.
		cc_binary := cmd.all_before(' ')
		if cc_binary.contains('tcc') {
			eprintln('Failed to compile with tcc, falling back to cc')
			eprintln('tcc cmd: ${cmd}')
			eprintln(result.output)
			fallback_cmd := cmd.replace_once(cc_binary, 'cc')
			run_cc_cmd_or_exit(fallback_cmd, stage, show_cc)
			return true
		}
		eprintln('${stage} failed:')
		lines := result.output.split_into_lines()
		limit := if lines.len < 50 { lines.len } else { 50 }
		for line in lines[..limit] {
			eprintln(line)
		}
		mut error_count := 0
		mut warning_count := 0
		for line in lines {
			if line.contains(': error:') || line.contains(': fatal error:') {
				error_count += 1
			} else if line.contains(': warning:') {
				warning_count += 1
			}
		}
		if stage == 'C compilation' {
			eprintln('Total: ${warning_count} warnings and ${error_count} errors')
		}
		exit(1)
	}
	return false
}

fn (mut b Builder) gen_native(backend_arch pref.Arch) {
	arch := if backend_arch == .auto { b.pref.get_effective_arch() } else { backend_arch }

	// Build all files into a single SSA module
	mut mod := ssa.Module.new('main')
	if mod == unsafe { nil } {
		eprintln('error: native backend not available (compiled with stubbed ssa module)')
		eprintln('hint: use v2 compiled with v1 for native code generation')
		return
	}
	mut ssa_builder := ssa.Builder.new_with_env(mod, b.env)
	mut native_sw := time.new_stopwatch()

	// Build all files together with proper multi-file ordering
	mut stage_start := native_sw.elapsed()
	ssa_builder.build_all(b.files)
	print_time('SSA Build', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	ssa_optimize.optimize(mut mod)
	print_time('SSA Optimize', time.Duration(native_sw.elapsed() - stage_start))
	$if debug {
		ssa_optimize.verify_and_panic(mod, 'full optimization')
	}

	stage_start = native_sw.elapsed()
	mut mir_mod := mir.lower_from_ssa(mod)
	print_time('MIR Lower', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	abi.lower(mut mir_mod, arch)
	print_time('ABI Lower', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	insel.select(mut mir_mod, arch)
	print_time('InsSel', time.Duration(native_sw.elapsed() - stage_start))

	// Determine output binary name from the last user file
	output_binary := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		b.default_output_name()
	} else {
		'out'
	}

	if arch == .arm64 && os.user_os() == 'macos' {
		// Use built-in linker for ARM64 macOS
		mut gen := arm64.Gen.new(&mir_mod)
		gen.gen()

		if b.pref.hot_fn.len > 0 {
			// Hot code reloading: extract raw machine code for a single function
			code := gen.extract_function(b.pref.hot_fn)
			if code.len > 0 {
				os.write_file_array(output_binary, code) or { panic(err) }
				println('hot-fn: wrote ${code.len} bytes for "${b.pref.hot_fn}" to ${output_binary}')
			}
			return
		}

		gen.link_executable(output_binary)

		if b.pref.verbose {
			println('[*] Linked ${output_binary} (built-in linker)')
		}
	} else {
		// Generate object file and use external linker
		obj_file := 'main.o'

		if arch == .arm64 {
			mut gen := arm64.Gen.new(&mir_mod)
			gen.gen()
			gen.write_file(obj_file)
		} else {
			mut gen := x64.Gen.new(&mir_mod)
			gen.gen()
			gen.write_file(obj_file)
		}

		if b.pref.verbose {
			println('[*] Wrote ${obj_file}')
		}

		// Link the object file into an executable
		if os.user_os() == 'macos' {
			sdk_res := os.execute('xcrun -sdk macosx --show-sdk-path')
			sdk_path := sdk_res.output.trim_space()
			arch_flag := if arch == .arm64 { 'arm64' } else { 'x86_64' }
			link_cmd := 'ld -o ${output_binary} ${obj_file} -lSystem -syslibroot "${sdk_path}" -e _main -arch ${arch_flag} -platform_version macos 11.0.0 11.0.0'
			link_result := os.execute(link_cmd)
			if link_result.exit_code != 0 {
				eprintln('Link failed:')
				eprintln(link_result.output)
				exit(1)
			}
		} else {
			// Linux linking
			link_result := os.execute('cc ${obj_file} -o ${output_binary} -no-pie')
			if link_result.exit_code != 0 {
				eprintln('Link failed:')
				eprintln(link_result.output)
				exit(1)
			}
		}

		if b.pref.verbose {
			println('[*] Linked ${output_binary}')
		}

		// Clean up object file
		os.rm(obj_file) or {}
	}
}

fn print_time(title string, time_d time.Duration) {
	println(' * ${title}: ${time_d.milliseconds()}ms')
}

fn (mut b Builder) update_parse_summary_counts() {
	mut parsed_full_files_n := 0
	mut parsed_vh_files_n := 0
	mut parsed_full_files := []string{}
	mut parsed_vh_files := []string{}
	for file in b.files {
		if file.name.ends_with('.vh') {
			parsed_vh_files_n++
			parsed_vh_files << file.name
		} else {
			parsed_full_files_n++
			parsed_full_files << file.name
		}
	}
	b.parsed_full_files_n = parsed_full_files_n
	b.parsed_vh_files_n = parsed_vh_files_n
	b.parsed_full_files = parsed_full_files
	b.parsed_vh_files = parsed_vh_files
	if b.pref.stats {
		b.entry_v_lines_n = count_v_lines_for_paths(b.user_files)
		b.parsed_v_lines_n = b.count_parsed_v_lines()
	} else {
		b.entry_v_lines_n = 0
		b.parsed_v_lines_n = 0
	}
}

/*
fn (b &Builder) print_flat_ast_summary() {
	legacy_stats := ast.legacy_ast_stats(b.files)
	legacy_nodes := ast.count_legacy_nodes(b.files)
	flat := ast.flatten_files(b.files)
	flat_stats := flat.stats()
	mut mem_delta_pct := f64(0)
	if legacy_stats.bytes_estimate > 0 {
		mem_delta_pct = (f64(legacy_stats.bytes_estimate) - f64(flat_stats.bytes_estimate)) * 100.0 / f64(legacy_stats.bytes_estimate)
	}
	println(' * AST nodes: legacy=${legacy_nodes}, flat=${flat_stats.nodes}')
	println(' * AST memory est: legacy=${legacy_stats.bytes_estimate}B, flat=${flat_stats.bytes_estimate}B (${mem_delta_pct:.2f}% reduction)')
}
*/

fn count_v_lines_for_paths(paths []string) int {
	mut seen_paths := map[string]bool{}
	mut total_v_lines := 0
	for path in paths {
		norm_path := os.norm_path(path)
		if norm_path in seen_paths {
			continue
		}
		seen_paths[norm_path] = true
		lines := os.read_lines(norm_path) or { continue }
		total_v_lines += lines.len
	}
	return total_v_lines
}

fn (b &Builder) count_parsed_v_lines() int {
	mut parsed_paths := []string{}
	mut seen_files := map[string]bool{}
	for file in b.files {
		if file.name in seen_files {
			continue
		}
		seen_files[file.name] = true
		parsed_paths << file.name
	}
	return count_v_lines_for_paths(parsed_paths)
}

fn print_parse_summary(parsed_full_files_n int, parsed_vh_files_n int, entry_v_lines_n int, parsed_v_lines_n int, show_stats bool, print_parsed_files bool, parsed_full_files []string, parsed_vh_files []string) {
	println(' * Parsed files: fully parsed files: ${parsed_full_files_n}, parsed .vh files: ${parsed_vh_files_n}')
	if print_parsed_files {
		if parsed_full_files.len > 0 {
			println(' * Fully parsed files:')
			for path in parsed_full_files {
				println('   [full] ${path}')
			}
		}
		if parsed_vh_files.len > 0 {
			println(' * Parsed .vh files:')
			for path in parsed_vh_files {
				println('   [vh] ${path}')
			}
		}
	}
	if show_stats {
		println(' * Parsed V LOC (entry files): ${entry_v_lines_n}')
		println(' * Parsed V LOC (all parsed sources): ${parsed_v_lines_n}')
	}
}
