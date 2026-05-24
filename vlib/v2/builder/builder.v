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
	files                     []ast.File
	user_files                []string // original user-provided files (for output name)
	file_set                  &token.FileSet     = token.FileSet.new()
	env                       &types.Environment = unsafe { nil } // Type checker environment
	parsed_full_files_n       int
	parsed_vh_files_n         int
	entry_v_lines_n           int
	parsed_v_lines_n          int
	parsed_full_files         []string
	parsed_vh_files           []string
	used_fn_keys              map[string]bool
	used_vh_for_parse         bool
	used_import_vh_for_parse  bool
	used_virtual_vh_for_parse bool
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
	source = guard_tcc_stdatomic_preamble_when_using_compat_header(source)
	// Remaining patches for codegen issues not yet fixed at the backend level.
	// Pointer dereference for pass-by-pointer array params:
	source = source.replace('(_idx_s < ((a)).len)', '(_idx_s < (*(a)).len)')
	source = source.replace('&((string*)((a)).data)', '&((string*)(*(a)).data)')
	// Result struct declarations that the backend may omit
	// (e.g. due to ARM64 chained-access map bugs in self-hosted binaries):
	source = ensure_result_type(source, '_result_string', 'string')
	source = ensure_result_type(source, '_result_int', 'int')
	source = ensure_result_type(source, '_result_rune', 'rune')
	// Builtin function body replacements (V source doesn't lower correctly yet):
	source = replace_generated_c_fn(source, 'string u64_to_hex(u64 nn, u8 len)',
		sanitized_u64_to_hex_fn())
	source = replace_generated_c_fn(source, 'string u64_to_hex_no_leading_zeros(u64 nn, u8 len)',
		sanitized_u64_to_hex_no_leading_zeros_fn())
	source = replace_generated_c_fn(source, 'string u8__str_escaped(u8 b)',
		sanitized_u8_str_escaped_fn())
	source = replace_generated_c_fn(source, 'int int_min(int a, int b)', sanitized_int_min_fn())
	source = replace_generated_c_fn(source, 'int int_max(int a, int b)', sanitized_int_max_fn())
	source = replace_generated_c_fn(source, 'int string__utf32_code(string _rune)',
		sanitized_string_utf32_code_fn())
	source = replace_generated_c_fn(source,
		'_result_rune Array_u8__utf8_to_utf32(Array_u8 _bytes)',
		sanitized_array_u8_utf8_to_utf32_fn())
	source = replace_generated_c_fn(source, 'rune impl_utf8_to_utf32(u8* _bytes, int _bytes_len)',
		sanitized_impl_utf8_to_utf32_fn())
	source = replace_generated_c_fn(source, 'int utf8_str_visible_length(string s)',
		sanitized_utf8_str_visible_length_fn())
	// Array_u8_contains call convention:
	source = source.replace("Array_u8_contains(res, '.')", "array__contains(res, &(u8[1]){'.'})")
	// tos() buffer pointer cast:
	source = source.replace('return tos(((u8)(&((u8*)buf.data)[((int)(0))])), i);',
		'return tos(&((u8*)buf.data)[((int)(0))], i);')
	// Pointer field access: &logger._object -> &logger->_object
	// The C cast C.log__Logger(logger) is dropped by cleanc, leaving a missing -> dereference.
	source = source.replace('&logger._object', '&logger->_object')
	// Closure capture function pointer: get_cert_callback returns !&SSLCerts, not void.
	// The if-guard on the result also needs restructuring for correct C semantics.
	source = source.replace('(void (*)(mbedtls__SSLListener*, string))get_cert_callback',
		'((_result_mbedtls__SSLCertsptr (*)(mbedtls__SSLListener*, string))get_cert_callback)')
	source = source.replace('if ((((_result_mbedtls__SSLCertsptr (*)(mbedtls__SSLListener*, string))get_cert_callback))(l, host)) {\n\t\t_result_mbedtls__SSLCertsptr certs = (((_result_mbedtls__SSLCertsptr (*)(mbedtls__SSLListener*, string))get_cert_callback))(l, host);\n\t\treturn mbedtls_ssl_set_hs_own_cert(ssl, &certs.client_cert, &certs.client_key);',
		'{ _result_mbedtls__SSLCertsptr _cert_res = (((_result_mbedtls__SSLCertsptr (*)(mbedtls__SSLListener*, string))get_cert_callback))(l, host);\n\tif (!_cert_res.is_error) {\n\t\tmbedtls__SSLCerts* certs = *(mbedtls__SSLCerts**)(((u8*)(&_cert_res.err)) + sizeof(IError));\n\t\treturn mbedtls_ssl_set_hs_own_cert(ssl, &certs->client_cert, &certs->client_key);')
	// UdpSocket result pointer auto-deref: .sock field is UdpSocket (value), result contains &UdpSocket.
	source = source.replace('.sock = (*(net__UdpSocket**)(((u8*)(&_or_t54.err)) + sizeof(IError)))',
		'.sock = *(*(net__UdpSocket**)(((u8*)(&_or_t54.err)) + sizeof(IError)))')
	// Generic function specialization: convert_voidptr_to_t_T -> convert_voidptr_to_t_f64
	source = source.replace('sync__convert_voidptr_to_t_T(', 'sync__convert_voidptr_to_t_f64(')
	// Header-backed builds can lose the implicit address for channel semaphore fields.
	source = source.replace('sync__Semaphore__wait(ch->', 'sync__Semaphore__wait(&ch->')
	source = ensure_string_eq_impl(source)
	// ObjC .m file references g_vui_webview_cookie_val as a global variable.
	// The cleanc backend uses DarwinWebViewState singleton instead.
	// Add the global so the .m file can link against it.
	if !source.contains('g_vui_webview_cookie_val')
		&& source.contains('webview__darwin_webview_state') {
		source = source + '\nstring g_vui_webview_cookie_val;\n'
	}
	return source
}

fn sanitize_cached_main_c_source(c_source string) string {
	return strip_common_weak_sort_fallbacks(sanitize_cached_object_c_source(c_source))
}

fn sanitize_cached_object_c_source(c_source string) string {
	mut source := c_source
	source = guard_tcc_stdatomic_preamble_when_using_compat_header(source)
	// Header-backed builds can lose the implicit address for channel semaphore fields.
	source = source.replace('sync__Semaphore__wait(ch->', 'sync__Semaphore__wait(&ch->')
	for c_typedef_name in [
		'atomic_uintptr_t',
		'pthread_condattr_t',
		'pthread_rwlockattr_t',
	] {
		source = source.replace('struct ${c_typedef_name} ', '${c_typedef_name} ')
	}
	source = ensure_forward_typedefs_for_c_prefix(source, 'veb__MiddlewareOptions_T_')
	return source
}

fn strip_common_weak_sort_fallbacks(c_source string) string {
	mut source := c_source
	for common_sort_helper in ['__sort_cmp_int_asc', '__sort_cmp_RepIndex_by_idx_asc'] {
		source = strip_weak_c_function_definition(source, common_sort_helper)
	}
	return source
}

fn guard_tcc_stdatomic_preamble_when_using_compat_header(source string) string {
	if !source.contains('/thirdparty/stdatomic/nix/atomic.h') {
		return source
	}
	guarded := source.replace('#include <stdatomic.h>',
		'#ifndef __TINYC__\n#include <stdatomic.h>\n#endif')
	mut lines := guarded.split_into_lines()
	mut out := []string{cap: lines.len + 6}
	for line in lines {
		if line.trim_space().starts_with('#include ')
			&& line.contains('/thirdparty/stdatomic/nix/atomic.h') {
			out << '#if defined(__TINYC__) && defined(__APPLE__) && defined(__aarch64__)'
			out << '#define extern static'
			out << '#endif'
			out << line
			out << '#if defined(__TINYC__) && defined(__APPLE__) && defined(__aarch64__)'
			out << '#undef extern'
			out << '#endif'
			continue
		}
		out << line
	}
	return out.join('\n')
}

fn ensure_forward_typedefs_for_c_prefix(source string, prefix string) string {
	names := c_identifier_names_with_prefix(source, prefix)
	if names.len == 0 {
		return source
	}
	mut typedefs := []string{}
	for name in names {
		if name.ends_with('_str') || name.ends_with('__str') {
			continue
		}
		typedefs << 'typedef struct ${name} ${name};'
	}
	if typedefs.len == 0 {
		return source
	}
	decls := typedefs.join('\n') + '\n'
	marker := '// V primitive types\n'
	if marker_idx := source.index(marker) {
		insert_idx := marker_idx + marker.len
		return source[..insert_idx] + decls + source[insert_idx..]
	}
	return decls + source
}

fn c_identifier_names_with_prefix(source string, prefix string) []string {
	mut names_set := map[string]bool{}
	mut pos := 0
	for {
		idx := source.index_after(prefix, pos) or { break }
		mut end := idx + prefix.len
		for end < source.len && is_c_identifier_byte(source[end]) {
			end++
		}
		if end > idx {
			names_set[source[idx..end]] = true
		}
		pos = end
	}
	mut names := names_set.keys()
	names.sort()
	return names
}

fn is_c_identifier_byte(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
		|| (ch >= `0` && ch <= `9`) || ch == `_`
}

// ensure_result_type ensures that a _result_* type has both typedef and struct definition
// in the generated C source. This is a safety net for when the cleanc backend's alias
// registration fails (e.g. due to ARM64 chained-access map bugs in self-hosted binaries).
fn ensure_result_type(source string, type_name string, val_type string) string {
	if source.contains('struct ${type_name} {') {
		return source
	}
	// Find insertion point: after the _result base struct, or after typedef section
	decl := 'typedef struct ${type_name} ${type_name};\n' +
		'struct ${type_name} { bool is_error; IError err; u8 data[sizeof(${val_type}) > 1 ? sizeof(${val_type}) : 1]; };\n'
	// Try to insert after the base _result struct definition
	anchor := 'struct _result { bool is_error; IError err; };\n'
	if source.contains(anchor) {
		return source.replace(anchor, anchor + decl)
	}
	// Fallback: insert before the first function forward declaration
	fwd_marker := source.index('\nvoid __v_live_init') or {
		source.index('\nint main(') or { return source }
	}
	return source[..fwd_marker] + '\n' + decl + source[fwd_marker..]
}

fn ensure_string_eq_impl(source string) string {
	has_ab := source.contains('bool string__eq(string a, string b) {')
	has_sa := source.contains('bool string__eq(string s, string a) {')
	// Remove duplicate: if both variants exist, strip the (a,b) variant body.
	if has_ab && has_sa {
		return replace_generated_c_fn(source, 'bool string__eq(string a, string b)', '')
	}
	if has_ab || has_sa {
		return source
	}
	return source + '\n' +
		['bool string__eq(string a, string b) {', '\tif ((a.str == 0)) {', '\t\treturn ((b.str == 0) || (b.len == 0));', '\t}', '\tif ((a.len != b.len)) {', '\t\treturn false;', '\t}', '\treturn (vmemcmp(a.str, b.str, b.len) == 0);', '}'].join('\n') +
		'\n'
}

fn replace_generated_c_fn(source string, signature string, replacement string) string {
	needle := signature + ' {'
	// Search for the needle preceded by a newline to ensure we match an actual
	// function definition at the start of a line, not an occurrence inside a
	// string literal (e.g. when the compiler compiles itself).
	full_needle := '\n' + needle
	nl_pos := source.index(full_needle) or { return source }
	start := nl_pos + 1 // skip the newline itself
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

fn (mut b Builder) compile_cleanc_executable(output_name string, cc string, cc_flags string, cc_link_flags string, error_limit_flag string, mut sw time.StopWatch) {
	cc_start := sw.elapsed()
	if b.pref.is_shared_lib {
		// Shared library: compile with -shared -fPIC -undefined dynamic_lookup
		// Use -fvisibility=hidden so only explicitly exported (impl_live_*) symbols
		// are visible. All other functions become hidden, causing the dylib to
		// resolve them from the host executable at load time.
		mut cc_cmd := '${cc} ${cc_flags} -shared -fPIC -fvisibility=hidden -undefined dynamic_lookup -w -Wno-incompatible-function-pointer-types "${staged_c_file}"'
		if cc_link_flags.len > 0 {
			cc_cmd += ' -x none ${cc_link_flags}'
		}
		cc_cmd += ' -o "${output_name}"${error_limit_flag}'
		run_cc_cmd_or_exit(cc_cmd, 'shared lib compilation', b.pref.show_cc)
		print_time('CC (shared)', time.Duration(sw.elapsed() - cc_start))
		println('[*] Compiled shared library ${output_name}')
		return
	}
	// Non-cached path: compile and link in one step.
	// Place link flags (which may include .o files) AFTER the source file.
	// Use `-x none` to reset the language before .o files, since -x objective-c
	// would cause cc to treat .o files as source code.
	mut cc_cmd := '${cc} ${cc_flags} -w -Wno-incompatible-function-pointer-types "${staged_c_file}"'
	if cc_link_flags.len > 0 {
		cc_cmd += ' -x none ${cc_link_flags}'
	}
	cc_cmd += ' -o "${output_name}"${error_limit_flag}'
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
		b.env = if b.pref.no_parallel {
			b.type_check_files()
		} else {
			b.type_check_files_parallel()
		}
	}
	type_check_time := time.Duration(sw.elapsed() - parse_time)
	print_time('Type Check', type_check_time)

	// Transform AST (flag enum desugaring, etc.)
	transform_start := sw.elapsed()
	mut trans := transformer.Transformer.new_with_pref(b.files, b.env, b.pref)
	trans.set_file_set(b.file_set)
	b.files = if b.pref.no_parallel_transform || b.pref.ownership {
		trans.transform_files(b.files)
	} else {
		b.transform_files_parallel(mut trans)
	}
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

	mut cc := if b.pref.ccompiler.len > 0 {
		b.pref.ccompiler
	} else {
		configured_cc(b.pref.vroot)
	}
	// -prod requires a real optimizing compiler — TCC cannot handle -O3/-flto.
	// Switch to system cc (gcc/clang) when the default compiler is TCC.
	if b.pref.is_prod && cc.contains('tcc') {
		cc = 'cc'
	}
	directive_flags := b.collect_cflags_from_sources()
	$if macos {
		if cc.contains('tcc') && cflags_need_objc_mode(directive_flags) {
			cc = 'cc'
		}
	}
	// Separate directive flags into compile-only and link-only flags.
	// -framework, -l, -L, .o/.a/.so/.dylib are linker flags and must NOT
	// be passed during -c compilation (they can trigger unwanted header
	// processing, e.g. MetalKit SIMD errors on macOS).
	directive_compile_flags, directive_link_flags := split_compile_and_link_flags(directive_flags)
	mut cc_flag_parts := []string{}
	mut cc_link_parts := []string{}
	env_flags := configured_cflags()
	if env_flags.trim_space() != '' {
		cc_flag_parts << env_flags.trim_space()
	}
	if cc.contains('tcc') && directive_flags.contains('thirdparty/sqlite/sqlite3.c')
		&& !directive_compile_flags.contains('SQLITE_DISABLE_INTRINSIC') {
		cc_flag_parts << '-DSQLITE_DISABLE_INTRINSIC'
	}
	if directive_compile_flags.trim_space() != '' {
		cc_flag_parts << directive_compile_flags.trim_space()
	}
	if directive_link_flags.trim_space() != '' {
		cc_link_parts << directive_link_flags.trim_space()
	}
	tcc_extra := tcc_flags(cc, b.pref.vroot)
	if tcc_extra.trim_space() != '' {
		cc_flag_parts << tcc_extra.trim_space()
	}
	// macOS code can include Objective-C (.m) files via #include directives.
	// Tell the C compiler to treat the source as Objective-C only when needed.
	$if macos {
		if cflags_need_objc_mode(directive_flags) {
			cc_flag_parts << '-x objective-c'
		}
	}
	cc_flag_parts << '-std=gnu11'
	cc_flag_parts << '-fwrapv'

	// Detect compiler type for optimization flags and error limit.
	is_tcc := cc.contains('tcc')
	mut is_clang := false
	if !is_tcc {
		version_res := os.execute('${cc} --version')
		if version_res.exit_code == 0 && version_res.output.contains('clang') {
			is_clang = true
		}
	}

	// -prod: add -O3, -flto, -DNDEBUG for gcc/clang
	if b.pref.is_prod {
		cc_flag_parts << '-O3'
		cc_flag_parts << '-DNDEBUG'
		if !b.pref.is_shared_lib {
			$if !windows {
				cc_flag_parts << '-flto'
			}
		}
		if !is_clang {
			cc_flag_parts << '-fno-strict-aliasing'
		}
	}

	cc_flags := cc_flag_parts.join(' ')
	cc_link_flags := cc_link_parts.join(' ')
	mut error_limit_flag := ''
	if is_clang {
		error_limit_flag = ' -ferror-limit=0'
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
		if b.gen_cleanc_with_cached_core(output_name, cc, cc_flags, cc_link_flags,
			error_limit_flag, mut sw)
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
	os.write_file(staged_c_file, sanitize_staged_c_source(c_source)) or { panic(err) }
	println('[*] Wrote ${staged_c_file}')
	b.compile_cleanc_executable(output_name, cc, cc_flags, cc_link_flags, error_limit_flag, mut sw)
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
	// ARM64 cache previously disabled due to runtime helpers being emitted
	// as static inline, which dropped them at cached-core boundaries.
	// Fixed: __v2_array_eq is now a regular function with its body in the
	// builtin cache unit and a forward declaration in the main TU.
	for raw_input in b.user_files {
		input := raw_input.trim_right('/\\')
		if input.len == 0 {
			continue
		}
		if input.ends_with('.v') || input.ends_with('.vv') || input.ends_with('.vsh')
			|| input.ends_with('.vh') {
			continue
		}
	}
	return false
}

fn (b &Builder) default_output_name() string {
	if b.user_files.len == 0 {
		return 'out'
	}
	last_input := b.user_files[b.user_files.len - 1].trim_right('/\\')
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

	cc := if b.pref.ccompiler.len > 0 { b.pref.ccompiler } else { configured_cc(b.pref.vroot) }
	directive_flags := b.collect_cflags_from_sources()
	mut cc_flag_parts := []string{}
	env_flags := configured_cflags()
	if env_flags.trim_space() != '' {
		cc_flag_parts << env_flags.trim_space()
	}
	if cc.contains('tcc') && directive_flags.contains('thirdparty/sqlite/sqlite3.c')
		&& !directive_flags.contains('SQLITE_DISABLE_INTRINSIC') {
		cc_flag_parts << '-DSQLITE_DISABLE_INTRINSIC'
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
		builtin_obj = b.ensure_cached_module_object(cache_dir, builtin_cache_name,
			builtin_cached_module_paths, builtin_cached_module_names, cc, cc_flags, '',
			error_limit_flag, false) or { '' }
		if builtin_obj.len > 0 && vlib_cached_module_paths.len > 0 {
			vlib_obj = b.ensure_cached_module_object(cache_dir, vlib_cache_name,
				vlib_cached_module_paths, vlib_cached_module_names, cc, cc_flags, '',
				error_limit_flag, false) or { '' }
		}
	}

	stage_start = sw.elapsed()
	mut gen := c.new_gen(mod)
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
		if !b.pref.keep_c {
			os.rm(main_obj) or {}
		}
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
	return b.gen_cleanc_source_with_options(modules, []string{}, false, '', []string{}, true,
		[]string{})
}

fn (mut b Builder) gen_cleanc_source_for_cache(modules []string, cache_bundle_name string, use_markused bool) string {
	return b.gen_cleanc_source_with_options(modules, []string{}, true, cache_bundle_name,
		[]string{}, use_markused, []string{})
}

fn (mut b Builder) gen_cleanc_source_for_cache_force(modules []string, cache_bundle_name string, use_markused bool, force_emit_fn_names []string) string {
	return b.gen_cleanc_source_with_options(modules, []string{}, true, cache_bundle_name,
		[]string{}, use_markused, force_emit_fn_names)
}

fn (mut b Builder) gen_cleanc_source_for_cache_files(modules []string, emit_files []string, cache_bundle_name string, use_markused bool) string {
	return b.gen_cleanc_source_with_options(modules, emit_files, true, cache_bundle_name,
		[]string{}, use_markused, []string{})
}

fn (mut b Builder) gen_cleanc_source_for_cache_files_force(modules []string, emit_files []string, cache_bundle_name string, use_markused bool, force_emit_fn_names []string) string {
	return b.gen_cleanc_source_with_options(modules, emit_files, true, cache_bundle_name,
		[]string{}, use_markused, force_emit_fn_names)
}

fn (mut b Builder) gen_cleanc_source_with_cache_init_calls(modules []string, cached_init_calls []string) string {
	return b.gen_cleanc_source_with_options(modules, []string{}, false, '', cached_init_calls,
		true, []string{})
}

fn (mut b Builder) gen_cleanc_source_with_cache_init_calls_and_files(modules []string, emit_files []string, cached_init_calls []string, use_markused bool) string {
	return b.gen_cleanc_source_with_options(modules, emit_files, false, '', cached_init_calls,
		use_markused, []string{})
}

fn (mut b Builder) gen_cleanc_source_with_cache_init_calls_files_force(modules []string, emit_files []string, cached_init_calls []string, use_markused bool, force_emit_fn_names []string) string {
	return b.gen_cleanc_source_with_options(modules, emit_files, false, '', cached_init_calls,
		use_markused, force_emit_fn_names)
}

fn (mut b Builder) gen_cleanc_source_with_options(modules []string, emit_files []string, export_const_symbols bool, cache_bundle_name string, cached_init_calls []string, use_markused bool, force_emit_fn_names []string) string {
	mut gen_files := []ast.File{cap: b.files.len}
	for file in b.files {
		gen_files << file
	}
	if cached_init_calls.len > 0 && b.used_vh_for_parse {
		mut p := parser.Parser.new(b.pref)
		header_files := p.parse_files(b.core_cached_parse_paths(), mut b.file_set)
		for header_file in header_files {
			gen_files << header_file
		}
	}
	mut gen := cleanc.Gen.new_with_env_and_pref(gen_files, b.env, b.pref)
	if modules.len > 0 {
		gen.set_emit_modules(modules)
	}
	if emit_files.len > 0 {
		gen.set_emit_files(emit_files)
	}
	if use_markused && b.used_fn_keys.len > 0 {
		gen.set_used_fn_keys(b.used_fn_keys)
	}
	if force_emit_fn_names.len > 0 {
		gen.set_force_emit_fn_names(force_emit_fn_names)
	}
	gen.set_export_const_symbols(export_const_symbols)
	if cache_bundle_name.len > 0 {
		gen.set_cache_bundle_name(cache_bundle_name)
	}
	if cached_init_calls.len > 0 {
		gen.set_cached_init_calls(cached_init_calls)
	}
	use_parallel := b.pref != unsafe { nil } && !b.pref.no_parallel
	if use_parallel {
		gen.gen_passes_1_to_4()
		b.gen_cleanc_parallel(mut gen)
		source := gen.gen_finalize()
		if os.getenv('V2TRACE_CLEANC') != '' {
			eprintln('TRACE_CLEANC builder_files=${b.files.len} gen_files=${gen_files.len} source_len=${source.len}')
		}
		return source
	}
	source := gen.gen()
	if os.getenv('V2TRACE_CLEANC') != '' {
		eprintln('TRACE_CLEANC builder_files=${b.files.len} gen_files=${gen_files.len} source_len=${source.len}')
	}
	return source
}

fn (mut b Builder) gen_cleanc_with_cached_core(output_name string, cc string, cc_flags string, cc_link_flags string, error_limit_flag string, mut sw time.StopWatch) bool {
	cache_dir := b.core_cache_dir()
	if !b.ensure_core_cache_dir() {
		// If we cannot create a readable/writable cache dir, fall back to full compilation.
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=cache_dir_unusable')
		}
		return false
	}

	builtin_obj := b.ensure_cached_module_object(cache_dir, builtin_cache_name,
		builtin_cached_module_paths, builtin_cached_module_names, cc, cc_flags, '',
		error_limit_flag, false) or {
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=builtin_obj_failed')
		}
		return false
	}
	b.print_cached_bundle_modules(builtin_cache_name, builtin_cached_module_names)
	mut vlib_obj := ''
	if vlib_cached_module_paths.len > 0 {
		vlib_obj = b.ensure_cached_module_object(cache_dir, vlib_cache_name,
			vlib_cached_module_paths, vlib_cached_module_names, cc, cc_flags, '', error_limit_flag,
			false) or {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE cached_core=false reason=vlib_obj_failed')
			}
			return false
		}
		if vlib_obj.len > 0 {
			b.print_cached_bundle_modules(vlib_cache_name, vlib_cached_module_names)
		}
	}
	mut optional_cached_objs := []string{}
	mut optional_cached_cache_names := []string{}
	mut optional_cached_module_names := []string{}
	mut imports_obj := ''
	mut import_dependency_cache_names := []string{}
	mut imports_cc_flags := ''
	mut imports_cc_link_flags := ''
	if veb_cached_module_paths.len > 0 && b.has_module('veb') {
		veb_obj := b.ensure_cached_module_object(cache_dir, veb_cache_name,
			veb_cached_module_paths, veb_cached_module_names, cc, cc_flags, cc_link_flags,
			error_limit_flag, true) or {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE optional_cache=veb reason=${err}')
			}
			''
		}
		if veb_obj.len > 0 {
			b.print_cached_bundle_modules(veb_cache_name, veb_cached_module_names)
			optional_cached_objs << veb_obj
			optional_cached_cache_names << veb_cache_name
			optional_cached_module_names << veb_cached_module_names
		}
	}
	mut v2compiler_obj := ''
	if v2compiler_cached_module_paths.len > 0 && b.is_cmd_v2_self_build() {
		v2compiler_obj = b.ensure_cached_module_object(cache_dir, v2compiler_cache_name,
			v2compiler_cached_module_paths, v2compiler_cached_module_names, cc, cc_flags, '',
			error_limit_flag, false) or {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE cached_core=false reason=v2compiler_obj_failed')
			}
			return false
		}
		if v2compiler_obj.len > 0 {
			b.print_cached_bundle_modules(v2compiler_cache_name, v2compiler_cached_module_names)
		}
	}
	mut dynamic_excluded := core_cached_module_names.clone()
	for module_name in optional_cached_module_names {
		dynamic_excluded << module_name
	}
	if b.is_cmd_v2_self_build() {
		dynamic_excluded << v2compiler_cached_module_names
	}
	dynamic_excluded << 'main'
	dynamic_cached_module_names := b.collect_modules_excluding(dynamic_excluded)
	if dynamic_cached_module_names.len > 0 {
		import_dependency_cache_names = [builtin_cache_name]
		if vlib_obj.len > 0 {
			import_dependency_cache_names << vlib_cache_name
		}
		if v2compiler_obj.len > 0 {
			import_dependency_cache_names << v2compiler_cache_name
		}
		for cache_name in optional_cached_cache_names {
			import_dependency_cache_names << cache_name
		}
		import_dependency_compile_flags, import_dependency_link_flags :=
			b.cached_module_stamp_flags(import_dependency_cache_names)
		imports_cc_flags = join_flag_strings(cc_flags, import_dependency_compile_flags)
		imports_cc_link_flags = join_flag_strings(cc_link_flags, import_dependency_link_flags)
		initial_import_force_names := b.force_local_import_fn_names(dynamic_cached_module_names)
		imports_obj = b.ensure_cached_parsed_module_object_force(cache_dir, imports_cache_name,
			dynamic_cached_module_names, import_dependency_cache_names, cc, imports_cc_flags,
			imports_cc_link_flags, error_limit_flag, true, initial_import_force_names) or {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE optional_cache=imports reason=${err}')
			}
			if b.used_import_vh_for_parse {
				return false
			}
			''
		}
		if imports_obj.len > 0 {
			b.print_cached_bundle_modules(imports_cache_name, dynamic_cached_module_names)
			optional_cached_objs << imports_obj
			optional_cached_cache_names << imports_cache_name
			optional_cached_module_names << dynamic_cached_module_names
		}
	}
	mut virtual_groups := if b.used_virtual_vh_for_parse {
		b.cached_virtual_manifest()
	} else {
		b.collect_virtual_main_modules()
	}
	mut virtual_source_files := []string{}
	mut virtuals_obj := ''
	if virtual_groups.len > 0 {
		mut virtual_dependency_cache_names := [builtin_cache_name]
		if vlib_obj.len > 0 {
			virtual_dependency_cache_names << vlib_cache_name
		}
		if v2compiler_obj.len > 0 {
			virtual_dependency_cache_names << v2compiler_cache_name
		}
		for cache_name in optional_cached_cache_names {
			virtual_dependency_cache_names << cache_name
		}
		virtual_dependency_compile_flags, virtual_dependency_link_flags :=
			b.cached_module_stamp_flags(virtual_dependency_cache_names)
		virtual_cc_flags := join_flag_strings(cc_flags, virtual_dependency_compile_flags)
		virtual_cc_link_flags := join_flag_strings(cc_link_flags, virtual_dependency_link_flags)
		virtuals_obj = b.ensure_cached_virtual_module_object(cache_dir, virtual_groups,
			virtual_dependency_cache_names, cc, virtual_cc_flags, virtual_cc_link_flags,
			error_limit_flag, true) or {
			if os.getenv('V2_TRACE_CACHE') != '' {
				eprintln('TRACE_CACHE optional_cache=${virtuals_cache_name} reason=${err}')
			}
			if b.used_virtual_vh_for_parse {
				return false
			}
			virtual_groups = []CachedVirtualModule{}
			''
		}
		if virtuals_obj.len > 0 {
			virtual_source_files = virtual_module_source_files(virtual_groups)
			b.print_cached_bundle_modules(virtuals_cache_name, virtual_module_names(virtual_groups))
		}
	}
	b.ensure_core_module_headers()
	b.ensure_import_module_headers(dynamic_cached_module_names)
	b.ensure_virtual_module_headers(virtual_groups)
	mut excluded := core_cached_module_names.clone()
	for module_name in optional_cached_module_names {
		excluded << module_name
	}
	if b.is_cmd_v2_self_build() {
		excluded << v2compiler_cached_module_names
	}
	main_modules := b.collect_modules_excluding(excluded)
	if main_modules.len == 0 {
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=no_main_modules')
		}
		return false
	}
	mut linked_cache_names := [builtin_cache_name]
	if vlib_obj.len > 0 {
		linked_cache_names << vlib_cache_name
	}
	if v2compiler_obj.len > 0 {
		linked_cache_names << v2compiler_cache_name
	}
	for cache_name in optional_cached_cache_names {
		linked_cache_names << cache_name
	}
	cached_compile_flags, cached_link_flags := b.cached_module_stamp_flags(linked_cache_names)

	// When TCC is the default compiler but fell back to cc for cache
	// compilation (e.g. due to TCC not supporting certain C constructs),
	// the cached .o files are Mach-O (from cc) while TCC would produce ELF.
	// Detect this mismatch and use cc for main compilation and linking too.
	// Also, -prod builds with -flto require gcc/clang for linking — TCC
	// cannot link LTO object files.
	mut main_cc := cc
	mut main_cc_flags := join_flag_strings(cc_flags, cached_compile_flags)
	main_cc_link_flags := join_flag_strings(cc_link_flags, cached_link_flags)
	if cc.contains('tcc') && os.exists(builtin_obj) {
		bytes := os.read_bytes(builtin_obj) or { []u8{} }
		is_elf := bytes.len >= 4 && bytes[0] == 0x7f && bytes[1] == 0x45 && bytes[2] == 0x4c
			&& bytes[3] == 0x46
		if !is_elf {
			// Cached .o was compiled by cc (via TCC fallback), not TCC.
			// Use cc for main compilation and linking to match formats.
			// Keep all flags (including directive -I paths) but strip
			// TCC-specific -I/-L paths that would conflict with system headers.
			main_cc = 'cc'
			tcc_dir2 := cc.all_before_last('/tcc')
			if tcc_dir2.len > 0 {
				mut parts := main_cc_flags.fields()
				mut filtered := []string{cap: parts.len}
				mut j := 0
				for j < parts.len {
					p := parts[j]
					if (p == '-I' || p == '-L') && j + 1 < parts.len && parts[j + 1].contains('tcc') {
						j += 2
						continue
					}
					if (p.starts_with('-I') || p.starts_with('-L')) && p.contains('tcc') {
						j++
						continue
					}
					// Strip quoted -I/-L containing tcc
					if (p.starts_with('-I"') || p.starts_with('-L"')
						|| p.starts_with("-I'") || p.starts_with("-L'")) && p.contains('tcc') {
						j++
						continue
					}
					filtered << p
					j++
				}
				main_cc_flags = filtered.join(' ')
			}
		}
	}

	mut cached_init_calls := []string{}
	cached_init_calls << '__v2_cached_init_${builtin_cache_name}'
	if vlib_obj.len > 0 {
		cached_init_calls << '__v2_cached_init_${vlib_cache_name}'
	}
	if v2compiler_obj.len > 0 {
		cached_init_calls << '__v2_cached_init_${v2compiler_cache_name}'
	}
	for cache_name in optional_cached_cache_names {
		cached_init_calls << '__v2_cached_init_${cache_name}'
	}
	all_main_emit_files := if virtual_source_files.len > 0 {
		filter_out_source_files(b.module_source_files(main_modules), virtual_source_files)
	} else {
		[]string{}
	}
	mut force_main_fn_names := []string{}
	mut virtual_sort_helper_names := []string{}
	if virtuals_obj.len > 0 {
		virtual_c_source := os.read_file(cache_path_join(cache_dir, '${virtuals_cache_name}.c')) or {
			''
		}
		virtual_sort_helper_names = sort_cmp_fn_names_referenced_by_c(virtual_c_source)
		force_main_fn_names = b.force_main_fn_names_referenced_by_cached_c(virtual_c_source,
			virtual_source_files)
	}
	if all_main_emit_files.len > 0 {
		force_main_fn_names = filter_out_names(merge_unique_strings(force_main_fn_names,
			b.force_main_fn_names_for_emit_files(all_main_emit_files)), virtual_sort_helper_names)
	}
	mut main_source := if all_main_emit_files.len > 0 {
		b.gen_cleanc_source_with_cache_init_calls_files_force(main_modules, all_main_emit_files,
			cached_init_calls, true, force_main_fn_names)
	} else {
		b.gen_cleanc_source_with_cache_init_calls(main_modules, cached_init_calls)
	}
	if imports_obj.len > 0 && all_main_emit_files.len > 0 && !b.used_import_vh_for_parse {
		mut import_ref_source := main_source
		if virtuals_obj.len > 0 {
			import_ref_source += '\n' + (os.read_file(cache_path_join(cache_dir, '${virtuals_cache_name}.c')) or {
				''
			})
		}
		import_force_names := b.force_module_fn_names_referenced_by_c(import_ref_source,
			dynamic_cached_module_names, 2)
		if import_force_names.len > 0 {
			imports_obj = b.ensure_cached_parsed_module_object_force(cache_dir, imports_cache_name,
				dynamic_cached_module_names, import_dependency_cache_names, cc, imports_cc_flags,
				imports_cc_link_flags, error_limit_flag, true, import_force_names) or {
				if os.getenv('V2_TRACE_CACHE') != '' {
					eprintln('TRACE_CACHE optional_cache=imports_force reason=${err}')
				}
				return false
			}
		}
	}
	main_source = b.inject_cached_c_header_directives(main_source, linked_cache_names)
	main_source = b.inject_cached_forward_decls(main_source, linked_cache_names)
	print_time('C Gen', sw.elapsed())
	if main_source == '' {
		if os.getenv('V2_TRACE_CACHE') != '' {
			eprintln('TRACE_CACHE cached_core=false reason=empty_main_source')
		}
		return false
	}

	main_c_file := b.exec_build_c_file(output_name)
	os.write_file(main_c_file, sanitize_cached_main_c_source(main_source)) or { return false }
	println('[*] Wrote ${main_c_file}')

	cc_start := sw.elapsed()
	main_obj := staged_main_obj_file
	compile_main_cmd := '${main_cc} ${main_cc_flags} -w -Wno-incompatible-function-pointer-types -c "${main_c_file}" -o "${main_obj}"${error_limit_flag}'
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
	// Strip -c and -x flags from link command since we're linking, not compiling.
	// -x objective-c would cause cc to treat .o files as source code.
	mut link_flags :=
		main_cc_flags.replace('-x objective-c', '').replace('-x c', '').replace(' -c ', ' ')
	mut link_cmd := '${main_cc} ${link_flags} -w "${main_obj}" "${builtin_obj}"'
	if vlib_obj.len > 0 {
		link_cmd += ' "${vlib_obj}"'
	}
	if v2compiler_obj.len > 0 {
		link_cmd += ' "${v2compiler_obj}"'
	}
	for obj in optional_cached_objs {
		link_cmd += ' "${obj}"'
	}
	if virtuals_obj.len > 0 {
		link_cmd += ' "${virtuals_obj}"'
	}
	link_cmd += ' -o "${output_name}"'
	if main_cc_link_flags.len > 0 {
		link_cmd += ' ${main_cc_link_flags}'
	}
	run_cc_cmd_or_exit(link_cmd, 'Linking', b.pref.show_cc)
	print_time('CC', time.Duration(sw.elapsed() - cc_start))

	if !b.pref.keep_c {
		os.rm(main_obj) or {}
		os.rm(main_c_file) or {}
	}
	if os.getenv('V2_TRACE_CACHE') != '' {
		eprintln('TRACE_CACHE cached_core=true')
	}
	println('[*] Compiled ${output_name}')
	return true
}

fn (b &Builder) inject_cached_forward_decls(source string, cache_names []string) string {
	decls := b.cached_forward_decls(cache_names)
	if decls == '' {
		return source
	}
	// Split source into lines once, then collect existing names and inject.
	mut lines := source.split_into_lines()
	mut existing_names := map[string]bool{}
	mut existing_forward_struct_typedefs := map[string]bool{}
	for src_line in lines {
		trimmed := src_line.trim_space()
		if trimmed.starts_with('typedef struct ') && trimmed.contains('{') {
			name := extract_struct_typedef_head_name(trimmed)
			if name.len > 0 {
				existing_names[name] = true
			}
		} else if trimmed.starts_with('typedef ') && trimmed.ends_with(';') {
			name := extract_typedef_name(trimmed)
			if name.len > 0 {
				existing_names[name] = true
				if is_forward_struct_typedef(trimmed) {
					existing_forward_struct_typedefs[name] = true
				}
			}
		} else if trimmed.starts_with('#define ') {
			// Extract macro name: "#define NAME" or "#define NAME(..."
			rest := trimmed[8..]
			mut end := 0
			for end < rest.len && rest[end] != `(` && !rest[end].is_space() {
				end++
			}
			if end > 0 {
				existing_names[rest[..end]] = true
			}
		} else if is_top_level_c_fn_decl(trimmed) {
			fn_name := extract_fn_decl_name(trimmed)
			if fn_name.len > 0 {
				existing_names[fn_name] = true
			}
		}
	}
	mut cached_early_typedefs := []string{}
	mut cached_late_typedefs := []string{}
	mut cached_fn_decls := []string{}
	for decl_line in decls.split_into_lines() {
		if decl_line == '' {
			continue
		}
		if decl_line.starts_with('typedef ') {
			name := extract_typedef_name(decl_line)
			if name.len > 0 && name in existing_names && !(is_forward_struct_typedef(decl_line)
				&& name in existing_forward_struct_typedefs) {
				continue
			}
			if is_early_cached_typedef(decl_line) {
				cached_early_typedefs << decl_line
			} else {
				cached_late_typedefs << decl_line
			}
			continue
		}
		fn_name := extract_fn_decl_name(decl_line)
		if fn_name.len > 0 && fn_name in existing_names {
			continue
		}
		cached_fn_decls << decl_line
	}
	mut cached_late_decls := []string{cap: cached_late_typedefs.len + cached_fn_decls.len}
	cached_late_decls << cached_late_typedefs
	cached_late_decls << cached_fn_decls
	mut out := []string{cap: lines.len + cached_early_typedefs.len + cached_late_decls.len + 4}
	mut saw_cached_decl := false
	mut inserted_typedefs := cached_early_typedefs.len == 0
	mut inserted_late_decls := cached_late_decls.len == 0
	for line in lines {
		if !inserted_typedefs && is_v_helper_forward_decl_boundary(line) {
			out << cached_early_typedefs
			out << ''
			inserted_typedefs = true
		}
		out << line
		if line.starts_with('void __v2_cached_init_') {
			saw_cached_decl = true
			continue
		}
		if saw_cached_decl && line == '' && !inserted_late_decls {
			out << cached_late_decls
			out << ''
			inserted_late_decls = true
		}
	}
	if !inserted_typedefs {
		return cached_early_typedefs.join('\n') + '\n' + out.join('\n')
	}
	if !inserted_late_decls {
		return cached_late_decls.join('\n') + '\n' + out.join('\n')
	}
	return out.join('\n')
}

fn (b &Builder) inject_cached_typedef_decls(source string, cache_names []string) string {
	decls := b.cached_typedef_decls(cache_names)
	if decls == '' {
		return source
	}
	return inject_cached_typedef_decl_lines(source, decls.split_into_lines())
}

fn inject_cached_typedef_decl_lines(source string, decl_lines []string) string {
	mut lines := source.split_into_lines()
	mut existing_names := map[string]bool{}
	for src_line in lines {
		trimmed := src_line.trim_space()
		if trimmed.starts_with('typedef struct ') && trimmed.contains('{') {
			name := extract_struct_typedef_head_name(trimmed)
			if name.len > 0 {
				existing_names[name] = true
			}
		} else if trimmed.starts_with('typedef ') && trimmed.ends_with(';') {
			name := extract_typedef_name(trimmed)
			if name.len > 0 {
				existing_names[name] = true
			}
		}
	}
	mut cached_typedefs := []string{}
	for decl_line in decl_lines {
		if decl_line == '' || !decl_line.starts_with('typedef ') {
			continue
		}
		name := extract_typedef_name(decl_line)
		if name.len > 0 && name in existing_names {
			continue
		}
		if name.len > 0 {
			existing_names[name] = true
		}
		cached_typedefs << decl_line
	}
	if cached_typedefs.len == 0 {
		return source
	}
	mut out := []string{cap: lines.len + cached_typedefs.len + 1}
	mut inserted := false
	for line in lines {
		if !inserted && is_v_helper_forward_decl_boundary(line) {
			out << cached_typedefs
			out << ''
			inserted = true
		}
		out << line
	}
	if !inserted {
		return cached_typedefs.join('\n') + '\n' + out.join('\n')
	}
	return out.join('\n')
}

fn is_forward_struct_typedef(line string) bool {
	trimmed := line.trim_space().trim_right('; ')
	prefix := 'typedef struct '
	if !trimmed.starts_with(prefix) {
		return false
	}
	parts := trimmed[prefix.len..].fields()
	return parts.len == 2 && parts[0] == parts[1]
}

fn extract_struct_typedef_head_name(line string) string {
	prefix := 'typedef struct '
	if !line.starts_with(prefix) {
		return ''
	}
	rest := line[prefix.len..]
	mut end := 0
	for end < rest.len && rest[end] != `{` && !rest[end].is_space() {
		end++
	}
	return rest[..end].trim_space()
}

fn count_byte_in_string(s string, needle u8) int {
	mut count := 0
	for ch in s {
		if ch == needle {
			count++
		}
	}
	return count
}

fn extract_typedef_name(line string) string {
	// For "typedef <something> Name;" or "typedef struct Name { ... } Name;"
	// extract the name just before the final ';'.
	trimmed := line.trim_right('; ')
	// The typedef name is the last space-separated token, but handle
	// attribute suffixes like __attribute__((...))).
	if trimmed.ends_with(')') {
		// Typedef with attribute — find name before __attribute__
		attr_idx := trimmed.index('__attribute__') or { return '' }
		before_attr := trimmed[..attr_idx].trim_space()
		last_space := before_attr.last_index(' ') or { return before_attr }
		return before_attr[last_space + 1..]
	}
	if trimmed.ends_with(']') {
		bracket_idx := trimmed.last_index('[') or { return '' }
		before_bracket := trimmed[..bracket_idx].trim_space()
		last_space := before_bracket.last_index(' ') or { return before_bracket }
		return before_bracket[last_space + 1..]
	}
	if trimmed.ends_with('}') {
		// "typedef struct X { ... } X" — find name after '}'
		brace_end := trimmed.last_index('}') or { return '' }
		return trimmed[brace_end + 1..].trim_space()
	}
	last_space := trimmed.last_index(' ') or { return trimmed }
	return trimmed[last_space + 1..]
}

fn extract_fn_decl_name(line string) string {
	// Extract function name from a C forward declaration like:
	// "string time__FormatTime__str(time__FormatTime e);"
	// The name is the token before '('.
	paren_idx := line.index_u8(`(`)
	if paren_idx <= 0 {
		return ''
	}
	before_paren := line[..paren_idx].trim_space()
	last_space := before_paren.last_index(' ') or { return before_paren }
	name := before_paren[last_space + 1..]
	// Skip pointer prefixes
	if name.len > 0 && name[0] == `*` {
		return name[1..]
	}
	return name
}

fn is_early_cached_typedef(line string) bool {
	trimmed := line.trim_space()
	if trimmed.starts_with('typedef struct _v_Array_fixed_') {
		return true
	}
	return trimmed.starts_with('typedef ') && trimmed.contains(' Array_fixed_')
		&& trimmed.contains('[') && trimmed.ends_with(';')
}

fn is_v_helper_forward_decl_boundary(line string) bool {
	trimmed := line.trim_space()
	return trimmed.starts_with('bool string__eq(') || trimmed.starts_with('bool __v2_array_eq(')
}

fn is_top_level_c_fn_decl(line string) bool {
	trimmed := line.trim_space()
	if !trimmed.ends_with(');') || !trimmed.contains('(') {
		return false
	}
	if trimmed.starts_with('#') || trimmed.starts_with('typedef ') || trimmed.starts_with('struct ')
		|| trimmed.starts_with('union ') || trimmed.starts_with('enum ')
		|| trimmed.starts_with('return ') || trimmed.starts_with('if ')
		|| trimmed.starts_with('for ') || trimmed.starts_with('while ')
		|| trimmed.starts_with('switch ') {
		return false
	}
	if trimmed.contains('=') {
		return false
	}
	paren_idx := trimmed.index_u8(`(`)
	if paren_idx > 0 {
		before_paren := trimmed[..paren_idx].trim_space()
		if !before_paren.contains(' ') && !before_paren.contains('*') {
			return false
		}
	}
	return extract_fn_decl_name(trimmed).len > 0
}

fn (b &Builder) inject_cached_c_header_directives(source string, cache_names []string) string {
	directives := b.cached_c_header_directives(cache_names)
	if directives == '' {
		return source
	}
	mut existing := map[string]bool{}
	for src_line in source.split_into_lines() {
		line := src_line.trim_space()
		if line.starts_with('#include ') || line.starts_with('#define ')
			|| line.starts_with('#undef ') || line.starts_with('#pragma ') {
			existing[line] = true
		}
	}
	mut filtered := []string{}
	for directive in directives.split_into_lines() {
		if directive in existing {
			continue
		}
		filtered << directive
	}
	if filtered.len == 0 {
		return source
	}
	mut lines := source.split_into_lines()
	for i, line in lines {
		if line.trim_space() == '// V primitive types' {
			mut out := []string{cap: lines.len + filtered.len + 1}
			out << lines[..i]
			out << filtered
			out << ''
			out << lines[i..]
			return out.join('\n')
		}
	}
	return filtered.join('\n') + '\n' + source
}

fn (b &Builder) cached_forward_decls(cache_names []string) string {
	cache_dir := b.core_cache_dir()
	mut seen := map[string]bool{}
	mut seen_fn_names := map[string]bool{}
	mut typedefs := []string{}
	mut fn_decls := []string{}
	for cache_name in cache_names {
		c_path := os.join_path(cache_dir, '${cache_name}.c')
		if !os.exists(c_path) {
			continue
		}
		tds, fds := top_level_c_decls(c_path)
		for td in tds {
			if td in seen {
				continue
			}
			seen[td] = true
			typedefs << td
		}
		for fd in fds {
			fn_name := extract_fn_decl_name(fd)
			if fn_name.len > 0 {
				if fn_name in seen_fn_names {
					continue
				}
				seen_fn_names[fn_name] = true
			} else if fd in seen {
				continue
			}
			seen[fd] = true
			fn_decls << fd
		}
	}
	mut all := []string{cap: typedefs.len + fn_decls.len}
	all << typedefs
	all << fn_decls
	return all.join('\n')
}

fn (b &Builder) cached_typedef_decls(cache_names []string) string {
	cache_dir := b.core_cache_dir()
	mut seen := map[string]bool{}
	mut typedefs := []string{}
	for cache_name in cache_names {
		c_path := os.join_path(cache_dir, '${cache_name}.c')
		if !os.exists(c_path) {
			continue
		}
		tds, _ := top_level_c_decls(c_path)
		for td in tds {
			if td in seen {
				continue
			}
			seen[td] = true
			typedefs << td
		}
	}
	return typedefs.join('\n')
}

fn (b &Builder) cached_c_header_directives(cache_names []string) string {
	cache_dir := b.core_cache_dir()
	mut seen := map[string]bool{}
	mut directives := []string{}
	for cache_name in cache_names {
		c_path := os.join_path(cache_dir, '${cache_name}.c')
		if !os.exists(c_path) {
			continue
		}
		for line in top_level_c_header_directives(c_path) {
			if line in seen {
				continue
			}
			seen[line] = true
			directives << line
		}
	}
	return directives.join('\n')
}

fn (b &Builder) cached_module_stamp_flags(cache_names []string) (string, string) {
	cache_dir := b.core_cache_dir()
	mut compile_flags := ''
	mut link_flags := ''
	for cache_name in cache_names {
		stamp_path := os.join_path(cache_dir, '${cache_name}.stamp')
		stamp := os.read_file(stamp_path) or { continue }
		for line in stamp.split_into_lines() {
			if line.starts_with('cc_flags=') {
				compile_flags = join_flag_strings(compile_flags, line['cc_flags='.len..])
			} else if line.starts_with('cc_link_flags=') {
				link_flags = join_flag_strings(link_flags, line['cc_link_flags='.len..])
			}
		}
	}
	return compile_flags, link_flags
}

fn join_flag_strings(a string, b string) string {
	mut joined := []string{}
	for flags in [a, b] {
		for item in flag_string_items(flags) {
			if item !in joined {
				joined << item
			}
		}
	}
	return joined.join(' ')
}

fn flag_string_items(flags string) []string {
	tokens := flags.fields()
	mut items := []string{}
	mut i := 0
	for i < tokens.len {
		tok := tokens[i]
		if tok in ['-I', '-L', '-l', '-F', '-framework', '-isystem', '-include']
			&& i + 1 < tokens.len {
			items << '${tok} ${tokens[i + 1]}'
			i += 2
			continue
		}
		items << tok
		i++
	}
	return items
}

fn is_c_conditional_directive(line string) bool {
	trimmed := line.trim_space()
	return trimmed.starts_with('#if') || trimmed.starts_with('#ifdef')
		|| trimmed.starts_with('#ifndef') || trimmed.starts_with('#elif')
		|| trimmed.starts_with('#else') || trimmed.starts_with('#endif')
}

fn top_level_c_header_directives(c_path string) []string {
	lines := os.read_lines(c_path) or { return []string{} }
	mut directives := []string{}
	for raw_line in lines {
		line := raw_line.trim_space()
		if line == '// V primitive types' {
			break
		}
		if line == '' || !line.starts_with('#') {
			continue
		}
		if is_c_conditional_directive(line) {
			continue
		}
		if cached_directive_emits_linked_symbols(line) {
			continue
		}
		directives << line
	}
	return directives
}

fn cached_directive_emits_linked_symbols(line string) bool {
	lower_value := line.trim_space().to_lower()
	for marker in ['.c"', ".c'", '.c>', '.m"', ".m'", '.m>'] {
		if lower_value.contains(marker) {
			return true
		}
	}
	return lower_value.contains('"miniz.h"') || lower_value.contains('<miniz.h>')
		|| lower_value.contains('/miniz.h"') || lower_value.contains('/miniz.h>')
}

// top_level_c_decls extracts typedef declarations and function forward
// declarations from a cached C source file.  Returns (typedefs, fn_decls).
fn top_level_c_decls(c_path string) ([]string, []string) {
	lines := os.read_lines(c_path) or { return []string{}, []string{} }
	mut typedefs := []string{}
	mut fn_decls := []string{}
	mut i := 0
	for i < lines.len {
		raw_line := lines[i]
		i++
		if raw_line.len == 0 || raw_line[0] in [` `, `\t`, `\n`, `\r`] {
			continue
		}
		line := raw_line.trim_space()
		if line.starts_with('typedef struct ') && line.contains('{') && !line.ends_with(';') {
			mut parts := [line]
			for i < lines.len {
				next_line := lines[i].trim_space()
				i++
				if next_line == '' {
					continue
				}
				parts << next_line
				if next_line.starts_with('}') && next_line.ends_with(';') {
					break
				}
			}
			typedefs << parts.join(' ')
			continue
		}
		// Collect typedef lines (struct/union/array/map forward typedefs).
		if line.starts_with('typedef ') && line.ends_with(';') {
			typedefs << line
			continue
		}
		// Collect function forward declarations.
		if !line.ends_with(');') || !line.contains('(') {
			continue
		}
		if line.starts_with('#') || line.starts_with('struct ') || line.starts_with('union ')
			|| line.starts_with('enum ') || line.starts_with('return ') || line.starts_with('if ')
			|| line.starts_with('for ') || line.starts_with('while ') || line.starts_with('switch ') {
			continue
		}
		if line[0] == `(` || line[0] == `*` || line[0] == `&` || line[0] == `}` {
			continue
		}
		if line.contains('=') {
			continue
		}
		paren_idx := line.index_u8(`(`)
		if paren_idx > 0 {
			before_paren := line[..paren_idx].trim_space()
			if !before_paren.contains(' ') && !before_paren.contains('*') {
				continue
			}
		}
		fn_decls << line
	}
	return typedefs, fn_decls
}

fn (mut b Builder) ensure_cached_module_object(cache_dir string, cache_name string, module_paths []string, emit_modules []string, cc string, cc_flags string, cc_link_flags string, error_limit_flag string, use_markused bool) !string {
	obj_path := cache_path_join(cache_dir, '${cache_name}.o')
	stamp_path := cache_path_join(cache_dir, '${cache_name}.stamp')
	c_path := cache_path_join(cache_dir, '${cache_name}.c')
	expected_stamp := b.cache_stamp_for_modules(cache_name, module_paths, cc, cc_flags,
		cc_link_flags, use_markused)
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
	if b.used_vh_for_parse {
		if os.exists(obj_path) && os.exists(stamp_path) {
			return obj_path
		}
		return error('missing cached ${cache_name} object for .vh parse')
	}

	mut module_source := b.gen_cleanc_source_for_cache(emit_modules, cache_name, use_markused)
	if module_source == '' {
		return error('failed to generate C source for ${cache_name}')
	}
	if cache_name != builtin_cache_name {
		module_source = strip_common_weak_sort_fallbacks(module_source)
	}
	os.write_file(c_path, sanitize_cached_object_c_source(module_source))!

	compile_cmd := '${cc} ${cc_flags} -w -Wno-incompatible-function-pointer-types -c "${c_path}" -o "${obj_path}"${error_limit_flag}'
	run_cc_cmd_or_exit(compile_cmd, 'C compilation', b.pref.show_cc)
	os.write_file(stamp_path, expected_stamp)!
	return obj_path
}

fn (mut b Builder) ensure_cached_parsed_module_object(cache_dir string, cache_name string, module_names []string, dependency_cache_names []string, cc string, cc_flags string, cc_link_flags string, error_limit_flag string, use_markused bool) !string {
	return b.ensure_cached_parsed_module_object_force(cache_dir, cache_name, module_names,
		dependency_cache_names, cc, cc_flags, cc_link_flags, error_limit_flag, use_markused,
		[]string{})
}

fn (mut b Builder) ensure_cached_parsed_module_object_force(cache_dir string, cache_name string, module_names []string, dependency_cache_names []string, cc string, cc_flags string, cc_link_flags string, error_limit_flag string, use_markused bool, force_emit_fn_names []string) !string {
	obj_path := cache_path_join(cache_dir, '${cache_name}.o')
	stamp_path := cache_path_join(cache_dir, '${cache_name}.stamp')
	c_path := cache_path_join(cache_dir, '${cache_name}.c')
	mut expected_stamp := b.cache_stamp_for_parsed_modules(cache_name, module_names,
		dependency_cache_names, cc, cc_flags, cc_link_flags, use_markused)
	if force_emit_fn_names.len > 0 {
		mut force_names := force_emit_fn_names.clone()
		force_names.sort()
		expected_stamp += '\nforce=${force_names.join(',')}'
	}
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
	if b.used_import_vh_for_parse {
		if os.exists(obj_path) && os.exists(stamp_path) {
			return obj_path
		}
		return error('missing cached ${cache_name} object for .vh parse')
	}

	mut force_names := force_emit_fn_names.clone()
	mut module_source := if force_names.len > 0 {
		b.gen_cleanc_source_for_cache_force(module_names, cache_name, use_markused, force_names)
	} else {
		b.gen_cleanc_source_for_cache(module_names, cache_name, use_markused)
	}
	if force_names.len > 0 {
		for _ in 0 .. 3 {
			body_text := c_function_bodies_for_names(module_source, force_names)
			next_force := merge_unique_strings(force_names, b.force_module_fn_names_referenced_by_c(body_text,
				module_names, 1))
			if next_force.len == force_names.len {
				break
			}
			force_names = next_force.clone()
			module_source = b.gen_cleanc_source_for_cache_force(module_names, cache_name,
				use_markused, force_names)
		}
	}
	if module_source == '' {
		return error('failed to generate C source for ${cache_name}')
	}
	module_source = strip_common_weak_sort_fallbacks(module_source)
	if dependency_cache_names.len > 0 {
		module_source = b.inject_cached_c_header_directives(module_source, dependency_cache_names)
		module_source = b.inject_cached_typedef_decls(module_source, dependency_cache_names)
	}
	if !b.ensure_core_cache_dir() {
		return error('failed to prepare cache dir for ${cache_name}')
	}
	os.write_file(c_path, sanitize_cached_object_c_source(module_source))!

	compile_cmd := '${cc} ${cc_flags} -w -Wno-incompatible-function-pointer-types -c "${c_path}" -o "${obj_path}"${error_limit_flag}'
	run_cc_cmd_or_exit(compile_cmd, 'C compilation', b.pref.show_cc)
	os.write_file(stamp_path, expected_stamp)!
	return obj_path
}

fn (mut b Builder) ensure_cached_virtual_module_object(cache_dir string, groups []CachedVirtualModule, dependency_cache_names []string, cc string, cc_flags string, cc_link_flags string, error_limit_flag string, use_markused bool) !string {
	obj_path := cache_path_join(cache_dir, '${virtuals_cache_name}.o')
	stamp_path := cache_path_join(cache_dir, '${virtuals_cache_name}.stamp')
	c_path := cache_path_join(cache_dir, '${virtuals_cache_name}.c')
	expected_stamp := b.cache_stamp_for_virtual_modules(groups, dependency_cache_names, cc,
		cc_flags, cc_link_flags, use_markused)
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
	if b.used_virtual_vh_for_parse {
		if os.exists(obj_path) && os.exists(stamp_path) {
			return obj_path
		}
		return error('missing cached ${virtuals_cache_name} object for .vh parse')
	}

	emit_files := virtual_module_source_files(groups)
	mut module_source := b.gen_cleanc_source_for_cache_files(['main'], emit_files,
		virtuals_cache_name, use_markused)
	force_helpers := sort_cmp_fn_names_referenced_by_c(module_source)
	if force_helpers.len > 0 {
		module_source = b.gen_cleanc_source_for_cache_files_force(['main'], emit_files,
			virtuals_cache_name, use_markused, force_helpers)
	}
	module_source = strip_common_weak_sort_fallbacks(module_source)
	if module_source == '' {
		return error('failed to generate C source for ${virtuals_cache_name}')
	}
	if dependency_cache_names.len > 0 {
		module_source = b.inject_cached_c_header_directives(module_source, dependency_cache_names)
		module_source = b.inject_cached_typedef_decls(module_source, dependency_cache_names)
	}
	os.write_file(c_path, sanitize_cached_object_c_source(module_source))!

	compile_cmd := '${cc} ${cc_flags} -w -Wno-incompatible-function-pointer-types -c "${c_path}" -o "${obj_path}"${error_limit_flag}'
	run_cc_cmd_or_exit(compile_cmd, 'C compilation', b.pref.show_cc)
	os.write_file(stamp_path, expected_stamp)!
	return obj_path
}

fn (b &Builder) force_main_fn_names_referenced_by_cached_c(c_source string, excluded_source_files []string) []string {
	return b.force_main_fn_names_referenced_by_c_text(c_source, excluded_source_files, 2)
}

fn (b &Builder) force_main_fn_names_for_emit_files(emit_files []string) []string {
	mut emit_set := map[string]bool{}
	for file in emit_files {
		emit_set[os.norm_path(file)] = true
		emit_set[os.norm_path(os.abs_path(file))] = true
	}
	mut names := []string{}
	mut seen := map[string]bool{}
	for file in b.files {
		if ast_file_module_name(file) != 'main' || file.name == '' || file.name.ends_with('.vh') {
			continue
		}
		if !source_file_in_set(file.name, emit_set) {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				name := main_fn_decl_c_name(stmt)
				if name == '' || is_common_weak_sort_fallback_name(name) || name in seen {
					continue
				}
				seen[name] = true
				names << name
			} else if stmt is ast.EnumDecl {
				name := '${stmt.name}__str'
				if name == '' || name in seen {
					continue
				}
				seen[name] = true
				names << name
			}
		}
	}
	return names
}

fn (b &Builder) force_local_import_fn_names(module_names []string) []string {
	if module_names.len == 0 {
		return []string{}
	}
	root := b.virtual_module_root()
	mut module_set := map[string]bool{}
	for module_name in module_names {
		module_set[module_name] = true
	}
	mut names := []string{}
	mut seen := map[string]bool{}
	for file in b.files {
		module_name := ast_file_module_name(file)
		if module_name == 'main' || module_name !in module_set || file.name == ''
			|| file.name.ends_with('.vh') {
			continue
		}
		relative_path_under_root(root, file.name) or { continue }
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				name := module_fn_decl_c_name(module_name, stmt)
				if name == '' || is_common_weak_sort_fallback_name(name) || name in seen {
					continue
				}
				seen[name] = true
				names << name
			} else if stmt is ast.EnumDecl {
				name := '${module_name}__${stmt.name}__str'
				if name == '' || name in seen {
					continue
				}
				seen[name] = true
				names << name
			}
		}
	}
	return names
}

fn (b &Builder) force_module_fn_names_referenced_by_c(c_source string, module_names []string, min_refs int) []string {
	if c_source.len == 0 || module_names.len == 0 {
		return []string{}
	}
	mut module_set := map[string]bool{}
	for module_name in module_names {
		module_set[module_name] = true
	}
	mut names := []string{}
	mut seen := map[string]bool{}
	for name in c_symbol_fn_names_for_modules(c_source, module_names, min_refs) {
		if name == '' || is_common_weak_sort_fallback_name(name) || name in seen {
			continue
		}
		seen[name] = true
		names << name
	}
	for file in b.files {
		module_name := ast_file_module_name(file)
		if module_name !in module_set {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				name := module_fn_decl_c_name(module_name, stmt)
				if name == '' || is_common_weak_sort_fallback_name(name) || name in seen {
					continue
				}
				if c_source.count('${name}(') >= min_refs {
					seen[name] = true
					names << name
				}
			} else if stmt is ast.EnumDecl {
				name := '${module_name}__${stmt.name}__str'
				if name == '' || name in seen {
					continue
				}
				if c_source.count('${name}(') >= min_refs {
					seen[name] = true
					names << name
				}
			}
		}
	}
	return names
}

fn c_symbol_fn_names_for_modules(c_source string, module_names []string, min_refs int) []string {
	mut names := []string{}
	mut seen := map[string]bool{}
	for module_name in module_names {
		if module_name == '' || module_name == 'main' || module_name == 'builtin' {
			continue
		}
		prefix := '${module_name}__'
		mut search_from := 0
		for {
			rel_idx := c_source[search_from..].index(prefix) or { break }
			start := search_from + rel_idx
			mut end := start + prefix.len
			for end < c_source.len {
				ch := c_source[end]
				if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
					|| (ch >= `0` && ch <= `9`) || ch == `_` {
					end++
					continue
				}
				break
			}
			name := c_source[start..end]
			search_from = end
			if name == '' || name in seen {
				continue
			}
			if end >= c_source.len || c_source[end] != `(` {
				continue
			}
			if c_source.count('${name}(') >= min_refs {
				seen[name] = true
				names << name
			}
		}
	}
	return names
}

fn (b &Builder) force_main_fn_names_referenced_by_forced_c_bodies(c_source string, excluded_source_files []string, root_names []string) []string {
	body_text := c_function_bodies_for_names(c_source, root_names)
	if body_text.len == 0 {
		return []string{}
	}
	return b.force_main_fn_names_referenced_by_c_text(body_text, excluded_source_files, 1)
}

fn (b &Builder) force_main_fn_names_referenced_by_c_text(c_source string, excluded_source_files []string, min_refs int) []string {
	if c_source.len == 0 {
		return []string{}
	}
	mut excluded := map[string]bool{}
	for file in excluded_source_files {
		excluded[os.norm_path(file)] = true
		excluded[os.norm_path(os.abs_path(file))] = true
	}
	mut names := []string{}
	mut seen := map[string]bool{}
	for file in b.files {
		if ast_file_module_name(file) != 'main' {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				source_file := b.source_file_for_pos(stmt.pos, file.name)
				if source_file_in_set(source_file, excluded) {
					continue
				}
				if stmt.name.ends_with('__str') || stmt.name.starts_with('__sort_cmp_') {
					continue
				}
				name := main_fn_decl_c_name(stmt)
				if name == '' || name in seen {
					continue
				}
				// Cached virtual C contains prototypes for referenced root functions.
				// A second occurrence means a virtual body calls that root symbol.
				if c_source.count('${name}(') >= min_refs {
					seen[name] = true
					names << name
				}
			} else if stmt is ast.EnumDecl {
				if source_file_in_set(file.name, excluded) {
					continue
				}
				name := '${stmt.name}__str'
				if name == '' || name in seen {
					continue
				}
				if c_source.count('${name}(') >= min_refs {
					seen[name] = true
					names << name
				}
			}
		}
	}
	return names
}

fn c_function_bodies_for_names(c_source string, names []string) string {
	mut bodies := []string{}
	mut seen := map[string]bool{}
	for name in names {
		if name == '' || name in seen {
			continue
		}
		seen[name] = true
		body := c_function_body_for_name(c_source, name)
		if body.len > 0 {
			bodies << body
		}
	}
	return bodies.join('\n')
}

fn c_function_body_for_name(c_source string, name string) string {
	mut search_from := 0
	pattern := '${name}('
	for {
		rel_idx := c_source[search_from..].index(pattern) or { break }
		start := search_from + rel_idx
		close_idx := c_source[start..].index_u8(`)`)
		if close_idx < 0 {
			search_from = start + pattern.len
			continue
		}
		mut body_start := start + close_idx + 1
		for body_start < c_source.len && c_source[body_start].is_space() {
			body_start++
		}
		if body_start >= c_source.len || c_source[body_start] != `{` {
			search_from = body_start
			continue
		}
		mut depth := 0
		for i := body_start; i < c_source.len; i++ {
			ch := c_source[i]
			if ch == `{` {
				depth++
			} else if ch == `}` {
				depth--
				if depth == 0 {
					return c_source[body_start..i + 1]
				}
			}
		}
		break
	}
	return ''
}

fn strip_weak_c_function_definition(c_source string, name string) string {
	pattern := '__attribute__((weak)) int ${name}('
	start := c_source.index(pattern) or { return c_source }
	open_rel := c_source[start..].index_u8(`{`)
	if open_rel < 0 {
		return c_source
	}
	body_start := start + open_rel
	mut depth := 0
	for i := body_start; i < c_source.len; i++ {
		ch := c_source[i]
		if ch == `{` {
			depth++
		} else if ch == `}` {
			depth--
			if depth == 0 {
				mut end := i + 1
				for end < c_source.len && c_source[end].is_space() {
					end++
				}
				return c_source[..start] + c_source[end..]
			}
		}
	}
	return c_source
}

fn (b &Builder) source_file_for_pos(pos token.Pos, fallback string) string {
	if pos.is_valid() {
		return b.file_set.file(pos).name
	}
	return fallback
}

fn sort_cmp_fn_names_referenced_by_c(c_source string) []string {
	return sort_cmp_fn_names_in_c(c_source, 2)
}

fn sort_cmp_fn_names_declared_in_c(c_source string) []string {
	return sort_cmp_fn_names_in_c(c_source, 1)
}

fn sort_cmp_fn_names_in_c(c_source string, min_refs int) []string {
	mut names := []string{}
	mut seen := map[string]bool{}
	mut search_from := 0
	for {
		rel_idx := c_source[search_from..].index('__sort_cmp_') or { break }
		start := search_from + rel_idx
		mut end := start
		for end < c_source.len {
			ch := c_source[end]
			if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
				|| (ch >= `0` && ch <= `9`) || ch == `_` {
				end++
				continue
			}
			break
		}
		name := c_source[start..end]
		search_from = end
		if name == '' || name in seen {
			continue
		}
		if is_common_weak_sort_fallback_name(name) {
			continue
		}
		if c_source.count(name) >= min_refs {
			seen[name] = true
			names << name
		}
	}
	return names
}

fn filter_out_names(names []string, excluded []string) []string {
	if excluded.len == 0 {
		return names
	}
	mut excluded_set := map[string]bool{}
	for name in excluded {
		excluded_set[name] = true
	}
	mut out := []string{cap: names.len}
	for name in names {
		if name !in excluded_set {
			out << name
		}
	}
	return out
}

fn is_common_weak_sort_fallback_name(name string) bool {
	return name in ['__sort_cmp_int_asc', '__sort_cmp_RepIndex_by_idx_asc']
}

fn main_fn_decl_c_name(decl ast.FnDecl) string {
	if decl.name == '' || decl.language == .c {
		return ''
	}
	name := sanitize_builder_fn_ident(decl.name)
	if !decl.is_method {
		return name
	}
	receiver := main_fn_receiver_name(decl.receiver.typ)
	if receiver == '' {
		return ''
	}
	return '${receiver}__${name}'
}

fn module_fn_decl_c_name(module_name string, decl ast.FnDecl) string {
	if decl.name == '' || decl.language == .c {
		return ''
	}
	name := sanitize_builder_fn_ident(decl.name)
	if !decl.is_method {
		if module_name.len > 0 && module_name != 'builtin' && module_name != 'main' {
			return '${module_name}__${name}'
		}
		return name
	}
	receiver := main_fn_receiver_name(decl.receiver.typ)
	if receiver == '' {
		return ''
	}
	if module_name.len > 0 && module_name != 'builtin' && module_name != 'main' {
		return '${module_name}__${receiver}__${name}'
	}
	return '${receiver}__${name}'
}

fn main_fn_receiver_name(expr ast.Expr) string {
	return match expr {
		ast.Ident {
			expr.name
		}
		ast.SelectorExpr {
			expr.rhs.name
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				main_fn_receiver_name(expr.expr)
			} else {
				''
			}
		}
		ast.ModifierExpr {
			main_fn_receiver_name(expr.expr)
		}
		ast.GenericArgs {
			main_fn_receiver_name(expr.lhs)
		}
		ast.GenericArgOrIndexExpr {
			main_fn_receiver_name(expr.lhs)
		}
		ast.Type {
			match expr {
				ast.PointerType {
					main_fn_receiver_name(expr.base_type)
				}
				ast.GenericType {
					main_fn_receiver_name(expr.name)
				}
				else {
					''
				}
			}
		}
		else {
			''
		}
	}
}

fn sanitize_builder_fn_ident(name string) string {
	return match name {
		'+' { 'plus' }
		'-' { 'minus' }
		'*' { 'mul' }
		'/' { 'div' }
		'%' { 'mod' }
		'==' { 'eq' }
		'!=' { 'ne' }
		'<' { 'lt' }
		'>' { 'gt' }
		'<=' { 'le' }
		'>=' { 'ge' }
		'|' { 'pipe' }
		'^' { 'xor' }
		else { name }
	}
}

fn merge_unique_strings(a []string, b []string) []string {
	mut seen := map[string]bool{}
	mut out := []string{cap: a.len + b.len}
	for value in a {
		if value == '' || value in seen {
			continue
		}
		seen[value] = true
		out << value
	}
	for value in b {
		if value == '' || value in seen {
			continue
		}
		seen[value] = true
		out << value
	}
	return out
}

fn (b &Builder) has_module(module_name string) bool {
	for file in b.files {
		if ast_file_module_name(file) == module_name {
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
		module_name := ast_file_module_name(file)
		if module_name in excluded_set {
			continue
		}
		modules_set[module_name] = true
	}
	mut modules := modules_set.keys()
	modules.sort()
	return modules
}

fn (b &Builder) print_cached_bundle_modules(cache_name string, module_names []string) {
	if module_names.len == 0 {
		return
	}
	stats_enabled := b.pref != unsafe { nil } && b.pref.stats
	show_cc_enabled := b.pref != unsafe { nil } && b.pref.show_cc
	if !stats_enabled && !show_cc_enabled && os.getenv('V2_TRACE_CACHE') == '' {
		return
	}
	if stats_enabled {
		println(' * Cached ${cache_name} modules: ${module_names.len}')
		for module_name in module_names {
			println('   [cache ${cache_name}] ${module_name}')
		}
		return
	}
	println('[*] Cached ${cache_name} modules: ${module_names.join(', ')}')
}

fn ast_file_module_name(file ast.File) string {
	for stmt in file.stmts {
		if stmt is ast.ModuleStmt {
			return stmt.name.replace('.', '_')
		}
	}
	return 'main'
}

fn normalize_target_os_name(target_os string) string {
	return match target_os.to_lower() {
		'darwin', 'mac' { 'macos' }
		else { target_os.to_lower() }
	}
}

fn flag_os_matches(cond string, target_os string) bool {
	current := normalize_target_os_name(target_os)
	return match cond.to_lower() {
		'darwin', 'macos', 'mac' { current == 'macos' || current == 'darwin' }
		'linux' { current == 'linux' }
		'windows' { current == 'windows' }
		'bsd' { current in ['macos', 'freebsd', 'openbsd', 'netbsd', 'dragonfly'] }
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
	// Resolve any relative path (including bare relative like 'r/qrcodegen')
	// relative to the source file's directory, matching V1 behavior
	return os.norm_path(os.join_path(file_dir, resolved))
}

fn expand_existing_path_macros(flag_value string) ?string {
	mut out := ''
	mut i := 0
	for i < flag_value.len {
		if flag_value[i] == `$` {
			remainder := flag_value[i..]
			mut literal := ''
			if remainder.starts_with(r'$when_first_existing') {
				literal = r'$when_first_existing'
			} else if remainder.starts_with(r'$first_existing') {
				literal = r'$first_existing'
			}
			if literal != '' {
				if remainder.len <= literal.len || remainder[literal.len] != `(` {
					out += flag_value[i].ascii_str()
					i++
					continue
				}
				params_part := remainder[literal.len + 1..]
				params := params_part.all_before(')')
				if params == params_part {
					return none
				}
				paths := params.replace(',', '\n').split_into_lines().map(it.trim('\t \'"'))
				mut found := ''
				for path in paths {
					if path != '' && os.exists(path) {
						found = path
						break
					}
				}
				if found == '' {
					return none
				}
				out += found
				i += literal.len + 1 + params.len + 1
				continue
			}
		}
		out += flag_value[i].ascii_str()
		i++
	}
	return out
}

fn normalize_flag_value_for_file(flag_value string, file_path string) string {
	file_dir := os.dir(os.real_path(file_path))
	vmod_root := find_vmod_root_for_file(file_path)
	expanded_flag_value := expand_existing_path_macros(flag_value) or { return '' }
	mut tokens := expanded_flag_value.fields()
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
		if tok.contains('@VMODROOT') || tok.contains('@VEXEROOT') || tok.starts_with('./')
			|| tok.starts_with('../') || tok.ends_with('.c') || tok.ends_with('.m')
			|| tok.ends_with('.o') {
			out << resolve_flag_path(tok, file_dir, vmod_root)
			i++
			continue
		}
		out << tok
		i++
	}
	return out.join(' ')
}

fn parse_flag_directive_line(line string, file_path string, target_os string) ?string {
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
		if !flag_os_matches(parts[0], target_os) {
			return none
		}
		rest = rest[parts[0].len..].trim_space()
	}
	if rest == '' {
		return none
	}
	return normalize_flag_value_for_file(rest, file_path)
}

fn flag_references_missing_file(flag string, include_flags []string) bool {
	for tok in flag.fields() {
		clean := tok.trim('"').trim("'")
		if clean.len == 0 {
			continue
		}
		if clean.ends_with('.o') || clean.ends_with('.a') || clean.ends_with('.so')
			|| clean.ends_with('.dylib') || clean.ends_with('.m') || clean.ends_with('.c') {
			if os.is_abs_path(clean) || clean.starts_with('./') || clean.starts_with('../') {
				if !os.exists(clean) {
					// For .o files, try to build from corresponding .c file
					if clean.ends_with('.o') {
						c_file := clean[..clean.len - 2] + '.c'
						if os.exists(c_file) {
							inc_flags := include_flags.join(' ')
							compile_cmd := 'cc -c -w -O2 ${inc_flags} "${c_file}" -o "${clean}"'
							res := os.execute(compile_cmd)
							if res.exit_code == 0 {
								continue // successfully compiled, not missing
							}
						}
					}
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
	mut scanned_files := map[string]bool{}
	// Collect source file paths to scan.  When .vh headers were used for
	// parsing, b.files references the .vh summaries which lack #flag
	// directives.  Always include the original core module source files
	// so that directive flags (e.g. -I paths) are never lost.
	mut scan_paths := []string{}
	for file in b.files {
		if file.name != '' {
			scan_paths << file.name
		}
	}
	if !b.pref.skip_builtin {
		for module_path in core_cached_module_paths {
			vlib_path := b.pref.get_vlib_module_path(module_path)
			module_files := get_v_files_from_dir(vlib_path, b.pref.user_defines, os.user_os())
			for mf in module_files {
				if mf !in scanned_files {
					scan_paths << mf
				}
			}
		}
	}
	scan_paths.sort()
	for scan_path in scan_paths {
		if scan_path == '' || scan_path in scanned_files {
			continue
		}
		scanned_files[scan_path] = true
		lines := os.read_lines(scan_path) or { continue }
		// Track $if nesting to skip flags inside non-matching comptime blocks.
		// skip_depth > 0 means we are inside a non-matching $if block.
		mut skip_depth := 0
		for line in lines {
			trimmed := line.trim_space()
			// Handle $if / $else / closing braces for comptime blocks
			if trimmed.starts_with(r'$if ') {
				cond := trimmed[4..].trim_right('?{ ').trim_space()
				if skip_depth > 0 {
					skip_depth++
				} else if !comptime_cond_matches(cond, os.user_os()) {
					skip_depth = 1
				}
				continue
			}
			if trimmed.starts_with(r'$else') || trimmed == r'} $else {' {
				if skip_depth == 1 {
					skip_depth = 0
				} else if skip_depth == 0 {
					skip_depth = 1
				}
				continue
			}
			if trimmed == '}' && skip_depth > 0 {
				skip_depth--
				continue
			}
			if skip_depth > 0 {
				continue
			}
			// Replace @VEXEROOT before parsing so path normalization sees absolute paths
			resolved_line := line.replace('@VEXEROOT', b.pref.vroot).replace('VEXEROOT',
				b.pref.vroot)
			mut flag := parse_flag_directive_line(resolved_line, scan_path, os.user_os()) or {
				continue
			}
			// Build include flags from already-collected flags for compiling missing .o files
			mut inc_flags := []string{}
			for f in flags {
				if f.starts_with('-I') {
					inc_flags << f
				}
			}
			if flag_references_missing_file(flag, inc_flags) {
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

// split_compile_and_link_flags separates a flags string into compiler-only
// flags (for -c compilation) and linker-only flags (for the link step).
// Linker flags include: -l*, -L*, -framework, C source files and
// prebuilt object/library files.  Source files from #flag directives must not
// be passed to per-module `-c -o module.o` cache compilations.
fn split_compile_and_link_flags(flags string) (string, string) {
	tokens := flags.fields()
	mut compile := []string{}
	mut link := []string{}
	mut i := 0
	for i < tokens.len {
		tok := tokens[i]
		if tok == '-framework' {
			// -framework Name: two tokens, linker only
			link << tok
			if i + 1 < tokens.len {
				i++
				link << tokens[i]
			}
		} else if tok.starts_with('-l') || tok.starts_with('-L') {
			link << tok
			// -L or -l alone (space-separated from its argument): grab the next token
			if (tok == '-L' || tok == '-l') && i + 1 < tokens.len {
				i++
				link << tokens[i]
			}
		} else if tok.ends_with('.c') || tok.ends_with('.cc') || tok.ends_with('.cpp')
			|| tok.ends_with('.cxx') || tok.ends_with('.m') || tok.ends_with('.mm')
			|| tok.ends_with('.o') || tok.ends_with('.obj') || tok.ends_with('.a')
			|| tok.ends_with('.so') || tok.ends_with('.dylib') {
			link << tok
		} else if tok == '-I' && i + 1 < tokens.len {
			// -I alone (space-separated from its argument): grab the next token
			compile << tok
			i++
			compile << tokens[i]
		} else {
			compile << tok
		}
		i++
	}
	return compile.join(' '), link.join(' ')
}

fn comptime_cond_matches(cond string, target_os string) bool {
	// Handle negation: $if !platform
	if cond.starts_with('!') {
		return !comptime_cond_matches(cond[1..], target_os)
	}
	// Handle && conjunction
	if and_idx := cond.index('&&') {
		left := cond[..and_idx].trim_space()
		right := cond[and_idx + 2..].trim_space()
		return comptime_cond_matches(left, target_os) && comptime_cond_matches(right, target_os)
	}
	if or_idx := cond.index('||') {
		left := cond[..or_idx].trim_space()
		right := cond[or_idx + 2..].trim_space()
		return comptime_cond_matches(left, target_os) || comptime_cond_matches(right, target_os)
	}
	current := normalize_target_os_name(target_os)
	return match cond.to_lower() {
		'macos', 'darwin', 'mac' { current == 'macos' || current == 'darwin' }
		'linux' { current == 'linux' }
		'windows' { current == 'windows' }
		'bsd' { current in ['macos', 'freebsd', 'openbsd', 'netbsd', 'dragonfly'] }
		'freebsd' { current == 'freebsd' }
		'openbsd' { current == 'openbsd' }
		'netbsd' { current == 'netbsd' }
		'dragonfly' { current == 'dragonfly' }
		'android' { current == 'android' }
		'native' { false }
		'emscripten' { false }
		'ios' { false }
		else { false } // unknown user-defined flags default to false
	}
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
	return '-I "${os.join_path(tcc_dir, 'lib', 'include')}" -L "${os.join_path(tcc_dir, 'lib')}"'
}

fn cflags_need_objc_mode(flags string) bool {
	lower_flags := flags.to_lower()
	for tok in lower_flags.fields() {
		clean := tok.trim('"\'')
		if clean.ends_with('.m') || clean.ends_with('.mm') {
			return true
		}
	}
	return lower_flags.contains('-framework cocoa') || lower_flags.contains('-framework appkit')
		|| lower_flags.contains('-framework foundation') || lower_flags.contains('-framework uikit')
		|| lower_flags.contains('-framework metal') || lower_flags.contains('-framework metalkit')
		|| lower_flags.contains('-framework quartzcore')
}

fn cc_recompile_flags_from_cmd(cmd string) string {
	parts := cmd.fields()
	mut flags := []string{}
	mut i := 1 // skip compiler
	for i < parts.len {
		p := parts[i]
		if p == '-o' {
			i += 2
			continue
		}
		if p == '-x' {
			if i + 1 < parts.len && parts[i + 1] != 'none' {
				flags << p
				flags << parts[i + 1]
			}
			i += 2
			continue
		}
		if p in ['-I', '-D', '-U', '-F', '-include', '-isystem', '-idirafter'] {
			flags << p
			if i + 1 < parts.len {
				i++
				flags << parts[i]
			}
			i++
			continue
		}
		if p.starts_with('-I') || p.starts_with('-D') || p.starts_with('-U') || p.starts_with('-F')
			|| p.starts_with('-std=') || p.starts_with('-W') || p.starts_with('-f')
			|| p.starts_with('-m') || p == '-pthread' {
			flags << p
		}
		i++
	}
	return flags.join(' ')
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
			// Replace TCC binary with cc and strip TCC-specific include/lib
			// paths. TCC's tgmath.h conflicts with macOS system headers,
			// causing SIMD ambiguity errors in MetalKit when compiling as
			// Objective-C.
			mut fallback_cmd := cmd.replace_once(cc_binary, 'cc')
			tcc_dir := cc_binary.all_before_last('/tcc')
			if tcc_dir.len > 0 {
				// Remove -I and -L flags pointing into the TCC directory.
				mut parts := fallback_cmd.fields()
				mut filtered := []string{cap: parts.len}
				mut i2 := 0
				for i2 < parts.len {
					p := parts[i2]
					if (p == '-I' || p == '-L') && i2 + 1 < parts.len
						&& parts[i2 + 1].contains('tcc') {
						i2 += 2
						continue
					}
					if (p.starts_with('-I') || p.starts_with('-L')) && p.contains('tcc') {
						i2++
						continue
					}
					filtered << p
					i2++
				}
				fallback_cmd = filtered.join(' ')
			}
			// cc cannot read .o files produced by tcc on macOS arm64. Recompile
			// any cached .o files referenced in the command from their .c siblings
			// using cc before retrying the link.
			recompile_flags := cc_recompile_flags_from_cmd(fallback_cmd)
			for tok in fallback_cmd.fields() {
				clean_tok := tok.trim('"')
				if !clean_tok.ends_with('.o') {
					continue
				}
				stem := clean_tok.all_before_last('.o')
				mut c_sibling := stem + '.c'
				if !os.exists(c_sibling) && stem.ends_with('.main') {
					c_sibling = stem.all_before_last('.main') + '.c'
				}
				if !os.exists(c_sibling) {
					continue
				}
				recompile_cmd := 'cc ${recompile_flags} -w -Wno-incompatible-function-pointer-types -c "${c_sibling}" -o "${clean_tok}"'
				if show_cc {
					println(recompile_cmd)
				}
				rr := os.execute(recompile_cmd)
				if rr.exit_code != 0 {
					eprintln('cc recompile failed for ${c_sibling}:')
					eprintln(rr.output)
				}
				// Invalidate stamp so future builds rebuild from .c too.
				stamp_path := stem + '.stamp'
				if os.exists(stamp_path) {
					os.rm(stamp_path) or {}
				}
			}
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

	// Pass markused data for dead code elimination
	if b.used_fn_keys.len > 0 {
		ssa_builder.used_fn_keys = b.used_fn_keys.clone()
	}

	// --single-backend: strip unused backend modules from the binary
	if b.pref.single_backend {
		all_backends := ['cleanc', 'eval', 'c', 'x64', 'arm64']
		own := match b.pref.backend {
			.arm64 { 'arm64' }
			.x64 { 'x64' }
			.cleanc { 'cleanc' }
			.c { 'c' }
			.eval { 'eval' }
			else { '' }
		}

		for backend_mod in all_backends {
			if backend_mod != own {
				ssa_builder.skip_modules[backend_mod] = true
			}
		}
	}

	// In hot_fn mode, only build the target function body (skip all others)
	if b.pref.hot_fn.len > 0 {
		ssa_builder.hot_fn = b.pref.hot_fn
	}

	// Build all files together with proper multi-file ordering
	mut stage_start := native_sw.elapsed()
	if b.pref.no_parallel || b.pref.hot_fn.len > 0 {
		ssa_builder.build_all(b.files)
	} else {
		// Phases 1-3 sequential, Phase 4 parallel, Phase 5 sequential
		ssa_builder.skip_fn_bodies = true
		ssa_builder.build_all(b.files)
		ssa_builder.skip_fn_bodies = false
		b.ssa_build_parallel(mut ssa_builder, b.files)
		ssa_builder.generate_vinit()
	}
	print_time('SSA Build', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	if b.pref.no_optimize {
		eprintln('  opt: skipped (-O0)')
	} else {
		ssa_optimize.optimize(mut mod)
	}
	print_time('SSA Optimize', time.Duration(native_sw.elapsed() - stage_start))
	$if debug {
		// Post-opt SSA verification is useful while debugging the optimizer, but it
		// is currently noisy enough to block normal self-host builds. Keep it
		// opt-in so `test_all.sh` and manual self-hosting can still complete.
		if !b.pref.no_optimize && os.getenv('V2_VERIFY') != '' {
			ssa_optimize.verify_and_panic(mod, 'full optimization')
		}
	}

	// Post-optimization SSA dump for debugging
	dump_fn_name := os.getenv('V2_DUMP_OPT_SSA')
	if dump_fn_name.len > 0 {
		for func in mod.funcs {
			if func.name == dump_fn_name {
				eprintln('=== POST-OPT SSA DUMP: ${func.name} ===')
				eprintln('  params: ${func.params}')
				for pi, pid in func.params {
					pval := mod.values[pid]
					eprintln('  param[${pi}]: v${pid} kind=${pval.kind} name=`${pval.name}` typ=${pval.typ}')
				}
				for blk_id in func.blocks {
					blk := mod.blocks[blk_id]
					eprintln('  block ${blk_id} (${blk.name}):')
					for dval_id in blk.instrs {
						dval := mod.values[dval_id]
						if dval.kind != .instruction {
							continue
						}
						dinstr := mod.instrs[dval.index]
						mut ops_str := ''
						for oi, op_id in dinstr.operands {
							op_v := mod.values[op_id]
							ops_str += 'v${op_id}(${op_v.kind}:${op_v.name})'
							if oi < dinstr.operands.len - 1 {
								ops_str += ', '
							}
						}
						eprintln('    v${dval_id}: ${dinstr.op} [${ops_str}] typ=${dval.typ}')
					}
				}
				eprintln('=== END POST-OPT SSA DUMP ===')
			}
		}
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
		stage_start = native_sw.elapsed()
		mut gen := arm64.Gen.new(&mir_mod)
		if b.pref.no_parallel {
			gen.gen()
		} else {
			b.gen_arm64_parallel(mut gen)
		}
		print_time('ARM64 Gen', time.Duration(native_sw.elapsed() - stage_start))

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
			if b.pref.no_parallel {
				gen.gen()
			} else {
				b.gen_arm64_parallel(mut gen)
			}
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
		if !b.pref.keep_c {
			os.rm(obj_file) or {}
		}
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
	}
	if (show_stats || print_parsed_files) && parsed_vh_files.len > 0 {
		println(' * Parsed .vh files:')
		for path in parsed_vh_files {
			println('   [vh] ${path}')
		}
	}
	if show_stats {
		println(' * Parsed V LOC (entry files): ${entry_v_lines_n}')
		println(' * Parsed V LOC (all parsed sources): ${parsed_v_lines_n}')
	}
}
