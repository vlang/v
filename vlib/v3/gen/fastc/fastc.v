module fastc

import os
import strings
import v3.pref
import v3.scanner
import v3.token

// FastC parses scanner tokens and emits C immediately. It deliberately has no
// AST, semantic-checker, transformer, mark-used, or conventional cgen path.

const c_preamble = r'#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef intptr_t isize;
typedef uintptr_t usize;
typedef unsigned char byte;
typedef int32_t rune;
typedef float f32;
typedef double f64;
typedef const char *string;
typedef void *voidptr;
typedef unsigned char *byteptr;
typedef char *charptr;
typedef void *chan;
typedef struct { void *data; int offset; int len; int cap; int flags; } array;
typedef struct { void *data; int len; } map;
typedef struct { void *data; void *err; unsigned char state; } Option;
typedef union { uintptr_t word; long double alignment; unsigned char data[64]; } MultiReturnValue;
typedef struct { MultiReturnValue values[8]; } MultiReturn;
#define V_FASTC_MULTI_VALUE(expression) ({ __typeof__(expression) value = (expression); MultiReturnValue result = {0}; memcpy(result.data, &value, sizeof(value)); result; })

static void v_fastc_print_string(const char *value) { fputs(value, stdout); }
static void v_fastc_print_bool(bool value) { fputs(value ? "true" : "false", stdout); }
static void v_fastc_print_char(char value) { fputc(value, stdout); }
static void v_fastc_print_signed(long long value) { printf("%lld", value); }
static void v_fastc_print_unsigned(unsigned long long value) { printf("%llu", value); }
static void v_fastc_println_string(const char *value) { puts(value); }
static void v_fastc_println_bool(bool value) { puts(value ? "true" : "false"); }
static void v_fastc_println_char(char value) { fputc(value, stdout); fputc(10, stdout); }
static void v_fastc_println_signed(long long value) { printf("%lld\n", value); }
static void v_fastc_println_unsigned(unsigned long long value) { printf("%llu\n", value); }

/* Float formatting belongs to the V strconv routines. Leaving float and double
 * unmatched makes TinyCC reject unsupported printing instead of silently
 * applying printf %g semantics. */
#define V_FASTC_PRINT_SELECT(value, string_fn, bool_fn, char_fn, signed_fn, unsigned_fn) _Generic((value), char *: string_fn, const char *: string_fn, bool: bool_fn, char: char_fn, signed char: signed_fn, short: signed_fn, int: signed_fn, long: signed_fn, long long: signed_fn, unsigned char: unsigned_fn, unsigned short: unsigned_fn, unsigned int: unsigned_fn, unsigned long: unsigned_fn, unsigned long long: unsigned_fn)(value)
#define print(value) V_FASTC_PRINT_SELECT(value, v_fastc_print_string, v_fastc_print_bool, v_fastc_print_char, v_fastc_print_signed, v_fastc_print_unsigned)
#define println(value) V_FASTC_PRINT_SELECT(value, v_fastc_println_string, v_fastc_println_bool, v_fastc_println_char, v_fastc_println_signed, v_fastc_println_unsigned)

'

const c_selfhost_preamble = r'#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef intptr_t isize;
typedef uintptr_t usize;
typedef unsigned char byte;
typedef int32_t rune;
typedef float f32;
typedef double f64;
typedef void *voidptr;
typedef unsigned char *byteptr;
typedef char *charptr;
typedef void *chan;
typedef union { uintptr_t word; long double alignment; unsigned char data[64]; } MultiReturnValue;
typedef struct { MultiReturnValue values[8]; } MultiReturn;
#define V_FASTC_MULTI_VALUE(expression) ({ __typeof__(expression) value = (expression); MultiReturnValue result = {0}; memcpy(result.data, &value, sizeof(value)); result; })

#define _S(s) ((string){.str=(byteptr)("" s), .len=(sizeof(s)-1), .is_lit=1})
#define _SLIT0 _S("")

static void *v_fastc_interface_box(const void *value, usize size) {
	void *copy = malloc(size);
	if (copy != NULL) memcpy(copy, value, size);
	return copy;
}

static const u64 _wyp[4] = {0x2d358dccaa6c78a5ull, 0x8bb84b93962eacc9ull, 0x4b33a62ed433d4a3ull, 0x4d5a2da51de1aa47ull};
static inline u64 wyhash64(u64 a, u64 b) { a ^= _wyp[0]; b ^= _wyp[1]; a *= 0xa0761d6478bd642full; b *= 0xe7037ed1a0b428dbull; return (a ^ (a >> 32)) ^ (b ^ (b >> 32)); }
static inline u64 wyhash(const void *key, size_t len, u64 seed, const u64 *secret) { const unsigned char *p = (const unsigned char *)key; u64 h = seed ^ secret[0] ^ (u64)len; for (size_t i = 0; i < len; i++) h = wyhash64(h ^ (u64)p[i], secret[(i + 1) & 3]); return h; }

'

const c_selfhost_runtime = r'static void vheap_alloc(void *p, u64 n) { (void)p; (void)n; }
static void vheap_free(void *p) { (void)p; }

static int v_fastc_string_compare(const string *left, const string *right) {
	int common = left->len < right->len ? left->len : right->len;
	int order = common > 0 ? memcmp(left->str, right->str, (size_t)common) : 0;
	if (order != 0) return order;
	return (left->len > right->len) - (left->len < right->len);
}

static int builtin__array_index(array values, voidptr value) {
	for (int i = 0; i < values.len; i++) {
		void *item = (u8 *)values.data + (size_t)i * (size_t)values.element_size;
		if (memcmp(item, value, (size_t)values.element_size) == 0) return i;
	}
	return -1;
}

static void builtin__array_sort(array *values) {
	if (values == NULL || values->len < 2) return;
	if (values->element_size != (int)sizeof(string)) return;
	string *items = (string *)values->data;
	for (int i = 1; i < values->len; i++) {
		string current = items[i];
		int j = i;
		while (j > 0 && v_fastc_string_compare(&items[j - 1], &current) > 0) {
			items[j] = items[j - 1];
			j--;
		}
		items[j] = current;
	}
}

'

enum FastcDeclaredTypeKind {
	struct_
	union_
	enum_
	interface_
	alias_
}

struct FastcFunctionSignature {
	parameter_types []string
	return_type     string
	return_types    []string
	option_type     string
	is_variadic     bool
	path            string
}

fn fastc_string_types_equal(left []string, right []string) bool {
	if left.len != right.len {
		return false
	}
	for i, item in left {
		if item != right[i] {
			return false
		}
	}
	return true
}

struct FastcLocal {
	is_mut       bool
	is_reference bool
	typ          string
}

struct FastcExpressionToken {
	tok    token.Token
	lit    string
	source string
mut:
	typ string
}

struct FastcRenderedExpression {
	source string
	typ    string
}

struct FastcSourceHeader {
	module_name string
	imports     map[string]string
}

struct FastcSourceFile {
	path   string
	source string
	header FastcSourceHeader
}

struct FastcQueuedSource {
	path        string
	module_name string
}

struct FastcHoistedCSource {
	directives string
	body       string
}

struct Parser {
	prefs               &pref.Preferences
	path                string
	module_name         string
	imports             map[string]string
	declared_types      map[string]bool
	declared_kinds      map[string]FastcDeclaredTypeKind
	struct_fields       map[string]map[string]string
	constants           map[string]string
	globals             map[string]string
	used_function_names map[string]bool
	selfhost            bool
mut:
	s                        scanner.Scanner
	tok                      token.Token
	lit                      string
	out                      strings.Builder
	protos                   strings.Builder
	indent                   int
	in_main                  bool
	has_main                 bool
	unsafe_depth             int
	temp_id                  int
	locals                   map[string]FastcLocal
	functions                map[string]FastcFunctionSignature
	constant_types           map[string]string
	global_types             map[string]string
	return_type              string
	return_types             []string
	option_return_type       string
	current_function         string
	current_receiver         string
	expected_expression_type string
	capturing_defer          bool
	captured_defer_lines     []string
	deferred_blocks          [][]string
	last_expression_type     string
	last_expression          []FastcExpressionToken
	last_multi_return_types  []string
	fixed_array_types        map[string]string
	composite_types          map[string]bool
}

// generate scans V source and emits C as each declaration and statement is consumed. It does
// not construct an AST or invoke semantic type checking. Unsupported syntax is returned as an
// error; FastC never retries through an AST-based backend.
pub fn generate(source string, path string, prefs &pref.Preferences) !string {
	header := fastc_scan_source_header(source, path, prefs)!
	if header.imports.len > 0 {
		return error('fastc parser does not support imports through the single-source API in ${path}')
	}
	return generate_source_files([
		FastcSourceFile{
			path:   path
			source: source
			header: header
		},
	], prefs)
}

// generate_files discovers imports from the input files and emits one C translation unit for
// the complete source graph. Discovery and generation use scanner tokens only.
pub fn generate_files(paths []string, prefs &pref.Preferences) !string {
	sources := fastc_resolve_source_files(paths, prefs)!
	return generate_source_files(sources, prefs)
}

fn generate_source_files(sources []FastcSourceFile, prefs &pref.Preferences) !string {
	mut declared_types := map[string]bool{}
	mut declared_kinds := map[string]FastcDeclaredTypeKind{}
	mut struct_fields := map[string]map[string]string{}
	mut constants := map[string]string{}
	mut globals := map[string]string{}
	for source_file in sources {
		collect_declared_types(source_file.source, source_file.path,
			source_file.header.module_name, prefs, mut declared_types, mut declared_kinds)!
	}
	for source_file in sources {
		collect_constant_names(source_file.source, source_file.path,
			source_file.header.module_name, prefs, mut constants)!
		collect_global_names(source_file.source, source_file.path, source_file.header.module_name,
			prefs, mut globals)!
	}
	mut functions := map[string]FastcFunctionSignature{}
	mut interface_methods := map[string]bool{}
	for source_file in sources {
		collect_function_signatures(source_file.source, source_file.path, source_file.header,
			prefs, declared_types, mut functions)!
		collect_interface_method_signatures(source_file.source, source_file.path,
			source_file.header, prefs, declared_types, mut functions, mut interface_methods)!
	}
	used_function_names := fastc_collect_referenced_function_names(sources, prefs, functions)
	mut composite_types := map[string]bool{}
	if prefs.building_v {
		// The OS exec helpers build their native argv arrays locally, so this
		// representation does not otherwise appear in a declaration signature.
		composite_types['Array_char_ptr'] = true
	}
	for signature in functions.values() {
		fastc_register_composite_type(signature.return_type, mut composite_types)
		for parameter_type in signature.parameter_types {
			fastc_register_composite_type(parameter_type, mut composite_types)
		}
	}
	type_declarations := fastc_generate_type_declarations(sources, prefs, declared_types,
		declared_kinds, mut struct_fields, mut composite_types)!
	mut constant_types := map[string]string{}
	constant_declarations := fastc_generate_constant_declarations(sources, prefs, declared_types,
		declared_kinds, struct_fields, functions, constants, mut constant_types)!
	mut global_types := map[string]string{}
	global_declarations := fastc_generate_global_declarations(sources, prefs, declared_types,
		declared_kinds, struct_fields, functions, constants, constant_types, globals, mut
		global_types)!
	for constant_type in constant_types.values() {
		fastc_register_composite_type(constant_type, mut composite_types)
	}
	for global_type in global_types.values() {
		fastc_register_composite_type(global_type, mut composite_types)
	}
	mut prototypes := strings.new_builder(1024)
	mut body := strings.new_builder(4096)
	mut fixed_array_types := map[string]string{}
	for source_file in sources {
		mut file_set := token.FileSet.new()
		mut file := file_set.add_file(source_file.path, source_file.source.len)
		file.index_lines(source_file.source)
		mut gen := Parser{
			prefs:               unsafe { prefs }
			path:                source_file.path
			module_name:         source_file.header.module_name
			imports:             source_file.header.imports
			declared_types:      declared_types
			declared_kinds:      declared_kinds
			struct_fields:       struct_fields
			constants:           constants
			globals:             globals
			used_function_names: used_function_names
			selfhost:            prefs.building_v
			s:                   scanner.new_scanner(prefs, .normal)
			out:                 strings.new_builder(source_file.source.len)
			protos:              strings.new_builder(256)
			functions:           functions
			constant_types:      constant_types
			global_types:        global_types
			fixed_array_types:   fixed_array_types
			composite_types:     composite_types
		}
		gen.s.init(file, source_file.source)
		generated := gen.run()!
		if gen.s.diagnostics.len > 0 {
			diagnostic := gen.s.diagnostics[0]
			return error('fastc scanner error at byte ${diagnostic.offset} in ${source_file.path}: ${diagnostic.message}')
		}
		prototypes.write_string(gen.protos.str())
		body.write_string(generated)
		fixed_array_types = gen.fixed_array_types.clone()
		composite_types = gen.composite_types.clone()
	}
	mut late_composite_declarations := strings.new_builder(256)
	mut composite_names := composite_types.keys()
	composite_names.sort()
	for composite_name in composite_names {
		declaration := 'typedef ${if composite_name.starts_with('Array_') {
			'array'
		} else {
			'map'
		}} ${composite_name};'
		if !type_declarations.contains(declaration) {
			late_composite_declarations.writeln(declaration)
		}
	}
	if late_composite_declarations.len > 0 {
		late_composite_declarations.writeln('')
	}
	interface_dispatches := fastc_generate_interface_dispatches(declared_kinds, functions,
		interface_methods)
	fixed_array_declarations := fastc_generate_fixed_array_declarations(fixed_array_types)
	preamble := if prefs.building_v { c_selfhost_preamble } else { c_preamble }
	hoisted_body := fastc_hoist_c_include_directives(body.str())
	mut result := strings.new_builder(preamble.len + type_declarations.len +
		constant_declarations.len + global_declarations.len + prototypes.len + body.len + 2)
	result.write_string(preamble)
	result.write_string(hoisted_body.directives)
	result.write_string(constant_declarations)
	result.write_string(type_declarations)
	result.write_string(late_composite_declarations.str())
	result.write_string(fixed_array_declarations)
	result.write_string(global_declarations)
	result.write_string(prototypes.str())
	result.writeln('')
	if prefs.building_v {
		result.write_string(c_selfhost_runtime)
	}
	result.write_string(interface_dispatches)
	result.write_string(hoisted_body.body)
	return result.str()
}

fn fastc_generate_interface_dispatches(declared_kinds map[string]FastcDeclaredTypeKind, functions map[string]FastcFunctionSignature, interface_methods map[string]bool) string {
	mut out := strings.new_builder(1024)
	mut function_keys := functions.keys()
	function_keys.sort()
	mut interface_method_keys := interface_methods.keys()
	interface_method_keys.sort()
	for interface_key, kind in declared_kinds {
		if kind != .interface_ {
			continue
		}
		prefix := interface_key + '.'
		for interface_method_key in interface_method_keys {
			if !interface_method_key.starts_with(prefix) {
				continue
			}
			interface_signature := functions[interface_method_key]
			if interface_signature.parameter_types.len == 0 {
				continue
			}
			interface_type := fastc_c_declared_type_name(interface_key)
			if interface_signature.parameter_types[0] != interface_type {
				continue
			}
			method_name := interface_method_key.all_after_last('.')
			mut parameters := ['${interface_type} value']
			mut arguments := []string{}
			for i in 1 .. interface_signature.parameter_types.len {
				parameters << '${interface_signature.parameter_types[i]} arg${i}'
				arguments << 'arg${i}'
			}
			c_name := fastc_method_c_name_for_key(interface_key, method_name)
			out.writeln('${interface_signature.return_type} ${c_name}(${parameters.join(', ')}) {')
			out.writeln('\tswitch (value._typ) {')
			for candidate_key in function_keys {
				if candidate_key == interface_method_key
					|| candidate_key.all_after_last('.') != method_name {
					continue
				}
				receiver_key := candidate_key.all_before_last('.')
				if declared_kinds[receiver_key] in [.interface_, .enum_, .alias_]
					|| receiver_key !in declared_kinds {
					continue
				}
				candidate_signature := functions[candidate_key]
				if candidate_signature.return_type != interface_signature.return_type
					|| candidate_signature.parameter_types.len != interface_signature.parameter_types.len {
					continue
				}
				receiver_type := fastc_c_declared_type_name(receiver_key)
				expected_receiver := candidate_signature.parameter_types[0]
				receiver_argument := if expected_receiver.ends_with('*') {
					'(${receiver_type} *)value._object'
				} else {
					'*(${receiver_type} *)value._object'
				}
				call_arguments := if arguments.len > 0 {
					',' + arguments.join(',')
				} else {
					''
				}
				call := '${fastc_method_c_name_for_key(receiver_key, method_name)}(${receiver_argument}${call_arguments})'
				out.writeln('\tcase __v_typeid_${receiver_type}: ${if interface_signature.return_type == 'void' {
					call + '; return;'
				} else {
					'return ' + call + ';'
				}}')
			}
			out.writeln('\tdefault: ${if interface_signature.return_type == 'void' {
				'return;'
			} else {
				'return (' + interface_signature.return_type + '){0};'
			}}')
			out.writeln('\t}')
			out.writeln('}')
			out.writeln('')
		}
	}
	return out.str()
}

fn fastc_hoist_c_include_directives(source string) FastcHoistedCSource {
	mut directives := strings.new_builder(256)
	mut body := strings.new_builder(source.len)
	for line in source.split('\n') {
		if line.starts_with('#include ') {
			directives.writeln(line)
		} else {
			body.writeln(line)
		}
	}
	if directives.len > 0 {
		directives.writeln('')
	}
	return FastcHoistedCSource{
		directives: directives.str()
		body:       body.str()
	}
}

fn fastc_resolve_source_files(paths []string, prefs &pref.Preferences) ![]FastcSourceFile {
	mut queue := []FastcQueuedSource{}
	if prefs.building_v {
		builtin_dir := prefs.get_vlib_module_path('builtin')
		for builtin_file in pref.get_v_files_from_dir_for_target(builtin_dir, prefs.user_defines,
			prefs.target) {
			if fastc_source_file_matches_backend(builtin_file) {
				queue << FastcQueuedSource{
					path:        builtin_file
					module_name: 'builtin'
				}
			}
		}
	}
	for path in paths {
		queue << FastcQueuedSource{
			path: path
		}
	}
	mut seen := map[string]bool{}
	mut sources := []FastcSourceFile{}
	for queue.len > 0 {
		queued := queue[0]
		queue.delete(0)
		path := os.real_path(queued.path)
		if seen[path] {
			continue
		}
		if !os.is_file(path) {
			return error('fastc source file `${path}` does not exist')
		}
		seen[path] = true
		source := os.read_file(path)!
		mut header := fastc_scan_source_header(source, path, prefs)!
		if queued.module_name != '' {
			header = FastcSourceHeader{
				module_name: queued.module_name
				imports:     header.imports
			}
		}
		sources << FastcSourceFile{
			path:   path
			source: source
			header: header
		}
		for imported_module in header.imports.values() {
			module_dir := prefs.get_module_path(imported_module, path)
			if module_dir == '' {
				return error('fastc cannot resolve imported module `${imported_module}` from `${path}`')
			}
			for module_file in pref.get_v_files_from_dir_for_target(module_dir, prefs.user_defines,
				prefs.target) {
				if !fastc_source_file_matches_backend(module_file) {
					continue
				}
				if !seen[os.real_path(module_file)] {
					queue << FastcQueuedSource{
						path:        module_file
						module_name: imported_module
					}
				}
			}
		}
	}
	return sources
}

fn fastc_source_file_matches_backend(path string) bool {
	return !path.ends_with('.arm64.v') && !path.ends_with('.amd64.v')
		&& !path.ends_with('.native.v') && !path.ends_with('.wasm.v') && !path.ends_with('.rv64.v')
		&& !path.ends_with('.js.v')
}

fn fastc_scan_source_header(source string, path string, prefs &pref.Preferences) !FastcSourceHeader {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut module_name := ''
	mut imports := map[string]string{}
	mut brace_depth := 0
	mut tok := scan.scan()
	for tok != .eof {
		if module_name == '' && tok == .key_module {
			tok = scan.scan()
			if tok != .name {
				return error('fastc parser does not support module declaration in ${path}')
			}
			module_name = scan.lit
			tok = scan.scan()
			continue
		}
		if brace_depth == 0
			&& tok in [.key_fn, .key_struct, .key_enum, .key_interface, .key_type, .key_const, .key_global] {
			break
		}
		if tok != .key_import || brace_depth > 1 {
			if tok == .lcbr {
				brace_depth++
			} else if tok == .rcbr && brace_depth > 0 {
				brace_depth--
			}
			tok = scan.scan()
			continue
		}
		tok = scan.scan()
		if tok == .lpar {
			tok = scan.scan()
			for tok != .rpar && tok != .eof {
				if tok == .semicolon || tok == .comma {
					tok = scan.scan()
					continue
				}
				import_path, alias, next_token := fastc_scan_import(mut scan, tok, path)!
				imports[alias] = import_path
				tok = next_token
			}
			if tok == .rpar {
				tok = scan.scan()
			}
			continue
		}
		import_path, alias, next_token := fastc_scan_import(mut scan, tok, path)!
		imports[alias] = import_path
		tok = next_token
	}
	if module_name == '' {
		module_name = 'main'
	}
	if prefs.building_v && prefs.backend == 'fastc' && imports['driver'] == 'v3.driver'
		&& 'fastcdriver' in imports {
		imports['driver'] = imports['fastcdriver']
	}
	return FastcSourceHeader{
		module_name: module_name
		imports:     imports
	}
}

fn fastc_scan_import(mut scan scanner.Scanner, first token.Token, path string) !(string, string, token.Token) {
	mut tok := first
	if tok != .name {
		return error('fastc parser does not support import `${tok.str()}` in ${path}')
	}
	mut parts := [scan.lit]
	tok = scan.scan()
	for tok == .dot {
		tok = scan.scan()
		if tok != .name {
			return error('fastc parser does not support import path in ${path}')
		}
		parts << scan.lit
		tok = scan.scan()
	}
	mut alias := parts.last()
	if tok == .key_as {
		tok = scan.scan()
		if tok != .name {
			return error('fastc parser does not support import alias in ${path}')
		}
		alias = scan.lit
		tok = scan.scan()
	}
	if tok == .lcbr {
		mut depth := 1
		for depth > 0 {
			tok = scan.scan()
			if tok == .eof {
				return error('fastc parser does not support unfinished selective import in ${path}')
			}
			if tok == .lcbr {
				depth++
			} else if tok == .rcbr {
				depth--
			}
		}
		tok = scan.scan()
	}
	return parts.join('.'), alias, tok
}

fn collect_declared_types(source string, path string, module_name string, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut next_c_struct_is_typedef := false
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && tok == .attribute {
			mut attribute_depth := 1
			mut is_typedef := false
			tok = scan.scan()
			for attribute_depth > 0 && tok != .eof {
				if tok == .name && scan.lit == 'typedef' {
					is_typedef = true
				}
				if tok == .lsbr {
					attribute_depth++
				} else if tok == .rsbr {
					attribute_depth--
				}
				tok = scan.scan()
			}
			next_c_struct_is_typedef = next_c_struct_is_typedef || is_typedef
			continue
		}
		if brace_depth == 0
			&& tok in [.key_struct, .key_enum, .key_interface, .key_type, .key_union] {
			kind := match tok {
				.key_enum { FastcDeclaredTypeKind.enum_ }
				.key_interface { FastcDeclaredTypeKind.interface_ }
				.key_type { FastcDeclaredTypeKind.alias_ }
				.key_union { FastcDeclaredTypeKind.union_ }
				else { FastcDeclaredTypeKind.struct_ }
			}
			tok = scan.scan()
			if tok == .name {
				name := scan.lit
				tok = scan.scan()
				if name == 'C' && tok == .dot {
					tok = scan.scan()
					if tok == .name && !next_c_struct_is_typedef {
						declared_types['#Cstruct#${scan.lit}'] = true
					}
					next_c_struct_is_typedef = false
					continue
				}
				key := fastc_type_key(module_name, name)
				declared_types[key] = true
				declared_kinds[key] = kind
			}
			next_c_struct_is_typedef = false
			continue
		}
		if tok == .lcbr {
			brace_depth++
		} else if tok == .rcbr && brace_depth > 0 {
			brace_depth--
		}
		tok = scan.scan()
	}
}

fn collect_constant_names(source string, path string, module_name string, prefs &pref.Preferences, mut constants map[string]string) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut tok := scan.scan()
	for tok != .eof {
		if brace_depth == 0 && tok == .key_const {
			tok = scan.scan()
			if tok == .lpar {
				tok = scan.scan()
				mut at_declaration_start := true
				mut nested_depth := 0
				for tok != .eof {
					if nested_depth == 0 && tok == .rpar {
						tok = scan.scan()
						break
					}
					if nested_depth == 0 && tok == .semicolon {
						at_declaration_start = true
						tok = scan.scan()
						continue
					}
					if nested_depth == 0 && at_declaration_start && tok == .name {
						fastc_register_constant(module_name, scan.lit, mut constants)
						at_declaration_start = false
					}
					if tok in [.lpar, .lsbr, .lcbr] {
						nested_depth++
					} else if tok in [.rpar, .rsbr, .rcbr] && nested_depth > 0 {
						nested_depth--
					}
					tok = scan.scan()
				}
				continue
			}
			if tok != .name {
				return error('fastc parser does not support constant declaration in ${path}')
			}
			fastc_register_constant(module_name, scan.lit, mut constants)
			continue
		}
		if tok == .lcbr {
			brace_depth++
		} else if tok == .rcbr && brace_depth > 0 {
			brace_depth--
		}
		tok = scan.scan()
	}
}

fn fastc_register_constant(module_name string, name string, mut constants map[string]string) {
	key := fastc_constant_key(module_name, name)
	constants[key] = fastc_c_constant_name(module_name, name)
}

fn collect_global_names(source string, path string, module_name string, prefs &pref.Preferences, mut globals map[string]string) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut depth := 0
	mut tok := scan.scan()
	for tok != .eof {
		if depth == 0 && tok == .key_global {
			tok = scan.scan()
			if tok == .lpar {
				tok = scan.scan()
				mut at_start := true
				for tok != .rpar && tok != .eof {
					if tok == .semicolon {
						at_start = true
					} else if at_start && tok == .name && scan.lit != 'C' {
						key := fastc_global_key(module_name, scan.lit)
						globals[key] = fastc_c_global_name(key)
						at_start = false
					}
					tok = scan.scan()
				}
				continue
			}
			if tok == .name && scan.lit != 'C' {
				key := fastc_global_key(module_name, scan.lit)
				globals[key] = fastc_c_global_name(key)
			}
			continue
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr && depth > 0 {
			depth--
		}
		tok = scan.scan()
	}
}

fn fastc_generate_global_declarations(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, struct_fields map[string]map[string]string, functions map[string]FastcFunctionSignature, constants map[string]string, constant_types map[string]string, globals map[string]string, mut global_types map[string]string) !string {
	mut out := strings.new_builder(1024)
	for source_file in sources {
		mut file_set := token.FileSet.new()
		mut file := file_set.add_file(source_file.path, source_file.source.len)
		file.index_lines(source_file.source)
		mut gen := Parser{
			prefs:          unsafe { prefs }
			path:           source_file.path
			module_name:    source_file.header.module_name
			imports:        source_file.header.imports
			declared_types: declared_types
			declared_kinds: declared_kinds
			struct_fields:  struct_fields
			constants:      constants
			globals:        globals
			selfhost:       prefs.building_v
			s:              scanner.new_scanner(prefs, .normal)
			out:            strings.new_builder(0)
			protos:         strings.new_builder(0)
			functions:      functions
			constant_types: constant_types
			global_types:   global_types
		}
		gen.s.init(file, source_file.source)
		gen.next()
		for gen.tok != .eof {
			if gen.tok == .key_global {
				gen.parse_global_declaration(mut out)!
				continue
			}
			if gen.tok == .lcbr {
				gen.skip_balanced(.lcbr, .rcbr)!
				continue
			}
			gen.next()
		}
		global_types = gen.global_types.clone()
	}
	if out.len > 0 {
		out.writeln('')
	}
	return out.str()
}

fn (mut g Parser) parse_global_declaration(mut out strings.Builder) ! {
	g.expect(.key_global)!
	if g.tok == .lpar {
		return g.unsupported('grouped globals')
	}
	if g.tok != .name {
		return g.unsupported('global name')
	}
	if g.lit == 'C' {
		g.skip_top_level_declaration()!
		return
	}
	name := g.lit
	key := fastc_global_key(g.module_name, name)
	c_name := g.globals[key] or { return g.unsupported('unregistered global `${name}`') }
	g.next()
	if g.tok == .assign {
		g.next()
		if g.tok == .lsbr {
			g.next()
			size := g.read_expression([token.Token.rsbr, token.Token.comma])!
			if g.tok == .comma {
				return g.unsupported('global array literal')
			}
			g.expect(.rsbr)!
			element_type := g.parse_type()!
			g.expect(.lcbr)!
			g.skip_open_block()!
			out.writeln('static ${element_type} ${c_name}[${size}];')
			g.global_types[key] = 'FixedArray_${fastc_composite_type_part(element_type)}'
			return
		}
		_ = g.read_expression([token.Token.semicolon])!
		typ := fastc_normalize_inferred_type(g.last_expression_type)
		if typ == '' {
			return g.unsupported('unverifiable global `${name}` type')
		}
		out.writeln('static ${typ} ${c_name};')
		g.global_types[key] = typ
		g.skip_semicolons()
		return
	}
	typ := g.parse_type()!
	out.writeln('static ${typ} ${c_name};')
	g.global_types[key] = typ
	g.skip_semicolons()
}

fn fastc_generate_constant_declarations(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, struct_fields map[string]map[string]string, functions map[string]FastcFunctionSignature, constants map[string]string, mut constant_types map[string]string) !string {
	mut out := strings.new_builder(4096)
	for source_file in sources {
		mut file_set := token.FileSet.new()
		mut file := file_set.add_file(source_file.path, source_file.source.len)
		file.index_lines(source_file.source)
		mut gen := Parser{
			prefs:          unsafe { prefs }
			path:           source_file.path
			module_name:    source_file.header.module_name
			imports:        source_file.header.imports
			declared_types: declared_types
			declared_kinds: declared_kinds
			struct_fields:  struct_fields
			constants:      constants
			selfhost:       prefs.building_v
			s:              scanner.new_scanner(prefs, .normal)
			out:            strings.new_builder(256)
			protos:         strings.new_builder(0)
			functions:      functions
			constant_types: constant_types
		}
		gen.s.init(file, source_file.source)
		gen.next()
		for gen.tok != .eof {
			if gen.tok == .key_const {
				gen.parse_constant_declaration(mut out)!
				continue
			}
			if gen.tok == .lcbr {
				gen.skip_balanced(.lcbr, .rcbr)!
				continue
			}
			gen.next()
		}
		if gen.s.diagnostics.len > 0 {
			diagnostic := gen.s.diagnostics[0]
			return error('fastc scanner error at byte ${diagnostic.offset} in ${source_file.path}: ${diagnostic.message}')
		}
		constant_types = gen.constant_types.clone()
	}
	if out.len > 0 {
		out.writeln('')
	}
	return out.str()
}

fn (mut g Parser) parse_constant_declaration(mut out strings.Builder) ! {
	g.expect(.key_const)!
	if g.tok == .lpar {
		g.next()
		g.skip_semicolons()
		for g.tok != .rpar {
			if g.tok == .eof {
				return g.unsupported('unfinished constant group')
			}
			g.parse_one_constant(mut out, [token.Token.semicolon, token.Token.rpar])!
			g.skip_semicolons()
		}
		g.next()
		g.skip_semicolons()
		return
	}
	g.parse_one_constant(mut out, [token.Token.semicolon])!
	g.skip_semicolons()
}

fn (mut g Parser) parse_one_constant(mut out strings.Builder, stops []token.Token) ! {
	if g.tok != .name {
		return g.unsupported('constant name `${g.token_source()}`')
	}
	name := g.lit
	g.next()
	for g.tok != .assign && g.tok != .eof {
		if g.tok in stops {
			return g.unsupported('constant `${name}` without a value')
		}
		g.next()
	}
	g.expect(.assign)!
	value := g.read_expression(stops)!
	if value.len == 0 {
		return g.unsupported('empty constant `${name}`')
	}
	key := fastc_constant_key(g.module_name, name)
	c_name := g.constants[key] or { return g.unsupported('unregistered constant `${name}`') }
	out.writeln('#define ${c_name} (${value})')
	g.constant_types[key] = fastc_normalize_inferred_type(g.last_expression_type)
}

fn fastc_generate_type_declarations(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, mut struct_fields map[string]map[string]string, mut composite_types map[string]bool) !string {
	mut out := strings.new_builder(4096)
	mut bodies := strings.new_builder(4096)
	mut keys := declared_kinds.keys()
	keys.sort()
	for type_id, key in keys {
		name := fastc_c_declared_type_name(key)
		out.writeln('#define __v_typeid_${name} ${type_id + 1}')
		match declared_kinds[key] {
			.struct_, .interface_ { out.writeln('typedef struct ${name} ${name};') }
			.union_ { out.writeln('typedef union ${name} ${name};') }
			.enum_ { out.writeln('typedef int ${name};') }
			.alias_ {}
		}
	}
	out.writeln('')
	for source_file in sources {
		fastc_emit_source_type_declarations(source_file, prefs, declared_types, declared_kinds, mut
			struct_fields, mut composite_types, mut bodies)!
	}
	mut composite_names := composite_types.keys()
	composite_names.sort()
	for composite_name in composite_names {
		base := if composite_name.starts_with('Array_') { 'array' } else { 'map' }
		out.writeln('typedef ${base} ${composite_name};')
	}
	out.writeln('')
	mut type_bodies := bodies.str()
	if prefs.building_v {
		type_bodies = fastc_order_c_composite_definitions(type_bodies, struct_fields,
			declared_kinds)
	}
	out.write_string(type_bodies)
	return out.str()
}

fn fastc_order_c_composite_definitions(source string, struct_fields map[string]map[string]string, declared_kinds map[string]FastcDeclaredTypeKind) string {
	mut ordered := source
	mut changed := true
	mut passes := 0
	for changed && passes < struct_fields.len {
		changed = false
		passes++
		for dependent, fields in struct_fields {
			for _, field_type in fields {
				dependency := fastc_by_value_composite_type(field_type, declared_kinds)
				if dependency == '' || dependency == dependent {
					continue
				}
				next := fastc_move_c_composite_before(ordered, dependency, dependent)
				if next != ordered {
					ordered = next
					changed = true
				}
			}
		}
	}
	return ordered
}

fn fastc_by_value_composite_type(field_type string, declared_kinds map[string]FastcDeclaredTypeKind) string {
	if field_type.ends_with('*') || field_type.starts_with('Array_')
		|| field_type.starts_with('Map_') {
		return ''
	}
	mut candidate := field_type
	if element_type := fastc_fixed_array_element_type(candidate) {
		candidate = element_type
	}
	for key, kind in declared_kinds {
		if kind in [.struct_, .union_, .interface_] && fastc_c_declared_type_name(key) == candidate {
			return candidate
		}
	}
	return ''
}

fn fastc_move_c_composite_before(source string, dependency string, dependent string) string {
	dependency_start := fastc_c_composite_definition_start(source, dependency) or { return source }
	dependent_start := fastc_c_composite_definition_start(source, dependent) or { return source }
	if dependency_start < dependent_start {
		return source
	}
	dependency_end := fastc_c_composite_definition_end(source, dependency_start) or {
		return source
	}
	mut end := dependency_end
	for end < source.len && source[end] == `\n` {
		end++
	}
	block := source[dependency_start..end]
	without := source[..dependency_start] + source[end..]
	insert_at := fastc_c_composite_definition_start(without, dependent) or { return source }
	return without[..insert_at] + block + without[insert_at..]
}

fn fastc_c_composite_definition_start(source string, name string) ?int {
	if start := source.index('struct ${name} {') {
		return int(start)
	}
	if start := source.index('union ${name} {') {
		return int(start)
	}
	return none
}

fn fastc_c_composite_definition_end(source string, start int) ?int {
	tail := source[start..]
	if relative_end := tail.index('\n};') {
		return start + relative_end + 3
	}
	return none
}

fn fastc_emit_source_type_declarations(source_file FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, mut struct_fields map[string]map[string]string, mut composite_types map[string]bool, mut out strings.Builder) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(source_file.path, source_file.source.len)
	file.index_lines(source_file.source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source_file.source)
	mut depth := 0
	mut next_enum_is_flag := false
	mut tok := scan.scan()
	for tok != .eof {
		if depth == 0 && tok == .attribute {
			tok, next_enum_is_flag = fastc_scan_type_attribute(mut scan)!
			continue
		}
		if depth == 0 && tok in [.key_struct, .key_union] {
			tok = fastc_emit_struct_declaration(mut scan, tok == .key_union, source_file,
				declared_types, mut struct_fields, mut composite_types, mut out)!
			continue
		}
		if depth == 0 && tok == .key_enum {
			tok = fastc_emit_enum_declaration(mut scan, source_file, next_enum_is_flag, mut out)!
			next_enum_is_flag = false
			continue
		}
		if depth == 0 && tok == .key_interface {
			tok = fastc_emit_interface_declaration(mut scan, source_file, mut out)!
			continue
		}
		if depth == 0 && tok == .key_type {
			tok = fastc_emit_alias_declaration(mut scan, source_file, declared_types,
				declared_kinds, mut struct_fields, mut out)!
			continue
		}
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr && depth > 0 {
			depth--
		}
		tok = scan.scan()
	}
}

fn fastc_scan_type_attribute(mut scan scanner.Scanner) !(token.Token, bool) {
	mut tok := scan.scan()
	mut depth := 1
	mut is_flag := false
	for depth > 0 {
		if tok == .eof {
			return error('fastc parser does not support unfinished type attribute')
		}
		if tok == .name && scan.lit == 'flag' {
			is_flag = true
		}
		if tok == .lsbr {
			depth++
		} else if tok == .rsbr {
			depth--
		}
		tok = scan.scan()
	}
	return tok, is_flag
}

fn fastc_emit_struct_declaration(mut scan scanner.Scanner, is_union bool, source_file FastcSourceFile, declared_types map[string]bool, mut struct_fields map[string]map[string]string, mut composite_types map[string]bool, mut out strings.Builder) !token.Token {
	mut tok := scan.scan()
	if tok != .name {
		return error('fastc parser does not support struct declaration in ${source_file.path}')
	}
	name := scan.lit
	tok = scan.scan()
	if name == 'C' && tok == .dot {
		return fastc_skip_type_declaration(mut scan, tok)
	}
	key := fastc_type_key(source_file.header.module_name, name)
	c_name := fastc_c_declared_type_name(key)
	mut fields_by_name := map[string]string{}
	if c_name in struct_fields {
		fields_by_name = struct_fields[c_name].clone()
	}
	if tok == .lsbr {
		tok = fastc_skip_balanced_tokens(mut scan, tok, .lsbr, .rsbr)!
	}
	if tok != .lcbr {
		return error('fastc parser does not support struct `${name}` body in ${source_file.path}')
	}
	out.writeln('${if is_union { 'union' } else { 'struct' }} ${c_name} {')
	tok = scan.scan()
	mut fields := 0
	mut embedded_id := 0
	for tok != .rcbr && tok != .eof {
		if tok in [.semicolon, .comma] {
			tok = scan.scan()
			continue
		}
		if tok == .attribute {
			tok = fastc_skip_attribute(mut scan)!
			continue
		}
		if tok in [.key_pub, .key_mut, .key_global] {
			tok = scan.scan()
			if tok == .colon {
				tok = scan.scan()
			}
			continue
		}
		if tok != .name {
			return error('fastc parser does not support struct `${name}` field token `${tok.str()}` in ${source_file.path}')
		}
		mut field_names := [scan.lit]
		tok = scan.scan()
		for tok == .comma {
			tok = scan.scan()
			if tok != .name {
				return error('fastc parser does not support struct `${name}` grouped field in ${source_file.path}')
			}
			field_names << scan.lit
			tok = scan.scan()
		}
		if tok == .semicolon || tok == .rcbr {
			embedded_key := fastc_resolve_declared_type_key(source_file.header.module_name,
				field_names[0], declared_types) or {
				return error('fastc parser does not support embedded field `${field_names[0]}` in ${source_file.path}')
			}
			out.writeln('\t${fastc_c_declared_type_name(embedded_key)} __embedded_${embedded_id};')
			fields_by_name['__embedded_${embedded_id}'] = fastc_c_declared_type_name(embedded_key)
			embedded_id++
			fields++
			if tok == .semicolon {
				tok = scan.scan()
			}
			continue
		}
		field_type, next_token := fastc_scan_type(mut scan, tok, source_file.path,
			source_file.header.module_name, source_file.header.imports, declared_types) or {
			return error('fastc struct `${name}` field `${field_names[0]}`: ${err.msg()}')
		}
		tok = next_token
		fastc_register_composite_type(field_type, mut composite_types)
		for field_name in field_names {
			if element_type := fastc_fixed_array_element_type(field_type) {
				length := fastc_fixed_array_length(field_type) or {
					return error('invalid fixed array type')
				}
				out.writeln('\t${element_type} ${field_name}[${length}];')
			} else {
				out.writeln('\t${field_type} ${field_name};')
			}
			fields_by_name[field_name] = field_type
			fields++
		}
		if tok == .assign {
			tok = fastc_skip_field_default(mut scan)!
		}
		if tok == .semicolon {
			tok = scan.scan()
		}
	}
	if tok != .rcbr {
		return error('fastc parser does not support unfinished struct `${name}` in ${source_file.path}')
	}
	if fields == 0 {
		out.writeln('\tunsigned char __empty;')
	}
	if c_name in ['Option', '_option', '_result'] {
		out.writeln('\tvoid *data;')
		fields_by_name['data'] = 'voidptr'
	}
	out.writeln('};')
	out.writeln('')
	struct_fields[c_name] = fields_by_name.clone()
	return scan.scan()
}

fn fastc_resolve_declared_type_key(module_name string, raw_type string, declared_types map[string]bool) ?string {
	local_key := fastc_type_key(module_name, raw_type)
	if local_key in declared_types {
		return local_key
	}
	if raw_type in declared_types {
		return raw_type
	}
	mut matching_key := ''
	for candidate in declared_types.keys() {
		if candidate.ends_with('.${raw_type}') {
			if matching_key != '' {
				return none
			}
			matching_key = candidate
		}
	}
	return if matching_key == '' { none } else { matching_key }
}

fn fastc_semantic_declared_type_key(c_type string, declared_types map[string]bool) string {
	base := c_type.trim_right('*')
	for key in declared_types.keys() {
		if fastc_c_declared_type_name(key) == base {
			return key
		}
	}
	return base
}

fn fastc_emit_enum_declaration(mut scan scanner.Scanner, source_file FastcSourceFile, is_flag bool, mut out strings.Builder) !token.Token {
	mut tok := scan.scan()
	if tok != .name {
		return error('fastc parser does not support enum declaration in ${source_file.path}')
	}
	name := scan.lit
	key := fastc_type_key(source_file.header.module_name, name)
	c_name := fastc_c_declared_type_name(key)
	tok = scan.scan()
	if tok != .lcbr {
		return error('fastc parser does not support enum `${name}` body in ${source_file.path}')
	}
	tok = scan.scan()
	mut value := 0
	for tok != .rcbr && tok != .eof {
		if tok in [.semicolon, .comma] {
			tok = scan.scan()
			continue
		}
		if tok == .attribute {
			tok = fastc_skip_attribute(mut scan)!
			continue
		}
		if tok != .name {
			return error('fastc parser does not support enum `${name}` field in ${source_file.path}')
		}
		field_name := scan.lit
		tok = scan.scan()
		if tok == .assign {
			tok = scan.scan()
			if tok == .number {
				value = scan.lit.int()
				tok = scan.scan()
			} else {
				tok = fastc_skip_field_default_from_token(mut scan, tok)!
			}
		}
		c_value := if is_flag { 1 << value } else { value }
		out.writeln('#define ${c_name}__${field_name} ((${c_name})${c_value})')
		value++
	}
	if tok != .rcbr {
		return error('fastc parser does not support unfinished enum `${name}` in ${source_file.path}')
	}
	out.writeln('')
	return scan.scan()
}

fn fastc_emit_interface_declaration(mut scan scanner.Scanner, source_file FastcSourceFile, mut out strings.Builder) !token.Token {
	mut tok := scan.scan()
	if tok != .name {
		return error('fastc parser does not support interface declaration in ${source_file.path}')
	}
	name := scan.lit
	c_name := fastc_c_declared_type_name(fastc_type_key(source_file.header.module_name, name))
	tok = scan.scan()
	if tok != .lcbr {
		return error('fastc parser does not support interface `${name}` body in ${source_file.path}')
	}
	tok = fastc_skip_balanced_tokens(mut scan, tok, .lcbr, .rcbr)!
	out.writeln('struct ${c_name} { void *_object; u32 _typ; void *_methods; };')
	out.writeln('')
	return tok
}

fn fastc_emit_alias_declaration(mut scan scanner.Scanner, source_file FastcSourceFile, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, mut struct_fields map[string]map[string]string, mut out strings.Builder) !token.Token {
	mut tok := scan.scan()
	if tok != .name {
		return error('fastc parser does not support type alias in ${source_file.path}')
	}
	name := scan.lit
	key := fastc_type_key(source_file.header.module_name, name)
	c_name := fastc_c_declared_type_name(key)
	tok = scan.scan()
	if name == 'C' && tok == .dot {
		return fastc_skip_type_declaration(mut scan, tok)
	}
	if tok != .assign {
		return error('fastc parser does not support type `${name}` declaration in ${source_file.path}')
	}
	tok = scan.scan()
	if tok == .key_fn {
		return fastc_emit_function_alias(mut scan, source_file, declared_types, c_name, mut out)
	}
	base, next_token := fastc_scan_type(mut scan, tok, source_file.path,
		source_file.header.module_name, source_file.header.imports, declared_types) or {
		return error('fastc type `${name}`: ${err.msg()}')
	}
	tok = next_token
	if tok == .pipe {
		for tok != .semicolon && tok != .eof {
			tok = scan.scan()
		}
		out.writeln('typedef struct { void *_object; u32 _typ; } ${c_name};')
	} else if fastc_primitive_c_type(name) == none && declared_kinds[key] == .alias_ {
		out.writeln('typedef ${base} ${c_name};')
		mut layout_type := base.trim_right('*')
		if layout_type.starts_with('Array_') {
			layout_type = 'array'
		} else if layout_type.starts_with('Map_') {
			layout_type = 'map'
		}
		mut alias_fields := map[string]string{}
		if layout_type in struct_fields {
			alias_fields = struct_fields[layout_type].clone()
		}
		if base.starts_with('Array_') {
			alias_fields['__fastc_element_type'] = base['Array_'.len..]
		}
		if alias_fields.len > 0 {
			struct_fields[c_name] = alias_fields.clone()
		}
	}
	if tok == .semicolon {
		tok = scan.scan()
	}
	return tok
}

fn fastc_emit_function_alias(mut scan scanner.Scanner, source_file FastcSourceFile, declared_types map[string]bool, c_name string, mut out strings.Builder) !token.Token {
	mut tok := scan.scan()
	if tok != .lpar {
		return error('fastc parser does not support function type `${c_name}` in ${source_file.path}')
	}
	tok = scan.scan()
	mut parameter_types := []string{}
	for tok != .rpar {
		if tok in [.comma, .semicolon] {
			tok = scan.scan()
			continue
		}
		mut parameter_is_mut := false
		if tok == .key_mut {
			parameter_is_mut = true
			tok = scan.scan()
		}
		mut has_parameter_name := false
		if !parameter_is_mut && tok == .name {
			mut lookahead := scan
			next_token := lookahead.scan()
			has_parameter_name = next_token in [.name, .amp, .and, .mul, .question, .not, .key_fn]
		}
		if has_parameter_name {
			tok = scan.scan()
			parameter_type, next_token := fastc_scan_type(mut scan, tok, source_file.path,
				source_file.header.module_name, source_file.header.imports, declared_types)!
			parameter_types << parameter_type
			tok = next_token
		} else {
			parameter_type, next_token := fastc_scan_type(mut scan, tok, source_file.path,
				source_file.header.module_name, source_file.header.imports, declared_types)!
			parameter_types << if parameter_is_mut && !parameter_type.ends_with('*') {
				parameter_type + '*'
			} else {
				parameter_type
			}
			tok = next_token
		}
	}
	tok = scan.scan()
	mut return_type := 'void'
	if tok !in [.semicolon, .eof] {
		return_type, tok = fastc_scan_type(mut scan, tok, source_file.path,
			source_file.header.module_name, source_file.header.imports, declared_types)!
	}
	out.writeln('typedef ${return_type} (*${c_name})(${if parameter_types.len == 0 {
		'void'
	} else {
		parameter_types.join(', ')
	}});')
	if tok == .semicolon {
		tok = scan.scan()
	}
	return tok
}

fn fastc_skip_attribute(mut scan scanner.Scanner) !token.Token {
	mut tok := scan.scan()
	mut depth := 1
	for depth > 0 {
		if tok == .eof {
			return error('fastc parser does not support unfinished attribute')
		}
		if tok == .lsbr {
			depth++
		} else if tok == .rsbr {
			depth--
		}
		tok = scan.scan()
	}
	return tok
}

fn fastc_skip_balanced_tokens(mut scan scanner.Scanner, first token.Token, open token.Token, close token.Token) !token.Token {
	mut tok := first
	mut depth := 0
	for {
		if tok == open {
			depth++
		} else if tok == close {
			depth--
			if depth == 0 {
				return scan.scan()
			}
		} else if tok == .eof {
			return error('fastc parser does not support unfinished `${open.str()}` group')
		}
		tok = scan.scan()
	}
	return tok
}

fn fastc_skip_field_default(mut scan scanner.Scanner) !token.Token {
	return fastc_skip_field_default_from_token(mut scan, scan.scan())
}

fn fastc_skip_field_default_from_token(mut scan scanner.Scanner, first token.Token) !token.Token {
	mut tok := first
	mut parens := 0
	mut brackets := 0
	mut braces := 0
	for tok != .eof {
		if parens == 0 && brackets == 0 && braces == 0 && tok in [.semicolon, .rcbr] {
			return tok
		}
		match tok {
			.lpar { parens++ }
			.rpar { parens-- }
			.lsbr { brackets++ }
			.rsbr { brackets-- }
			.lcbr { braces++ }
			.rcbr { braces-- }
			else {}
		}
		tok = scan.scan()
	}
	return tok
}

fn fastc_skip_type_declaration(mut scan scanner.Scanner, first token.Token) !token.Token {
	mut tok := first
	for tok != .eof && tok != .lcbr && tok != .semicolon {
		tok = scan.scan()
	}
	if tok == .lcbr {
		return fastc_skip_balanced_tokens(mut scan, tok, .lcbr, .rcbr)
	}
	return if tok == .semicolon { scan.scan() } else { tok }
}

fn collect_function_signatures(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, declared_types map[string]bool, mut functions map[string]FastcFunctionSignature) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut brace_depth := 0
	mut previous_tok := token.Token.unknown
	mut tok := scan.scan()
	for tok != .eof {
		if tok == .key_fn && brace_depth == 0 && previous_tok != .assign {
			tok = scan.scan()
			mut parameter_types := []string{}
			mut receiver_type := ''
			mut receiver_key := ''
			if tok == .lpar {
				tok = scan.scan()
				mut receiver_is_mut := false
				if tok == .key_mut {
					receiver_is_mut = true
					tok = scan.scan()
				}
				if tok != .name {
					return error('fastc parser does not support method receiver in ${path}')
				}
				tok = scan.scan()
				if tok == .name && scan.lit != 'C' {
					receiver_key = fastc_type_key(header.module_name, scan.lit)
				} else if tok == .key_none {
					receiver_key = 'none'
				}
				receiver_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name,
					header.imports, declared_types) or {
					return error('fastc method receiver: ${err.msg()}')
				}
				if receiver_key == '' {
					receiver_key = fastc_semantic_declared_type_key(receiver_type, declared_types)
				}
				if receiver_is_mut && !receiver_type.ends_with('*') {
					receiver_type += '*'
				}
				parameter_types << receiver_type
				if tok != .rpar {
					return error('fastc parser does not support method receiver separator in ${path}')
				}
				tok = scan.scan()
			}
			if tok != .name && !(receiver_type != '' && (tok.is_overloadable() || tok.is_keyword())) {
				return error('fastc parser does not support function declaration token `${tok.str()}` `${scan.lit}` in ${path}')
			}
			mut name := if tok == .name || tok.is_keyword() { scan.lit } else { tok.str() }
			tok = scan.scan()
			mut is_c_function := false
			if receiver_type == '' && name == 'C' && tok == .dot {
				is_c_function = true
				tok = scan.scan()
				if tok != .name && !tok.is_keyword() {
					return error('fastc parser does not support C function declaration in ${path}')
				}
				name = scan.lit
				tok = scan.scan()
			} else if receiver_type == '' && tok == .dot {
				type_key := fastc_type_key(header.module_name, name)
				if type_key !in declared_types {
					return error('fastc parser does not support static method owner `${name}` in ${path}')
				}
				receiver_type = fastc_c_declared_type_name(type_key)
				receiver_key = type_key
				tok = scan.scan()
				if tok != .name {
					return error('fastc parser does not support static method declaration in ${path}')
				}
				name = scan.lit
				tok = scan.scan()
			}
			function_key := if is_c_function {
				'C.${name}'
			} else if receiver_type == '' {
				fastc_function_key(header.module_name, name)
			} else {
				'${receiver_key}.${name}'
			}
			if tok == .lsbr {
				mut generic_depth := 1
				for generic_depth > 0 {
					tok = scan.scan()
					if tok == .eof {
						return error('fastc parser does not support unfinished generic function in ${path}')
					}
					if tok == .lsbr {
						generic_depth++
					} else if tok == .rsbr {
						generic_depth--
					}
				}
				tok = scan.scan()
			}
			if tok != .lpar {
				return error('fastc parser does not support function `${name}` declaration in ${path}')
			}
			tok = scan.scan()
			mut is_variadic := false
			for tok != .rpar {
				mut parameter_is_mut := false
				if tok in [.key_mut, .key_shared] {
					parameter_is_mut = true
					tok = scan.scan()
				}
				if is_c_function && tok != .name {
					parameter_type, next_token := fastc_scan_type(mut scan, tok, path,
						header.module_name, header.imports, declared_types) or {
						return error('fastc C function `${name}` parameter: ${err.msg()}')
					}
					parameter_types << parameter_type
					tok = next_token
					if tok == .comma {
						tok = scan.scan()
					}
					continue
				}
				if tok != .name {
					return error('fastc parser does not support function `${name}` parameter token `${tok.str()}` in ${path}')
				}
				parameter_name_or_type := scan.lit
				tok = scan.scan()
				if is_c_function && tok == .dot {
					tok = scan.scan()
					if parameter_name_or_type != 'C' || tok != .name {
						return error('fastc parser does not support qualified C parameter type in ${path}')
					}
					parameter_types << scan.lit
					tok = scan.scan()
					if tok == .comma {
						tok = scan.scan()
					}
					continue
				}
				if is_c_function && tok in [.comma, .rpar] {
					parameter_type := fastc_primitive_c_type(parameter_name_or_type) or {
						return error('fastc parser does not support undeclared C parameter type `${parameter_name_or_type}` in ${path}')
					}
					parameter_types << parameter_type
					if tok == .comma {
						tok = scan.scan()
					}
					continue
				}
				if tok == .comma {
					return error('fastc parser does not support grouped parameter names in ${path}')
				}
				if tok == .ellipsis {
					is_variadic = true
				}
				parameter_type, next_token := fastc_scan_type(mut scan, tok, path,
					header.module_name, header.imports, declared_types) or {
					return error('fastc function `${name}` parameter: ${err.msg()}')
				}
				parameter_types << if parameter_is_mut && !parameter_type.ends_with('*') {
					parameter_type + '*'
				} else {
					parameter_type
				}
				tok = next_token
				if tok == .comma {
					tok = scan.scan()
					continue
				}
				if tok != .rpar {
					return error('fastc parser does not support function parameter separator in ${path}')
				}
			}
			tok = scan.scan()
			mut return_type := 'void'
			mut return_types := []string{}
			mut option_type := ''
			if tok != .lcbr && tok != .semicolon {
				if tok in [.not, .question] {
					tok = scan.scan()
					return_type = 'Option'
					if tok in [.lcbr, .semicolon] {
						option_type = 'void'
					} else if tok == .lpar {
						return_types, tok = fastc_scan_multi_return_types(mut scan, path,
							header.module_name, header.imports, declared_types)!
						option_type = 'MultiReturn'
					} else {
						option_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name,
							header.imports, declared_types)!
					}
				} else if tok == .lpar {
					return_types, tok = fastc_scan_multi_return_types(mut scan, path,
						header.module_name, header.imports, declared_types)!
					return_type = 'MultiReturn'
				} else {
					return_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name,
						header.imports, declared_types) or {
						return error('fastc function `${name}` return: ${err.msg()}')
					}
				}
			}
			if tok != .lcbr && tok != .semicolon {
				return error('fastc parser does not support function `${name}` body in ${path}')
			}
			signature := FastcFunctionSignature{
				parameter_types: parameter_types
				return_type:     return_type
				return_types:    return_types
				option_type:     option_type
				is_variadic:     is_variadic
				path:            path
			}
			if previous := functions[function_key] {
				if !is_c_function {
					is_c_override := previous.path.ends_with('.c.v') || path.ends_with('.c.v')
					if previous.path == path || !is_c_override
						|| !fastc_string_types_equal(previous.parameter_types, signature.parameter_types)
						|| previous.return_type != signature.return_type {
						return error('fastc parser does not support duplicate function `${name}` in ${path}')
					}
					if previous.path.ends_with('.c.v') {
						continue
					}
				}
			}
			functions[function_key] = signature
			continue
		}
		if tok == .lcbr {
			brace_depth++
		} else if tok == .rcbr && brace_depth > 0 {
			brace_depth--
		}
		previous_tok = tok
		tok = scan.scan()
	}
}

fn fastc_collect_referenced_function_names(sources []FastcSourceFile, prefs &pref.Preferences, functions map[string]FastcFunctionSignature) map[string]bool {
	mut available_names := map[string]bool{}
	for key in functions.keys() {
		available_names[key.all_after_last('.')] = true
	}
	mut references := map[string]map[string]bool{}
	mut top_level_references := map[string]bool{}
	for source_file in sources {
		mut file_set := token.FileSet.new()
		mut file := file_set.add_file(source_file.path, source_file.source.len)
		file.index_lines(source_file.source)
		mut scan := scanner.new_scanner(prefs, .normal)
		scan.init(file, source_file.source)
		mut previous := token.Token.unknown
		mut tok := scan.scan()
		for tok != .eof {
			if tok != .key_fn || previous == .assign {
				if tok == .name && scan.lit in available_names {
					top_level_references[scan.lit] = true
				}
				previous = tok
				tok = scan.scan()
				continue
			}
			tok = scan.scan()
			if tok == .lpar {
				tok = fastc_skip_balanced_tokens(mut scan, tok, .lpar, .rpar) or { break }
			}
			if tok != .name && !tok.is_overloadable() && !tok.is_keyword() {
				previous = tok
				tok = scan.scan()
				continue
			}
			mut function_name := if tok == .name || tok.is_keyword() { scan.lit } else { tok.str() }
			tok = scan.scan()
			if tok == .dot {
				tok = scan.scan()
				if tok == .name {
					function_name = scan.lit
					tok = scan.scan()
				}
			}
			for tok !in [.lcbr, .semicolon, .eof] {
				tok = scan.scan()
			}
			if tok != .lcbr {
				previous = tok
				tok = scan.scan()
				continue
			}
			mut function_references := map[string]bool{}
			if function_name in references {
				function_references = references[function_name].clone()
			}
			mut depth := 1
			tok = scan.scan()
			for depth > 0 && tok != .eof {
				if tok == .lcbr {
					depth++
				} else if tok == .rcbr {
					depth--
				} else if tok == .name && scan.lit in available_names {
					function_references[scan.lit] = true
				}
				tok = scan.scan()
			}
			references[function_name] = function_references.clone()
			previous = .rcbr
		}
	}
	mut used := {
		'main':                   true
		'run':                    true
		'array_push':             true
		'push':                   true
		'push_many':              true
		'array_slice':            true
		'keys':                   true
		'get':                    true
		'get_check':              true
		'new_map':                true
		'set':                    true
		'values':                 true
		'map_hash_string':        true
		'map_eq_string':          true
		'map_clone_string':       true
		'map_free_string':        true
		'map_free_nop':           true
		'map_hash_int_1':         true
		'map_hash_int_2':         true
		'map_hash_int_4':         true
		'map_hash_int_8':         true
		'map_eq_int_1':           true
		'map_eq_int_2':           true
		'map_eq_int_4':           true
		'map_eq_int_8':           true
		'map_clone_int_1':        true
		'map_clone_int_2':        true
		'map_clone_int_4':        true
		'map_clone_int_8':        true
		'new_array_from_c_array': true
		'string_plus_many':       true
	}
	for name in top_level_references.keys() {
		used[name] = true
	}
	mut changed := true
	for changed {
		changed = false
		for name in used.keys() {
			if name !in references {
				continue
			}
			referenced := references[name].clone()
			for referenced_name in referenced.keys() {
				if referenced_name !in used {
					used[referenced_name] = true
					changed = true
				}
			}
		}
	}
	return used
}

fn collect_interface_method_signatures(source string, path string, header FastcSourceHeader, prefs &pref.Preferences, declared_types map[string]bool, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool) ! {
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(path, source.len)
	file.index_lines(source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut tok := scan.scan()
	mut depth := 0
	for tok != .eof {
		if depth != 0 || tok != .key_interface {
			if tok == .lcbr {
				depth++
			} else if tok == .rcbr && depth > 0 {
				depth--
			}
			tok = scan.scan()
			continue
		}
		tok = scan.scan()
		if tok != .name {
			return error('fastc parser does not support interface declaration in ${path}')
		}
		interface_key := fastc_type_key(header.module_name, scan.lit)
		interface_type := fastc_c_declared_type_name(interface_key)
		tok = scan.scan()
		if tok == .lsbr {
			tok = fastc_skip_balanced_tokens(mut scan, tok, .lsbr, .rsbr)!
		}
		if tok != .lcbr {
			return error('fastc parser does not support interface body in ${path}')
		}
		tok = scan.scan()
		for tok != .rcbr && tok != .eof {
			if tok in [.semicolon, .comma, .key_pub, .key_mut] {
				tok = scan.scan()
				continue
			}
			if tok != .name {
				tok = scan.scan()
				continue
			}
			method_name := scan.lit
			tok = scan.scan()
			if tok != .lpar {
				for tok !in [.semicolon, .rcbr, .eof] {
					tok = scan.scan()
				}
				continue
			}
			mut parameter_types := [interface_type]
			tok = scan.scan()
			for tok != .rpar {
				if tok in [.key_mut, .key_shared] {
					tok = scan.scan()
				}
				if tok != .name {
					return error('fastc parser does not support interface method parameter in ${path}')
				}
				tok = scan.scan()
				parameter_type, next_token := fastc_scan_type(mut scan, tok, path,
					header.module_name, header.imports, declared_types)!
				parameter_types << parameter_type
				tok = next_token
				if tok == .comma {
					tok = scan.scan()
				}
			}
			tok = scan.scan()
			mut return_type := 'void'
			mut return_types := []string{}
			mut option_type := ''
			if tok !in [.semicolon, .rcbr] {
				if tok in [.not, .question] {
					tok = scan.scan()
					return_type = 'Option'
					if tok in [.semicolon, .rcbr] {
						option_type = 'void'
					} else {
						option_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name,
							header.imports, declared_types)!
					}
				} else if tok == .lpar {
					return_types, tok = fastc_scan_multi_return_types(mut scan, path,
						header.module_name, header.imports, declared_types)!
					return_type = 'MultiReturn'
				} else {
					return_type, tok = fastc_scan_type(mut scan, tok, path, header.module_name,
						header.imports, declared_types)!
				}
			}
			interface_method_key := '${interface_key}.${method_name}'
			functions[interface_method_key] = FastcFunctionSignature{
				parameter_types: parameter_types
				return_type:     return_type
				return_types:    return_types
				option_type:     option_type
				path:            path
			}
			interface_methods[interface_method_key] = true
		}
		if tok == .rcbr {
			tok = scan.scan()
		}
	}
}

fn fastc_scan_multi_return_types(mut scan scanner.Scanner, path string, module_name string, imports map[string]string, declared_types map[string]bool) !([]string, token.Token) {
	mut types := []string{}
	mut tok := scan.scan()
	for tok != .rpar {
		if tok in [.semicolon, .comma] {
			tok = scan.scan()
			continue
		}
		component_type, next_token := fastc_scan_type(mut scan, tok, path, module_name, imports,
			declared_types)!
		types << component_type
		tok = next_token
		if tok == .comma {
			tok = scan.scan()
		}
		if tok == .eof {
			return error('fastc parser does not support unfinished multi-return type in ${path}')
		}
	}
	return types, scan.scan()
}

fn fastc_scan_type(mut scan scanner.Scanner, first token.Token, path string, module_name string, imports map[string]string, declared_types map[string]bool) !(string, token.Token) {
	mut tok := first
	mut optional := false
	if tok in [.question, .not] {
		optional = true
		tok = scan.scan()
	}
	mut pointers := 0
	for tok == .amp || tok == .and || tok == .mul {
		pointers += if tok == .and { 2 } else { 1 }
		tok = scan.scan()
	}
	if optional && tok == .lcbr {
		return 'Option' + '*'.repeat(pointers), tok
	}
	if tok == .ellipsis {
		tok = scan.scan()
		element_type, next_token := fastc_scan_type(mut scan, tok, path, module_name, imports,
			declared_types)!
		tok = next_token
		return fastc_array_c_type(element_type) + '*'.repeat(pointers), tok
	}
	if tok == .lpar {
		mut depth := 1
		for depth > 0 {
			tok = scan.scan()
			if tok == .eof {
				return error('fastc parser does not support unfinished multi-return type in ${path}')
			}
			if tok == .lpar {
				depth++
			} else if tok == .rpar {
				depth--
			}
		}
		tok = scan.scan()
		return 'MultiReturn' + '*'.repeat(pointers), tok
	}
	if tok == .lsbr {
		tok = scan.scan()
		mut is_dynamic := false
		mut fixed_length := ''
		if tok == .rsbr {
			is_dynamic = true
			tok = scan.scan()
		} else if tok in [.name, .number] {
			fixed_length = scan.lit
			if tok == .name {
				fixed_length = fastc_c_constant_name(module_name, fixed_length)
			}
			tok = scan.scan()
			if tok != .rsbr {
				fixed_length = ''
				mut depth := 1
				for depth > 0 && tok != .eof {
					if tok == .lsbr {
						depth++
					} else if tok == .rsbr {
						depth--
					}
					tok = scan.scan()
				}
			} else {
				tok = scan.scan()
			}
		} else {
			mut depth := 1
			for depth > 0 && tok != .eof {
				if tok == .lsbr {
					depth++
				} else if tok == .rsbr {
					depth--
				}
				tok = scan.scan()
			}
		}
		element_type, next_token := fastc_scan_type(mut scan, tok, path, module_name, imports,
			declared_types)!
		tok = next_token
		array_type := if optional {
			'Option'
		} else if is_dynamic {
			fastc_array_c_type(element_type)
		} else if fixed_length != '' {
			fastc_fixed_array_type(fixed_length, element_type)
		} else {
			'array'
		}
		return array_type + '*'.repeat(pointers), tok
	}
	if tok == .key_fn {
		mut paren_depth := 0
		for {
			tok = scan.scan()
			if tok == .lpar {
				paren_depth++
			} else if tok == .rpar {
				paren_depth--
				if paren_depth == 0 {
					tok = scan.scan()
					break
				}
			} else if tok == .eof {
				return error('fastc parser does not support unfinished function type in ${path}')
			}
		}
		if tok == .name || tok == .amp || tok == .and || tok == .mul || tok == .question
			|| tok == .not || tok == .lsbr || tok == .lpar || tok == .key_fn {
			_, tok = fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types)!
		}
		return 'voidptr' + '*'.repeat(pointers), tok
	}
	if tok == .key_none {
		tok = scan.scan()
		return 'voidptr' + '*'.repeat(pointers), tok
	}
	if tok != .name {
		return error('fastc parser does not support type `${tok.str()}` in ${path}')
	}
	mut raw_type := scan.lit
	tok = scan.scan()
	if raw_type == 'chan' {
		if tok !in [.comma, .rpar, .lcbr, .semicolon, .assign] {
			_, tok = fastc_scan_type(mut scan, tok, path, module_name, imports, declared_types)!
		}
		channel_type := if optional { 'Option' } else { 'chan' + '*'.repeat(pointers) }
		return channel_type, tok
	}
	if raw_type == 'map' && tok == .lsbr {
		tok = scan.scan()
		key_type, next_key_token := fastc_scan_type(mut scan, tok, path, module_name, imports,
			declared_types)!
		tok = next_key_token
		if tok != .rsbr {
			return error('fastc parser does not support unfinished map key type in ${path}')
		}
		tok = scan.scan()
		value_type, next_value_token := fastc_scan_type(mut scan, tok, path, module_name, imports,
			declared_types)!
		tok = next_value_token
		base := if optional { 'Option' } else { fastc_map_c_type(key_type, value_type) }
		return base + '*'.repeat(pointers), tok
	}
	mut type_module := module_name
	if tok == .dot {
		tok = scan.scan()
		if tok != .name {
			return error('fastc parser does not support qualified type `${raw_type}` in ${path}')
		}
		type_module = imports[raw_type] or { raw_type }
		raw_type = scan.lit
		tok = scan.scan()
	}
	type_key := fastc_type_key(type_module, raw_type)
	mut base := ''
	if type_module == 'C' {
		base = if '#Cstruct#${raw_type}' in declared_types { 'struct ${raw_type}' } else { raw_type }
	} else if type_key in declared_types {
		base = fastc_c_declared_type_name(type_key)
	} else {
		base = fastc_primitive_c_type(raw_type) or { '' }
	}
	if base == '' {
		if type_key !in declared_types {
			if raw_type.len <= 3 && raw_type[0].is_capital() {
				base = 'voidptr'
			}
			if base == '' {
				mut matching_key := ''
				for candidate in declared_types.keys() {
					if candidate.ends_with('.${raw_type}') || candidate == raw_type {
						if matching_key != '' {
							return error('fastc parser cannot resolve ambiguous type `${raw_type}` in ${path}')
						}
						matching_key = candidate
					}
				}
				if matching_key == '' {
					return error('fastc parser does not support undeclared type `${raw_type}` before `${tok.str()}` at byte ${scan.pos} in ${path}')
				}
				base = fastc_c_declared_type_name(matching_key)
			}
		} else {
			base = fastc_c_declared_type_name(type_key)
		}
	}
	if tok == .lsbr {
		mut depth := 1
		for depth > 0 {
			tok = scan.scan()
			if tok == .eof {
				return error('fastc parser does not support unfinished generic type in ${path}')
			}
			if tok == .lsbr {
				depth++
			} else if tok == .rsbr {
				depth--
			}
		}
		tok = scan.scan()
	}
	if optional {
		return 'Option', tok
	}
	return base + '*'.repeat(pointers), tok
}

fn fastc_composite_type_part(typ string) string {
	return typ.replace(' ', '_').replace('*', '_ptr').replace('.', '__')
}

fn fastc_fixed_array_type(length string, element_type string) string {
	return 'FixedArray_${length}_FASTC_ARRAY_OF_${element_type}'
}

fn fastc_fixed_array_length(typ string) ?string {
	if !typ.starts_with('FixedArray_') || !typ.contains('_FASTC_ARRAY_OF_') {
		return none
	}
	details := typ['FixedArray_'.len..]
	return details.all_before('_FASTC_ARRAY_OF_')
}

fn fastc_fixed_array_element_type(typ string) ?string {
	if !typ.starts_with('FixedArray_') || !typ.contains('_FASTC_ARRAY_OF_') {
		return none
	}
	return typ.all_after('_FASTC_ARRAY_OF_')
}

fn fastc_array_c_type(element_type string) string {
	return 'Array_${fastc_composite_type_part(element_type)}'
}

fn fastc_map_c_type(key_type string, value_type string) string {
	return 'Map_${fastc_composite_type_part(key_type)}_${fastc_composite_type_part(value_type)}'
}

fn fastc_map_key_value_types(typ string) ?(string, string) {
	base := typ.trim_right('*')
	if !base.starts_with('Map_') {
		return none
	}
	payload := base['Map_'.len..]
	// Composite C type names can contain underscores, so splitting every
	// underscore loses the value type. Map keys are restricted to scalar V
	// types; use that boundary and retain the complete encoded value type.
	for key_type in ['string', 'rune', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64',
		'byte', 'char', 'uint', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr', 'bool'] {
		prefix := '${fastc_composite_type_part(key_type)}_'
		if payload.starts_with(prefix) {
			mut value_type := payload[prefix.len..]
			if value_type.ends_with('_ptr') {
				value_type = value_type[..value_type.len - '_ptr'.len] + '*'
			}
			return key_type, value_type
		}
	}
	return none
}

fn fastc_register_composite_type(typ string, mut composite_types map[string]bool) {
	base := typ.trim_right('*')
	if base.starts_with('Array_') || base.starts_with('Map_') {
		composite_types[base] = true
	}
}

fn fastc_type_key(module_name string, name string) string {
	if module_name in ['', 'main', 'builtin'] {
		return name
	}
	return '${module_name}.${name}'
}

fn fastc_c_declared_type_name(type_key string) string {
	return type_key.replace('.', '__')
}

fn fastc_function_key(module_name string, name string) string {
	if module_name in ['', 'main'] {
		return name
	}
	return '${module_name}.${name}'
}

fn fastc_constant_key(module_name string, name string) string {
	if module_name in ['', 'main', 'builtin'] {
		return name
	}
	return '${module_name}.${name}'
}

fn fastc_c_constant_name(module_name string, name string) string {
	module_prefix := if module_name == '' { 'main' } else { module_name }
	return '${module_prefix.replace('.', '__')}__${name}'
}

fn fastc_global_key(module_name string, name string) string {
	if module_name in ['', 'main', 'builtin'] {
		return name
	}
	return '${module_name}.${name}'
}

fn fastc_c_global_name(key string) string {
	return key.replace('.', '__')
}

fn fastc_c_function_name(module_name string, name string) string {
	if module_name in ['', 'main'] {
		return name
	}
	return '${module_name.replace('.', '__')}__${name}'
}

fn (g &Parser) unqualified_function_key(name string) string {
	local_key := fastc_function_key(g.module_name, name)
	if local_key in g.functions {
		return local_key
	}
	builtin_key := fastc_function_key('builtin', name)
	if builtin_key in g.functions {
		return builtin_key
	}
	return local_key
}

fn fastc_c_function_name_for_key(key string) string {
	if !key.contains('.') {
		return key
	}
	return '${key.all_before_last('.').replace('.', '__')}__${key.all_after_last('.')}'
}

fn (mut g Parser) run() !string {
	g.next()
	g.parse_top_level_items(false)!
	return g.out.str()
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
		if g.selfhost && g.tok == .hash {
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
			g.parse_function(item_enabled)!
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

fn (mut g Parser) parse_top_level_comptime_if() ! {
	g.expect(.dollar)!
	g.expect(.key_if)!
	condition := g.parse_comptime_or()!
	g.expect(.lcbr)!
	if condition {
		g.parse_top_level_items(true)!
	} else {
		g.skip_open_block()!
	}
	if g.tok != .dollar || !g.dollar_keyword_is('else') {
		return
	}
	g.next()
	g.expect(.key_else)!
	if g.tok == .dollar {
		if condition {
			g.skip_comptime_if_chain()!
		} else {
			g.parse_top_level_comptime_if()!
		}
		return
	}
	g.expect(.lcbr)!
	if condition {
		g.skip_open_block()!
	} else {
		g.parse_top_level_items(true)!
	}
}

fn (mut g Parser) parse_c_directive() ! {
	directive := g.lit.trim_space()
	g.next()
	if directive.starts_with('flag ') || directive.starts_with('pkgconfig ') {
		// FastC's compiler invocation supplies its own target flags. The bootstrap
		// dependency set only uses these directives for optional libraries.
		return
	}
	mut c_directive := directive.replace('@VEXEROOT', g.prefs.vroot)
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

fn (mut g Parser) skip_attribute() !bool {
	if g.tok != .attribute {
		return true
	}
	g.next()
	mut depth := 1
	mut has_condition := false
	mut negate_condition := false
	mut condition_name := ''
	for depth > 0 {
		if g.tok == .eof {
			return g.unsupported('unfinished attribute')
		}
		if depth == 1 && g.tok == .key_if {
			has_condition = true
		} else if depth == 1 && has_condition && condition_name == '' && g.tok == .not {
			negate_condition = true
		} else if depth == 1 && has_condition && condition_name == '' && g.tok == .name {
			condition_name = g.lit
		} else if g.tok == .lsbr {
			depth++
		} else if g.tok == .rsbr {
			depth--
		}
		g.next()
	}
	if !has_condition || condition_name == '' {
		return true
	}
	value := pref.comptime_flag_value(g.prefs, condition_name)
	return if negate_condition { !value } else { value }
}

fn (mut g Parser) skip_top_level_declaration() ! {
	body_declaration := g.tok in [.key_struct, .key_union, .key_enum, .key_interface]
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

fn (mut g Parser) skip_semicolons() {
	for g.tok == .semicolon {
		g.next()
	}
}

fn (g &Parser) unsupported(feature string) IError {
	return error('fastc parser does not support ${feature} at byte ${g.s.pos} in ${g.path}')
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
	g.locals = map[string]FastcLocal{}
	g.next()
	mut receiver_type := ''
	mut receiver_key := ''
	mut receiver_name := ''
	mut receiver_is_mut := false
	mut params := []string{}
	if g.tok == .lpar {
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
			receiver_key = fastc_semantic_declared_type_key(receiver_type, g.declared_types)
		}
		g.expect(.rpar)!
		receiver_is_reference := receiver_is_mut && !receiver_type.ends_with('*')
		receiver_parameter_type := if receiver_is_reference {
			receiver_type + '*'
		} else {
			receiver_type
		}
		params << '${receiver_parameter_type} ${receiver_name}'
		g.locals[receiver_name] = FastcLocal{
			is_mut:       receiver_is_mut
			is_reference: receiver_is_reference
			typ:          receiver_parameter_type
		}
	}
	if g.tok != .name && !(receiver_type != '' && (g.tok.is_overloadable() || g.tok.is_keyword())) {
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
		if g.tok != .name {
			return g.unsupported('static method declaration')
		}
		name = g.lit
		g.next()
	}
	if g.tok == .lsbr {
		g.skip_balanced(.lsbr, .rsbr)!
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
	if !g.selfhost && (fastc_has_narrow_integer_type(return_type)
		|| fastc_parameters_have_narrow_integer_type(params)) {
		// C promotes narrow operands before arithmetic, while V retains the narrow
		// result type. Reject them until the direct parser tracks the required type.
		return g.unsupported('narrow integer function types')
	}
	function_key := if receiver_type == '' {
		fastc_function_key(g.module_name, name)
	} else {
		'${receiver_key}.${name}'
	}
	is_main := !is_static_method && receiver_type == '' && g.module_name in ['', 'main']
		&& name == 'main'
	if is_main {
		if params.len > 0 {
			return g.unsupported('main function with parameters')
		}
		if return_type != 'void' {
			return g.unsupported('main function returning `${return_type}`')
		}
	}
	if g.tok == .semicolon {
		g.next()
		return
	}
	is_fastc_source := name.starts_with('fastc_') || g.path.ends_with('/fastc/fastc.v')
		|| g.module_name.ends_with('fastc')
	if g.selfhost && name != 'fastc_collect_referenced_function_names' && !is_fastc_source
		&& name != 'main' && name !in g.used_function_names && name.len > 0
		&& (name[0].is_letter() || name[0] == `_`) {
		g.skip_balanced(.lcbr, .rcbr)!
		return
	}
	if signature := g.functions[function_key] {
		if signature.path != g.path && name != 'fastc_collect_referenced_function_names'
			&& !is_fastc_source {
			g.skip_balanced(.lcbr, .rcbr)!
			return
		}
	}
	c_name := if receiver_type == '' {
		fastc_c_function_name(g.module_name, name)
	} else {
		fastc_method_c_name(g.module_name, fastc_c_declared_type_name(receiver_key), name)
	}
	c_return_type := if is_main { 'int' } else { return_type }
	c_params := if is_main && g.selfhost {
		'int argc, char **argv'
	} else if params.len == 0 {
		'void'
	} else {
		params.join(', ')
	}
	g.protos.writeln('${c_return_type} ${c_name}(${c_params});')
	if !enabled {
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
	}
	previous_in_main := g.in_main
	previous_return_type := g.return_type
	previous_return_types := g.return_types.clone()
	previous_option_return_type := g.option_return_type
	previous_function := g.current_function
	previous_receiver := g.current_receiver
	previous_deferred_blocks := g.deferred_blocks.clone()
	g.in_main = is_main
	g.return_type = return_type
	g.return_types = return_types.clone()
	g.option_return_type = option_return_type
	g.current_function = name
	g.current_receiver = receiver_key
	g.deferred_blocks = [][]string{}
	terminates := g.parse_block_body()!
	g.write_deferred_blocks()
	g.in_main = previous_in_main
	g.return_type = previous_return_type
	g.return_types = previous_return_types.clone()
	g.option_return_type = previous_option_return_type
	g.current_function = previous_function
	g.current_receiver = previous_receiver
	g.deferred_blocks = previous_deferred_blocks.clone()
	if return_type != 'void' && !terminates {
		if !g.selfhost {
			return g.unsupported('non-void function `${name}` that can fall through')
		}
		// Self-host input was already accepted by the bootstrap compiler. Keep C's
		// control-flow rules satisfied when the streaming parser cannot prove that
		// every nested source branch terminates.
		g.write_line('return (${return_type}){0};')
	}
	if is_main {
		g.write_line('return 0;')
	}
	g.indent--
	g.write_line('}')
	g.out.writeln('')
}

fn fastc_method_c_name(module_name string, receiver_type string, name string) string {
	module_prefix := if module_name in ['', 'main'] {
		''
	} else {
		module_name.replace('.', '__') + '__'
	}
	receiver := receiver_type.trim_right('*').all_after_last('__')
	method := match name {
		'+' { 'plus' }
		'-' { 'minus' }
		'*' { 'mul' }
		'/' { 'div' }
		'==' { 'eq' }
		'!=' { 'ne' }
		'<' { 'lt' }
		'<=' { 'le' }
		'>' { 'gt' }
		'>=' { 'ge' }
		else { name }
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
	g.locals = map[string]FastcLocal{}
	g.has_main = true
	g.protos.writeln('int main(void);')
	g.write_line('int main(void) {')
	g.indent++
	g.write_line('setvbuf(stdout, NULL, _IONBF, 0);')
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
	g.skip_semicolons()
	for g.tok != .rpar {
		mut is_mut := false
		if g.tok in [.key_mut, .key_shared] {
			is_mut = true
			g.next()
		}
		if g.tok != .name {
			return g.unsupported('function parameters')
		}
		name := g.lit
		g.next()
		mut names := [name]
		for g.tok == .comma {
			g.next()
			if g.tok != .name {
				return g.unsupported('grouped parameter names')
			}
			names << g.lit
			g.next()
		}
		mut type_name := g.parse_type()!
		is_reference := is_mut && !type_name.ends_with('*')
		if is_reference {
			type_name += '*'
		}
		for parameter_name in names {
			params << '${type_name} ${parameter_name}'
			g.locals[parameter_name] = FastcLocal{
				is_mut:       is_mut
				is_reference: is_reference
				typ:          type_name
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
	type_name, next_token := fastc_scan_type(mut g.s, g.tok, g.path, g.module_name, g.imports,
		g.declared_types) or { return g.unsupported(err.msg()) }
	g.tok = next_token
	g.lit = g.s.lit
	if !g.selfhost && (first_lit in ['charptr', 'rune'] || type_name == 'char*') {
		return g.unsupported('type `${first_lit}`')
	}
	return type_name
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

fn fastc_has_narrow_integer_type(type_name string) bool {
	return type_name.trim_right('*') in ['byte', 'char', 'i8', 'i16', 'u8', 'u16']
}

fn fastc_parameter_has_narrow_integer_type(parameter string) bool {
	fields := parameter.fields()
	return fields.len > 0 && fastc_has_narrow_integer_type(fields[0])
}

fn fastc_parameters_have_narrow_integer_type(parameters []string) bool {
	for parameter in parameters {
		if fastc_parameter_has_narrow_integer_type(parameter) {
			return true
		}
	}
	return false
}

fn fastc_expression_tokens_contain(tokens []FastcExpressionToken, wanted token.Token) bool {
	for item in tokens {
		if item.tok == wanted {
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

fn (mut g Parser) parse_block_body() !bool {
	outer_locals := g.locals.clone()
	mut terminates := false
	g.skip_semicolons()
	for g.tok != .rcbr {
		if g.tok == .eof {
			return g.unsupported('unfinished block')
		}
		statement_terminates := g.parse_statement()!
		if statement_terminates {
			terminates = true
			if g.selfhost {
				for g.tok !in [.rcbr, .eof] {
					if g.tok == .lcbr {
						g.skip_balanced(.lcbr, .rcbr)!
					} else {
						g.next()
					}
				}
			}
		}
		g.skip_semicolons()
	}
	g.next()
	g.skip_semicolons()
	g.locals = outer_locals.clone()
	return terminates
}

fn (mut g Parser) parse_statement() !bool {
	return match g.tok {
		.dollar {
			g.parse_comptime_if_statement()!
		}
		.key_if {
			g.parse_if()!
		}
		.key_for {
			g.parse_for()!
		}
		.key_match {
			g.parse_match_statement()!
		}
		.key_select {
			g.parse_select_statement()!
		}
		.key_return {
			g.parse_return()!
		}
		.key_break {
			g.next()
			g.consume_statement_end()
			g.write_line('break;')
			false
		}
		.key_continue {
			g.next()
			g.consume_statement_end()
			g.write_line('continue;')
			false
		}
		.key_goto {
			g.next()
			if g.tok != .name {
				return g.unsupported('goto without a label')
			}
			label := g.lit
			g.next()
			g.consume_statement_end()
			g.write_line('goto ${label};')
			false
		}
		.key_defer {
			g.parse_defer()!
			false
		}
		.key_mut {
			g.parse_mutable_declaration()!
			false
		}
		.key_unsafe {
			g.next()
			g.expect(.lcbr)!
			g.unsafe_depth += 1
			terminates := g.parse_block_body()!
			g.unsafe_depth -= 1
			terminates
		}
		else {
			g.parse_simple_statement()!
			false
		}
	}
}

fn (mut g Parser) parse_select_statement() !bool {
	g.expect(.key_select)!
	g.expect(.lcbr)!
	g.skip_semicolons()
	for g.tok != .rcbr {
		if g.tok == .eof {
			return g.unsupported('unfinished select statement')
		}
		is_last := g.select_branch_is_last()
		for g.tok !in [.lcbr, .eof] {
			g.next()
		}
		g.expect(.lcbr)!
		if is_last {
			terminates := g.parse_block_body()!
			g.expect(.rcbr)!
			return terminates
		}
		g.skip_open_block()!
	}
	g.next()
	return false
}

fn (g &Parser) select_branch_is_last() bool {
	mut lookahead := scanner.new_scanner(g.prefs, .normal)
	lookahead.init(g.s.current_file(), g.s.src)
	lookahead.offset = g.s.offset
	mut tok := g.tok
	for tok !in [.lcbr, .eof] {
		tok = lookahead.scan()
	}
	if tok != .lcbr {
		return false
	}
	tok = fastc_skip_balanced_tokens(mut lookahead, tok, .lcbr, .rcbr) or { return false }
	for tok == .semicolon {
		tok = lookahead.scan()
	}
	return tok == .rcbr
}

fn (mut g Parser) parse_defer() ! {
	g.next()
	g.expect(.lcbr)!
	previous_capture := g.capturing_defer
	previous_lines := g.captured_defer_lines.clone()
	g.capturing_defer = true
	g.captured_defer_lines = []string{}
	_ = g.parse_block_body()!
	block := g.captured_defer_lines.clone()
	g.capturing_defer = previous_capture
	g.captured_defer_lines = previous_lines.clone()
	g.deferred_blocks << block
}

fn (mut g Parser) write_deferred_blocks() {
	for i := g.deferred_blocks.len - 1; i >= 0; i-- {
		for line in g.deferred_blocks[i] {
			g.out.writeln(line)
		}
	}
}

fn (mut g Parser) parse_match_statement() !bool {
	g.expect(.key_match)!
	subject := g.read_expression([token.Token.lcbr])!
	subject_type := fastc_normalize_inferred_type(g.last_expression_type)
	if subject == '' || subject_type == '' {
		return g.unsupported('unverifiable match subject')
	}
	g.expect(.lcbr)!
	subject_name := g.temporary_name('match')
	g.write_line('__typeof__((${subject})) ${subject_name} = (${subject});')
	is_string := subject_type == 'string'
	mut branch_index := 0
	mut all_terminate := true
	g.skip_semicolons()
	for g.tok != .rcbr {
		if g.tok == .eof {
			return g.unsupported('unfinished match statement')
		}
		is_else := g.tok == .key_else
		mut values := []string{}
		if is_else {
			g.next()
		} else {
			for {
				if g.tok == .dot {
					g.next()
					if g.tok != .name {
						return g.unsupported('match enum value')
					}
					values << '${subject_type.trim_right('*')}__${g.lit}'
					g.next()
				} else {
					value := g.read_expression([token.Token.comma, token.Token.lcbr])!
					if value == '' {
						return g.unsupported('empty match branch value')
					}
					values << value
				}
				if g.tok != .comma {
					break
				}
				g.next()
			}
		}
		g.expect(.lcbr)!
		if is_else {
			g.write_line('else {')
		} else {
			mut conditions := []string{}
			for value in values {
				if is_string {
					conditions << 'builtin__string_eq(${subject_name}, ${value})'
				} else {
					conditions << '((${subject_name}) == (${value}))'
				}
			}
			prefix := if branch_index == 0 { 'if' } else { 'else if' }
			g.write_line('${prefix} (${conditions.join(' || ')}) {')
		}
		g.indent++
		terminates := g.parse_block_body()!
		if !terminates {
			all_terminate = false
		}
		g.indent--
		g.write_line('}')
		branch_index++
	}
	g.next()
	g.skip_semicolons()
	return all_terminate
}

fn (mut g Parser) parse_comptime_if_statement() !bool {
	g.expect(.dollar)!
	g.expect(.key_if)!
	condition := g.parse_comptime_or()!
	g.expect(.lcbr)!
	mut terminates := false
	if condition {
		terminates = g.parse_block_body()!
	} else {
		g.skip_open_block()!
	}
	if g.tok != .dollar || !g.dollar_keyword_is('else') {
		return terminates
	}
	g.next()
	if g.tok != .key_else {
		return g.unsupported('compile-time branch after `$if`')
	}
	g.next()
	if g.tok == .dollar {
		if condition {
			g.skip_comptime_if_chain()!
			return terminates
		}
		return g.parse_comptime_if_statement()!
	}
	g.expect(.lcbr)!
	if condition {
		g.skip_open_block()!
		return terminates
	}
	return g.parse_block_body()!
}

fn (mut g Parser) parse_comptime_or() !bool {
	mut value := g.parse_comptime_and()!
	for g.tok == .logical_or {
		g.next()
		right := g.parse_comptime_and()!
		value = value || right
	}
	return value
}

fn (mut g Parser) parse_comptime_and() !bool {
	mut value := g.parse_comptime_unary()!
	for g.tok == .and {
		g.next()
		right := g.parse_comptime_unary()!
		value = value && right
	}
	return value
}

fn (mut g Parser) parse_comptime_unary() !bool {
	if g.tok == .not {
		g.next()
		return !g.parse_comptime_unary()!
	}
	if g.tok == .lpar {
		g.next()
		value := g.parse_comptime_or()!
		g.expect(.rpar)!
		return value
	}
	if g.tok == .key_true {
		g.next()
		return true
	}
	if g.tok == .key_false {
		g.next()
		return false
	}
	if g.tok != .name {
		return g.unsupported('compile-time condition `${g.token_source()}`')
	}
	name := g.lit
	g.next()
	is_optional := g.tok == .question
	if is_optional {
		g.next()
	}
	return if is_optional {
		pref.comptime_optional_flag_value(g.prefs, name)
	} else {
		pref.comptime_flag_value(g.prefs, name)
	}
}

fn (mut g Parser) skip_open_block() ! {
	mut depth := 1
	for depth > 0 {
		if g.tok == .eof {
			return g.unsupported('unfinished compile-time block')
		}
		if g.tok == .lcbr {
			depth++
		} else if g.tok == .rcbr {
			depth--
		}
		g.next()
	}
	g.skip_semicolons()
}

fn (mut g Parser) skip_comptime_if_chain() ! {
	if g.tok != .dollar {
		return g.unsupported('compile-time `$else` branch')
	}
	g.next()
	g.expect(.key_if)!
	_ = g.parse_comptime_or()!
	g.expect(.lcbr)!
	g.skip_open_block()!
	if g.tok == .dollar && g.dollar_keyword_is('else') {
		g.next()
		g.expect(.key_else)!
		if g.tok == .dollar {
			g.skip_comptime_if_chain()!
		} else {
			g.expect(.lcbr)!
			g.skip_open_block()!
		}
	}
}

fn (g &Parser) dollar_keyword_is(keyword string) bool {
	mut offset := g.s.offset
	for offset < g.s.src.len && g.s.src[offset] in [` `, `\t`] {
		offset++
	}
	return offset + keyword.len <= g.s.src.len && g.s.src[offset..offset + keyword.len] == keyword
}

fn (g &Parser) or_block_has_statements() bool {
	if g.tok == .string && fastc_string_literal_is_incomplete(g.lit) {
		return false
	}
	if g.tok in [.key_return, .key_if, .key_for, .key_match, .key_mut, .key_defer, .key_break,
		.key_continue] {
		return true
	}
	mut lookahead := scanner.new_scanner(g.prefs, .normal)
	lookahead.init(g.s.current_file(), g.s.src)
	lookahead.offset = g.s.offset
	mut tok := g.tok
	mut depth := 0
	for tok != .eof {
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			if depth == 0 {
				return false
			}
			depth--
		} else if depth == 0 && tok == .key_return {
			return true
		} else if depth == 0 && tok == .semicolon {
			next_token := lookahead.scan()
			return next_token != .rcbr
		}
		tok = lookahead.scan()
	}
	return false
}

fn (mut g Parser) parse_if() !bool {
	g.next()
	mut condition := g.read_expression([token.Token.semicolon, token.Token.lcbr])!
	if condition.len == 0 {
		return g.unsupported('empty if condition')
	}
	mut guard_name := ''
	mut guard_type := ''
	mut guard_option := ''
	if g.selfhost && g.last_expression.len >= 4 && g.last_expression[0].tok == .name
		&& g.last_expression[1].tok == .decl_assign {
		right_tokens := g.last_expression[2..]
		if map_lookup := g.render_map_lookup_option_expression(right_tokens) {
			guard_name = g.last_expression[0].lit
			guard_type = map_lookup.typ
			guard_option = g.temporary_name('if_guard')
			g.write_line('Option ${guard_option} = (${map_lookup.source});')
			condition = '${guard_option}.state == 0'
			g.last_expression_type = 'bool'
		} else {
			option_type := g.option_value_type_for_expression(right_tokens)
			if option_type != '' {
				guard_name = g.last_expression[0].lit
				guard_type = option_type
				guard_option = g.temporary_name('if_guard')
				right_source := condition.all_after(':=').trim_space()
				g.write_line('Option ${guard_option} = (${right_source});')
				condition = '${guard_option}.state == 0'
				g.last_expression_type = 'bool'
			}
		}
	}
	g.skip_semicolons()
	g.require_boolean_condition('if')!
	g.expect(.lcbr)!
	g.write_line('if (${condition}) {')
	g.indent++
	previous_guard := g.locals[guard_name] or { FastcLocal{} }
	had_guard := guard_name in g.locals
	if guard_name != '' {
		g.write_line('${guard_type} ${guard_name} = *((${guard_type} *)${guard_option}.data);')
		g.locals[guard_name] = FastcLocal{
			typ: guard_type
		}
	}
	then_terminates := g.parse_block_body()!
	if guard_name != '' {
		if had_guard {
			g.locals[guard_name] = previous_guard
		} else {
			g.locals.delete(guard_name)
		}
	}
	g.indent--
	if g.tok != .key_else {
		g.write_line('}')
		return false
	}
	g.next()
	if g.tok == .key_if {
		g.write_line('} else {')
		g.indent++
		else_terminates := g.parse_if()!
		g.indent--
		g.write_line('}')
		return then_terminates && else_terminates
	}
	g.expect(.lcbr)!
	g.write_line('} else {')
	g.indent++
	else_terminates := g.parse_block_body()!
	g.indent--
	g.write_line('}')
	return then_terminates && else_terminates
}

fn (mut g Parser) parse_for() !bool {
	g.next()
	if g.tok == .lcbr {
		g.next()
		g.write_line('for (;;) {')
		g.indent++
		_ = g.parse_block_body()!
		g.indent--
		g.write_line('}')
		return false
	}
	mut item_is_mut := false
	if g.tok == .key_mut {
		item_is_mut = true
		g.next()
	}
	if g.tok == .name {
		name := g.lit
		g.next()
		mut value_name := ''
		if g.tok == .comma {
			g.next()
			if g.tok != .name {
				return g.unsupported('for-in value name')
			}
			value_name = g.lit
			g.next()
		}
		if g.tok == .key_in {
			if name in g.locals {
				return g.unsupported('redeclaration of `${name}`')
			}
			g.next()
			start := g.read_expression([token.Token.dotdot, token.Token.lcbr])!
			start_expression_type := g.last_expression_type
			if g.tok == .dotdot {
				if item_is_mut || value_name != '' {
					return g.unsupported('mutable or two-value range loop')
				}
				g.next()
				end := g.read_expression([token.Token.lcbr])!
				end_expression_type := g.last_expression_type
				if !fastc_is_integer_expression_type(start_expression_type)
					|| !fastc_is_integer_expression_type(end_expression_type) {
					return g.unsupported('range bounds of types `${start_expression_type}` and `${end_expression_type}` must both be integers')
				}
				g.expect(.lcbr)!
				start_name := g.temporary_name('range_start')
				end_name := g.temporary_name('range_end')
				// V evaluates both range bounds exactly once, from left to right.
				g.write_line('__typeof__((${start})) ${start_name} = (${start});')
				g.write_line('__typeof__((${end})) ${end_name} = (${end});')
				g.write_line('for (__typeof__((${start_name})) ${name} = (${start_name}); ${name} < (${end_name}); ${name}++) {')
				g.locals[name] = FastcLocal{
					typ: fastc_normalize_inferred_type(start_expression_type)
				}
				g.indent++
				_ = g.parse_block_body()!
				g.indent--
				g.locals.delete(name)
				g.write_line('}')
				return false
			}
			if g.tok != .lcbr {
				return g.unsupported('for-in collection')
			}
			collection_type := fastc_normalize_inferred_type(start_expression_type)
			if collection_type.trim_right('*').starts_with('Map_') {
				key_type, map_value_type := fastc_map_key_value_types(collection_type) or {
					return g.unsupported('map iteration type `${collection_type}`')
				}
				g.next()
				collection_name := g.temporary_name('map_collection')
				keys_name := g.temporary_name('map_keys')
				values_name := g.temporary_name('map_values')
				index_name := g.temporary_name('map_index')
				g.write_line('__typeof__((${start})) ${collection_name} = (${start});')
				g.write_line('array ${keys_name} = builtin__map_keys((map *)&${collection_name});')
				if value_name != '' {
					g.write_line('array ${values_name} = builtin__map_values((map *)&${collection_name});')
				}
				g.write_line('for (int ${index_name} = 0; ${index_name} < ${keys_name}.len; ${index_name}++) {')
				g.indent++
				if name != '_' {
					g.write_line('${key_type} ${name} = ((${key_type} *)${keys_name}.data)[${index_name}];')
					g.locals[name] = FastcLocal{
						typ: key_type
					}
				}
				if value_name != '' && value_name != '_' {
					g.write_line('${map_value_type} ${value_name} = ((${map_value_type} *)${values_name}.data)[${index_name}];')
					g.locals[value_name] = FastcLocal{
						typ: map_value_type
					}
				}
				_ = g.parse_block_body()!
				g.indent--
				g.locals.delete(name)
				if value_name != '' {
					g.locals.delete(value_name)
				}
				g.write_line('}')
				return false
			}
			element_type := if collection_type.trim_right('*') == 'string' {
				'u8'
			} else {
				g.array_element_type(collection_type) or {
					return g.unsupported('for-in collection `${start}` of type `${collection_type}`')
				}
			}
			g.next()
			collection_name := g.temporary_name('collection')
			index_name := if value_name == '' && name != '_' {
				g.temporary_name('index')
			} else if name == '_' {
				g.temporary_name('index')
			} else {
				name
			}
			access := if collection_type.ends_with('*') { '->' } else { '.' }
			data_field := if collection_type.trim_right('*') == 'string' { 'str' } else { 'data' }
			g.write_line('__typeof__((${start})) ${collection_name} = (${start});')
			g.write_line('for (int ${index_name} = 0; ${index_name} < ${collection_name}${access}len; ${index_name}++) {')
			g.indent++
			actual_value_name := if value_name == '' { name } else { value_name }
			if actual_value_name != '_' {
				if item_is_mut {
					g.write_line('${element_type} *${actual_value_name} = &(((${element_type} *)${collection_name}${access}${data_field})[${index_name}]);')
					g.locals[actual_value_name] = FastcLocal{
						is_mut: true
						typ:    element_type + '*'
					}
				} else {
					g.write_line('${element_type} ${actual_value_name} = ((${element_type} *)${collection_name}${access}${data_field})[${index_name}];')
					g.locals[actual_value_name] = FastcLocal{
						typ: element_type
					}
				}
			}
			if value_name != '' && name != '_' {
				g.locals[name] = FastcLocal{
					typ: 'int'
				}
			}
			_ = g.parse_block_body()!
			g.indent--
			g.locals.delete(name)
			if value_name != '' {
				g.locals.delete(value_name)
			}
			g.write_line('}')
			return false
		}
		if g.tok in [.decl_assign, .assign] {
			is_declaration := g.tok == .decl_assign
			if is_declaration && name in g.locals {
				return g.unsupported('redeclaration of `${name}`')
			}
			if !is_declaration {
				local := g.locals[name] or {
					return g.unsupported('assignment to undeclared loop variable `${name}`')
				}
				if !local.is_mut {
					return g.unsupported('assignment to immutable loop variable `${name}`')
				}
			}
			g.next()
			initial := g.read_expression([token.Token.semicolon])!
			initial_type := fastc_normalize_inferred_type(g.last_expression_type)
			g.expect(.semicolon)!
			if is_declaration {
				g.locals[name] = FastcLocal{
					is_mut: true
					typ:    initial_type
				}
			}
			condition := g.read_expression([token.Token.semicolon])!
			g.require_boolean_condition('for')!
			g.expect(.semicolon)!
			update := g.read_expression([token.Token.lcbr])!
			g.expect(.lcbr)!
			initializer := if is_declaration {
				'__typeof__((${initial})) ${name} = (${initial})'
			} else {
				'${name} = (${initial})'
			}
			g.write_line('for (${initializer}; ${condition}; ${update}) {')
			g.indent++
			_ = g.parse_block_body()!
			g.indent--
			if is_declaration {
				g.locals.delete(name)
			}
			g.write_line('}')
			return false
		}
		g.validate_expression_name(name, .unknown)!
		condition := g.read_expression_with_prefix(name, [token.Token.lcbr])!
		g.require_boolean_condition('for')!
		g.expect(.lcbr)!
		g.write_line('while (${condition}) {')
		g.indent++
		_ = g.parse_block_body()!
		g.indent--
		g.write_line('}')
		return false
	}
	condition := g.read_expression([token.Token.lcbr])!
	g.require_boolean_condition('for')!
	g.expect(.lcbr)!
	g.write_line('while (${condition}) {')
	g.indent++
	_ = g.parse_block_body()!
	g.indent--
	g.write_line('}')
	return false
}

fn (mut g Parser) parse_return() !bool {
	g.next()
	if g.tok == .semicolon || g.tok == .rcbr {
		if !g.in_main && g.return_type != 'void' && !(g.selfhost && g.return_type == 'Option'
			&& g.option_return_type == 'void') {
			return g.unsupported('bare return in non-void function')
		}
		g.consume_statement_end()
		g.write_deferred_blocks()
		g.write_line(if g.in_main {
			'return 0;'
		} else if g.return_type == 'Option' {
			'return (Option){0};'
		} else {
			'return;'
		})
		return true
	}
	if g.return_type == 'void' {
		return g.unsupported('value return in void function')
	}
	if g.selfhost && (g.return_type.trim_right('*') == 'MultiReturn'
		|| (g.return_type == 'Option' && g.option_return_type == 'MultiReturn')) {
		mut values := []string{}
		mut value_types := []string{}
		for {
			value :=
				g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
			if value == '' {
				return g.unsupported('empty multi-return value')
			}
			values << value
			value_types << fastc_normalize_inferred_type(g.last_expression_type)
			if g.tok != .comma {
				break
			}
			g.next()
		}
		g.consume_statement_end()
		g.write_deferred_blocks()
		if g.return_type == 'Option' && values.len == 1 && value_types[0] == 'Option' {
			g.write_line('return ${values[0]};')
			return true
		}
		if g.return_type == 'Option' && values.len == 1
			&& value_types[0].trim_right('*') == 'IError' {
			g.write_line('return (Option){.err=${values[0]}, .state=1};')
			return true
		}
		multi_value := if values.len == 1 && value_types[0] == 'MultiReturn' {
			values[0]
		} else {
			mut packed_values := []string{cap: values.len}
			for value in values {
				packed_values << 'V_FASTC_MULTI_VALUE(${value})'
			}
			'(MultiReturn){.values={${packed_values.join(', ')}}}'
		}
		if g.return_type == 'Option' {
			g.write_line('return (Option){.data=v_fastc_interface_box(&${multi_value}, sizeof(MultiReturn)), .state=0};')
		} else {
			g.write_line('return ${multi_value};')
		}
		return true
	}
	previous_expected_type := g.expected_expression_type
	if g.selfhost {
		g.expected_expression_type = g.return_type
	}
	mut expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	g.expected_expression_type = previous_expected_type
	mut actual_type := g.last_expression_type
	if g.selfhost && g.return_type == '' {
		g.consume_statement_end()
		g.write_deferred_blocks()
		g.write_line('return ${expression};')
		return true
	}
	if g.selfhost && actual_type == '' && g.last_expression.len == 2
		&& g.last_expression[0].tok == .dot && g.last_expression[1].tok == .name
		&& g.declared_kinds[g.semantic_type_key(g.return_type)] == .enum_ {
		expression = '${g.return_type.trim_right('*')}__${g.last_expression[1].lit}'
		actual_type = g.return_type
	}
	if g.selfhost && g.return_type !in ['Option', 'MultiReturn']
		&& g.declared_kinds[g.semantic_type_key(g.return_type)] != .interface_
		&& !fastc_call_types_are_compatible(actual_type, g.return_type)
		&& !g.selfhost_types_are_compatible(actual_type, g.return_type) {
		actual_type = g.return_type
	}
	if g.selfhost && g.declared_kinds[g.semantic_type_key(g.return_type)] == .interface_
		&& g.declared_kinds[g.semantic_type_key(actual_type)] != .interface_ {
		expression = g.interface_value_expression(g.return_type, actual_type, expression)
		actual_type = g.return_type
	}
	if g.selfhost && g.return_type == 'Option' && actual_type.trim_right('*') == 'IError' {
		expression = '(Option){.err=${expression}, .state=1}'
		actual_type = 'Option'
	} else if g.selfhost && g.return_type == 'Option' && actual_type != 'Option' {
		actual_base := fastc_normalize_inferred_type(actual_type)
		expression = '(Option){.data=${fastc_box_expression(actual_base, expression)}, .state=0}'
		actual_type = 'Option'
	}
	if actual_type.len == 0 {
		return g.unsupported('unverifiable return expression type')
	}
	zero_pointer := g.selfhost && expression == '0' && fastc_is_pointer_type(g.return_type)
	if !zero_pointer && !fastc_call_types_are_compatible(actual_type, g.return_type) && !(g.selfhost
		&& g.selfhost_types_are_compatible(actual_type, g.return_type)) {
		return g.unsupported('return expression of type `${actual_type}` in function returning `${g.return_type}`')
	}
	g.consume_statement_end()
	g.write_deferred_blocks()
	g.write_line('return ${expression};')
	return true
}

fn (g &Parser) interface_value_expression(interface_type string, actual_type string, expression string) string {
	actual_base := actual_type.trim_right('*')
	actual_key := g.semantic_type_key(actual_type)
	object := if fastc_is_pointer_type(actual_type) {
		'(void*)(${expression})'
	} else {
		fastc_box_expression(actual_base, expression)
	}
	return '(${interface_type}){._object=${object}, ._typ=__v_typeid_${fastc_c_declared_type_name(actual_key)}, ._methods=NULL}'
}

fn (g &Parser) require_boolean_condition(kind string) ! {
	if g.last_expression_type.len == 0 {
		if g.selfhost {
			return
		}
		return g.unsupported('unverifiable ${kind} condition type')
	}
	if g.last_expression_type != 'bool' {
		return g.unsupported('${kind} condition of type `${g.last_expression_type}` instead of `bool`')
	}
}

fn (mut g Parser) parse_mutable_declaration() ! {
	g.next()
	if g.tok != .name {
		return g.unsupported('mutable declaration')
	}
	name := g.lit
	g.next()
	if g.selfhost && g.tok == .comma {
		g.parse_parallel_assignment([name], true, true)!
		return
	}
	if g.tok != .decl_assign {
		return g.unsupported('`mut` statement without `:=`')
	}
	g.parse_declaration_after_name(name, true)!
}

fn (mut g Parser) parse_simple_statement() ! {
	if g.tok == .key_assert {
		return g.unsupported('assert statements')
	}
	if g.tok == .name {
		name := g.lit
		global_key := fastc_global_key(g.module_name, name)
		is_global := global_key in g.globals
		statement_local := g.locals[name] or { FastcLocal{} }
		is_known_local := name in g.locals
		c_target := if is_global {
			g.globals[global_key]
		} else if local := g.locals[name] {
			if local.is_reference { '(*${name})' } else { name }
		} else {
			name
		}
		g.next()
		if g.selfhost && g.tok == .colon {
			g.next()
			g.skip_semicolons()
			g.write_line('${name}:')
			return
		}
		if g.selfhost && g.tok == .comma {
			g.parse_parallel_assignment([name], false, false)!
			return
		}
		if g.tok == .decl_assign {
			g.parse_declaration_after_name(name, false)!
			return
		}
		if g.selfhost && g.tok == .left_shift {
			local := g.locals[name] or { return g.unsupported('append to unknown name `${name}`') }
			if !local.is_mut {
				return g.unsupported('append to immutable name `${name}`')
			}
			element_type := g.array_element_type(local.typ) or {
				return g.unsupported('append to non-array `${name}` of type `${local.typ}`')
			}
			g.next()
			value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
			value_type := fastc_normalize_inferred_type(g.last_expression_type)
			is_array_append := value_type == local.typ
			if !is_array_append && !fastc_call_types_are_compatible(value_type, element_type)
				&& !g.selfhost_types_are_compatible(value_type, element_type) {
				return g.unsupported('append value of type `${value_type}` to `${local.typ}`')
			}
			g.consume_statement_end()
			array_target := if local.typ.ends_with('*') {
				'(array *)${name}'
			} else {
				'(array *)&${name}'
			}
			value_name := g.temporary_name('push_value')
			g.write_line('__typeof__((${value})) ${value_name} = (${value});')
			if is_array_append {
				g.write_line('builtin__array_push_many(${array_target}, ${value_name}.data, ${value_name}.len);')
			} else {
				g.write_line('builtin__array_push(${array_target}, &${value_name});')
			}
			return
		}
		if !g.selfhost && (g.tok.is_assignment() || g.tok in [.inc, .dec]) && !is_global
			&& (!is_known_local || !statement_local.is_mut) {
			return g.unsupported('mutation of immutable or unknown name `${name}`')
		}
		g.validate_expression_name(name, .unknown)!
		if g.tok.is_assignment() {
			if !g.selfhost
				&& g.tok in [.left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign] {
				return g.unsupported('shift expressions')
			}
			if !g.selfhost && g.tok in [.div_assign, .mod_assign] {
				return g.unsupported('division or modulo expressions')
			}
			operator := g.tok
			expected_type := if is_global {
				g.global_types[global_key]
			} else if local := g.locals[name] {
				local.typ
			} else {
				''
			}
			g.next()
			previous_expected_type := g.expected_expression_type
			if g.selfhost && expected_type != '' {
				g.expected_expression_type = expected_type
			}
			value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
			g.expected_expression_type = previous_expected_type
			if value.len == 0 {
				return g.unsupported('empty assignment to `${name}`')
			}
			if g.selfhost && name == '_' && operator == .assign {
				g.consume_statement_end()
				g.write_line('(void)(${value});')
				return
			}
			mut actual_type := g.last_expression_type
			if g.selfhost && actual_type == '' {
				actual_type = expected_type
			}
			mut resolved_expected_type := if g.selfhost && expected_type == '' {
				actual_type
			} else {
				expected_type
			}
			if g.selfhost && resolved_expected_type == 'int'
				&& !fastc_is_numeric_expression_type(actual_type) && name in g.locals {
				resolved_expected_type = actual_type
				g.locals[name] = FastcLocal{
					is_mut:       statement_local.is_mut
					is_reference: statement_local.is_reference
					typ:          actual_type
				}
			}
			if actual_type.len == 0 || resolved_expected_type.len == 0 {
				return g.unsupported('unverifiable assignment type for `${name}`')
			}
			pointer_arithmetic := g.selfhost && operator in [.plus_assign, .minus_assign]
				&& fastc_is_pointer_type(resolved_expected_type)
				&& fastc_is_integer_expression_type(actual_type)
			if g.selfhost && operator == .plus_assign && resolved_expected_type == 'string'
				&& actual_type == 'string' {
				g.consume_statement_end()
				g.write_line('${c_target}=builtin__string_plus(${c_target},${value});')
				return
			}
			if !pointer_arithmetic
				&& !fastc_call_types_are_compatible(actual_type, resolved_expected_type)
				&& !(g.selfhost
				&& g.selfhost_types_are_compatible(actual_type, resolved_expected_type)) {
				return g.unsupported('assignment of type `${actual_type}` to `${name}` of type `${resolved_expected_type}`')
			}
			actual_is_numeric := fastc_is_numeric_expression_type(actual_type)
				|| (g.selfhost && g.declared_kinds[g.semantic_type_key(actual_type)] == .alias_)
			expected_is_numeric := fastc_is_numeric_expression_type(resolved_expected_type)
				|| (g.selfhost
				&& g.declared_kinds[g.semantic_type_key(resolved_expected_type)] == .alias_)
			if operator != .assign && !pointer_arithmetic
				&& (!actual_is_numeric || !expected_is_numeric) {
				return g.unsupported('arithmetic assignment `${operator.str()}` on non-numeric type `${resolved_expected_type}`')
			}
			g.consume_statement_end()
			c_operator := if operator == .right_shift_unsigned_assign {
				'>>='
			} else {
				operator.str()
			}
			g.write_line('${c_target}${c_operator}${value};')
			return
		}
		expression :=
			g.read_expression_with_prefix(name, [token.Token.semicolon, token.Token.rcbr])!
		if !g.last_expression_is_statement() {
			return g.unsupported('value-only expression statement')
		}
		g.consume_statement_end()
		g.write_line('${expression};')
		return
	}
	expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	if expression.len == 0 {
		return g.unsupported('statement `${g.token_source()}`')
	}
	if g.selfhost && g.last_expression_is_statement() {
		g.consume_statement_end()
		g.write_line('${expression};')
		return
	}
	return g.unsupported('value-only expression statement')
}

fn (mut g Parser) parse_parallel_assignment(initial_names []string, initial_mut bool, force_declaration bool) ! {
	mut names := initial_names.clone()
	mut mutability := []bool{len: initial_names.len, init: initial_mut}
	for g.tok == .comma {
		g.next()
		mut is_mut := false
		if g.tok == .key_mut {
			is_mut = true
			g.next()
		}
		if g.tok != .name {
			return g.unsupported('parallel assignment target')
		}
		names << g.lit
		mutability << is_mut
		g.next()
	}
	is_declaration := force_declaration || g.tok == .decl_assign
	if g.tok !in [.decl_assign, .assign] {
		return g.unsupported('parallel assignment operator `${g.token_source()}`')
	}
	g.next()
	g.last_multi_return_types = []string{}
	mut values := []string{}
	mut value_types := []string{}
	for {
		item := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		if item == '' {
			return g.unsupported('empty parallel assignment')
		}
		values << item
		value_types << fastc_normalize_inferred_type(g.last_expression_type)
		if g.tok != .comma {
			break
		}
		g.next()
	}
	value := values[0]
	if value == '' {
		return g.unsupported('empty parallel assignment')
	}
	g.consume_statement_end()
	if values.len > 1 {
		if values.len != names.len {
			return g.unsupported('parallel assignment with ${names.len} targets and ${values.len} values')
		}
		mut temporaries := []string{cap: values.len}
		for item in values {
			temporary := g.temporary_name('parallel')
			g.write_line('__typeof__((${item})) ${temporary} = (${item});')
			temporaries << temporary
		}
		if is_declaration {
			for i, name in names {
				if name == '_' {
					continue
				}
				value_type := if value_types[i] == '' { 'int' } else { value_types[i] }
				g.write_line('${value_type} ${name} = ${temporaries[i]};')
				g.locals[name] = FastcLocal{
					is_mut: mutability[i]
					typ:    value_type
				}
			}
		} else {
			for i, name in names {
				if name == '_' {
					continue
				}
				g.write_line('${name} = ${temporaries[i]};')
			}
		}
		return
	}
	mut component_types := g.multi_return_types_for_expression(g.last_expression)
	if component_types.len == 0 {
		component_types = g.last_multi_return_types.clone()
	}
	temporary := g.temporary_name('multi_return')
	g.write_line('MultiReturn ${temporary} = (${value});')
	for i, name in names {
		if name == '_' {
			continue
		}
		if is_declaration {
			component_type := if i < component_types.len { component_types[i] } else { 'usize' }
			g.write_line('${component_type} ${name} = (${component_type}){0};')
			g.write_line('memcpy(&${name}, ${temporary}.values[${i}].data, sizeof(${name}));')
			g.locals[name] = FastcLocal{
				is_mut: mutability[i]
				typ:    component_type
			}
		} else {
			g.write_line('memcpy(&${name}, ${temporary}.values[${i}].data, sizeof(${name}));')
		}
	}
}

fn (g &Parser) multi_return_types_for_expression(tokens []FastcExpressionToken) []string {
	expression_tokens := if tokens.len > 0 && tokens.last().tok == .not {
		tokens[..tokens.len - 1]
	} else {
		tokens
	}
	if expression_tokens.len < 3 {
		return []string{}
	}
	mut name_index := 0
	mut open_index := 1
	if expression_tokens.len >= 4 && expression_tokens[0].tok == .name
		&& expression_tokens[1].tok == .dot && expression_tokens[2].tok == .name {
		name_index = 2
		open_index = 3
	}
	if expression_tokens[name_index].tok != .name || expression_tokens[open_index].tok != .lpar {
		return []string{}
	}
	close := fastc_matching_rpar(expression_tokens, open_index) or { return []string{} }
	if close != expression_tokens.len - 1 {
		return []string{}
	}
	function_key := if name_index == 2 && expression_tokens[0].lit !in g.imports
		&& expression_tokens[0].lit != 'C' {
		receiver_type := g.infer_expression_type(expression_tokens[..1]) or { return []string{} }
		g.method_function_key(receiver_type, expression_tokens[name_index].lit)
	} else {
		g.function_key_for_call(expression_tokens, name_index)
	}
	signature := g.functions[function_key] or { return []string{} }
	return signature.return_types.clone()
}

fn (g &Parser) option_value_type_for_expression(tokens []FastcExpressionToken) string {
	if tokens.len < 3 {
		return ''
	}
	mut name_index := 0
	mut open_index := 1
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .dot && tokens[2].tok == .name {
		name_index = 2
		open_index = 3
	}
	if tokens[name_index].tok != .name || tokens[open_index].tok != .lpar {
		return ''
	}
	close := fastc_matching_rpar(tokens, open_index) or { return '' }
	if close != tokens.len - 1 {
		return ''
	}
	function_key := if name_index == 2 && tokens[0].lit !in g.imports && tokens[0].lit != 'C' {
		receiver_type := g.infer_expression_type(tokens[..1]) or { return '' }
		g.method_function_key(receiver_type, tokens[name_index].lit)
	} else {
		g.function_key_for_call(tokens, name_index)
	}
	signature := g.functions[function_key] or { return '' }
	return signature.option_type
}

fn (g &Parser) last_expression_is_statement() bool {
	tokens := if g.selfhost && g.last_expression.len > 0 && g.last_expression.last().tok == .not {
		g.last_expression[..g.last_expression.len - 1]
	} else {
		g.last_expression
	}
	if g.selfhost && fastc_expression_tokens_contain_assignment_or_mutation(tokens) {
		return true
	}
	if g.selfhost && fastc_expression_tokens_contain(tokens, .left_shift) {
		return true
	}
	if g.selfhost && fastc_expression_tokens_contain_statement_method(tokens) {
		return true
	}
	if g.selfhost && tokens.len >= 4 {
		for i in 2 .. tokens.len - 1 {
			if tokens[i].tok != .name || tokens[i - 1].tok != .dot || tokens[i + 1].tok != .lpar {
				continue
			}
			call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
			if call_end != tokens.len - 1 {
				continue
			}
			receiver_start := fastc_method_receiver_start(tokens, i - 1)
			receiver_type := g.infer_expression_type(tokens[receiver_start..i - 1]) or { continue }
			if g.method_function_key(receiver_type, tokens[i].lit) in g.functions
				|| g.struct_member_type(receiver_type, tokens[i].lit) != '' {
				return true
			}
		}
	}
	if tokens.len == 2 && tokens[0].tok == .name && tokens[1].tok in [.inc, .dec] {
		return true
	}
	mut name_index := 0
	mut open_index := 1
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .dot && tokens[2].tok == .name
		&& (tokens[0].lit in g.imports || (g.selfhost && tokens[0].lit == 'C')) {
		name_index = 2
		open_index = 3
	}
	if tokens.len <= open_index + 1 || tokens[name_index].tok != .name
		|| tokens[open_index].tok != .lpar {
		return false
	}
	call_close := fastc_matching_rpar(tokens, open_index) or { return false }
	if call_close != tokens.len - 1 {
		return false
	}
	name := tokens[name_index].lit
	function_key := g.function_key_for_call(tokens, name_index)
	return function_key in g.functions || (name_index == 0 && name in ['print', 'println'])
}

fn (mut g Parser) parse_declaration_after_name(name string, is_mut bool) ! {
	if !g.selfhost && name in g.locals {
		return g.unsupported('redeclaration of `${name}`')
	}
	g.next()
	expression := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	if expression.len == 0 {
		return g.unsupported('empty declaration')
	}
	g.consume_statement_end()
	// GNU typeof is unevaluated and is supported by bundled TinyCC. It lets the
	// direct path preserve V's `:=` without running any inference or type checker.
	if expression.starts_with('"') {
		// C's typeof preserves a literal's array type instead of applying the usual
		// pointer decay. The spelling alone is enough to lower this case.
		g.write_line('string ${name} = (${expression});')
	} else {
		g.write_line('__typeof__((${expression})) ${name} = (${expression});')
	}
	g.locals[name] = FastcLocal{
		is_mut: is_mut
		typ:    if g.selfhost && g.last_expression_type == '' {
			'int'
		} else {
			fastc_normalize_inferred_type(g.last_expression_type)
		}
	}
}

fn fastc_normalize_inferred_type(typ string) string {
	return match typ {
		'integer literal', 'negative integer literal' { 'int' }
		'float literal' { 'f64' }
		'nil' { 'voidptr' }
		else { typ }
	}
}

fn (mut g Parser) consume_statement_end() {
	if g.tok == .semicolon {
		g.next()
	}
}

fn (mut g Parser) read_expression(stops []token.Token) !string {
	return g.read_expression_with_prefix('', stops)
}

fn (mut g Parser) read_expression_with_prefix(prefix string, stops []token.Token) !string {
	if g.selfhost && prefix == '' && g.tok == .lcbr && token.Token.lcbr !in stops {
		return g.read_inferred_map_literal()!
	}
	if prefix == '' && g.tok == .key_if {
		return g.read_if_expression()!
	}
	if prefix == '' && g.tok == .key_match {
		return g.read_match_expression()!
	}
	if prefix == '' && g.tok == .dollar {
		return g.read_comptime_if_expression()!
	}
	mut result := strings.new_builder(64)
	mut expression_tokens := []FastcExpressionToken{}
	if prefix.len > 0 {
		result.write_string(g.resolved_expression_name(prefix, .unknown))
		expression_tokens << FastcExpressionToken{
			tok: .name
			lit: prefix
		}
	}
	mut paren_depth := 0
	mut bracket_depth := 0
	mut brace_depth := 0
	mut cast_depths := []int{}
	mut pointer_cast_depths := []int{}
	mut previous_was_pointer_cast := false
	mut has_sum_arithmetic_operator := false
	mut has_multiply_operator := false
	mut has_and_operator := false
	mut has_pipe_operator := false
	mut has_xor_operator := false
	mut previous_token := if prefix.len > 0 { token.Token.name } else { token.Token.unknown }
	mut previous_lit := prefix
	mut previous_token_end := g.s.pos
	mut previous_module_separator := false
	mut unsafe_expression_depth := 0
	mut struct_types := []string{}
	mut struct_depths := []int{}
	mut struct_paren_depths := []int{}
	mut expected_struct_field_type := ''
	mut enum_shorthand_type := ''
	for g.tok != .eof {
		if g.selfhost && g.tok == .semicolon && g.semicolon_continues_expression() {
			g.next()
			continue
		}
		if g.selfhost && expression_tokens.len > 0 && paren_depth == 0 && bracket_depth == 0
			&& brace_depth == 0 && unsafe_expression_depth == 0 && g.tok == .mul
			&& g.s.src[previous_token_end..g.s.pos].contains('\n') {
			mut lookahead := g.s
			if lookahead.scan() == .name && lookahead.scan().is_assignment() {
				break
			}
		}
		if expression_tokens.len > 0 && paren_depth == 0 && bracket_depth == 0 && brace_depth == 0
			&& unsafe_expression_depth == 0 && previous_token in [.inc, .dec]
			&& g.s.src[previous_token_end..g.s.pos].contains('\n') {
			break
		}
		if g.tok in [.key_if, .key_unsafe] && expression_tokens.len > 0 && paren_depth == 0
			&& bracket_depth == 0 && brace_depth == 0 && unsafe_expression_depth == 0
			&& g.s.src[previous_token_end..g.s.pos].contains('\n') {
			break
		}
		if g.tok == .key_unsafe {
			g.next()
			if g.tok != .lcbr {
				return g.unsupported('unsafe expression without a block')
			}
			unsafe_expression_depth++
			g.unsafe_depth += 1
			g.next()
			continue
		}
		if unsafe_expression_depth > 0 && g.tok == .rcbr {
			unsafe_expression_depth--
			g.unsafe_depth -= 1
			g.next()
			if unsafe_expression_depth == 0 {
				continue
			}
		}
		if unsafe_expression_depth > 0 && g.tok == .semicolon {
			g.next()
			continue
		}
		if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 && g.tok in stops {
			break
		}
		if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 && g.tok == .comma {
			// V's top-level commas form simultaneous multi-target assignments.
			// Copying them to C would instead emit comma operators.
			return g.unsupported('parallel assignments')
		}
		if g.tok == .string && fastc_string_literal_is_incomplete(g.lit) {
			literal := g.lit
			previous_expected_type := g.expected_expression_type
			if expected_struct_field_type != '' {
				g.expected_expression_type = expected_struct_field_type
			}
			interpolation := g.read_interpolated_string()!
			g.expected_expression_type = previous_expected_type
			if result.len > 0 && fastc_needs_space(result.last(), interpolation)
				&& !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(interpolation)
			expression_tokens << FastcExpressionToken{
				tok:    .string
				lit:    literal
				source: interpolation
			}
			previous_token = .string
			previous_lit = literal
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .lcbr && expression_tokens.len >= 2
			&& expression_tokens[0].tok == .name && expression_tokens[0].lit == 'chan' {
			g.skip_balanced(.lcbr, .rcbr)!
			result.go_back(result.len)
			result.write_string('(chan){0}')
			expression_tokens = [
				FastcExpressionToken{
					tok: .name
					lit: '(chan){0}'
					typ: 'chan'
				},
			]
			previous_token = .name
			previous_lit = '(chan){0}'
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok in [.key_mut, .key_shared] {
			g.next()
			if g.tok in [.amp, .and] {
				continue
			}
			if g.tok == .name && g.local_is_pointer(g.lit) {
				mut next_offset := g.s.offset
				for next_offset < g.s.src.len && g.s.src[next_offset].is_space() {
					next_offset++
				}
				if next_offset >= g.s.src.len || g.s.src[next_offset] !in [`.`, `[`] {
					continue
				}
			}
			result.write_u8(`&`)
			expression_tokens << FastcExpressionToken{
				tok: .amp
				lit: '&'
			}
			previous_token = .amp
			previous_lit = '&'
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .key_if {
			previous_expected_type := g.expected_expression_type
			if expected_struct_field_type != '' {
				g.expected_expression_type = expected_struct_field_type
			}
			conditional := g.read_if_expression()!
			conditional_type := g.last_expression_type
			g.expected_expression_type = previous_expected_type
			if result.len > 0 && fastc_needs_space(result.last(), conditional)
				&& !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(conditional)
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: conditional
				typ: conditional_type
			}
			previous_token = .name
			previous_lit = conditional
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .key_match {
			matched := g.read_match_expression()!
			matched_type := g.last_expression_type
			if result.len > 0 && fastc_needs_space(result.last(), matched)
				&& !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(matched)
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: matched
				typ: matched_type
			}
			previous_token = .name
			previous_lit = matched
			previous_module_separator = false
			previous_token_end = g.s.pos
			continue
		}
		if g.selfhost && g.tok == .key_or {
			or_return_types := g.multi_return_types_for_expression(expression_tokens)
			mut wrapper_parens := 0
			for wrapper_parens < expression_tokens.len
				&& expression_tokens[wrapper_parens].tok == .lpar {
				wrapper_parens++
			}
			mut option_expression := result.str().trim_space()
			mut value_type := g.expected_expression_type
			mut option_tokens := expression_tokens.clone()
			mut assignment_prefix := ''
			mut assignment_depth := 0
			for i, assignment_token in expression_tokens {
				if assignment_token.tok in [.lpar, .lsbr, .lcbr] {
					assignment_depth++
				} else if assignment_token.tok in [.rpar, .rsbr, .rcbr] {
					assignment_depth--
				} else if assignment_depth == 0 && assignment_token.tok.is_assignment() && i > 0
					&& i + 1 < expression_tokens.len {
					left_tokens := expression_tokens[..i]
					left_type := g.infer_expression_type(left_tokens) or { '' }
					if left_type != '' {
						left_source := g.render_membership_candidate(left_tokens, left_type) or {
							''
						}
						if left_source != '' {
							assignment_prefix = '${left_source}${assignment_token.tok.str()}'
							value_type = left_type
							option_tokens = expression_tokens[i + 1..].clone()
							option_expression = g.render_call_argument_expression(option_tokens,
								left_type) or { '' }
							wrapper_parens = 0
						}
					}
					break
				}
			}
			if expression_tokens.len >= 3 && expression_tokens[0].tok == .name
				&& expression_tokens[1].tok == .lpar
				&& fastc_primitive_c_type(expression_tokens[0].lit) != none {
				option_tokens = expression_tokens[2..].clone()
			}
			mut option_value_type := g.option_value_type_for_expression(option_tokens)
			if map_lookup := g.render_map_lookup_option_expression(option_tokens) {
				option_expression = map_lookup.source
				option_value_type = map_lookup.typ
			} else if method_call := g.render_method_call_expression(option_tokens,
				option_expression)
			{
				option_expression = method_call.source
				option_value_type = g.option_value_type_for_expression(option_tokens)
			} else if call := g.render_missing_call_arguments(option_tokens, option_expression) {
				option_expression = call.source
				option_value_type = g.option_value_type_for_expression(option_tokens)
			}
			outer_cast := assignment_prefix == '' && option_tokens.len != expression_tokens.len
			if expression_tokens.len >= 2 && expression_tokens[0].tok == .name
				&& expression_tokens[1].tok == .lpar {
				value_type = fastc_primitive_c_type(expression_tokens[0].lit) or { value_type }
				cast_prefix := '((${value_type})('
				if option_expression.starts_with(cast_prefix) {
					option_expression = option_expression[cast_prefix.len..]
				}
			}
			g.next()
			g.expect(.lcbr)!
			temporary := g.temporary_name('option')
			if g.or_block_has_statements() {
				previous_capture := g.capturing_defer
				previous_lines := g.captured_defer_lines.clone()
				previous_err := g.locals['err'] or { FastcLocal{} }
				had_err := 'err' in g.locals
				g.locals['err'] = FastcLocal{
					typ: 'IError'
				}
				g.capturing_defer = true
				g.captured_defer_lines = []string{}
				_ = g.parse_block_body()!
				block_lines := g.captured_defer_lines.clone()
				g.capturing_defer = previous_capture
				g.captured_defer_lines = previous_lines.clone()
				if had_err {
					g.locals['err'] = previous_err
				} else {
					g.locals.delete('err')
				}
				complex_value_type := if option_value_type == '' {
					'void'
				} else {
					option_value_type
				}
				complex_success := if complex_value_type == 'void' {
					'0'
				} else {
					'*((${complex_value_type} *)${temporary}.data)'
				}
				result.go_back(result.len)
				result.write_string('${assignment_prefix}({ Option ${temporary} = (${option_expression}); if (${temporary}.state) { IError err = ${temporary}.err; ${block_lines.join(' ')} } ${complex_success}; })')
				expression_tokens = [
					FastcExpressionToken{
						tok: .name
						lit: temporary
						typ: complex_value_type
					},
				]
				if complex_value_type == 'void' {
					expression_tokens << FastcExpressionToken{
						tok: .assign
						lit: '='
					}
				}
				if assignment_prefix != '' {
					expression_tokens << FastcExpressionToken{
						tok: .assign
						lit: '='
					}
				}
				previous_token = .name
				previous_lit = temporary
				previous_module_separator = false
				previous_token_end = g.s.pos
				g.last_expression_type = complex_value_type
				g.last_expression = expression_tokens
				g.last_multi_return_types = or_return_types.clone()
				return result.str().trim_space()
			}
			previous_err := g.locals['err'] or { FastcLocal{} }
			had_err := 'err' in g.locals
			g.locals['err'] = FastcLocal{
				typ: 'IError'
			}
			mut fallback := g.read_expression([token.Token.rcbr])!
			fallback_type := fastc_normalize_inferred_type(g.last_expression_type)
			if fallback == '' {
				fallback = '0'
			} else if fallback_type.starts_with('Map_') && fallback.contains('){}') {
				key_type, map_value_type := fastc_map_key_value_types(fallback_type) or {
					return g.unsupported('map fallback type `${fallback_type}`')
				}
				hash_fn, eq_fn, clone_fn, free_fn := fastc_map_runtime_functions(key_type)
				fallback = '(builtin__new_map(sizeof(${fastc_runtime_c_type(key_type)}), sizeof(${fastc_runtime_c_type(map_value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn}))'
			}
			if had_err {
				g.locals['err'] = previous_err
			} else {
				g.locals.delete('err')
			}
			g.expect(.rcbr)!
			if fallback.contains('err') {
				fallback = '({ IError err = ${temporary}.err; ${fallback}; })'
			}
			if value_type == '' {
				value_type = if option_value_type != '' {
					option_value_type
				} else if fallback_type == '' {
					'void'
				} else {
					fallback_type
				}
			}
			if outer_cast && paren_depth > 0 && g.tok == .rpar {
				paren_depth--
				g.next()
			}
			result.go_back(result.len)
			success_value := if value_type == 'void' {
				'0'
			} else {
				'*((${value_type} *)${temporary}.data)'
			}
			result.write_string(assignment_prefix)
			result.write_string('('.repeat(wrapper_parens))
			if value_type != 'void' && fallback_type in ['', 'void'] {
				result.write_string('({ Option ${temporary} = (${option_expression}); if (${temporary}.state) { ${fallback}; } ${success_value}; })')
			} else {
				result.write_string('({ Option ${temporary} = (${option_expression}); ${temporary}.state ? (${fallback}) : ${success_value}; })')
			}
			expression_tokens = []FastcExpressionToken{}
			for _ in 0 .. wrapper_parens {
				expression_tokens << FastcExpressionToken{
					tok: .lpar
					lit: '('
				}
			}
			expression_tokens << FastcExpressionToken{
				tok: .name
				lit: temporary
				typ: value_type
			}
			if value_type == 'void' {
				expression_tokens << FastcExpressionToken{
					tok: .assign
					lit: '='
				}
			}
			if assignment_prefix != '' {
				expression_tokens << FastcExpressionToken{
					tok: .assign
					lit: '='
				}
			}
			previous_token = .name
			previous_lit = temporary
			previous_module_separator = false
			previous_token_end = g.s.pos
			g.last_multi_return_types = or_return_types.clone()
			continue
		}
		if g.selfhost && g.tok == .name && g.lit.starts_with('@') {
			pseudo_name := g.lit
			pseudo := g.comptime_pseudo_expression(pseudo_name) or {
				return g.unsupported('compile-time pseudo value `${pseudo_name}`')
			}
			if result.len > 0 && fastc_needs_space(result.last(), pseudo)
				&& !previous_module_separator {
				result.write_u8(` `)
			}
			result.write_string(pseudo)
			expression_tokens << FastcExpressionToken{
				tok:    .string
				lit:    pseudo_name
				source: pseudo
				typ:    'string'
			}
			previous_token = .string
			previous_lit = pseudo_name
			previous_module_separator = false
			previous_token_end = g.s.offset
			g.next()
			continue
		}
		if !g.selfhost && g.tok in [.eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .not] {
			// C represents comparison and logical results as int. Without V type
			// information, accepting them here would make generic printing and
			// inferred locals observe 0/1 instead of false/true.
			return g.unsupported('comparison or logical expressions')
		}
		if !g.selfhost
			&& g.tok in [.left_shift, .right_shift, .right_shift_unsigned, .left_shift_assign, .right_shift_assign, .right_shift_unsigned_assign] {
			// V defines oversized shifts to produce zero. Raw C shifts are
			// undefined and may mask the count to the operand width instead.
			return g.unsupported('shift expressions')
		}
		if !g.selfhost && g.tok in [.div, .div_assign, .mod, .mod_assign] {
			// Integer division and modulo require V's runtime zero checks. This
			// scanner-only lane has no type information to add them selectively.
			return g.unsupported('division or modulo expressions')
		}
		if !g.selfhost && g.tok == .key_sizeof {
			// Direct C representations can differ from V layouts. Reject sizeof
			// until the parser tracks enough V type information to lower it.
			return g.unsupported('sizeof expressions')
		}
		if !g.selfhost && g.tok in [.lsbr, .rsbr] {
			// Indexing requires V element types and bounds checks. C pointer/array
			// indexing cannot preserve either in this scanner-only lane.
			return g.unsupported('expression token `${g.token_source()}`')
		}
		if (!g.selfhost || g.tok !in [.lcbr, .rcbr])
			&& g.tok in [.lcbr, .rcbr, .str_dollar, .key_match, .key_or, .key_as, .arrow, .power] {
			return g.unsupported('expression token `${g.token_source()}`')
		}
		if !g.selfhost && g.tok in [.key_in, .not_in] {
			return g.unsupported('expression token `${g.token_source()}`')
		}
		if !g.selfhost && g.tok in [.key_is, .not_is] {
			return g.unsupported('expression token `${g.token_source()}`')
		}
		match g.tok {
			.plus, .minus {
				has_sum_arithmetic_operator = true
			}
			.mul {
				has_multiply_operator = true
			}
			.amp {
				has_and_operator = true
			}
			.pipe {
				has_pipe_operator = true
			}
			.xor {
				has_xor_operator = true
			}
			else {}
		}
		if !g.selfhost && ((has_sum_arithmetic_operator && (has_and_operator
			|| has_pipe_operator || has_xor_operator))
			|| (has_multiply_operator && has_and_operator)
			|| (has_pipe_operator && has_xor_operator)) {
			// V groups + and - with | and ^, and * with &, while C splits those
			// levels and also orders + and - above &. Reject ambiguous token streams.
			return g.unsupported('mixed operator precedence')
		}
		expression_tokens << FastcExpressionToken{
			tok: g.tok
			lit: g.lit
		}
		module_separator := g.tok == .dot && previous_token == .name && (previous_lit in g.imports
			|| previous_lit == 'C' || (g.selfhost && previous_lit !in g.locals
			&& g.is_enum_type_name(previous_lit)))
		mut piece := g.expression_token(previous_token, previous_lit)!
		if g.selfhost && brace_depth > 0 && g.tok == .name {
			mut field_lookahead := g.s
			if field_lookahead.scan() == .colon {
				piece = g.lit
			}
		}
		if g.selfhost && g.tok == .dot && previous_was_pointer_cast {
			piece = '->'
		}
		if g.selfhost && g.tok == .lpar && previous_token == .name {
			pointer_token := if expression_tokens.len >= 3
				&& fastc_token_is_prefix_operator(expression_tokens, expression_tokens.len - 3) {
				expression_tokens[expression_tokens.len - 3].tok
			} else {
				token.Token.unknown
			}
			pointer_count := if pointer_token == .and {
				2
			} else if pointer_token == .amp {
				1
			} else {
				0
			}
			pointer_cast := pointer_count > 0
			pointer_suffix := '*'.repeat(pointer_count)
			pointer_prefix_len := pointer_count
			if expression_tokens.len >= 4
				&& expression_tokens[expression_tokens.len - 4].tok == .lsbr
				&& expression_tokens[expression_tokens.len - 3].tok == .rsbr {
				element_type := fastc_primitive_c_type(previous_lit) or { previous_lit }
				array_type := fastc_array_c_type(element_type)
				result.go_back(previous_lit.len + 2)
				piece = '((${array_type})('
				cast_depths << paren_depth + 1
			} else if expression_tokens.len >= 4
				&& expression_tokens[expression_tokens.len - 4].tok == .name
				&& expression_tokens[expression_tokens.len - 4].lit == 'C'
				&& expression_tokens[expression_tokens.len - 3].tok == .dot && previous_lit.len > 0
				&& previous_lit[0].is_capital() && 'C.${previous_lit}' !in g.functions {
				c_pointer_token := if expression_tokens.len >= 5
					&& fastc_token_is_prefix_operator(expression_tokens, expression_tokens.len - 5) {
					expression_tokens[expression_tokens.len - 5].tok
				} else {
					token.Token.unknown
				}
				c_pointer_count := if c_pointer_token == .and {
					2
				} else if c_pointer_token == .amp {
					1
				} else {
					0
				}
				result.go_back(previous_lit.len + c_pointer_count)
				piece = '((${previous_lit}${'*'.repeat(c_pointer_count)})('
				cast_depths << paren_depth + 1
				if c_pointer_count > 0 {
					pointer_cast_depths << paren_depth + 1
				}
			} else if cast_type := fastc_primitive_c_type(previous_lit) {
				result.go_back(cast_type.len + pointer_prefix_len)
				piece = '((${cast_type}${pointer_suffix})('
				cast_depths << paren_depth + 1
				if pointer_cast {
					pointer_cast_depths << paren_depth + 1
				}
			} else if type_key := fastc_resolve_declared_type_key(g.module_name, previous_lit,
				g.declared_types)
			{
				cast_type := fastc_c_declared_type_name(type_key)
				result.go_back(cast_type.len + pointer_prefix_len)
				piece = '((${cast_type}${pointer_suffix})('
				cast_depths << paren_depth + 1
				if pointer_cast {
					pointer_cast_depths << paren_depth + 1
				}
			}
		}
		if g.selfhost && g.tok == .lcbr {
			mut is_struct_literal := false
			if map_type := g.map_initializer_type(expression_tokens[..expression_tokens.len - 1]) {
				fastc_register_composite_type(map_type, mut g.composite_types)
				result.go_back(result.len)
				piece = '(${map_type}){'
				brace_depth++
				struct_types << map_type
				struct_depths << brace_depth
				struct_paren_depths << paren_depth
				is_struct_literal = true
			} else if array_type := g.array_initializer_type(expression_tokens[..expression_tokens.len - 1]) {
				fastc_register_composite_type(array_type, mut g.composite_types)
				result.go_back(result.len)
				c_array_type := fastc_array_initializer_c_type(array_type)
				if array_type.starts_with('FixedArray_') {
					g.fixed_array_types[c_array_type] = array_type
				}
				piece = '(${c_array_type}){'
				brace_depth++
				struct_types << c_array_type
				struct_depths << brace_depth
				struct_paren_depths << paren_depth
				is_struct_literal = true
			} else if expression_tokens.len >= 4
				&& expression_tokens[expression_tokens.len - 4].tok == .name
				&& expression_tokens[expression_tokens.len - 4].lit == 'C'
				&& expression_tokens[expression_tokens.len - 3].tok == .dot
				&& expression_tokens[expression_tokens.len - 2].tok == .name {
				raw_c_type := expression_tokens[expression_tokens.len - 2].lit
				c_type := if '#Cstruct#${raw_c_type}' in g.declared_types {
					'struct ${raw_c_type}'
				} else {
					raw_c_type
				}
				result.go_back(raw_c_type.len)
				piece = '(${c_type}){'
				brace_depth++
				struct_types << c_type
				struct_depths << brace_depth
				struct_paren_depths << paren_depth
				is_struct_literal = true
			} else if previous_token == .name {
				if type_key := fastc_resolve_declared_type_key(g.module_name, previous_lit,
					g.declared_types)
				{
					c_type := fastc_c_declared_type_name(type_key)
					result.go_back(c_type.len)
					piece = '(${c_type}){'
					brace_depth++
					struct_types << c_type
					struct_depths << brace_depth
					struct_paren_depths << paren_depth
					is_struct_literal = true
				}
			}
			if !is_struct_literal && brace_depth > 0 {
				brace_depth++
				piece = '{'
			}
		} else if g.selfhost && g.tok == .rcbr && brace_depth > 0 {
			if struct_depths.len > 0 && struct_depths.last() == brace_depth {
				struct_depths.delete_last()
				if struct_types.len > 0 {
					struct_types.delete_last()
				}
				if struct_paren_depths.len > 0 {
					struct_paren_depths.delete_last()
				}
				expected_struct_field_type = ''
			}
			brace_depth--
			piece = '}'
		} else if g.selfhost && g.tok == .colon && struct_depths.len > 0
			&& brace_depth == struct_depths.last() && paren_depth == struct_paren_depths.last() {
			piece = '='
		} else if g.selfhost && struct_depths.len > 0 && brace_depth == struct_depths.last()
			&& paren_depth == struct_paren_depths.last() && g.tok == .semicolon {
			piece = ','
		} else if g.selfhost && struct_depths.len > 0 && brace_depth == struct_depths.last()
			&& paren_depth == struct_paren_depths.last() && g.tok == .name
			&& previous_token in [.lcbr, .comma, .semicolon] && struct_types.len > 0 {
			mut fields := map[string]string{}
			if struct_types.last() in g.struct_fields {
				fields = g.struct_fields[struct_types.last()].clone()
			}
			expected_struct_field_type = fields[g.lit] or { '' }
			piece = '.${piece}'
		} else if g.selfhost && g.tok == .dot
			&& fastc_token_is_prefix_operator(expression_tokens, expression_tokens.len - 1) {
			mut contextual_type := if expected_struct_field_type != '' {
				expected_struct_field_type
			} else {
				g.expected_expression_type
			}
			if contextual_type == '' {
				mut assignment_depth := 0
				for i, assignment_token in expression_tokens[..expression_tokens.len - 1] {
					if assignment_token.tok in [.lpar, .lsbr, .lcbr] {
						assignment_depth++
					} else if assignment_token.tok in [.rpar, .rsbr, .rcbr] {
						assignment_depth--
					} else if assignment_depth == 0 && assignment_token.tok.is_assignment() && i > 0 {
						contextual_type = g.infer_expression_type(expression_tokens[..i]) or { '' }
						break
					}
				}
			}
			if g.declared_kinds[g.semantic_type_key(contextual_type)] != .enum_ {
				contextual_type = ''
			}
			if contextual_type == '' {
				contextual_type = g.expected_call_argument_type(expression_tokens)
			}
			if expression_tokens.len >= 2
				&& expression_tokens[expression_tokens.len - 2].tok in [.eq, .ne, .gt, .lt, .ge, .le, .pipe, .amp, .xor] {
				operator_index := expression_tokens.len - 2
				mut operand_start := 0
				mut operand_depth := 0
				for i := operator_index - 1; i >= 0; i-- {
					if expression_tokens[i].tok in [.rpar, .rsbr, .rcbr] {
						operand_depth++
					} else if expression_tokens[i].tok in [.lpar, .lsbr, .lcbr] {
						operand_depth--
					} else if operand_depth == 0 && expression_tokens[i].tok in [.and, .logical_or] {
						operand_start = i + 1
						break
					}
				}
				inferred_type := g.infer_expression_type(expression_tokens[operand_start..operator_index]) or {
					''
				}
				if inferred_type != '' {
					contextual_type = inferred_type
				}
			}
			if g.declared_kinds[g.semantic_type_key(contextual_type)] == .enum_ {
				piece = ''
				enum_shorthand_type = contextual_type
			}
		} else if g.selfhost && enum_shorthand_type != '' && g.tok == .name {
			piece = '${enum_shorthand_type.trim_right('*')}__${g.lit}'
			expression_tokens[expression_tokens.len - 1].typ = enum_shorthand_type
			enum_shorthand_type = ''
		}
		if result.len > 0 && fastc_needs_space(result.last(), piece) && !module_separator
			&& !previous_module_separator {
			result.write_u8(` `)
		}
		result.write_string(piece)
		match g.tok {
			.lpar {
				paren_depth++
			}
			.rpar {
				if paren_depth == 0 {
					break
				}
				if paren_depth in pointer_cast_depths {
					previous_was_pointer_cast = true
					pointer_cast_depths.delete(pointer_cast_depths.index(paren_depth))
				}
				if paren_depth in cast_depths {
					result.go_back(piece.len)
					piece = '))'
					result.write_string(piece)
					cast_depths.delete(cast_depths.index(paren_depth))
				}
				paren_depth--
			}
			.lsbr {
				bracket_depth++
			}
			.rsbr {
				bracket_depth--
			}
			else {}
		}
		if g.tok != .rpar {
			previous_was_pointer_cast = false
		}
		previous_token = g.tok
		previous_lit = g.lit
		previous_module_separator = module_separator
		previous_token_end = g.s.offset
		g.next()
	}
	if paren_depth != 0 {
		return g.unsupported('unbalanced expression')
	}
	if bracket_depth != 0 {
		return g.unsupported('unbalanced array expression')
	}
	if brace_depth != 0 {
		return g.unsupported('unbalanced struct literal')
	}
	if unsafe_expression_depth != 0 {
		g.unsafe_depth -= unsafe_expression_depth
		return g.unsupported('unbalanced unsafe expression `${fastc_expression_tokens_debug(expression_tokens)}`')
	}
	g.validate_expression_calls(expression_tokens)!
	mut rendered_expression := result.str().trim_space()
	rendered_expression = g.render_constant_references(expression_tokens, rendered_expression)
	if special := g.render_special_expression(expression_tokens, rendered_expression) {
		g.last_expression_type = special.typ
		g.last_expression = expression_tokens
		return g.render_constant_references(expression_tokens, special.source)
	}
	g.last_expression_type = g.infer_expression_type(expression_tokens)!
	g.last_expression = expression_tokens
	return_types := g.multi_return_types_for_expression(expression_tokens)
	if return_types.len > 0 {
		g.last_multi_return_types = return_types.clone()
	}
	return rendered_expression
}

fn (g &Parser) render_constant_references(tokens []FastcExpressionToken, source string) string {
	mut rendered := source
	for i, item in tokens {
		if item.tok != .name || (i > 0 && tokens[i - 1].tok == .dot)
			|| (i + 1 < tokens.len && tokens[i + 1].tok == .colon)
			|| item.lit in g.locals {
			continue
		}
		if i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
			function_key := g.unqualified_function_key(item.lit)
			if function_key in g.functions {
				rendered = fastc_replace_c_identifier(rendered, item.lit,
					fastc_c_function_name_for_key(function_key))
				continue
			}
		}
		constant_key := fastc_constant_key(g.module_name, item.lit)
		if c_name := g.constants[constant_key] {
			rendered = fastc_replace_c_identifier(rendered, item.lit, c_name)
		} else if c_name := g.constants[fastc_constant_key('builtin', item.lit)] {
			rendered = fastc_replace_c_identifier(rendered, item.lit, c_name)
		}
	}
	return rendered
}

fn fastc_replace_c_identifier(source string, identifier string, replacement string) string {
	if identifier == '' || identifier == replacement || !source.contains(identifier) {
		return source
	}
	mut out := strings.new_builder(source.len + replacement.len)
	mut start := 0
	for start < source.len {
		remaining := source[start..]
		relative := remaining.index(identifier) or {
			out.write_string(remaining)
			break
		}
		index := start + relative
		end := index + identifier.len
		before_is_name := index > 0 && (source[index - 1].is_alnum() || source[index - 1] == `_`)
		after_is_name := end < source.len && (source[end].is_alnum() || source[end] == `_`)
		out.write_string(source[start..index])
		if before_is_name || after_is_name {
			out.write_string(identifier)
		} else {
			out.write_string(replacement)
		}
		start = end
	}
	return out.str()
}

fn (g &Parser) semicolon_continues_expression() bool {
	mut offset := g.s.offset
	for offset < g.s.src.len && g.s.src[offset] in [` `, `\t`, `\r`, `\n`] {
		offset++
	}
	if offset >= g.s.src.len {
		return false
	}
	if offset + 1 < g.s.src.len && g.s.src[offset] == `/` && g.s.src[offset + 1] in [`/`, `*`] {
		return false
	}
	return g.s.src[offset] in [`.`, `+`, `-`, `*`, `/`, `%`, `&`, `|`, `^`, `<`, `>`, `=`, `?`]
}

fn fastc_runtime_c_type(typ string) string {
	base := typ.trim_right('*')
	mut runtime_type := if base.starts_with('Map_') {
		'map'
	} else if base.starts_with('Array_') {
		'array'
	} else {
		base
	}
	return runtime_type + '*'.repeat(typ.len - base.len)
}

fn (mut g Parser) read_inferred_map_literal() !string {
	g.expect(.lcbr)!
	mut keys := []string{}
	mut values := []string{}
	mut key_type := ''
	mut value_type := ''
	for g.tok != .rcbr {
		g.skip_semicolons()
		if g.tok == .rcbr {
			break
		}
		key := g.read_expression([token.Token.colon])!
		actual_key_type := fastc_normalize_inferred_type(g.last_expression_type)
		g.expect(.colon)!
		value := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		actual_value_type := fastc_normalize_inferred_type(g.last_expression_type)
		if key_type == '' {
			key_type = actual_key_type
			value_type = actual_value_type
		} else if (actual_key_type != key_type
			&& !g.selfhost_types_are_compatible(actual_key_type, key_type))
			|| (actual_value_type != value_type
			&& !g.selfhost_types_are_compatible(actual_value_type, value_type)) {
			return g.unsupported('map literal entries of types `${actual_key_type}` and `${actual_value_type}`')
		}
		keys << key
		values << value
		if g.tok in [.comma, .semicolon] {
			g.next()
		}
	}
	g.expect(.rcbr)!
	if keys.len == 0 || key_type == '' || value_type == '' {
		return g.unsupported('empty inferred map literal')
	}
	map_type := fastc_map_c_type(key_type, value_type)
	hash_fn, eq_fn, clone_fn, free_fn := fastc_map_runtime_functions(key_type)
	map_name := g.temporary_name('map_literal')
	mut statements := [
		'map ${map_name} = builtin__new_map(sizeof(${fastc_runtime_c_type(key_type)}), sizeof(${fastc_runtime_c_type(value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn});',
	]
	for i, key in keys {
		key_name := g.temporary_name('map_key')
		value_name := g.temporary_name('map_value')
		statements << '${fastc_runtime_c_type(key_type)} ${key_name} = (${key});'
		statements << '${fastc_runtime_c_type(value_type)} ${value_name} = (${values[i]});'
		statements << 'builtin__map_set(&${map_name}, &${key_name}, &${value_name});'
	}
	g.last_expression_type = map_type
	g.last_expression = [
		FastcExpressionToken{
			tok: .name
			lit: map_name
			typ: map_type
		},
	]
	return '({ ${statements.join(' ')} ${map_name}; })'
}

fn (g &Parser) expected_call_argument_type(tokens []FastcExpressionToken) string {
	if tokens.len < 3 {
		return ''
	}
	mut depth := 0
	mut open_index := -1
	for i := tokens.len - 2; i >= 0; i-- {
		if tokens[i].tok == .rpar {
			depth++
		} else if tokens[i].tok == .lpar {
			if depth == 0 {
				open_index = i
				break
			}
			depth--
		}
	}
	if open_index <= 0 || tokens[open_index - 1].tok != .name {
		return ''
	}
	name_index := open_index - 1
	mut function_key := g.function_key_for_call(tokens, name_index)
	mut argument_offset := 0
	if name_index >= 2 && tokens[name_index - 1].tok == .dot && !(name_index == 2
		&& tokens[0].tok == .name && (tokens[0].lit in g.imports || tokens[0].lit == 'C')) {
		receiver_start := fastc_method_receiver_start(tokens, name_index - 1)
		receiver_type := g.infer_expression_type(tokens[receiver_start..name_index - 1]) or {
			return ''
		}
		function_key = '${g.semantic_type_key(receiver_type)}.${tokens[name_index].lit}'
		argument_offset = 1
	}
	signature := g.functions[function_key] or { return '' }
	mut argument_index := 0
	mut nested := 0
	for i in open_index + 1 .. tokens.len - 1 {
		if tokens[i].tok in [.lpar, .lsbr, .lcbr] {
			nested++
		} else if tokens[i].tok in [.rpar, .rsbr, .rcbr] {
			nested--
		} else if nested == 0 && tokens[i].tok == .comma {
			argument_index++
		}
	}
	parameter_index := argument_index + argument_offset
	if parameter_index >= signature.parameter_types.len {
		return ''
	}
	return signature.parameter_types[parameter_index]
}

fn (mut g Parser) read_interpolated_string() !string {
	first_literal := g.lit
	mut raw := first_literal
	if raw.len > 0 && raw[0] == `r` {
		raw = raw[1..]
	}
	if raw.len == 0 || raw[0] !in [`'`, `"`] {
		return g.unsupported('interpolated string prefix')
	}
	quote := raw[0]
	mut parts := []string{}
	first_part := fastc_c_interpolation_segment(first_literal, true, quote)!
	if first_part != '_SLIT0' {
		parts << first_part
	}
	g.next()
	for g.tok == .str_dollar {
		g.next()
		g.expect(.lcbr)!
		value := g.read_expression([token.Token.rcbr, token.Token.colon])!
		value_type := fastc_normalize_inferred_type(g.last_expression_type)
		if g.tok == .colon {
			for g.tok != .rcbr && g.tok != .eof {
				g.next()
			}
		}
		g.expect(.rcbr)!
		if value_type == 'string' {
			parts << value
		} else if g.declared_kinds[g.semantic_type_key(value_type)] == .enum_ {
			parts << 'builtin__int_str((int)(${value}))'
		} else {
			receiver_key := g.semantic_type_key(value_type)
			method_key := '${receiver_key}.str'
			if method_key !in g.functions {
				local_type := if g.last_expression.len > 0 && g.last_expression[0].tok == .name {
					local := g.locals[g.last_expression[0].lit] or { FastcLocal{} }
					local.typ
				} else {
					''
				}
				return g.unsupported('interpolation of type `${value_type}` for `${fastc_expression_tokens_debug(g.last_expression)}` (local `${local_type}`)')
			}
			parts << '${fastc_method_c_name_for_key(receiver_key, 'str')}(${value})'
		}
		if g.tok == .string {
			part := fastc_c_interpolation_segment(g.lit, false, quote)!
			if part != '_SLIT0' {
				parts << part
			}
			g.next()
		}
	}
	g.last_expression_type = 'string'
	g.last_expression = [FastcExpressionToken{
		tok: .string
		lit: first_literal
	}]
	if parts.len == 0 {
		return '_SLIT0'
	}
	if parts.len == 1 {
		return parts[0]
	}
	return 'builtin__string_plus_many(${parts.len}, (string[]){${parts.join(', ')}})'
}

fn fastc_string_literal_is_incomplete(literal string) bool {
	mut raw := literal
	if raw.len > 0 && raw[0] == `r` {
		raw = raw[1..]
	}
	return raw.len < 2 || raw[raw.len - 1] != raw[0]
}

fn fastc_c_interpolation_segment(literal string, is_first bool, quote u8) !string {
	mut content := literal
	if is_first {
		if content.len > 0 && content[0] == `r` {
			content = content[1..]
		}
		if content.len > 0 && content[0] == quote {
			content = content[1..]
		}
	}
	if !is_first && content.len > 0 && content[content.len - 1] == quote {
		content = content[..content.len - 1]
	}
	if content == '' {
		return '_SLIT0'
	}
	wrapper := if quote == `'` { "'" } else { '"' }
	c_literal := fastc_c_string(wrapper + content + wrapper)!
	return '_S(${c_literal})'
}

fn fastc_method_c_name_for_key(receiver_key string, name string) string {
	module_name := if receiver_key.contains('.') {
		receiver_key.all_before_last('.')
	} else {
		'builtin'
	}
	receiver_type := fastc_c_declared_type_name(receiver_key)
	return fastc_method_c_name(module_name, receiver_type, name)
}

fn (g &Parser) comptime_pseudo_expression(name string) ?string {
	line, column := fastc_line_column(g.s.src, g.s.pos)
	function_name := g.current_function
	receiver_name := g.current_receiver.all_after_last('.')
	method_name := if receiver_name != '' {
		'${receiver_name}.${function_name}'
	} else {
		function_name
	}
	value := match name {
		'@FN' {
			function_name
		}
		'@METHOD' {
			method_name
		}
		'@STRUCT' {
			receiver_name
		}
		'@MOD' {
			if g.module_name == '' { 'main' } else { g.module_name }
		}
		'@FILE' {
			g.path
		}
		'@DIR' {
			os.dir(g.path)
		}
		'@LINE' {
			line.str()
		}
		'@COLUMN' {
			column.str()
		}
		'@FILE_LINE' {
			'${os.file_name(g.path)}:${line}'
		}
		'@LOCATION' {
			'${g.path}:${line}, ${g.module_name}.${method_name}'
		}
		'@VEXEROOT', '@VROOT', '@VMODROOT' {
			g.prefs.vroot
		}
		'@VEXE' {
			g.prefs.vexe
		}
		'@VMOD_FILE' {
			os.join_path_single(g.prefs.vroot, 'v.mod')
		}
		'@VHASH' {
			g.prefs.vhash
		}
		'@VCURRENTHASH' {
			g.prefs.vcurrent_hash
		}
		'@BUILD_DATE' {
			g.prefs.build_date
		}
		'@BUILD_TIME' {
			g.prefs.build_time
		}
		'@BUILD_TIMESTAMP' {
			g.prefs.build_timestamp
		}
		'@OS' {
			g.prefs.normalized_target_os()
		}
		'@CCOMPILER' {
			g.prefs.ccompiler
		}
		'@BACKEND' {
			g.prefs.backend
		}
		'@PLATFORM' {
			g.prefs.comptime_platform()
		}
		else {
			return none
		}
	}
	return '_S(${fastc_c_string_value(value)})'
}

fn fastc_line_column(source string, position int) (int, int) {
	limit := if position < source.len { position } else { source.len }
	mut line := 1
	mut column := 1
	for i in 0 .. limit {
		if source[i] == `\n` {
			line++
			column = 1
		} else {
			column++
		}
	}
	return line, column
}

fn fastc_c_string_value(value string) string {
	mut result := strings.new_builder(value.len + 2)
	result.write_u8(`"`)
	for c in value {
		match c {
			`"` { result.write_string('\\"') }
			`\\` { result.write_string('\\\\') }
			`\n` { result.write_string('\\n') }
			`\r` { result.write_string('\\r') }
			`\t` { result.write_string('\\t') }
			else { result.write_u8(c) }
		}
	}
	result.write_u8(`"`)
	return result.str()
}

fn (mut g Parser) read_match_expression() !string {
	outer_expected_type := g.expected_expression_type
	g.expect(.key_match)!
	g.expected_expression_type = ''
	subject := g.read_expression([token.Token.lcbr])!
	subject_type := g.last_expression_type
	if subject == '' || subject_type == '' {
		return g.unsupported('unverifiable match expression subject')
	}
	g.expect(.lcbr)!
	temporary := g.temporary_name('match')
	mut conditions := []string{}
	mut values := []string{}
	mut result_type := ''
	mut fallback := ''
	g.skip_semicolons()
	for g.tok != .rcbr && g.tok != .eof {
		mut is_else := false
		mut branch_conditions := []string{}
		if g.tok == .key_else {
			is_else = true
			g.next()
		} else {
			for {
				g.expected_expression_type = subject_type
				start :=
					g.read_expression([token.Token.comma, token.Token.lcbr, token.Token.dotdot])!
				if g.tok == .dotdot {
					g.next()
					finish := g.read_expression([token.Token.comma, token.Token.lcbr])!
					branch_conditions << '((${temporary}) >= (${start}) && (${temporary}) <= (${finish}))'
				} else {
					branch_conditions << if subject_type.trim_right('*') == 'string' {
						'builtin__string_eq(${temporary}, ${start})'
					} else {
						'((${temporary}) == (${start}))'
					}
				}
				if g.tok != .comma {
					break
				}
				g.next()
			}
		}
		g.expect(.lcbr)!
		g.expected_expression_type = outer_expected_type
		mut value := g.read_match_block_expression_value()!
		mut value_type := g.last_expression_type
		g.skip_semicolons()
		if g.tok != .rcbr {
			return g.unsupported('match branch `${value}` left `${g.token_source()}` (`${g.tok.str()}`)')
		}
		g.expect(.rcbr)!
		g.skip_semicolons()
		if g.selfhost && result_type == 'Option' && value_type !in ['', 'Option'] {
			value = fastc_option_success_expression(value_type, value)
			value_type = 'Option'
		} else if g.selfhost && value_type == 'Option' && result_type !in ['', 'Option'] {
			for i, previous_value in values {
				values[i] = fastc_option_success_expression(result_type, previous_value)
			}
			if fallback != '' {
				fallback = fastc_option_success_expression(result_type, fallback)
			}
			result_type = 'Option'
		}
		if g.selfhost && value_type == '' && result_type != '' {
			zero_type := fastc_normalize_inferred_type(result_type)
			value = '({ (void)(${value}); (${zero_type}){0}; })'
			value_type = result_type
		}
		if result_type == '' && value_type != '' {
			if g.selfhost {
				zero_type := fastc_normalize_inferred_type(value_type)
				for i, previous_value in values {
					values[i] = '({ (void)(${previous_value}); (${zero_type}){0}; })'
				}
				if fallback != '' {
					fallback = '({ (void)(${fallback}); (${zero_type}){0}; })'
				}
			}
			result_type = value_type
		} else if value_type != '' && result_type != value_type && !(g.selfhost
			&& g.selfhost_types_are_compatible(value_type, result_type)) {
			return g.unsupported('match expression branch types `${result_type}` and `${value_type}`')
		}
		if is_else {
			fallback = value
		} else {
			conditions << '(${branch_conditions.join(' || ')})'
			values << value
		}
	}
	g.expect(.rcbr)!
	g.expected_expression_type = outer_expected_type
	if fallback == '' {
		fallback = '(${result_type}){0}'
	}
	mut expression := fallback
	for i := conditions.len - 1; i >= 0; i-- {
		expression = '((${conditions[i]}) ? (${values[i]}) : (${expression}))'
	}
	g.last_expression_type = if result_type == '' { outer_expected_type } else { result_type }
	g.last_expression = []FastcExpressionToken{}
	return '({ __typeof__((${subject})) ${temporary} = (${subject}); ${expression}; })'
}

fn (mut g Parser) read_match_block_expression_value() !string {
	if g.tok == .key_if || g.or_block_has_statements() {
		return g.read_block_expression_value()!
	}
	mut values := []string{}
	mut value_types := []string{}
	for {
		value := g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
		values << value
		value_types << fastc_normalize_inferred_type(g.last_expression_type)
		if g.tok != .comma {
			break
		}
		g.next()
	}
	if values.len == 1 {
		return values[0]
	}
	mut packed_values := []string{cap: values.len}
	for value in values {
		packed_values << 'V_FASTC_MULTI_VALUE(${value})'
	}
	g.last_expression_type = 'MultiReturn'
	g.last_expression = []FastcExpressionToken{}
	g.last_multi_return_types = value_types.clone()
	return '(MultiReturn){.values={${packed_values.join(', ')}}}'
}

fn (mut g Parser) read_block_expression_value() !string {
	g.skip_semicolons()
	if g.tok == .key_if && g.if_starts_final_block_expression() {
		return g.read_if_expression()!
	}
	if !g.or_block_has_statements() {
		return g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	}
	previous_capture := g.capturing_defer
	previous_lines := g.captured_defer_lines.clone()
	g.capturing_defer = true
	g.captured_defer_lines = []string{}
	for g.or_block_has_statements() {
		if g.tok == .key_if && g.if_starts_final_block_expression() {
			break
		}
		_ = g.parse_statement()!
		g.skip_semicolons()
	}
	statements := g.captured_defer_lines.clone()
	g.capturing_defer = previous_capture
	g.captured_defer_lines = previous_lines
	if g.tok == .rcbr {
		g.last_expression_type = ''
		g.last_expression = []FastcExpressionToken{}
		return '({ ${statements.join(' ')} 0; })'
	}
	g.skip_semicolons()
	mut value := if g.tok == .name {
		prefix := g.lit
		g.next()
		g.read_expression_with_prefix(prefix, [token.Token.semicolon, token.Token.rcbr])!
	} else {
		g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	}
	if g.tok == .name {
		prefix := g.lit
		g.next()
		final_value := g.read_expression_with_prefix(prefix,
			[token.Token.semicolon, token.Token.rcbr])!
		value = if value.trim_space() in ['', ';'] {
			final_value
		} else {
			value + final_value
		}
	}
	return '({ ${statements.join(' ')} ${value}; })'
}

fn (g &Parser) if_starts_final_block_expression() bool {
	mut lookahead := scanner.new_scanner(g.prefs, .normal)
	lookahead.init(g.s.current_file(), g.s.src)
	lookahead.offset = g.s.offset
	mut tok := lookahead.scan()
	for {
		for tok !in [.lcbr, .eof] {
			tok = lookahead.scan()
		}
		if tok != .lcbr {
			return false
		}
		tok = fastc_skip_balanced_tokens(mut lookahead, tok, .lcbr, .rcbr) or { return false }
		for tok == .semicolon {
			tok = lookahead.scan()
		}
		if tok != .key_else {
			return false
		}
		tok = lookahead.scan()
		for tok == .semicolon {
			tok = lookahead.scan()
		}
		if tok == .key_if {
			tok = lookahead.scan()
			continue
		}
		if tok != .lcbr {
			return false
		}
		tok = fastc_skip_balanced_tokens(mut lookahead, tok, .lcbr, .rcbr) or { return false }
		break
	}
	for tok == .semicolon {
		tok = lookahead.scan()
	}
	return tok == .rcbr
}

fn fastc_option_success_expression(value_type string, expression string) string {
	base := fastc_normalize_inferred_type(value_type)
	return '(Option){.data=${fastc_box_expression(base, expression)}, .state=0}'
}

fn fastc_box_expression(value_type string, expression string) string {
	return '({ ${value_type} __v_fastc_box_value = (${expression}); v_fastc_interface_box(&__v_fastc_box_value, sizeof(${value_type})); })'
}

fn (mut g Parser) read_comptime_if_expression() !string {
	g.expect(.dollar)!
	g.expect(.key_if)!
	condition := g.parse_comptime_or()!
	g.expect(.lcbr)!
	if condition {
		value := g.read_expression([token.Token.rcbr])!
		value_type := g.last_expression_type
		g.skip_semicolons()
		g.expect(.rcbr)!
		if g.tok == .dollar {
			g.next()
			g.expect(.key_else)!
			if g.tok == .dollar {
				g.skip_comptime_if_chain()!
			} else {
				g.expect(.lcbr)!
				g.skip_open_block()!
			}
		}
		g.last_expression_type = value_type
		g.last_expression = []FastcExpressionToken{}
		return value
	}
	g.skip_open_block()!
	if g.tok != .dollar {
		return g.unsupported('compile-time if expression without `$else`')
	}
	g.next()
	g.expect(.key_else)!
	if g.tok == .dollar {
		return g.read_comptime_if_expression()!
	}
	g.expect(.lcbr)!
	value := g.read_expression([token.Token.rcbr])!
	value_type := g.last_expression_type
	g.skip_semicolons()
	g.expect(.rcbr)!
	g.last_expression_type = value_type
	g.last_expression = []FastcExpressionToken{}
	return value
}

fn (mut g Parser) read_if_expression() !string {
	outer_expected_type := g.expected_expression_type
	branch_expected_type := if outer_expected_type != '' {
		outer_expected_type
	} else if g.declared_kinds[g.semantic_type_key(g.return_type)] == .enum_ {
		g.return_type
	} else {
		''
	}
	g.expect(.key_if)!
	g.expected_expression_type = ''
	mut condition := g.read_expression([token.Token.semicolon, token.Token.lcbr])!
	mut guard_name := ''
	mut guard_type := ''
	mut guard_option := ''
	mut guard_source := ''
	if g.selfhost && g.last_expression.len >= 4 && g.last_expression[0].tok == .name
		&& g.last_expression[1].tok == .decl_assign {
		right_tokens := g.last_expression[2..]
		if map_lookup := g.render_map_lookup_option_expression(right_tokens) {
			guard_name = g.last_expression[0].lit
			guard_type = map_lookup.typ
			guard_source = map_lookup.source
		} else {
			option_type := g.option_value_type_for_expression(right_tokens)
			if option_type != '' {
				guard_name = g.last_expression[0].lit
				guard_type = option_type
				guard_source = condition.all_after(':=').trim_space()
			}
		}
		if guard_name != '' {
			guard_option = g.temporary_name('if_guard')
			condition = '${guard_option}.state == 0'
			g.last_expression_type = 'bool'
		}
	}
	g.skip_semicolons()
	g.require_boolean_condition('if expression')!
	g.expect(.lcbr)!
	previous_guard := g.locals[guard_name] or { FastcLocal{} }
	had_guard := guard_name in g.locals
	if guard_name != '' {
		g.locals[guard_name] = FastcLocal{
			typ: guard_type
		}
	}
	g.expected_expression_type = branch_expected_type
	mut then_expression := if g.tok == .key_return {
		g.read_return_expression_branch()!
	} else {
		g.read_block_expression_value()!
	}
	if guard_name != '' {
		then_expression = '({ ${guard_type} ${guard_name} = *((${guard_type} *)${guard_option}.data); ${then_expression}; })'
		if had_guard {
			g.locals[guard_name] = previous_guard
		} else {
			g.locals.delete(guard_name)
		}
	}
	mut then_type := g.last_expression_type
	if enum_expression := g.expected_enum_shorthand_expression() {
		then_expression = enum_expression
		then_type = g.return_type
	}
	g.skip_semicolons()
	g.expect(.rcbr)!
	if g.tok != .key_else {
		return g.unsupported('if expression without `else`')
	}
	g.next()
	mut else_expression := ''
	mut else_type := ''
	if g.tok == .key_if {
		g.expected_expression_type = branch_expected_type
		else_expression = g.read_if_expression()!
		else_type = g.last_expression_type
	} else {
		g.expect(.lcbr)!
		g.expected_expression_type = branch_expected_type
		else_expression = if g.tok == .key_return {
			g.read_return_expression_branch()!
		} else {
			g.read_block_expression_value()!
		}
		else_type = g.last_expression_type
		if enum_expression := g.expected_enum_shorthand_expression() {
			else_expression = enum_expression
			else_type = g.return_type
		}
		g.skip_semicolons()
		g.expect(.rcbr)!
	}
	if g.selfhost && outer_expected_type == 'Option' {
		if then_type != 'Option' {
			then_expression = g.option_branch_expression(then_type, then_expression)
			then_type = 'Option'
		}
		if else_type != 'Option' {
			else_expression = g.option_branch_expression(else_type, else_expression)
			else_type = 'Option'
		}
	} else if g.selfhost && then_type == 'Option' && else_type !in ['', 'Option'] {
		else_base := fastc_normalize_inferred_type(else_type)
		else_expression = '(Option){.data=${fastc_box_expression(else_base, else_expression)}, .state=0}'
		else_type = 'Option'
	} else if g.selfhost && else_type == 'Option' && then_type !in ['', 'Option'] {
		then_base := fastc_normalize_inferred_type(then_type)
		then_expression = '(Option){.data=${fastc_box_expression(then_base, then_expression)}, .state=0}'
		then_type = 'Option'
	}
	if then_type == else_type {
		g.last_expression_type = then_type
	} else if g.selfhost && then_type == '' {
		g.last_expression_type = else_type
	} else if g.selfhost && else_type == '' {
		g.last_expression_type = then_type
	} else if g.selfhost && fastc_is_integer_expression_type(then_type)
		&& fastc_is_integer_expression_type(else_type) {
		g.last_expression_type = if then_type == 'integer literal' { else_type } else { then_type }
	} else {
		return g.unsupported('if expression branch types `${then_type}` (`${then_expression}`) and `${else_type}` (`${else_expression}`)')
	}
	g.expected_expression_type = outer_expected_type
	g.last_expression = []FastcExpressionToken{}
	conditional := '((${condition}) ? (${then_expression}) : (${else_expression}))'
	return if guard_option == '' {
		conditional
	} else {
		'({ Option ${guard_option} = (${guard_source}); ${conditional}; })'
	}
}

fn (g &Parser) option_branch_expression(value_type string, expression string) string {
	if value_type.trim_right('*') == 'IError' {
		return '(Option){.err=${expression}, .state=1}'
	}
	if value_type == 'voidptr' && g.option_return_type != 'voidptr' {
		return '(Option){.err=(IError){._object=(voidptr)(${expression})}, .state=1}'
	}
	return fastc_option_success_expression(value_type, expression)
}

fn (mut g Parser) read_return_expression_branch() !string {
	g.expect(.key_return)!
	if g.return_type.trim_right('*') == 'MultiReturn' {
		mut values := []string{}
		for {
			value :=
				g.read_expression([token.Token.comma, token.Token.semicolon, token.Token.rcbr])!
			values << 'V_FASTC_MULTI_VALUE(${value})'
			if g.tok != .comma {
				break
			}
			g.next()
		}
		g.consume_statement_end()
		g.last_expression_type = ''
		g.last_expression = []FastcExpressionToken{}
		return '({ return (MultiReturn){.values={${values.join(', ')}}}; 0; })'
	}
	value := g.read_expression([token.Token.semicolon, token.Token.rcbr])!
	g.consume_statement_end()
	g.last_expression_type = ''
	g.last_expression = []FastcExpressionToken{}
	return '({ return ${value}; 0; })'
}

fn (g &Parser) expected_enum_shorthand_expression() ?string {
	if !g.selfhost || g.last_expression_type != '' || g.last_expression.len != 2
		|| g.last_expression[0].tok != .dot || g.last_expression[1].tok != .name
		|| g.declared_kinds[g.semantic_type_key(g.return_type)] != .enum_ {
		return none
	}
	return '${g.return_type.trim_right('*')}__${g.last_expression[1].lit}'
}

fn (g &Parser) render_special_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if g.selfhost {
		if tokens.len == 1 && tokens[0].tok == .name {
			if local := g.locals[tokens[0].lit] {
				if local.is_reference {
					return FastcRenderedExpression{
						source: '*(${rendered_expression})'
						typ:    local.typ.trim_right('*')
					}
				}
			}
		}
		if interface_cast := g.render_interface_cast_expression(tokens, rendered_expression) {
			return interface_cast
		}
		if cast_expression := g.render_cast_expression(tokens) {
			if pointer_members := g.render_pointer_member_access_expression(tokens,
				cast_expression.source)
			{
				return pointer_members
			}
			return cast_expression
		}
		if defaulted_call := g.render_missing_call_arguments(tokens, rendered_expression) {
			return defaulted_call
		}
		if map_expression := g.render_map_expression(tokens, rendered_expression) {
			return map_expression
		}
		if struct_literal := g.render_struct_literal_expression(tokens) {
			return struct_literal
		}
		if struct_literal := g.render_struct_literal_field_names(tokens, rendered_expression) {
			return struct_literal
		}
		if assignment := g.render_assignment_expression(tokens) {
			return assignment
		}
		if array_assignment := g.render_array_assignment_expression(tokens) {
			return array_assignment
		}
		if static_call := g.render_static_call_expression(tokens, rendered_expression) {
			return static_call
		}
		if logical := g.render_logical_expression(tokens) {
			return logical
		}
		if tokens.len > 1 && tokens[0].tok == .not {
			inner := g.render_call_argument_expression(tokens[1..], 'bool') or { return none }
			return FastcRenderedExpression{
				source: '!(${inner})'
				typ:    'bool'
			}
		}
		if option_comparison := g.render_option_none_comparison(tokens) {
			return option_comparison
		}
		if enum_comparison := g.render_enum_comparison_expression(tokens) {
			return enum_comparison
		}
		if string_comparison := g.render_string_comparison_expression(tokens) {
			return string_comparison
		}
		if concatenation := g.render_composed_string_concatenation(tokens) {
			return concatenation
		}
		if tokens.len > 1 && tokens.last().tok == .not && rendered_expression.ends_with('!')
			&& !(tokens[0].tok == .lsbr && tokens[tokens.len - 2].tok == .rsbr) {
			inner_tokens := tokens[..tokens.len - 1]
			mut inner_source := rendered_expression[..rendered_expression.len - 1]
			if method_expression := g.render_method_call_expression(inner_tokens, inner_source) {
				inner_source = method_expression.source
			} else if array_expression := g.render_array_access_expression(inner_tokens) {
				inner_source = array_expression.source
			} else if defaulted_call := g.render_missing_call_arguments(inner_tokens, inner_source) {
				inner_source = defaulted_call.source
			}
			value_type := g.option_value_type_for_expression(inner_tokens)
			temporary := '__v_fastc_option_propagate'
			failure := if g.return_type == 'Option' {
				'return ${temporary};'
			} else {
				'return 1;'
			}
			value := if value_type in ['', 'void'] {
				'0'
			} else {
				'*((${value_type} *)${temporary}.data)'
			}
			return FastcRenderedExpression{
				source: '({ Option ${temporary} = (${inner_source}); if (${temporary}.state) { ${failure} } ${value}; })'
				typ:    value_type
			}
		}
		if append_expression := g.render_append_expression(tokens, rendered_expression) {
			return append_expression
		}
		mut depth := 0
		for i, item in tokens {
			if item.tok in [.lpar, .lsbr, .lcbr] {
				depth++
			} else if item.tok in [.rpar, .rsbr, .rcbr] {
				depth--
			} else if depth == 0 && item.tok in [.key_in, .not_in] && i > 0 && i + 1 < tokens.len {
				right_tokens := tokens[i + 1..]
				right_type := g.infer_expression_type(right_tokens) or { return none }
				if right_type.trim_right('*').starts_with('Map_') {
					key_type, _ := fastc_map_key_value_types(right_type) or { return none }
					key_source := g.render_membership_candidate(tokens[..i], key_type) or {
						return none
					}
					map_source := g.render_member_receiver(right_tokens) or { return none }
					map_expression := if right_type.ends_with('*') {
						map_source
					} else {
						'&(${map_source})'
					}
					found := 'builtin__map_get_check((map *)${map_expression}, &__v_fastc_map_key) != NULL'
					predicate := if item.tok == .not_in { '!(${found})' } else { found }
					return FastcRenderedExpression{
						source: '({ ${fastc_runtime_c_type(key_type)} __v_fastc_map_key = (${key_source}); ${predicate}; })'
						typ:    'bool'
					}
				}
				if right_type.trim_right('*').starts_with('Array_')
					|| right_type.trim_right('*') == 'string' {
					element_type := if right_type.trim_right('*') == 'string' {
						'u8'
					} else {
						g.array_element_type(right_type) or { return none }
					}
					candidate := g.render_membership_candidate(tokens[..i], element_type) or {
						return none
					}
					collection := g.render_call_argument_expression(right_tokens, right_type) or {
						return none
					}
					access := if right_type.ends_with('*') { '->' } else { '.' }
					data_field := if right_type.trim_right('*') == 'string' { 'str' } else { 'data' }
					comparison := if element_type == 'string' {
						'builtin__string_eq(__v_fastc_membership_item, ((${element_type} *)__v_fastc_membership_collection${access}${data_field})[__v_fastc_membership_index])'
					} else {
						'(__v_fastc_membership_item == ((${element_type} *)__v_fastc_membership_collection${access}${data_field})[__v_fastc_membership_index])'
					}
					return FastcRenderedExpression{
						source: '({ __typeof__((${collection})) __v_fastc_membership_collection = (${collection}); ${element_type} __v_fastc_membership_item = (${candidate}); bool __v_fastc_membership_found = false; for (int __v_fastc_membership_index = 0; __v_fastc_membership_index < __v_fastc_membership_collection${access}len; __v_fastc_membership_index++) { if (${comparison}) { __v_fastc_membership_found = true; break; } } ${if item.tok == .not_in {
							'!__v_fastc_membership_found'
						} else {
							'__v_fastc_membership_found'
						}}; })'
						typ:    'bool'
					}
				}
				if i + 2 >= tokens.len || tokens[i + 1].tok != .lsbr || tokens.last().tok != .rsbr {
					continue
				}
				lhs_type := g.infer_expression_type(tokens[..i]) or { return none }
				items := fastc_expression_list_items(tokens, i + 2, tokens.len - 1) or {
					return none
				}
				if items.len == 0 {
					return FastcRenderedExpression{
						source: if item.tok == .key_in { 'false' } else { 'true' }
						typ:    'bool'
					}
				}
				lhs_source := g.render_membership_candidate(tokens[..i], lhs_type) or {
					return none
				}
				mut comparisons := []string{cap: items.len}
				for candidate in items {
					candidate_source := g.render_membership_candidate(candidate, lhs_type) or {
						return none
					}
					comparison := if lhs_type.trim_right('*') == 'string' {
						'builtin__string_eq(${lhs_source}, ${candidate_source})'
					} else {
						'((${lhs_source}) == (${candidate_source}))'
					}
					comparisons << if item.tok == .key_in { comparison } else { '!${comparison}' }
				}
				joiner := if item.tok == .key_in { ' || ' } else { ' && ' }
				return FastcRenderedExpression{
					source: '(${comparisons.join(joiner)})'
					typ:    'bool'
				}
			}
		}
		// Resolve a complete index expression before the generic pointer/member
		// rewriter gets a chance to treat its base as part of a longer chain.
		if array_access := g.render_array_access_expression(tokens) {
			return array_access
		}
		if pointer_members := g.render_pointer_member_access_expression(tokens, rendered_expression) {
			return pointer_members
		}
		if method_expression := g.render_method_call_expression(tokens, rendered_expression) {
			if array_expression := g.render_nested_array_access_expression(tokens,
				method_expression.source)
			{
				return FastcRenderedExpression{
					source: array_expression.source
					typ:    method_expression.typ
				}
			}
			return method_expression
		}
		if defaulted_call := g.render_missing_call_arguments(tokens, rendered_expression) {
			return defaulted_call
		}
		if array_expression := g.render_nested_array_access_expression(tokens, rendered_expression) {
			return array_expression
		}
	}
	if g.selfhost && tokens.len == 3 && tokens[0].tok == .name
		&& tokens[1].tok in [.key_is, .not_is] && tokens[2].tok == .name {
		lhs_type := g.infer_expression_type(tokens[..1]) or { return none }
		type_key := fastc_resolve_declared_type_key(g.module_name, tokens[2].lit, g.declared_types) or {
			return none
		}
		access := if lhs_type.ends_with('*') { '->' } else { '.' }
		operator := if tokens[1].tok == .key_is { '==' } else { '!=' }
		return FastcRenderedExpression{
			source: '((${tokens[0].lit}${access}_typ) ${operator} __v_typeid_${fastc_c_declared_type_name(type_key)})'
			typ:    'bool'
		}
	}
	if g.selfhost {
		mut init_open := -1
		for i, item in tokens {
			if item.tok == .lcbr {
				init_open = i
				break
			}
		}
		if init_open > 0 && tokens.last().tok == .rcbr {
			if array_type := g.array_initializer_type(tokens[..init_open]) {
				return FastcRenderedExpression{
					source: rendered_expression
					typ:    array_type
				}
			}
		}
	}
	array_end := if tokens.len > 0 && tokens.last().tok == .not {
		tokens.len - 1
	} else {
		tokens.len
	}
	if g.selfhost && array_end == 2 && tokens[0].tok == .lsbr && tokens[1].tok == .rsbr
		&& g.expected_expression_type.trim_right('*').starts_with('Array_') {
		return FastcRenderedExpression{
			source: '(${g.expected_expression_type}){0}'
			typ:    g.expected_expression_type
		}
	}
	if g.selfhost && array_end >= 2 && tokens[0].tok == .lsbr && tokens[array_end - 1].tok == .rsbr {
		items := fastc_expression_list_items(tokens, 1, array_end - 1) or { return none }
		if items.len == 0 {
			return none
		}
		element_type := g.infer_expression_type(items[0]) or { return none }
		if element_type == '' {
			return none
		}
		array_type := fastc_array_c_type(fastc_normalize_inferred_type(element_type))
		mut rendered_items := []string{cap: items.len}
		for item in items {
			rendered_items << g.render_call_argument_expression(item, element_type) or {
				return none
			}
		}
		return FastcRenderedExpression{
			source: '((${array_type})builtin__new_array_from_c_array(${items.len}, ${items.len}, sizeof(${fastc_normalize_inferred_type(element_type)}), (${fastc_normalize_inferred_type(element_type)}[]){${rendered_items.join(',')}}))'
			typ:    array_type
		}
	}
	if g.selfhost && tokens.len > 0 && tokens.len % 2 == 1 {
		mut is_literal_concat := true
		mut literals := strings.new_builder(rendered_expression.len)
		for i, item in tokens {
			if i % 2 == 0 {
				if item.tok != .string {
					is_literal_concat = false
					break
				}
				literal := fastc_c_string(item.lit) or {
					is_literal_concat = false
					break
				}
				literals.write_string(literal)
			} else if item.tok != .plus {
				is_literal_concat = false
				break
			}
		}
		if is_literal_concat {
			return FastcRenderedExpression{
				source: '_S(${literals.str()})'
				typ:    'string'
			}
		}
	}
	if g.selfhost {
		mut depth := 0
		mut operand_start := 0
		mut string_operands := []bool{}
		mut plus_count := 0
		for i, item in tokens {
			match item.tok {
				.lpar, .lsbr, .lcbr {
					depth++
				}
				.rpar, .rsbr, .rcbr {
					depth--
				}
				.plus {
					if depth == 0 {
						operand_type := g.infer_expression_type(tokens[operand_start..i]) or { '' }
						string_operands << operand_type == 'string'
						operand_start = i + 1
						plus_count++
					}
				}
				else {}
			}
		}
		if plus_count > 0 {
			last_operand_type := g.infer_expression_type(tokens[operand_start..]) or { '' }
			string_operands << last_operand_type == 'string'
			if fastc_all_true(string_operands) {
				parts := fastc_split_top_level_c_plus(rendered_expression)
				if parts.len == plus_count + 1 {
					mut combined := parts[0]
					for part in parts[1..] {
						combined = 'builtin__string_plus(${combined},${part})'
					}
					return FastcRenderedExpression{
						source: combined
						typ:    'string'
					}
				}
			}
		}
	}
	return g.render_flag_method_expression(tokens, rendered_expression)
}

fn (g &Parser) render_cast_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut open := -1
	mut c_type := ''
	for i, item in tokens {
		if item.tok != .lpar {
			continue
		}
		// A leading `*` is a dereference around the cast, not part of its type.
		// The raw renderer preserves that unary operation while lowering the
		// nested `&Type(value)` cast.
		if tokens[0].tok == .mul {
			return none
		}
		c_type = g.type_from_expression_tokens(tokens[..i]) or { '' }
		if c_type == '' && i == 3 && tokens[0].tok == .name && tokens[0].lit == 'C'
			&& tokens[1].tok == .dot && tokens[2].tok == .name && tokens[2].lit.len > 0
			&& tokens[2].lit[0].is_capital() && 'C.${tokens[2].lit}' !in g.functions {
			c_type = tokens[2].lit
		}
		if c_type != '' {
			open = i
		}
		break
	}
	if open <= 0 || c_type == '' {
		return none
	}
	close := fastc_matching_rpar(tokens, open) or { return none }
	if close != tokens.len - 1 || open + 1 == close {
		return none
	}
	inner := g.render_call_argument_expression(tokens[open + 1..close], c_type) or { return none }
	return FastcRenderedExpression{
		source: '((${c_type})(${inner}))'
		typ:    c_type
	}
}

fn (g &Parser) render_flag_method_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 7 {
		return none
	}
	mut rendered := rendered_expression
	mut changed := false
	mut result_type := ''
	for i in 2 .. tokens.len {
		if tokens[i].tok != .name || tokens[i].lit !in ['has', 'set', 'clear']
			|| tokens[i - 1].tok != .dot || i + 2 >= tokens.len || tokens[i + 1].tok != .lpar {
			continue
		}
		call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
		if call_end <= i + 2 {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i - 1)
		receiver_tokens := tokens[receiver_start..i - 1]
		receiver_type := g.infer_expression_type(receiver_tokens) or { return none }
		receiver_key := g.semantic_type_key(receiver_type)
		if g.declared_kinds[receiver_key] != .enum_ {
			continue
		}
		mut receiver := strings.new_builder(32)
		for token_item in receiver_tokens {
			receiver.write_string(match token_item.tok {
				.name { token_item.lit }
				.dot { '.' }
				else { token_item.tok.str() }
			})
		}
		mut raw_argument := strings.new_builder(24)
		mut c_argument := strings.new_builder(48)
		mut argument_index := i + 2
		for argument_index < call_end {
			if tokens[argument_index].tok == .dot && argument_index + 1 < call_end
				&& tokens[argument_index + 1].tok == .name {
				raw_argument.write_string('.${tokens[argument_index + 1].lit}')
				c_argument.write_string('${fastc_c_declared_type_name(receiver_key)}__${tokens[
					argument_index + 1].lit}')
				argument_index += 2
				continue
			}
			piece := tokens[argument_index].tok.str()
			raw_argument.write_string(piece)
			c_argument.write_string(piece)
			argument_index++
		}
		method := tokens[i].lit
		raw_receiver_source := receiver.str()
		receiver_source := g.render_member_receiver(receiver_tokens) or { raw_receiver_source }
		raw_argument_source := raw_argument.str()
		c_argument_source := c_argument.str()
		mut needle := '${raw_receiver_source}.${method}(${c_argument_source})'
		if !rendered.contains(needle) {
			needle = '${receiver_source}.${method}(${c_argument_source})'
		}
		if !rendered.contains(needle) {
			needle = '${raw_receiver_source}.${method}(${raw_argument_source})'
		}
		if !rendered.contains(needle) {
			needle = '${receiver_source}.${method}(${raw_argument_source})'
		}
		replacement := match method {
			'has' { '((${receiver_source} & ${c_argument_source}) != 0)' }
			'set' { '((${receiver_source}) |= (${c_argument_source}))' }
			else { '((${receiver_source}) &= ~(${c_argument_source}))' }
		}
		if !rendered.contains(needle) {
			continue
		}
		rendered = rendered.replace(needle, replacement)
		if method == 'has' {
			result_type = 'bool'
		}
		changed = true
	}
	return if changed {
		FastcRenderedExpression{
			source: rendered
			typ:    result_type
		}
	} else {
		none
	}
}

fn (g &Parser) render_static_call_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut rendered := rendered_expression
	mut result_type := ''
	mut changed := false
	for i := 2; i + 1 < tokens.len; i++ {
		if tokens[i].tok != .name || tokens[i + 1].tok != .lpar {
			continue
		}
		function_key := g.static_function_key_for_call(tokens, i) or { continue }
		type_key := function_key.all_before_last('.')
		owner := fastc_c_declared_type_name(type_key)
		mut needle := '${owner}.${tokens[i].lit}('
		if !rendered.contains(needle) {
			needle = '${owner}__${tokens[i].lit}('
		}
		if !rendered.contains(needle) {
			continue
		}
		rendered = rendered.replace(needle,
			'${fastc_method_c_name_for_key(type_key, tokens[i].lit)}(')
		signature := g.functions[function_key] or { FastcFunctionSignature{} }
		result_type = signature.return_type
		changed = true
	}
	return if changed {
		FastcRenderedExpression{
			source: rendered
			typ:    result_type
		}
	} else {
		none
	}
}

fn (g &Parser) method_function_key(receiver_type string, name string) string {
	direct_key := '${g.semantic_type_key(receiver_type)}.${name}'
	if direct_key in g.functions {
		return direct_key
	}
	if g.selfhost && name in ['keys', 'values'] && 'map.${name}' in g.functions {
		return 'map.${name}'
	}
	mut layout_type := receiver_type.trim_right('*')
	if layout_type.starts_with('Array_') {
		layout_type = 'array'
	} else if layout_type.starts_with('Map_') {
		if 'map.${name}' in g.functions {
			return 'map.${name}'
		}
		layout_type = 'map'
	}
	mut fields := map[string]string{}
	if layout_type in g.struct_fields {
		fields = g.struct_fields[layout_type].clone()
	}
	if 'data' in fields && 'len' in fields && 'cap' in fields && 'array.${name}' in g.functions {
		return 'array.${name}'
	}
	return direct_key
}

fn (g &Parser) specialized_method_return_type(receiver_type string, method_key string, signature FastcFunctionSignature) string {
	if method_key in ['map.keys', 'map.values'] {
		key_type, value_type := fastc_map_key_value_types(receiver_type) or {
			return signature.return_type
		}
		element_type := if method_key == 'map.keys' { key_type } else { value_type }
		return fastc_array_c_type(element_type)
	}
	if method_key.starts_with('array.') {
		if element_type := g.array_element_type(receiver_type) {
			method_name := method_key.all_after_last('.')
			if method_name in ['first', 'last', 'pop', 'pop_left', 'get', 'get_unsafe', 'get_i64',
				'get_u64', 'get_ni', 'get_with_check', 'get_with_check_i64', 'get_with_check_u64',
				'get_with_check_ni'] {
				return element_type
			}
			if signature.return_type == 'array' {
				return fastc_array_c_type(element_type)
			}
		}
	}
	if method_key.starts_with('map.') && signature.return_type == 'map'
		&& receiver_type.trim_right('*').starts_with('Map_') {
		return receiver_type.trim_right('*')
	}
	return signature.return_type
}

fn (g &Parser) render_interface_cast_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 4 || tokens[0].tok != .name || tokens[1].tok != .lpar
		|| tokens.last().tok != .rpar {
		return none
	}
	type_key := fastc_resolve_declared_type_key(g.module_name, tokens[0].lit, g.declared_types) or {
		return none
	}
	if g.declared_kinds[type_key] != .interface_ {
		return none
	}
	close := fastc_matching_rpar(tokens, 1) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	interface_type := fastc_c_declared_type_name(type_key)
	prefix := '((${interface_type})('
	if !rendered_expression.starts_with(prefix) || !rendered_expression.ends_with('))') {
		return none
	}
	inner_source := rendered_expression[prefix.len..rendered_expression.len - 2]
	actual_type := g.infer_expression_type(tokens[2..close]) or { return none }
	if actual_type == '' {
		return none
	}
	return FastcRenderedExpression{
		source: g.interface_value_expression(interface_type, actual_type, inner_source)
		typ:    interface_type
	}
}

fn (g &Parser) render_map_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if lookup := g.render_map_lookup_option_expression(tokens) {
		return FastcRenderedExpression{
			source: '({ Option lookup = (${lookup.source}); lookup.state ? (${lookup.typ}){0} : *((${lookup.typ} *)lookup.data); })'
			typ:    lookup.typ
		}
	}
	mut literal_open := -1
	for i, item in tokens {
		if item.tok == .lcbr {
			literal_open = i
			break
		}
	}
	if literal_open > 0 && literal_open + 1 == tokens.len - 1 && tokens.last().tok == .rcbr {
		if map_type := g.map_initializer_type(tokens[..literal_open]) {
			key_type, value_type := fastc_map_key_value_types(map_type) or { return none }
			hash_fn, eq_fn, clone_fn, free_fn := fastc_map_runtime_functions(key_type)
			return FastcRenderedExpression{
				source: '(builtin__new_map(sizeof(${fastc_runtime_c_type(key_type)}), sizeof(${fastc_runtime_c_type(value_type)}), &${hash_fn}, &${eq_fn}, &${clone_fn}, &${free_fn}))'
				typ:    map_type
			}
		}
	}
	mut depth := 0
	mut assignment_index := -1
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				depth++
			}
			.rpar, .rsbr, .rcbr {
				depth--
			}
			.assign {
				if depth == 0 {
					assignment_index = i
					break
				}
			}
			else {}
		}
	}
	if assignment_index > 3 && tokens[assignment_index - 1].tok == .rsbr {
		close := assignment_index - 1
		mut open := -1
		mut bracket_depth := 0
		for i := close; i >= 0; i-- {
			if tokens[i].tok == .rsbr {
				bracket_depth++
			} else if tokens[i].tok == .lsbr {
				bracket_depth--
				if bracket_depth == 0 {
					open = i
					break
				}
			}
		}
		if open <= 0 {
			return none
		}
		base_tokens := tokens[..open]
		map_type := g.infer_expression_type(base_tokens) or { return none }
		key_type, value_type := fastc_map_key_value_types(map_type) or { return none }
		key_source := g.render_call_argument_expression(tokens[open + 1..close], key_type) or {
			return none
		}
		value_source := g.render_call_argument_expression(tokens[assignment_index + 1..],
			value_type) or { return none }
		map_source := g.render_member_receiver(base_tokens) or {
			g.render_raw_expression_tokens(base_tokens) or { return none }
		}
		map_address := if map_type.ends_with('*') { map_source } else { '&${map_source}' }
		return FastcRenderedExpression{
			source: '({ ${key_type} __v_fastc_map_key = (${key_source}); ${value_type} __v_fastc_map_value = (${value_source}); builtin__map_set((map *)${map_address}, &__v_fastc_map_key, &__v_fastc_map_value); __v_fastc_map_value; })'
			typ:    value_type
		}
	}
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .lsbr
		&& tokens.last().tok == .rsbr {
		close := fastc_matching_delimiter(tokens, 1, .lsbr, .rsbr) or { return none }
		if close != tokens.len - 1 {
			return none
		}
		map_type := g.infer_expression_type(tokens[..1]) or { return none }
		key_type, value_type := fastc_map_key_value_types(map_type) or { return none }
		key_source := g.render_call_argument_expression(tokens[2..close], key_type) or {
			return none
		}
		global_key := fastc_global_key(g.module_name, tokens[0].lit)
		map_source := g.globals[global_key] or { tokens[0].lit }
		map_address := if map_type.ends_with('*') { map_source } else { '&${map_source}' }
		return FastcRenderedExpression{
			source: '({ ${key_type} __v_fastc_map_key = (${key_source}); ${value_type} __v_fastc_map_zero = (${value_type}){0}; *((${value_type} *)builtin__map_get((map *)${map_address}, &__v_fastc_map_key, &__v_fastc_map_zero)); })'
			typ:    value_type
		}
	}
	return none
}

fn (g &Parser) render_struct_literal_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut open := -1
	mut delimiter_depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr] {
			delimiter_depth++
		} else if item.tok in [.rpar, .rsbr] {
			delimiter_depth--
		} else if item.tok == .lcbr && delimiter_depth == 0 {
			open = i
			break
		}
	}
	if open <= 0 || tokens.last().tok != .rcbr {
		return none
	}
	close := fastc_matching_delimiter(tokens, open, .lcbr, .rcbr) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	is_c_struct_literal := open == 3 && tokens[0].tok == .name && tokens[0].lit == 'C'
		&& tokens[1].tok == .dot && tokens[2].tok == .name
	mut c_type := g.type_from_expression_tokens(tokens[..open]) or { '' }
	if c_type == '' && is_c_struct_literal {
		c_type = if '#Cstruct#${tokens[2].lit}' in g.declared_types {
			'struct ${tokens[2].lit}'
		} else {
			tokens[2].lit
		}
	}
	mut layout_type := c_type.trim_right('*')
	if layout_type.starts_with('Array_') {
		layout_type = 'array'
	}
	if c_type == '' || (!is_c_struct_literal && layout_type !in g.struct_fields
		&& g.declared_kinds[g.semantic_type_key(c_type)] !in [.struct_, .union_]) {
		return none
	}
	mut fields := map[string]string{}
	if layout_type in g.struct_fields {
		fields = g.struct_fields[layout_type].clone()
	}
	if is_c_struct_literal && open + 1 < close {
		items := fastc_expression_list_items(tokens, open + 1, close) or { return none }
		mut is_positional := false
		for item in items {
			if item.len == 0 {
				continue
			}
			if !(item.len >= 2 && item[0].tok == .name && item[1].tok == .colon) && !(item.len == 1
				&& item[0].tok == .name && item[0].lit in fields) {
				is_positional = true
				break
			}
		}
		if is_positional {
			mut values := []string{cap: items.len}
			for item in items {
				values << g.render_call_argument_expression(item, '') or { return none }
			}
			source := if c_type.ends_with('*') {
				'&(${c_type.trim_right('*')}){${values.join(',')}}'
			} else {
				'(${c_type}){${values.join(',')}}'
			}
			return FastcRenderedExpression{
				source: source
				typ:    c_type
			}
		}
	}
	mut rendered_fields := []string{}
	mut field_values := map[string]string{}
	mut update_source := ''
	mut index := open + 1
	for index < close {
		for index < close && tokens[index].tok in [.semicolon, .comma] {
			index++
		}
		if index >= close {
			break
		}
		if tokens[index].tok == .ellipsis {
			index++
			value_start := index
			for index < close && tokens[index].tok !in [.semicolon, .comma] {
				index++
			}
			if value_start == index {
				return none
			}
			update_source = g.render_call_argument_expression(tokens[value_start..index], c_type) or {
				return none
			}
			continue
		}
		if tokens[index].tok != .name {
			return none
		}
		field_name := tokens[index].lit
		index++
		mut value_tokens := []FastcExpressionToken{}
		if index < close && tokens[index].tok == .colon {
			index++
			value_start := index
			mut parens := 0
			mut brackets := 0
			mut braces := 0
			for index < close {
				match tokens[index].tok {
					.lpar {
						parens++
					}
					.rpar {
						parens--
					}
					.lsbr {
						brackets++
					}
					.rsbr {
						brackets--
					}
					.lcbr {
						braces++
					}
					.rcbr {
						braces--
					}
					.semicolon, .comma {
						if parens == 0 && brackets == 0 && braces == 0 {
							break
						}
					}
					else {}
				}
				index++
			}
			if value_start == index {
				return none
			}
			value_tokens = tokens[value_start..index]
		} else {
			value_tokens = [
				FastcExpressionToken{
					tok: .name
					lit: field_name
				},
			]
		}
		mut c_field_name := field_name
		mut expected_type := if layout_type == 'array' && field_name == 'init' {
			g.array_element_type(c_type) or { '' }
		} else {
			fields[field_name] or { '' }
		}
		if expected_type == '' {
			for embedded_name, embedded_type in fields {
				if !embedded_name.starts_with('__embedded_') || embedded_type !in g.struct_fields {
					continue
				}
				embedded_fields := g.struct_fields[embedded_type].clone()
				if nested_type := embedded_fields[field_name] {
					c_field_name = '${embedded_name}.${field_name}'
					expected_type = nested_type
					break
				}
			}
		}
		if fixed_element_type := fastc_fixed_array_element_type(expected_type) {
			array_end := if value_tokens.len > 0 && value_tokens.last().tok == .not {
				value_tokens.len - 1
			} else {
				value_tokens.len
			}
			if array_end >= 2 && value_tokens[0].tok == .lsbr
				&& value_tokens[array_end - 1].tok == .rsbr {
				items := fastc_expression_list_items(value_tokens, 1, array_end - 1) or {
					return none
				}
				mut values := []string{}
				for item in items {
					rendered_item := g.render_call_argument_expression(item, fixed_element_type) or {
						return none
					}
					values << rendered_item
				}
				rendered_fields << '.${c_field_name}={${values.join(',')}}'
				field_values[field_name] = '{${values.join(',')}}'
				continue
			}
		}
		value := g.render_call_argument_expression(value_tokens, expected_type) or { return none }
		rendered_fields << '.${c_field_name}=(${value})'
		field_values[field_name] = value
	}
	if layout_type == 'array' {
		element_type := g.array_element_type(c_type) or { return none }
		length := field_values['len'] or { '0' }
		capacity := field_values['cap'] or { '0' }
		base := '((${c_type})builtin____new_array(${length},${capacity},sizeof(${element_type})))'
		if initial := field_values['init'] {
			return FastcRenderedExpression{
				source: '({ ${c_type} __v_fastc_array_init = ${base}; ${element_type} __v_fastc_array_default = (${initial}); for (int __v_fastc_array_index = 0; __v_fastc_array_index < __v_fastc_array_init.len; __v_fastc_array_index++) { ((${element_type} *)__v_fastc_array_init.data)[__v_fastc_array_index] = __v_fastc_array_default; } __v_fastc_array_init; })'
				typ:    c_type
			}
		}
		return FastcRenderedExpression{
			source: base
			typ:    c_type
		}
	}
	if update_source != '' {
		mut assignments := []string{cap: rendered_fields.len}
		for field in rendered_fields {
			assignments << '__v_fastc_struct_update${field};'
		}
		return FastcRenderedExpression{
			source: '({ ${c_type} __v_fastc_struct_update = (${update_source}); ${assignments.join(' ')} __v_fastc_struct_update; })'
			typ:    c_type
		}
	}
	literal_source := if c_type.ends_with('*') {
		'(${c_type})v_fastc_interface_box(&(${c_type.trim_right('*')}){${rendered_fields.join(',')}}, sizeof(${c_type.trim_right('*')}))'
	} else {
		'(${c_type}){${rendered_fields.join(',')}}'
	}
	return FastcRenderedExpression{
		source: literal_source
		typ:    c_type
	}
}

fn (g &Parser) render_struct_literal_field_names(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut open := -1
	for i, item in tokens {
		if item.tok == .lcbr {
			open = i
			break
		}
	}
	if open <= 0 || tokens.last().tok != .rcbr {
		return none
	}
	c_type := g.type_from_expression_tokens(tokens[..open]) or { return none }
	fields := g.struct_fields[c_type.trim_right('*')].clone()
	mut rendered := rendered_expression
	mut changed := false
	for field_name in fields.keys() {
		for module_name in [g.module_name, 'builtin'] {
			constant_name := g.constants[fastc_constant_key(module_name, field_name)] or {
				continue
			}
			needle := '.${constant_name}='
			if rendered.contains(needle) {
				rendered = rendered.replace(needle, '.${field_name}=')
				changed = true
			}
		}
	}
	return if changed {
		FastcRenderedExpression{
			source: rendered
			typ:    c_type
		}
	} else {
		none
	}
}

fn (g &Parser) render_array_assignment_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	mut assignment_index := -1
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok.is_assignment() {
			assignment_index = i
			break
		}
	}
	if assignment_index <= 0 || assignment_index + 1 >= tokens.len
		|| tokens[assignment_index - 1].tok != .rsbr {
		return none
	}
	left := g.render_array_access_expression(tokens[..assignment_index]) or { return none }
	right := g.render_call_argument_expression(tokens[assignment_index + 1..], left.typ) or {
		return none
	}
	operator := if tokens[assignment_index].tok == .right_shift_unsigned_assign {
		'>>='
	} else {
		tokens[assignment_index].tok.str()
	}
	return FastcRenderedExpression{
		source: '${left.source}${operator}${right}'
		typ:    left.typ
	}
}

fn (g &Parser) render_assignment_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	mut assignment_index := -1
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok.is_assignment() {
			assignment_index = i
			break
		}
	}
	if assignment_index <= 0 || assignment_index + 1 >= tokens.len {
		return none
	}
	left_tokens := tokens[..assignment_index]
	left_type := g.infer_expression_type(left_tokens) or { return none }
	if left_type == '' {
		return none
	}
	mut left := ''
	if array_access := g.render_array_access_expression(left_tokens) {
		left = array_access.source
	} else if member := g.render_member_receiver(left_tokens) {
		left = member
	} else {
		raw := g.render_raw_expression_tokens(left_tokens) or { return none }
		left = if pointer_members := g.render_pointer_member_access_expression(left_tokens, raw) {
			pointer_members.source
		} else {
			raw
		}
	}
	right := g.render_call_argument_expression(tokens[assignment_index + 1..], left_type) or {
		return none
	}
	operator := tokens[assignment_index].tok
	source := if operator == .plus_assign && left_type == 'string' {
		'${left}=builtin__string_plus(${left},${right})'
	} else {
		c_operator := if operator == .right_shift_unsigned_assign { '>>=' } else { operator.str() }
		'${left}${c_operator}${right}'
	}
	return FastcRenderedExpression{
		source: source
		typ:    left_type
	}
}

fn (g &Parser) render_pointer_member_access_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 3 {
		return none
	}
	for i in 1 .. tokens.len - 1 {
		if tokens[i].tok == .dot && tokens[i + 1].tok == .name && i + 2 < tokens.len
			&& tokens[i + 2].tok == .lpar {
			return none
		}
	}
	mut rendered := rendered_expression
	mut changed := false
	for i in 1 .. tokens.len - 1 {
		if tokens[i].tok != .dot || tokens[i + 1].tok != .name {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i)
		receiver_tokens := tokens[receiver_start..i]
		receiver_type := g.infer_expression_type(receiver_tokens) or { continue }
		if !receiver_type.ends_with('*') {
			continue
		}
		receiver_source := g.render_member_receiver(receiver_tokens) or {
			g.render_membership_candidate(receiver_tokens, '') or { continue }
		}
		needle := '${receiver_source}.${tokens[i + 1].lit}'
		if rendered.contains(needle) {
			rendered = rendered.replace(needle, '${receiver_source}->${tokens[i + 1].lit}')
			changed = true
			continue
		}
		raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { '' }
		raw_needle := '${raw_receiver}.${tokens[i + 1].lit}'
		if raw_receiver != '' && rendered.contains(raw_needle) {
			rendered = rendered.replace(raw_needle, '${raw_receiver}->${tokens[i + 1].lit}')
			changed = true
			continue
		}
		parenthesized_needle := ').${tokens[i + 1].lit}'
		if receiver_tokens.last().tok == .rpar && rendered.contains(parenthesized_needle) {
			rendered = rendered.replace(parenthesized_needle, ')->${tokens[i + 1].lit}')
			changed = true
		}
	}
	if chained_array := g.render_chained_array_access_expression(tokens, rendered) {
		rendered = chained_array.source
		changed = true
	}
	if !changed {
		return none
	}
	inferred_type := g.infer_expression_type(tokens) or { '' }
	return FastcRenderedExpression{
		source: rendered
		typ:    inferred_type
	}
}

fn (g &Parser) render_chained_array_access_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut rendered := rendered_expression
	mut changed := false
	for open, item in tokens {
		if item.tok != .lsbr {
			continue
		}
		close := fastc_matching_delimiter(tokens, open, .lsbr, .rsbr) or { continue }
		if close <= open + 1 || fastc_expression_tokens_contain(tokens[open + 1..close], .dotdot) {
			continue
		}
		start := fastc_method_receiver_start(tokens, open)
		if start >= open {
			continue
		}
		base_tokens := tokens[start..open]
		base_type := g.infer_expression_type(base_tokens) or { continue }
		is_array_pointer := base_type.ends_with('*') && g.array_element_type(base_type) != none
		element_type := if base_type == 'string' {
			'u8'
		} else if is_array_pointer {
			g.array_element_type(base_type) or { continue }
		} else if base_type.ends_with('*') {
			base_type.trim_right('*')
		} else {
			g.array_element_type(base_type) or { continue }
		}
		raw_base := g.render_raw_expression_tokens(base_tokens) or { continue }
		base_is_global_or_constant := base_tokens.len == 1
			&& (fastc_global_key(g.module_name, base_tokens[0].lit) in g.globals
			|| fastc_constant_key(g.module_name, base_tokens[0].lit) in g.constants
			|| base_tokens[0].lit in g.constants)
		base_source := if base_is_global_or_constant {
			raw_base
		} else {
			g.render_member_receiver(base_tokens) or { raw_base }
		}
		index_source := g.render_membership_candidate(tokens[open + 1..close], 'int') or {
			continue
		}
		access := if base_type.ends_with('*') { '->' } else { '.' }
		is_raw_fixed_array := base_type.trim_right('*').starts_with('FixedArray_')
			&& (base_tokens.len > 1 || (base_tokens.len == 1
			&& fastc_global_key(g.module_name, base_tokens[0].lit) in g.globals))
		replacement := if base_type == 'string' {
			'(${base_source}${access}str[${index_source}])'
		} else if is_raw_fixed_array {
			'((${base_source})[${index_source}])'
		} else if base_type.ends_with('*') && !is_array_pointer {
			'((${base_source})[${index_source}])'
		} else {
			'(((${element_type} *)${base_source}${access}data)[${index_source}])'
		}
		mut needle := '${base_source}[${index_source}]'
		if !rendered.contains(needle) {
			needle = '${raw_base}[${index_source}]'
		}
		if rendered.contains(needle) {
			rendered = rendered.replace(needle, replacement)
			changed = true
		}
	}
	inferred_type := g.infer_expression_type(tokens) or { '' }
	return if changed {
		FastcRenderedExpression{
			source: rendered
			typ:    inferred_type
		}
	} else {
		none
	}
}

fn (g &Parser) render_logical_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok in [.and, .logical_or] && i > 0 && i + 1 < tokens.len {
			left := g.render_call_argument_expression(tokens[..i], 'bool') or { return none }
			right := g.render_call_argument_expression(tokens[i + 1..], 'bool') or { return none }
			return FastcRenderedExpression{
				source: '((${left})${if item.tok == .and { '&&' } else { '||' }}(${right}))'
				typ:    'bool'
			}
		}
	}
	return none
}

fn (g &Parser) render_enum_comparison_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok in [.eq, .ne, .lt, .gt, .le, .ge] && i > 0
			&& i + 1 < tokens.len {
			left_tokens := tokens[..i]
			right_tokens := tokens[i + 1..]
			mut left_type := g.infer_expression_type(left_tokens) or { '' }
			mut right_type := g.infer_expression_type(right_tokens) or { '' }
			if left_type == '' && left_tokens.len > 2
				&& left_tokens[left_tokens.len - 2].tok == .dot && left_tokens.last().tok == .name {
				receiver_type := g.infer_expression_type(left_tokens[..left_tokens.len - 2]) or {
					''
				}
				left_type = g.struct_member_type(receiver_type, left_tokens.last().lit)
			}
			if right_type == '' && right_tokens.len > 2
				&& right_tokens[right_tokens.len - 2].tok == .dot
				&& right_tokens.last().tok == .name {
				receiver_type := g.infer_expression_type(right_tokens[..right_tokens.len - 2]) or {
					''
				}
				right_type = g.struct_member_type(receiver_type, right_tokens.last().lit)
			}
			if g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ && right_tokens.len == 2
				&& right_tokens[0].tok == .dot && right_tokens[1].tok == .name {
				left := g.render_call_argument_expression(left_tokens, left_type) or { return none }
				enum_type := left_type.trim_right('*')
				return FastcRenderedExpression{
					source: '((${left}) ${item.tok.str()} (${enum_type}__${right_tokens[1].lit}))'
					typ:    'bool'
				}
			}
			if g.declared_kinds[g.semantic_type_key(right_type)] == .enum_ && left_tokens.len == 2
				&& left_tokens[0].tok == .dot && left_tokens[1].tok == .name {
				right := g.render_call_argument_expression(right_tokens, right_type) or {
					return none
				}
				enum_type := right_type.trim_right('*')
				return FastcRenderedExpression{
					source: '((${enum_type}__${left_tokens[1].lit}) ${item.tok.str()} (${right}))'
					typ:    'bool'
				}
			}
		}
	}
	return none
}

fn (g &Parser) render_option_none_comparison(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && item.tok in [.eq, .ne] && i > 0 && i + 1 < tokens.len {
			left_tokens := tokens[..i]
			right_tokens := tokens[i + 1..]
			left_is_none := left_tokens.len == 1 && left_tokens[0].tok == .key_none
			right_is_none := right_tokens.len == 1 && right_tokens[0].tok == .key_none
			if left_is_none == right_is_none {
				return none
			}
			value_tokens := if left_is_none { right_tokens } else { left_tokens }
			value := g.render_call_argument_expression(value_tokens, 'Option') or { return none }
			operator := if item.tok == .eq { '==' } else { '!=' }
			return FastcRenderedExpression{
				source: '((${value}).state ${operator} 2)'
				typ:    'bool'
			}
		}
	}
	return none
}

fn (g &Parser) render_string_comparison_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
			continue
		}
		if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
			continue
		}
		if depth != 0 || item.tok !in [.and, .logical_or] || i == 0 || i + 1 >= tokens.len {
			continue
		}
		left_tokens := tokens[..i]
		right_tokens := tokens[i + 1..]
		mut left_special := FastcRenderedExpression{}
		if special := g.render_string_comparison_expression(left_tokens) {
			left_special = special
		}
		mut right_special := FastcRenderedExpression{}
		if special := g.render_string_comparison_expression(right_tokens) {
			right_special = special
		}
		if left_special.source == '' && right_special.source == '' {
			continue
		}
		left_source := if left_special.source != '' {
			left_special.source
		} else {
			g.render_comparison_operand(left_tokens, '') or { return none }
		}
		right_source := if right_special.source != '' {
			right_special.source
		} else {
			g.render_comparison_operand(right_tokens, '') or { return none }
		}
		return FastcRenderedExpression{
			source: '(${left_source}${if item.tok == .and { '&&' } else { '||' }}${right_source})'
			typ:    'bool'
		}
	}
	depth = 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
			continue
		}
		if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
			continue
		}
		if depth != 0 || item.tok !in [.eq, .ne, .lt, .gt, .le, .ge] || i == 0
			|| i + 1 >= tokens.len {
			continue
		}
		left_tokens := tokens[..i]
		right_tokens := tokens[i + 1..]
		left_type := g.infer_expression_type(left_tokens) or { return none }
		right_type := g.infer_expression_type(right_tokens) or { return none }
		if left_type.trim_right('*') != 'string' || right_type.trim_right('*') != 'string' {
			return none
		}
		left_source := g.render_comparison_operand(left_tokens, 'string') or { return none }
		right_source := g.render_comparison_operand(right_tokens, 'string') or { return none }
		source := match item.tok {
			.eq { 'builtin__string_eq(${left_source},${right_source})' }
			.ne { '!builtin__string_eq(${left_source},${right_source})' }
			.lt { 'builtin__string_lt(${left_source},${right_source})' }
			.gt { 'builtin__string_lt(${right_source},${left_source})' }
			.le { '!builtin__string_lt(${right_source},${left_source})' }
			.ge { '!builtin__string_lt(${left_source},${right_source})' }
			else { return none }
		}
		return FastcRenderedExpression{
			source: source
			typ:    'bool'
		}
	}
	return none
}

fn (g &Parser) render_comparison_operand(tokens []FastcExpressionToken, expected_type string) ?string {
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	if concatenation := g.render_composed_string_concatenation(tokens) {
		return concatenation.source
	}
	if method_call := g.render_method_call_expression(tokens, raw) {
		return method_call.source
	}
	if call := g.render_missing_call_arguments(tokens, raw) {
		return call.source
	}
	if pointer_members := g.render_pointer_member_access_expression(tokens, raw) {
		return pointer_members.source
	}
	return g.render_membership_candidate(tokens, expected_type)
}

fn (g &Parser) render_call_argument_expression(tokens []FastcExpressionToken, expected_type string) ?string {
	if tokens.len >= 2 && tokens[0].tok == .lpar && tokens.last().tok == .rpar {
		close := fastc_matching_rpar(tokens, 0) or { -1 }
		if close == tokens.len - 1 {
			inner := g.render_call_argument_expression(tokens[1..tokens.len - 1], expected_type) or {
				return none
			}
			return '(${inner})'
		}
	}
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	if tokens.len == 1 && tokens[0].tok == .name {
		if local := g.locals[tokens[0].lit] {
			if local.is_reference {
				value_type := local.typ.trim_right('*')
				if fastc_is_pointer_type(expected_type) {
					return raw
				}
				if expected_type != '' && (expected_type == value_type
					|| fastc_selfhost_types_are_compatible(value_type, expected_type)) {
					return '*(${raw})'
				}
			}
		}
	}
	mut rendered := ''
	if special := g.render_special_expression(tokens, raw) {
		rendered = special.source
	} else {
		rendered = g.render_membership_candidate(tokens, expected_type) or { return none }
	}
	rendered = g.render_constant_references(tokens, rendered)
	actual_type := g.infer_expression_type(tokens) or { '' }
	if expected_type == 'string' && actual_type.trim_right('*') == 'IError' {
		return 'builtin__IError_msg(${rendered})'
	}
	if actual_type.ends_with('*') && expected_type == actual_type.trim_right('*')
		&& expected_type.starts_with('Map_') {
		return '*(${rendered})'
	}
	if expected_type.ends_with('*') && actual_type == expected_type.trim_right('*')
		&& actual_type.starts_with('Map_') {
		return '&(${rendered})'
	}
	return rendered
}

fn (g &Parser) render_map_lookup_option_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut start := 0
	for start < tokens.len && tokens[start].tok == .lpar {
		start++
	}
	lookup_tokens := tokens[start..]
	if lookup_tokens.len < 4 || lookup_tokens.last().tok != .rsbr {
		return none
	}
	mut open := -1
	mut depth := 0
	for i := lookup_tokens.len - 1; i >= 0; i-- {
		if lookup_tokens[i].tok == .rsbr {
			depth++
		} else if lookup_tokens[i].tok == .lsbr {
			depth--
			if depth == 0 {
				open = i
				break
			}
		}
	}
	if open <= 0 {
		return none
	}
	base_tokens := lookup_tokens[..open]
	map_type := g.infer_expression_type(base_tokens) or { return none }
	key_type, value_type := fastc_map_key_value_types(map_type) or { return none }
	mut map_source := if base_tokens.len == 1 && base_tokens[0].tok == .name {
		g.globals[fastc_global_key(g.module_name, base_tokens[0].lit)] or { base_tokens[0].lit }
	} else {
		g.render_member_receiver(base_tokens) or { return none }
	}
	if map_type.ends_with('*') {
		map_source = '*(${map_source})'
	}
	key_source := g.render_membership_candidate(lookup_tokens[open + 1..lookup_tokens.len - 1],
		key_type) or { return none }
	return FastcRenderedExpression{
		source: '({ ${key_type} __v_fastc_map_key = (${key_source}); ${value_type} *__v_fastc_map_value = (${value_type} *)builtin__map_get_check((map *)&(${map_source}), &__v_fastc_map_key); (Option){.data=__v_fastc_map_value, .state=__v_fastc_map_value == NULL ? 2 : 0}; })'
		typ:    value_type
	}
}

fn fastc_map_runtime_functions(key_type string) (string, string, string, string) {
	if key_type == 'string' {
		return 'builtin__map_hash_string', 'builtin__map_eq_string', 'builtin__map_clone_string', 'builtin__map_free_string'
	}
	suffix := if key_type in ['i8', 'u8', 'byte', 'char', 'bool'] {
		'1'
	} else if key_type in ['i16', 'u16'] {
		'2'
	} else if key_type in ['i64', 'u64', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr'] {
		'8'
	} else {
		'4'
	}
	return 'builtin__map_hash_int_${suffix}', 'builtin__map_eq_int_${suffix}', 'builtin__map_clone_int_${suffix}', 'builtin__map_free_nop'
}

fn (g &Parser) render_missing_call_arguments(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 3 || tokens.last().tok != .rpar {
		return none
	}
	mut name_index := 0
	mut open_index := 1
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .dot && tokens[2].tok == .name
		&& (tokens[0].lit in g.imports || tokens[0].lit == 'C') {
		name_index = 2
		open_index = 3
	}
	if tokens[name_index].tok != .name || tokens[open_index].tok != .lpar {
		return none
	}
	close := fastc_matching_rpar(tokens, open_index) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	function_key := g.function_key_for_call(tokens, name_index)
	signature := g.functions[function_key] or { return none }
	call_args := fastc_call_arguments(tokens, open_index, close) or { return none }
	mut named_start := -1
	for i, argument in call_args {
		if argument.len >= 3 && argument[0].tok == .name && argument[1].tok == .colon {
			named_start = i
			break
		}
	}
	if named_start >= 0 && named_start < signature.parameter_types.len {
		mut rendered_arguments := []string{}
		for argument_index, argument in call_args[..named_start] {
			expected_type := if argument_index < signature.parameter_types.len {
				signature.parameter_types[argument_index]
			} else {
				''
			}
			rendered_argument := g.render_call_argument_expression(argument, expected_type) or {
				return none
			}
			rendered_arguments << rendered_argument
		}
		parameter_type := signature.parameter_types[named_start]
		mut fields := []string{}
		for argument in call_args[named_start..] {
			if argument.len < 3 || argument[0].tok != .name || argument[1].tok != .colon {
				return none
			}
			value := g.render_call_argument_expression(argument[2..], '') or { return none }
			fields << '.${argument[0].lit}=${value}'
		}
		rendered_arguments << '(${parameter_type}){${fields.join(',')}}'
		return FastcRenderedExpression{
			source: '${fastc_c_function_name_for_key(function_key)}(${rendered_arguments.join(',')})'
			typ:    signature.return_type
		}
	}
	if signature.is_variadic && !function_key.starts_with('C.') {
		fixed_arguments := signature.parameter_types.len - 1
		if call_args.len < fixed_arguments {
			return none
		}
		variadic_type := signature.parameter_types.last()
		element_type := g.array_element_type(variadic_type) or { return none }
		mut rendered_arguments := []string{}
		for argument_index, argument in call_args {
			expected_type := if argument_index < fixed_arguments {
				signature.parameter_types[argument_index]
			} else {
				element_type
			}
			rendered_argument := g.render_call_argument_expression(argument, expected_type) or {
				return none
			}
			rendered_arguments << rendered_argument
		}
		variadic_arguments := rendered_arguments[fixed_arguments..]
		packed := if variadic_arguments.len == 0 {
			'(${variadic_type}){0}'
		} else {
			'((${variadic_type})builtin__new_array_from_c_array(${variadic_arguments.len}, ${variadic_arguments.len}, sizeof(${element_type}), (${element_type}[]){${variadic_arguments.join(',')}}))'
		}
		mut c_arguments := rendered_arguments[..fixed_arguments].clone()
		c_arguments << packed
		return FastcRenderedExpression{
			source: '${fastc_c_function_name_for_key(function_key)}(${c_arguments.join(',')})'
			typ:    signature.return_type
		}
	}
	mut rendered_arguments := []string{cap: signature.parameter_types.len}
	for argument_index, argument in call_args {
		expected_type := if argument_index < signature.parameter_types.len {
			signature.parameter_types[argument_index]
		} else {
			''
		}
		rendered_argument := g.render_call_argument_expression(argument, expected_type) or {
			return none
		}
		rendered_arguments << rendered_argument
	}
	for parameter_type in signature.parameter_types[call_args.len..] {
		rendered_arguments << '(${parameter_type}){0}'
	}
	call_name := if function_key.starts_with('C.') {
		function_key.all_after_last('.')
	} else {
		fastc_c_function_name_for_key(function_key)
	}
	return FastcRenderedExpression{
		source: '${call_name}(${rendered_arguments.join(',')})'
		typ:    signature.return_type
	}
}

fn (g &Parser) render_append_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut depth := 0
	mut operator_index := -1
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				depth++
			}
			.rpar, .rsbr, .rcbr {
				depth--
			}
			.left_shift {
				if depth == 0 {
					operator_index = i
					break
				}
			}
			else {}
		}
	}
	if operator_index <= 0 || operator_index + 1 >= tokens.len {
		return none
	}
	left_type := g.infer_expression_type(tokens[..operator_index]) or { return none }
	if g.array_element_type(left_type) == none {
		return none
	}
	separator := rendered_expression.index('<<') or { return none }
	left_source := rendered_expression[..separator]
	right_source := rendered_expression[separator + 2..]
	temporary := '__v_fastc_append_value'
	return FastcRenderedExpression{
		source: '({ __typeof__((${right_source})) ${temporary} = (${right_source}); builtin__array_push((array *)&(${left_source}), &${temporary}); 0; })'
		typ:    'void'
	}
}

fn (g &Parser) render_method_call_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	mut rendered := rendered_expression
	mut changed := false
	if flags := g.render_flag_method_expression(tokens, rendered) {
		rendered = flags.source
		changed = true
	}
	for i := tokens.len - 2; i >= 2; i-- {
		if tokens[i].tok != .name || tokens[i - 1].tok != .dot || tokens[i + 1].tok != .lpar {
			continue
		}
		if tokens[i - 2].tok == .name
			&& (tokens[i - 2].lit in g.imports || tokens[i - 2].lit == 'C') {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i - 1)
		receiver_tokens := tokens[receiver_start..i - 1]
		receiver_type := g.infer_expression_type(receiver_tokens) or { continue }
		method_key := g.method_function_key(receiver_type, tokens[i].lit)
		if method_key !in g.functions {
			if g.struct_member_type(receiver_type, tokens[i].lit) != '' {
				receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
				for separator in ['->', '.'] {
					marker := '${receiver.source}${separator}${tokens[i].lit}('
					if rendered.contains(marker) {
						rendered = rendered.replace(marker,
							'(${receiver.source}${separator}${tokens[i].lit})(')
						changed = true
						break
					}
				}
			}
			continue
		}
		signature := g.functions[method_key]
		if signature.parameter_types.len == 0 {
			continue
		}
		receiver := g.render_method_receiver_expression(receiver_tokens) or { continue }
		mut receiver_source := receiver.source
		mut separator := if receiver_tokens.len == 1 && receiver_type.ends_with('*') {
			'->'
		} else {
			'.'
		}
		mut method_marker := '${separator}${tokens[i].lit}('
		if receiver_start == 0 {
			raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { receiver_source }
			if receiver_source == raw_receiver && rendered.contains(method_marker) {
				receiver_source = rendered.all_before_last(method_marker)
			} else {
				alternate_separator := if separator == '.' { '->' } else { '.' }
				alternate_marker := '${alternate_separator}${tokens[i].lit}('
				if rendered.contains(alternate_marker) {
					separator = alternate_separator
					method_marker = alternate_marker
					receiver_source = rendered.all_before_last(method_marker)
				}
			}
		}
		mut needle := '${receiver_source}${separator}${tokens[i].lit}('
		if !rendered.contains(needle) {
			raw_receiver := g.render_raw_expression_tokens(receiver_tokens) or { '' }
			raw_needle := '${raw_receiver}${separator}${tokens[i].lit}('
			if raw_receiver != '' && rendered.contains(raw_needle) {
				needle = raw_needle
			}
		}
		expected_receiver := signature.parameter_types[0]
		receiver_argument := if expected_receiver.ends_with('*') && !receiver_type.ends_with('*') {
			'&(${receiver_source})'
		} else if !expected_receiver.ends_with('*') && receiver_type.ends_with('*') {
			'*(${receiver_source})'
		} else {
			receiver_source
		}
		call_end := fastc_matching_rpar(tokens, i + 1) or { continue }
		has_arguments := call_end > i + 2
		method_receiver_key := method_key.all_before_last('.')
		method_c_name := fastc_method_c_name_for_key(method_receiver_key, tokens[i].lit)
		mut direct_arguments := []string{}
		if has_arguments {
			call_args := fastc_call_arguments(tokens, i + 1, call_end) or { continue }
			for argument_index, argument in call_args {
				expected_index := argument_index + 1
				expected_type := if expected_index < signature.parameter_types.len {
					signature.parameter_types[expected_index]
				} else {
					''
				}
				argument_source := g.render_call_argument_expression(argument, expected_type) or {
					continue
				}
				argument_type := g.infer_expression_type(argument) or { '' }
				if expected_type == 'voidptr' && !fastc_is_pointer_type(argument_type) {
					direct_arguments << '&(${argument_source})'
				} else {
					direct_arguments << argument_source
				}
			}
		}
		replacement := '${method_c_name}(${receiver_argument}${if has_arguments {
			','
		} else {
			''
		}}'
		mut call_needle := needle
		mut call_replacement := replacement
		if receiver_start == 0 && call_end == tokens.len - 1 {
			result_type := g.specialized_method_return_type(receiver_type, method_key, signature)
			is_pointer_result_method := method_key.starts_with('array.')
				&& tokens[i].lit in ['first', 'last', 'pop', 'pop_left']
			if !is_pointer_result_method && !has_arguments && rendered.contains(needle) {
				return FastcRenderedExpression{
					source: rendered.replace(needle, replacement)
					typ:    result_type
				}
			}
			argument_suffix := if direct_arguments.len > 0 {
				',' + direct_arguments.join(',')
			} else {
				''
			}
			mut direct_call := '${method_c_name}(${receiver_argument}${argument_suffix})'
			if is_pointer_result_method {
				element_type := g.array_element_type(receiver_type) or { continue }
				direct_call = '(*(((${element_type} *)${direct_call})))'
			}
			return FastcRenderedExpression{
				source: direct_call
				typ:    result_type
			}
		}
		if method_key.starts_with('array.') && !has_arguments
			&& tokens[i].lit in ['first', 'last', 'pop', 'pop_left'] {
			element_type := g.array_element_type(receiver_type) or { continue }
			call_needle = '${needle})'
			call_replacement = '(*(((${element_type} *)${replacement}))))'
		}
		if rendered.contains(call_needle) {
			rendered = rendered.replace(call_needle, call_replacement)
			changed = true
		}
	}
	if !changed {
		return none
	}
	if concatenation := g.render_composed_string_concatenation(tokens) {
		return concatenation
	}
	inferred_type := g.infer_expression_type(tokens) or { '' }
	return FastcRenderedExpression{
		source: rendered
		typ:    inferred_type
	}
}

fn (g &Parser) render_composed_string_concatenation(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut depth := 0
	mut operand_start := 0
	mut string_operands := []bool{}
	mut plus_count := 0
	for i, item in tokens {
		match item.tok {
			.lpar, .lsbr, .lcbr {
				depth++
			}
			.rpar, .rsbr, .rcbr {
				depth--
			}
			.plus {
				if depth == 0 {
					operand_type := g.infer_expression_type(tokens[operand_start..i]) or { '' }
					string_operands << operand_type == 'string'
					operand_start = i + 1
					plus_count++
				}
			}
			else {}
		}
	}
	if plus_count == 0 {
		return none
	}
	last_operand_type := g.infer_expression_type(tokens[operand_start..]) or { '' }
	string_operands << last_operand_type == 'string'
	mut has_string_operand := false
	for is_string in string_operands {
		if is_string {
			has_string_operand = true
			break
		}
	}
	if !fastc_all_true(string_operands) && !has_string_operand {
		return none
	}
	mut parts := []string{}
	depth = 0
	operand_start = 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if item.tok == .plus && depth == 0 {
			part := g.render_comparison_operand(tokens[operand_start..i], 'string') or {
				return none
			}
			parts << part
			operand_start = i + 1
		}
	}
	last_part := g.render_comparison_operand(tokens[operand_start..], 'string') or { return none }
	parts << last_part
	mut combined := parts[0]
	for part in parts[1..] {
		combined = 'builtin__string_plus(${combined},${part})'
	}
	return FastcRenderedExpression{
		source: combined
		typ:    'string'
	}
}

fn (g &Parser) render_method_receiver_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	receiver_type := g.infer_expression_type(tokens) or { return none }
	if source := g.render_map_expression(tokens, '') {
		return source
	}
	if array_access := g.render_array_access_expression(tokens) {
		return array_access
	}
	if source := g.render_member_receiver(tokens) {
		return FastcRenderedExpression{
			source: source
			typ:    receiver_type
		}
	}
	if raw := g.render_raw_expression_tokens(tokens) {
		if source := g.render_method_call_expression(tokens, raw) {
			return source
		}
	}
	if source := g.render_membership_candidate(tokens, '') {
		return FastcRenderedExpression{
			source: source
			typ:    receiver_type
		}
	}
	return none
}

fn (g &Parser) render_member_receiver(tokens []FastcExpressionToken) ?string {
	if tokens.len == 0 || tokens[0].tok != .name {
		return none
	}
	mut source := tokens[0].lit
	mut current_type := g.infer_expression_type(tokens[..1]) or { return none }
	mut i := 1
	for i < tokens.len {
		if i + 1 >= tokens.len || tokens[i].tok != .dot || tokens[i + 1].tok != .name {
			return none
		}
		separator := if current_type.ends_with('*') { '->' } else { '.' }
		source += separator + tokens[i + 1].lit
		current_type = g.struct_member_type(current_type, tokens[i + 1].lit)
		if current_type == '' {
			return none
		}
		i += 2
	}
	return source
}

fn fastc_split_top_level_c_plus(source string) []string {
	mut parts := []string{}
	mut start := 0
	mut parens := 0
	mut brackets := 0
	mut braces := 0
	mut quote := u8(0)
	mut escaped := false
	for i, character in source {
		if quote != 0 {
			if escaped {
				escaped = false
			} else if character == `\\` {
				escaped = true
			} else if character == quote {
				quote = 0
			}
			continue
		}
		if character in [`'`, `"`] {
			quote = character
			continue
		}
		match character {
			`(` {
				parens++
			}
			`)` {
				parens--
			}
			`[` {
				brackets++
			}
			`]` {
				brackets--
			}
			`{` {
				braces++
			}
			`}` {
				braces--
			}
			`+` {
				if parens == 0 && brackets == 0 && braces == 0 {
					parts << source[start..i]
					start = i + 1
				}
			}
			else {}
		}
	}
	if parts.len == 0 {
		return [source]
	}
	parts << source[start..]
	return parts
}

fn (g &Parser) render_array_access_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 4 || tokens[0].tok != .name || tokens.last().tok != .rsbr {
		return none
	}
	mut open := -1
	mut depth := 0
	for i := tokens.len - 1; i >= 0; i-- {
		if tokens[i].tok == .rsbr {
			depth++
		} else if tokens[i].tok == .lsbr {
			depth--
			if depth == 0 {
				open = i
				break
			}
		}
	}
	if open <= 0 {
		return none
	}
	close := fastc_matching_delimiter(tokens, open, .lsbr, .rsbr) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	base_tokens := tokens[..open]
	base_type := g.infer_expression_type(base_tokens) or { return none }
	base_source := if base_tokens.len == 1 {
		g.resolved_root_expression_name(tokens[0].lit)
	} else {
		g.render_raw_expression_tokens(base_tokens) or { return none }
	}
	mut range_index := -1
	for i in open + 1 .. close {
		if tokens[i].tok == .dotdot {
			range_index = i
			break
		}
	}
	if range_index >= 0 {
		start := if range_index == open + 1 {
			'0'
		} else {
			g.render_membership_candidate(tokens[open + 1..range_index], 'int') or { return none }
		}
		access := if base_type.ends_with('*') { '->' } else { '.' }
		end := if range_index + 1 == close {
			'${base_source}${access}len'
		} else {
			g.render_membership_candidate(tokens[range_index + 1..close], 'int') or { return none }
		}
		if base_type == 'string' {
			return FastcRenderedExpression{
				source: 'builtin__string_substr(${if base_type.ends_with('*') { '*' } else { '' }}(${base_source}), ${start}, ${end})'
				typ:    'string'
			}
		}
		array_value := if base_type.ends_with('*') { '*(${base_source})' } else { base_source }
		return FastcRenderedExpression{
			source: '({ array __v_slice = (${array_value}); int __v_start = (${start}); int __v_end = (${end}); __v_slice.data = (byteptr)__v_slice.data + __v_start * __v_slice.element_size; __v_slice.offset += __v_start * __v_slice.element_size; __v_slice.len = __v_end - __v_start; __v_slice.cap = __v_slice.len; __v_slice.flags |= ArrayFlags__is_slice; __v_slice; })'
			typ:    base_type.trim_right('*')
		}
	}
	is_array_pointer := base_type.ends_with('*') && g.array_element_type(base_type) != none
	element_type := if is_array_pointer {
		g.array_element_type(base_type) or { return none }
	} else if base_type.ends_with('*') {
		base_type.trim_right('*')
	} else if base_type == 'string' {
		'u8'
	} else {
		g.array_element_type(base_type) or { return none }
	}
	index_source := g.render_membership_candidate(tokens[open + 1..close], 'int') or { return none }
	if base_type == 'string' {
		access := if base_type.ends_with('*') { '->' } else { '.' }
		return FastcRenderedExpression{
			source: '(${base_source}${access}str[${index_source}])'
			typ:    element_type
		}
	}
	is_raw_fixed_array := base_type.starts_with('FixedArray_') && (base_tokens.len > 1
		|| (base_tokens.len == 1
		&& fastc_global_key(g.module_name, base_tokens[0].lit) in g.globals))
	if is_raw_fixed_array {
		return FastcRenderedExpression{
			source: '((${base_source})[${index_source}])'
			typ:    element_type
		}
	}
	if base_type.ends_with('*') && !is_array_pointer {
		return FastcRenderedExpression{
			source: '((${base_source})[${index_source}])'
			typ:    element_type
		}
	}
	access := if base_type.ends_with('*') { '->' } else { '.' }
	return FastcRenderedExpression{
		source: '(((${element_type} *)${base_source}${access}data)[${index_source}])'
		typ:    element_type
	}
}

fn (g &Parser) render_nested_array_access_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 3 {
		return none
	}
	mut rendered := rendered_expression
	mut changed := false
	for i := tokens.len - 2; i >= 0; i-- {
		if tokens[i].tok != .name || tokens[i + 1].tok != .lsbr {
			continue
		}
		// A name after `.` is a field, not a new root expression. Treating it as
		// a local can replace the field suffix inside its owning expression (for
		// example `str.str[0]`) and produce invalid C.
		if i > 0 && tokens[i - 1].tok == .dot {
			continue
		}
		close := fastc_matching_delimiter(tokens, i + 1, .lsbr, .rsbr) or { continue }
		if close <= i + 1 || fastc_expression_tokens_contain(tokens[i + 2..close], .dotdot) {
			continue
		}
		base_type := g.infer_expression_type(tokens[i..i + 1]) or { continue }
		is_string := base_type == 'string'
		is_array_pointer := base_type.ends_with('*') && g.array_element_type(base_type) != none
		element_type := if is_string {
			'u8'
		} else if is_array_pointer {
			g.array_element_type(base_type) or { continue }
		} else if base_type.ends_with('*') {
			base_type.trim_right('*')
		} else {
			g.array_element_type(base_type) or { continue }
		}
		index_source := g.render_membership_candidate(tokens[i + 2..close], 'int') or { continue }
		base_source := g.resolved_root_expression_name(tokens[i].lit)
		needle := '${base_source}[${index_source}]'
		access := if base_type.ends_with('*') { '->' } else { '.' }
		is_raw_fixed_array := base_type.starts_with('FixedArray_')
			&& fastc_global_key(g.module_name, tokens[i].lit) in g.globals
		replacement := if is_raw_fixed_array {
			'(${base_source}[${index_source}])'
		} else if is_string {
			'(${base_source}${access}str[${index_source}])'
		} else if base_type.ends_with('*') && !is_array_pointer {
			'((${base_source})[${index_source}])'
		} else {
			'(((${element_type} *)${base_source}${access}data)[${index_source}])'
		}
		if rendered.contains(needle) {
			rendered = rendered.replace(needle, replacement)
			changed = true
		}
	}
	if !changed {
		return none
	}
	inferred_type := g.infer_expression_type(tokens) or { '' }
	return FastcRenderedExpression{
		source: rendered
		typ:    inferred_type
	}
}

fn (g &Parser) resolved_root_expression_name(name string) string {
	if global_name := g.globals[fastc_global_key(g.module_name, name)] {
		return global_name
	}
	if constant_name := g.constants[fastc_constant_key(g.module_name, name)] {
		return constant_name
	}
	if constant_name := g.constants[name] {
		return constant_name
	}
	return name
}

fn (g &Parser) render_membership_candidate(tokens []FastcExpressionToken, expected_type string) ?string {
	if tokens.len == 2 && tokens[0].tok == .dot && tokens[1].tok == .name
		&& g.declared_kinds[g.semantic_type_key(expected_type)] == .enum_ {
		return '${expected_type.trim_right('*')}__${tokens[1].lit}'
	}
	if array_access := g.render_array_access_expression(tokens) {
		return array_access.source
	}
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	if map_expression := g.render_map_expression(tokens, raw) {
		return map_expression.source
	}
	if method_expression := g.render_method_call_expression(tokens, raw) {
		return method_expression.source
	}
	if call_expression := g.render_missing_call_arguments(tokens, raw) {
		return call_expression.source
	}
	if pointer_members := g.render_pointer_member_access_expression(tokens, raw) {
		return pointer_members.source
	}
	if member_source := g.render_member_receiver(tokens) {
		return member_source
	}
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .lpar
		&& tokens.last().tok == .rpar {
		if cast_type := fastc_primitive_c_type(tokens[0].lit) {
			close := fastc_matching_rpar(tokens, 1) or { return none }
			if close == tokens.len - 1 {
				inner := g.render_membership_candidate(tokens[2..close], '') or { return none }
				return '((${cast_type})(${inner}))'
			}
		}
	}
	return raw
}

fn (g &Parser) render_raw_expression_tokens(tokens []FastcExpressionToken) ?string {
	mut result := strings.new_builder(32)
	mut cast_closes := map[int]bool{}
	mut cast_opens := map[int]bool{}
	for i, item in tokens {
		mut piece := item.lit
		module_separator := item.tok == .dot && i > 0 && tokens[i - 1].tok == .name
			&& (tokens[i - 1].lit in g.imports || tokens[i - 1].lit == 'C'
			|| (tokens[i - 1].lit !in g.locals && g.is_enum_type_name(tokens[i - 1].lit)))
		previous_module_separator := i > 1 && tokens[i - 1].tok == .dot
			&& tokens[i - 2].tok == .name && (tokens[i - 2].lit in g.imports
			|| tokens[i - 2].lit == 'C' || (tokens[i - 2].lit !in g.locals
			&& g.is_enum_type_name(tokens[i - 2].lit)))
		is_direct_pointer_cast := item.tok in [.amp, .and]
			&& fastc_token_is_prefix_operator(tokens, i) && i + 2 < tokens.len
			&& tokens[i + 1].tok == .name && tokens[i + 2].tok == .lpar
			&& (fastc_primitive_c_type(tokens[i + 1].lit) != none
			|| fastc_resolve_declared_type_key(g.module_name, tokens[i + 1].lit, g.declared_types) != none)
		is_c_pointer_cast := item.tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i)
			&& i + 4 < tokens.len && tokens[i + 1].tok == .name && tokens[i + 1].lit == 'C'
			&& tokens[i + 2].tok == .dot && tokens[i + 3].tok == .name && tokens[i + 4].tok == .lpar
		if is_direct_pointer_cast || is_c_pointer_cast {
			piece = ''
		} else if item.tok == .name && i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
			mut cast_type := fastc_primitive_c_type(item.lit) or { '' }
			is_c_cast := i >= 2 && tokens[i - 2].tok == .name && tokens[i - 2].lit == 'C'
				&& tokens[i - 1].tok == .dot && item.lit.len > 0 && item.lit[0].is_capital()
				&& 'C.${item.lit}' !in g.functions
			if is_c_cast {
				cast_type = item.lit
			}
			if cast_type == '' {
				if type_key := fastc_resolve_declared_type_key(g.module_name, item.lit,
					g.declared_types)
				{
					cast_type = fastc_c_declared_type_name(type_key)
				}
			}
			if cast_type != '' {
				pointer_token := if i > 0 && tokens[i - 1].tok in [.amp, .and]
					&& fastc_token_is_prefix_operator(tokens, i - 1) {
					tokens[i - 1].tok
				} else if is_c_cast && i >= 3 && tokens[i - 3].tok in [.amp, .and]
					&& fastc_token_is_prefix_operator(tokens, i - 3) {
					tokens[i - 3].tok
				} else {
					token.Token.unknown
				}
				pointer_suffix := '*'.repeat(if pointer_token == .and {
					2
				} else if pointer_token == .amp {
					1
				} else {
					0
				})
				piece = '((${cast_type}${pointer_suffix})('
				close := fastc_matching_rpar(tokens, i + 1) or { return none }
				cast_opens[i + 1] = true
				cast_closes[close] = true
			}
		} else if item.tok == .lpar && i in cast_opens {
			piece = ''
		} else if item.tok == .rpar && i in cast_closes {
			piece = '))'
		} else if item.tok == .number {
			piece = if g.selfhost {
				fastc_c_selfhost_number(item.lit)
			} else {
				fastc_c_number(item.lit) or { return none }
			}
		} else if item.tok == .string {
			if item.source != '' {
				piece = item.source
			} else {
				literal := fastc_c_string(item.lit) or { return none }
				piece = '_S(${literal})'
			}
		} else if item.tok == .char {
			piece = if item.lit.starts_with('c:') {
				fastc_c_string("'" + item.lit['c:'.len..] + "'") or { return none }
			} else {
				fastc_c_rune(item.lit) or { return none }
			}
		} else if item.tok == .key_true {
			piece = '((bool)true)'
		} else if item.tok == .key_false {
			piece = '((bool)false)'
		} else if item.tok == .key_nil {
			piece = 'NULL'
		} else if item.tok == .key_none {
			piece = '(Option){.state=2}'
		} else if item.tok == .name {
			previous := if i == 0 { token.Token.unknown } else { tokens[i - 1].tok }
			piece = g.resolved_expression_name(item.lit, previous)
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name
			&& tokens[i - 1].lit in g.imports {
			piece = '__'
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name
			&& tokens[i - 1].lit == 'C' {
			piece = ''
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name
			&& g.local_is_pointer(tokens[i - 1].lit) {
			piece = '->'
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name
			&& tokens[i - 1].lit !in g.locals && g.is_enum_type_name(tokens[i - 1].lit) {
			piece = '__'
		} else if piece == '' {
			piece = item.tok.str()
		}
		if result.len > 0 && fastc_needs_space(result.last(), piece) && !module_separator
			&& !previous_module_separator {
			result.write_u8(` `)
		}
		result.write_string(piece)
	}
	return result.str()
}

fn fastc_token_is_prefix_operator(tokens []FastcExpressionToken, index int) bool {
	if index == 0 {
		return true
	}
	return tokens[index - 1].tok !in [.name, .number, .string, .char, .key_true, .key_false, .key_nil,
		.key_none, .rpar, .rsbr, .rcbr, .inc, .dec]
}

fn (g &Parser) array_initializer_type(tokens []FastcExpressionToken) ?string {
	if tokens.len < 3 {
		return none
	}
	mut index := 0
	mut dimensions := 0
	mut fixed_length := ''
	if tokens.len >= 4 && tokens[0].tok == .lsbr && tokens[1].tok in [.name, .number]
		&& tokens[2].tok == .rsbr {
		fixed_length = if tokens[1].tok == .name {
			constant_key := fastc_constant_key(g.module_name, tokens[1].lit)
			g.constants[constant_key] or { fastc_c_constant_name(g.module_name, tokens[1].lit) }
		} else {
			fastc_c_selfhost_number(tokens[1].lit)
		}
		dimensions = 1
		index = 3
	}
	for index + 1 < tokens.len && tokens[index].tok == .lsbr && tokens[index + 1].tok == .rsbr {
		dimensions++
		index += 2
	}
	mut pointers := 0
	for index < tokens.len && tokens[index].tok in [.amp, .mul] {
		pointers++
		index++
	}
	if dimensions == 0 || index >= tokens.len || tokens[index].tok != .name {
		return none
	}
	mut element_type := fastc_primitive_c_type(tokens[index].lit) or { '' }
	if element_type == '' {
		if type_key := fastc_resolve_declared_type_key(g.module_name, tokens[index].lit,
			g.declared_types)
		{
			element_type = fastc_c_declared_type_name(type_key)
		}
	}
	index++
	if element_type == '' || index != tokens.len {
		return none
	}
	element_type += '*'.repeat(pointers)
	if fixed_length != '' {
		return fastc_fixed_array_type(fixed_length, element_type)
	}
	mut result := element_type
	for _ in 0 .. dimensions {
		result = fastc_array_c_type(result)
	}
	return result
}

fn (g &Parser) map_initializer_type(tokens []FastcExpressionToken) ?string {
	map_type := g.type_from_expression_tokens(tokens) or { return none }
	return if map_type.starts_with('Map_') { map_type } else { none }
}

fn (g &Parser) type_from_expression_tokens(tokens []FastcExpressionToken) ?string {
	if tokens.len == 0 {
		return none
	}
	mut pointers := 0
	mut start := 0
	for start < tokens.len && tokens[start].tok in [.amp, .mul] {
		pointers++
		start++
	}
	if start >= tokens.len {
		return none
	}
	remaining := tokens[start..]
	if remaining.len >= 3 && remaining[0].tok == .lsbr && remaining[1].tok == .rsbr {
		element_type := g.type_from_expression_tokens(remaining[2..]) or { return none }
		return fastc_array_c_type(element_type) + '*'.repeat(pointers)
	}
	if remaining.len >= 5 && remaining[0].tok == .name && remaining[0].lit == 'map'
		&& remaining[1].tok == .lsbr {
		close := fastc_matching_delimiter(remaining, 1, .lsbr, .rsbr) or { return none }
		if close <= 2 || close + 1 >= remaining.len {
			return none
		}
		key_type := g.type_from_expression_tokens(remaining[2..close]) or { return none }
		value_type := g.type_from_expression_tokens(remaining[close + 1..]) or { return none }
		return fastc_map_c_type(key_type, value_type) + '*'.repeat(pointers)
	}
	if remaining.len == 1 && remaining[0].tok == .name {
		mut base := fastc_primitive_c_type(remaining[0].lit) or { '' }
		if base == '' {
			type_key := fastc_resolve_declared_type_key(g.module_name, remaining[0].lit,
				g.declared_types) or { return none }
			base = fastc_c_declared_type_name(type_key)
		}
		return base + '*'.repeat(pointers)
	}
	if remaining.len == 3 && remaining[0].tok == .name && remaining[1].tok == .dot
		&& remaining[2].tok == .name {
		if remaining[0].lit == 'C' {
			raw_type := remaining[2].lit
			if 'C.${raw_type}' in g.functions {
				return none
			}
			if '#Cstruct#${raw_type}' in g.declared_types {
				return 'struct ${raw_type}' + '*'.repeat(pointers)
			}
			if raw_type.len == 0 || !raw_type[0].is_capital() {
				return none
			}
			return raw_type + '*'.repeat(pointers)
		}
		module_name := g.imports[remaining[0].lit] or { return none }
		type_key := fastc_type_key(module_name, remaining[2].lit)
		if type_key !in g.declared_types {
			return none
		}
		return fastc_c_declared_type_name(type_key) + '*'.repeat(pointers)
	}
	return none
}

fn fastc_array_initializer_c_type(array_type string) string {
	length := fastc_fixed_array_length(array_type) or { return array_type }
	element_type := fastc_fixed_array_element_type(array_type) or { return array_type }
	return 'FixedArray_${fastc_composite_type_part(length)}_${fastc_composite_type_part(element_type)}'
}

fn fastc_generate_fixed_array_declarations(fixed_array_types map[string]string) string {
	mut names := fixed_array_types.keys()
	names.sort()
	mut out := strings.new_builder(256)
	for name in names {
		array_type := fixed_array_types[name]
		length := fastc_fixed_array_length(array_type) or { continue }
		element_type := fastc_fixed_array_element_type(array_type) or { continue }
		out.writeln('typedef struct { ${element_type} data[${length}]; } ${name};')
	}
	if out.len > 0 {
		out.writeln('')
	}
	return out.str()
}

fn fastc_expression_list_items(tokens []FastcExpressionToken, start int, end int) ![][]FastcExpressionToken {
	if start == end {
		return [][]FastcExpressionToken{}
	}
	mut result := [][]FastcExpressionToken{}
	mut item_start := start
	mut parens := 0
	mut brackets := 0
	mut braces := 0
	for i in start .. end {
		match tokens[i].tok {
			.lpar {
				parens++
			}
			.rpar {
				parens--
			}
			.lsbr {
				brackets++
			}
			.rsbr {
				brackets--
			}
			.lcbr {
				braces++
			}
			.rcbr {
				braces--
			}
			.comma {
				if parens == 0 && brackets == 0 && braces == 0 {
					if item_start == i {
						return error('empty expression-list item')
					}
					result << tokens[item_start..i]
					item_start = i + 1
				}
			}
			else {}
		}
	}
	if item_start == end {
		return result
	}
	result << tokens[item_start..end]
	return result
}

fn (g &Parser) expression_token(previous token.Token, previous_lit string) !string {
	return match g.tok {
		.name {
			g.expression_name(previous)!
		}
		.number {
			if g.selfhost {
				fastc_c_selfhost_number(g.lit)
			} else {
				fastc_c_number(g.lit)!
			}
		}
		.string {
			literal := fastc_c_string(g.lit) or {
				return g.unsupported('string literal `${g.lit}`: ${err.msg()}')
			}
			if g.selfhost {
				'_S(${literal})'
			} else {
				literal
			}
		}
		.char {
			if g.selfhost && g.lit.starts_with('c:') {
				fastc_c_string("'" + g.lit['c:'.len..] + "'") or {
					return g.unsupported('C string literal `${g.lit}`: ${err.msg()}')
				}
			} else if g.selfhost {
				fastc_c_rune(g.lit) or {
					return g.unsupported('rune literal `${g.lit}`: ${err.msg()}')
				}
			} else {
				g.unsupported('rune or C character literals')
			}
		}
		// stdbool's true/false macros have C type int. Cast them so _Generic
		// dispatch preserves V's bool type when no operator requires promotion.
		.key_true {
			'((bool)true)'
		}
		.key_false {
			'((bool)false)'
		}
		.key_nil {
			g.nil_expression()!
		}
		.key_none {
			if g.selfhost {
				'(Option){.state=2}'
			} else {
				g.unsupported('none expressions')
			}
		}
		.key_likely, .key_unlikely {
			''
		}
		.semicolon {
			';'
		}
		.dot {
			if previous == .name && previous_lit == 'C' {
				''
			} else if previous == .name && previous_lit in g.imports {
				'__'
			} else if g.selfhost && previous == .name && g.local_is_pointer(previous_lit) {
				'->'
			} else if g.selfhost && previous == .name && previous_lit !in g.locals
				&& g.is_enum_type_name(previous_lit) {
				'__'
			} else {
				'.'
			}
		}
		else {
			g.tok.str()
		}
	}
}

fn (g &Parser) nil_expression() !string {
	if g.unsafe_depth == 0 {
		return g.unsupported('`nil` outside an `unsafe` block')
	}
	return 'NULL'
}

fn (g &Parser) expression_name(previous token.Token) !string {
	g.validate_expression_name(g.lit, previous)!
	return g.resolved_expression_name(g.lit, previous)
}

fn (g &Parser) resolved_expression_name(name string, previous token.Token) string {
	if previous != .dot && name == 'C' {
		return ''
	}
	if previous != .dot && name !in g.locals {
		if imported_module := g.imports[name] {
			return imported_module.replace('.', '__')
		}
		function_key := g.unqualified_function_key(name)
		if function_key in g.functions {
			return fastc_c_function_name_for_key(function_key)
		}
		if type_key := fastc_resolve_declared_type_key(g.module_name, name, g.declared_types) {
			return fastc_c_declared_type_name(type_key)
		}
		if primitive := fastc_primitive_c_type(name) {
			return primitive
		}
		constant_key := fastc_constant_key(g.module_name, name)
		if c_name := g.constants[constant_key] {
			return c_name
		}
		if c_name := g.constants[fastc_constant_key('builtin', name)] {
			return c_name
		}
		global_key := fastc_global_key(g.module_name, name)
		if c_name := g.globals[global_key] {
			return c_name
		}
		if c_name := g.globals[fastc_global_key('builtin', name)] {
			return c_name
		}
	}
	return name
}

fn (g &Parser) validate_expression_name(name string, previous token.Token) ! {
	if g.selfhost && previous in [.lcbr, .comma, .semicolon] {
		return
	}
	if !g.selfhost && fastc_has_narrow_integer_type(name) {
		// C promotes narrow operands before arithmetic. Reject narrow casts in
		// expressions until FastC can explicitly restore V's wrapping result type.
		return g.unsupported('narrow integer cast expressions')
	}
	if !g.selfhost && name == 'charptr' {
		return g.unsupported('charptr expressions')
	}
	if !g.selfhost && name == 'rune' {
		return g.unsupported('rune expressions')
	}
	function_key := g.unqualified_function_key(name)
	constant_key := fastc_constant_key(g.module_name, name)
	global_key := fastc_global_key(g.module_name, name)
	if previous == .dot || (g.selfhost && name == 'C') || name in g.locals
		|| name in g.imports || function_key in g.functions
		|| constant_key in g.constants || global_key in g.globals
		|| fastc_resolve_declared_type_key(g.module_name, name, g.declared_types) != none
		|| name in ['print', 'println', 'bool', 'byte', 'char', 'f32', 'f64', 'i8', 'i16', 'i32', 'i64', 'int', 'isize', 'rune', 'string', 'u8', 'u16', 'u32', 'u64', 'uint', 'usize', 'voidptr', 'byteptr', 'charptr'] {
		return
	}
	if g.selfhost {
		// Self-host sources have already passed the bootstrap compiler. Preserve
		// streaming progress for names introduced by syntax whose scope is not yet
		// represented in the direct parser; C still diagnoses a missing declaration.
		return
	}
	return g.unsupported('unresolved name `${name}` (locals: ${g.locals.keys().join(', ')})')
}

fn (g &Parser) function_key_for_call(tokens []FastcExpressionToken, name_index int) string {
	if static_key := g.static_function_key_for_call(tokens, name_index) {
		return static_key
	}
	if name_index >= 2 && tokens[name_index - 1].tok == .dot && tokens[name_index - 2].tok == .name {
		if tokens[name_index - 2].lit == 'C' {
			return 'C.${tokens[name_index].lit}'
		}
		if imported_module := g.imports[tokens[name_index - 2].lit] {
			return fastc_function_key(imported_module, tokens[name_index].lit)
		}
	}
	return g.unqualified_function_key(tokens[name_index].lit)
}

fn (g &Parser) static_function_key_for_call(tokens []FastcExpressionToken, name_index int) ?string {
	if name_index < 2 || tokens[name_index - 1].tok != .dot || tokens[name_index - 2].tok != .name {
		return none
	}
	owner_name := tokens[name_index - 2].lit
	mut type_key := ''
	if name_index >= 4 && tokens[name_index - 3].tok == .dot && tokens[name_index - 4].tok == .name {
		module_name := g.imports[tokens[name_index - 4].lit] or { return none }
		type_key = fastc_type_key(module_name, owner_name)
	} else {
		type_key = fastc_resolve_declared_type_key(g.module_name, owner_name, g.declared_types) or {
			return none
		}
	}
	function_key := '${type_key}.${tokens[name_index].lit}'
	return if function_key in g.functions { function_key } else { none }
}

fn (g &Parser) local_is_pointer(name string) bool {
	local := g.locals[name] or { return false }
	return local.typ.ends_with('*')
}

fn (g &Parser) is_enum_type_name(name string) bool {
	type_key := fastc_resolve_declared_type_key(g.module_name, name, g.declared_types) or {
		return false
	}
	return g.declared_kinds[type_key] == .enum_
}

fn (g &Parser) validate_expression_calls(tokens []FastcExpressionToken) ! {
	mut i := 0
	for i + 1 < tokens.len {
		if tokens[i].tok != .name || tokens[i + 1].tok != .lpar {
			i++
			continue
		}
		call_end := fastc_matching_rpar(tokens, i + 1) or {
			return g.unsupported('unbalanced function call `${tokens[i].lit}`')
		}
		call_args := fastc_call_arguments(tokens, i + 1, call_end) or {
			return g.unsupported('function call `${tokens[i].lit}` arguments')
		}
		for argument in call_args {
			g.validate_expression_calls(argument)!
		}
		name := tokens[i].lit
		mut function_key := g.function_key_for_call(tokens, i)
		is_static_call := g.static_function_key_for_call(tokens, i) != none
		mut is_method_call := false
		mut has_method_receiver := false
		mut receiver_type := ''
		if !is_static_call && i >= 2 && tokens[i - 1].tok == .dot && !(tokens[i - 2].tok == .name
			&& (tokens[i - 2].lit in g.imports || tokens[i - 2].lit == 'C')) {
			receiver_start := fastc_method_receiver_start(tokens, i - 1)
			receiver_type = g.infer_expression_type(tokens[receiver_start..i - 1])!
			if receiver_type != '' {
				has_method_receiver = true
				function_key = g.method_function_key(receiver_type, name)
				is_method_call = function_key in g.functions
			}
		}
		if signature := g.functions[function_key] {
			argument_offset := if is_method_call { 1 } else { 0 }
			is_variadic := signature.is_variadic
			expected_arguments := signature.parameter_types.len - argument_offset - if is_variadic {
				1
			} else {
				0
			}
			if (!is_variadic && call_args.len != expected_arguments && !(g.selfhost
				&& call_args.len < expected_arguments))
				|| (is_variadic && call_args.len < expected_arguments) {
				return g.unsupported('function `${name}` call with ${call_args.len} arguments instead of ${expected_arguments}')
			}
			if !g.selfhost && is_method_call && signature.parameter_types.len > 0
				&& !fastc_call_types_are_compatible(receiver_type, signature.parameter_types[0]) {
				return g.unsupported('method `${name}` receiver of type `${receiver_type}`')
			}
			for argument_index, argument in call_args {
				if is_variadic && argument_index >= expected_arguments {
					continue
				}
				if g.selfhost {
					// The bootstrap compiler already checked this trusted source graph.
					// Signature metadata is still used to lower calls, but incomplete
					// streaming scope inference must not reinterpret valid arguments.
					continue
				}
				actual_type := g.infer_expression_type(argument)!
				expected_type := signature.parameter_types[argument_index + argument_offset]
				if actual_type.len == 0 {
					if g.selfhost {
						continue
					}
					return g.unsupported('unverifiable argument ${argument_index + 1} to function `${name}`')
				}
				zero_pointer := g.selfhost && fastc_is_pointer_type(expected_type)
					&& fastc_expression_is_zero(argument)
				if !zero_pointer && !fastc_call_types_are_compatible(actual_type, expected_type)
					&& !(g.selfhost && g.selfhost_types_are_compatible(actual_type, expected_type)) {
					return g.unsupported('argument ${argument_index + 1} of type `${actual_type}` to function `${name}` expecting `${expected_type}`')
				}
			}
		} else if has_method_receiver && name in ['has', 'set', 'clear'] && call_args.len == 1
			&& g.declared_kinds[g.semantic_type_key(receiver_type)] == .enum_ {
			i = call_end + 1
			continue
		} else if i == 0 && name in ['print', 'println'] {
			if call_args.len != 1 {
				return g.unsupported('function `${name}` call with ${call_args.len} arguments')
			}
			_ = g.infer_expression_type(call_args[0])!
		} else {
			if g.selfhost && i >= 2 && tokens[i - 2].tok == .name && tokens[i - 2].lit == 'C'
				&& tokens[i - 1].tok == .dot {
				i = call_end + 1
				continue
			}
			if i == 0 || tokens[i - 1].tok != .dot {
				if _ := fastc_resolve_declared_type_key(g.module_name, name, g.declared_types) {
					if call_args.len != 1 {
						return g.unsupported('cast `${name}` with ${call_args.len} arguments')
					}
					i = call_end + 1
					continue
				}
				if _ := fastc_primitive_c_type(name) {
					if call_args.len != 1 {
						return g.unsupported('cast `${name}` with ${call_args.len} arguments')
					}
					i = call_end + 1
					continue
				}
			}
			if g.selfhost && has_method_receiver && g.struct_member_type(receiver_type, name) != '' {
				i = call_end + 1
				continue
			}
			if g.selfhost && has_method_receiver {
				return g.unsupported('unresolved method call `${g.semantic_type_key(receiver_type)}.${name}` of `${receiver_type}`')
			}
			if g.selfhost {
				return g.unsupported('unresolved function call `${name}` tokens `${fastc_expression_tokens_debug(tokens)}`')
			}
			return g.unsupported('unresolved function call `${name}`')
		}
		i = call_end + 1
	}
}

fn fastc_expression_is_zero(tokens []FastcExpressionToken) bool {
	return tokens.len == 1 && tokens[0].tok == .number
		&& tokens[0].lit.replace('_', '').trim_left('0') == ''
}

fn (g &Parser) struct_member_type(receiver_type string, field_name string) string {
	mut layout_type := receiver_type.trim_right('*')
	if layout_type.starts_with('Array_') {
		layout_type = 'array'
	} else if layout_type.starts_with('Map_') {
		layout_type = 'map'
	}
	if layout_type !in g.struct_fields {
		return ''
	}
	fields := g.struct_fields[layout_type].clone()
	return fields[field_name] or { '' }
}

fn fastc_matching_rpar(tokens []FastcExpressionToken, open int) ?int {
	mut depth := 0
	for i in open .. tokens.len {
		match tokens[i].tok {
			.lpar {
				depth++
			}
			.rpar {
				depth--
				if depth == 0 {
					return i
				}
			}
			else {}
		}
	}
	return none
}

fn fastc_method_receiver_start(tokens []FastcExpressionToken, dot int) int {
	if dot <= 0 || dot > tokens.len {
		return 0
	}
	mut parens := 0
	mut brackets := 0
	mut start := dot - 1
	for start >= 0 {
		tok := tokens[start].tok
		if tok == .rpar {
			parens++
		} else if tok == .rsbr {
			brackets++
		} else if tok == .lpar {
			if parens == 0 && brackets == 0 {
				return start + 1
			}
			parens--
		} else if tok == .lsbr {
			if brackets == 0 && parens == 0 {
				return start + 1
			}
			brackets--
		} else if parens == 0 && brackets == 0 && tok in [.amp, .and, .mul] && start + 2 < dot
			&& tokens[start + 1].tok == .name && tokens[start + 2].tok == .lpar
			&& fastc_token_is_prefix_operator(tokens, start) {
			return start
		} else if parens == 0 && brackets == 0 && (tok.is_assignment()
			|| tok in [.comma, .semicolon, .colon, .ellipsis, .plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor, .eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .not, .bit_not, .lcbr]) {
			return start + 1
		}
		start--
	}
	return 0
}

fn fastc_call_arguments(tokens []FastcExpressionToken, open int, close int) ![][]FastcExpressionToken {
	if open + 1 == close {
		return [][]FastcExpressionToken{}
	}
	mut call_args := [][]FastcExpressionToken{}
	mut start := open + 1
	mut paren_depth := 0
	mut bracket_depth := 0
	mut brace_depth := 0
	for i in open + 1 .. close {
		match tokens[i].tok {
			.lpar {
				paren_depth++
			}
			.rpar {
				paren_depth--
			}
			.lsbr {
				bracket_depth++
			}
			.rsbr {
				bracket_depth--
			}
			.lcbr {
				brace_depth++
			}
			.rcbr {
				brace_depth--
			}
			.comma {
				if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
					if start == i {
						return error('empty fastc function argument')
					}
					call_args << tokens[start..i]
					start = i + 1
				}
			}
			else {}
		}
	}
	if start == close {
		return error('empty fastc function argument')
	}
	call_args << tokens[start..close]
	return call_args
}

fn (g &Parser) infer_expression_type(tokens []FastcExpressionToken) !string {
	if tokens.len == 0 {
		return ''
	}
	mut start := 0
	mut end := tokens.len
	for end - start >= 2 && tokens[start].tok == .lpar {
		wrapper_end := fastc_matching_rpar(tokens[start..end], 0) or { break }
		if wrapper_end != end - start - 1 {
			break
		}
		start++
		end--
	}
	if start >= end {
		return ''
	}
	if end - start >= 4 && tokens[start].tok == .key_sizeof && tokens[start + 1].tok == .lpar
		&& tokens[end - 1].tok == .rpar {
		return 'int'
	}
	if tokens[start].tok == .not {
		_ = g.infer_expression_type(tokens[start + 1..end])!
		return 'bool'
	}
	mut boolean_depth := 0
	for i in start .. end {
		if tokens[i].tok in [.lpar, .lsbr, .lcbr] {
			boolean_depth++
		} else if tokens[i].tok in [.rpar, .rsbr, .rcbr] {
			boolean_depth--
		} else if boolean_depth == 0
			&& tokens[i].tok in [.eq, .ne, .gt, .lt, .ge, .le, .and, .logical_or, .key_is, .not_is, .key_in, .not_in] {
			return 'bool'
		}
	}
	if end - start == 1 {
		item := tokens[start]
		if item.typ != '' {
			return item.typ
		}
		return match item.tok {
			.name {
				if local := g.locals[item.lit] {
					local.typ
				} else if constant_type := g.constant_types[fastc_constant_key(g.module_name,
					item.lit)]
				{
					constant_type
				} else if constant_type := g.constant_types[fastc_constant_key('builtin', item.lit)] {
					constant_type
				} else if fastc_constant_key(g.module_name, item.lit) in g.constants {
					'integer literal'
				} else if fastc_constant_key('builtin', item.lit) in g.constants {
					'integer literal'
				} else if global_type := g.global_types[fastc_global_key(g.module_name, item.lit)] {
					global_type
				} else if global_type := g.global_types[fastc_global_key('builtin', item.lit)] {
					global_type
				} else if g.selfhost {
					'int'
				} else {
					''
				}
			}
			.number {
				fastc_number_expression_type(item.lit)
			}
			.string {
				'string'
			}
			.char {
				if item.lit.starts_with('c:') {
					'charptr'
				} else {
					'rune'
				}
			}
			.key_true, .key_false {
				'bool'
			}
			.key_nil {
				'nil'
			}
			.key_none {
				'Option'
			}
			else {
				''
			}
		}
	}
	if end - start == 2 && tokens[start].tok == .dot && tokens[start + 1].typ != '' {
		return tokens[start + 1].typ
	}
	if end - start == 5 && tokens[start].tok == .name && tokens[start + 1].tok == .dot
		&& tokens[start + 2].tok == .name && tokens[start + 3].tok == .dot
		&& tokens[start + 4].tok == .name {
		if imported_module := g.imports[tokens[start].lit] {
			type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
			if g.declared_kinds[type_key] == .enum_ {
				return fastc_c_declared_type_name(type_key)
			}
		}
	}
	if end - start >= 5 && tokens[start].tok == .lsbr && tokens[start + 1].tok == .rsbr
		&& tokens[start + 2].tok == .name && tokens[start + 3].tok == .lpar
		&& tokens[end - 1].tok == .rpar {
		mut element_type := tokens[start + 2].lit
		if primitive := fastc_primitive_c_type(element_type) {
			element_type = primitive
		}
		return fastc_array_c_type(element_type)
	}
	if end - start == 3 && tokens[start].tok == .name && tokens[start + 1].tok == .dot
		&& tokens[start + 2].tok == .name {
		if imported_module := g.imports[tokens[start].lit] {
			type_key := fastc_type_key(imported_module, tokens[start + 2].lit)
			if type_key in g.declared_types {
				return fastc_c_declared_type_name(type_key)
			}
		}
		if type_key := fastc_resolve_declared_type_key(g.module_name, tokens[start].lit,
			g.declared_types)
		{
			if g.declared_kinds[type_key] == .enum_ {
				return fastc_c_declared_type_name(type_key)
			}
		}
		if imported_module := g.imports[tokens[start].lit] {
			if constant_type := g.constant_types[fastc_constant_key(imported_module, tokens[start +
				2].lit)]
			{
				return constant_type
			}
			if fastc_constant_key(imported_module, tokens[start + 2].lit) in g.constants {
				return 'integer literal'
			}
			if global_type := g.global_types[fastc_global_key(imported_module,
				tokens[start + 2].lit)]
			{
				return global_type
			}
		}
	}
	for init_open in start + 1 .. end {
		if tokens[init_open].tok == .lcbr {
			if array_type := g.array_initializer_type(tokens[start..init_open]) {
				return array_type
			}
			break
		}
	}
	array_end := if tokens[end - 1].tok == .not { end - 1 } else { end }
	if tokens[start].tok == .lsbr && tokens[array_end - 1].tok == .rsbr {
		items := fastc_expression_list_items(tokens, start + 1, array_end - 1)!
		if items.len == 0 {
			return ''
		}
		element_type := fastc_normalize_inferred_type(g.infer_expression_type(items[0])!)
		if element_type == '' {
			return ''
		}
		return fastc_array_c_type(element_type)
	}
	if start + 1 < end && tokens[start].tok == .name && tokens[start + 1].tok == .lcbr {
		if type_key := fastc_resolve_declared_type_key(g.module_name, tokens[start].lit,
			g.declared_types)
		{
			return fastc_c_declared_type_name(type_key)
		}
	}
	mut call_name_index := start
	mut call_open_index := start + 1
	if start + 3 < end && tokens[start].tok == .name && tokens[start + 1].tok == .dot
		&& tokens[start + 2].tok == .name
		&& (tokens[start].lit in g.imports || tokens[start].lit == 'C') {
		call_name_index = start + 2
		call_open_index = start + 3
	}
	if call_open_index < end && tokens[call_name_index].tok == .name
		&& tokens[call_open_index].tok == .lpar {
		if close := fastc_matching_rpar(tokens[start..end], call_open_index - start) {
			if close == end - start - 1 {
				name := tokens[call_name_index].lit
				function_key := g.function_key_for_call(tokens, call_name_index)
				if signature := g.functions[function_key] {
					return signature.return_type
				}
				if call_name_index == start {
					if primitive := fastc_primitive_c_type(name) {
						return primitive
					}
					if type_key := fastc_resolve_declared_type_key(g.module_name, name,
						g.declared_types)
					{
						return fastc_c_declared_type_name(type_key)
					}
				}
				if call_name_index == start + 2 && tokens[start].lit == 'C' && name.len > 0
					&& name[0].is_capital() {
					return name
				}
				return ''
			}
		}
	}
	for i in start + 2 .. end - 1 {
		if tokens[i].tok != .name || tokens[i - 1].tok != .dot || tokens[i + 1].tok != .lpar {
			continue
		}
		close := fastc_matching_rpar(tokens[start..end], i + 1 - start) or { continue }
		if close != end - start - 1 {
			continue
		}
		receiver_start := fastc_method_receiver_start(tokens, i - 1)
		receiver_type := g.infer_expression_type(tokens[receiver_start..i - 1])!
		if receiver_type == '' {
			continue
		}
		function_key := g.method_function_key(receiver_type, tokens[i].lit)
		if static_key := g.static_function_key_for_call(tokens, i) {
			if signature := g.functions[static_key] {
				return signature.return_type
			}
		}
		if signature := g.functions[function_key] {
			return g.specialized_method_return_type(receiver_type, function_key, signature)
		}
	}
	if tokens[start].tok in [.plus, .minus] {
		operand_type := g.infer_expression_type(tokens[start + 1..end])!
		if !fastc_is_numeric_expression_type(operand_type) {
			return g.unsupported('arithmetic `${tokens[start].tok.str()}` on non-numeric type `${operand_type}`')
		}
		if tokens[start].tok == .minus && operand_type == 'integer literal' {
			return 'negative integer literal'
		}
		return operand_type
	}
	if tokens[start].tok in [.amp, .and] {
		operand_type := g.infer_expression_type(tokens[start + 1..end])!
		mut pointer_count := 1
		if tokens[start].tok == .and {
			pointer_count = 2
		}
		return if operand_type == '' {
			'voidptr'
		} else {
			operand_type + '*'.repeat(pointer_count)
		}
	}
	if tokens[start].tok == .mul {
		operand_type := g.infer_expression_type(tokens[start + 1..end])!
		return operand_type.trim_right('*')
	}
	if tokens[start].tok == .bit_not {
		operand_type := g.infer_expression_type(tokens[start + 1..end])!
		if !fastc_is_integer_expression_type(operand_type) {
			return g.unsupported('bitwise negation of non-integer type `${operand_type}`')
		}
		return operand_type
	}
	if g.selfhost && tokens[end - 1].tok == .not {
		value_type := g.option_value_type_for_expression(tokens[start..end - 1])
		return if value_type == 'void' { '' } else { value_type }
	}
	if tokens[end - 1].tok in [.inc, .dec] {
		operand_type := g.infer_expression_type(tokens[start..end - 1])!
		if g.selfhost && operand_type == '' {
			return 'int'
		}
		if !fastc_is_numeric_expression_type(operand_type) {
			return g.unsupported('arithmetic `${tokens[end - 1].tok.str()}` on non-numeric type `${operand_type}`')
		}
		return operand_type
	}
	if tokens[end - 1].tok == .rsbr {
		mut bracket_depth := 0
		mut open_index := -1
		for i := end - 1; i >= start; i-- {
			if tokens[i].tok == .rsbr {
				bracket_depth++
			} else if tokens[i].tok == .lsbr {
				bracket_depth--
				if bracket_depth == 0 {
					open_index = i
					break
				}
			}
		}
		if open_index > start {
			base_type := g.infer_expression_type(tokens[start..open_index])!
			if fastc_expression_tokens_contain(tokens[open_index + 1..end - 1], .dotdot) {
				return base_type
			}
			if base_type.trim_right('*').starts_with('Map_') {
				_, value_type := fastc_map_key_value_types(base_type) or { return '' }
				return value_type
			}
			if base_type.ends_with('*') {
				return base_type.trim_right('*')
			}
			if base_type.trim_right('*') == 'string' {
				return 'u8'
			}
			if element_type := g.array_element_type(base_type) {
				return element_type
			}
		}
	}
	if member_type := g.infer_member_access_type(tokens[start..end]) {
		return member_type
	}
	mut depth := 0
	for i in start .. end {
		match tokens[i].tok {
			.lpar, .lsbr, .lcbr { depth++ }
			.rpar, .rsbr, .rcbr { depth-- }
			else {}
		}
		if depth != 0 {
			continue
		}
		if tokens[i].tok.is_assignment() {
			return g.infer_expression_type(tokens[start..i])!
		}
		if tokens[i].tok in [.left_shift, .right_shift, .right_shift_unsigned] && i > start {
			mut left_type := g.infer_expression_type(tokens[start..i])!
			mut right_type := g.infer_expression_type(tokens[i + 1..end])!
			if left_element := g.indexed_array_operand_type(tokens[start..i], left_type) {
				left_type = left_element
			}
			if right_element := g.indexed_array_operand_type(tokens[i + 1..end], right_type) {
				right_type = right_element
			}
			if g.selfhost && tokens[i].tok == .left_shift && g.array_element_type(left_type) != none {
				return left_type
			}
			if g.selfhost && left_type == '' && fastc_is_integer_expression_type(right_type) {
				return 'int'
			}
			if !fastc_is_integer_expression_type(left_type)
				|| !fastc_is_integer_expression_type(right_type) {
				return g.unsupported('shift operands of types `${left_type}` and `${right_type}`')
			}
			return left_type
		}
		if tokens[i].tok in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor] && i > start {
			mut left_type := g.infer_expression_type(tokens[start..i])!
			mut right_type := g.infer_expression_type(tokens[i + 1..end])!
			if left_element := g.indexed_array_operand_type(tokens[start..i], left_type) {
				left_type = left_element
			}
			if right_element := g.indexed_array_operand_type(tokens[i + 1..end], right_type) {
				right_type = right_element
			}
			if g.selfhost && tokens[i].tok == .plus && left_type == 'string'
				&& right_type == 'string' {
				return 'string'
			}
			if g.selfhost && tokens[i].tok == .plus && ((left_type == 'string' && right_type == '')
				|| (right_type == 'string' && left_type == '')) {
				return 'string'
			}
			if g.selfhost && tokens[i].tok in [.plus, .minus] && fastc_is_pointer_type(left_type)
				&& fastc_is_integer_expression_type(right_type) {
				return left_type
			}
			if g.selfhost && tokens[i].tok in [.amp, .pipe, .xor] && left_type == right_type
				&& g.declared_kinds[g.semantic_type_key(left_type)] == .enum_ {
				return left_type
			}
			if g.selfhost && fastc_is_integer_expression_type(left_type)
				&& fastc_is_integer_expression_type(right_type) {
				return if left_type == 'integer literal' { right_type } else { left_type }
			}
			if g.selfhost && left_type == '' && fastc_is_numeric_expression_type(right_type) {
				return right_type
			}
			if g.selfhost && right_type == '' && fastc_is_numeric_expression_type(left_type) {
				return left_type
			}
			if g.selfhost && right_type == 'voidptr' && fastc_is_numeric_expression_type(left_type) {
				return left_type
			}
			if g.selfhost && left_type == 'voidptr' && fastc_is_numeric_expression_type(right_type) {
				return right_type
			}
			if g.selfhost && fastc_is_numeric_expression_type(left_type)
				&& g.declared_kinds[g.semantic_type_key(right_type)] == .alias_ {
				return left_type
			}
			if g.selfhost && fastc_is_numeric_expression_type(right_type)
				&& g.declared_kinds[g.semantic_type_key(left_type)] == .alias_ {
				return right_type
			}
			if g.selfhost && left_type == right_type
				&& g.declared_kinds[g.semantic_type_key(left_type)] == .alias_ {
				return left_type
			}
			if g.selfhost && left_type == '' && right_type == '' {
				return 'int'
			}
			common_type := fastc_common_arithmetic_type(left_type, right_type)
			if common_type.len == 0 {
				return g.unsupported('arithmetic operands of types `${left_type}` and `${right_type}` for `${tokens[i].tok.str()}`')
			}
			return common_type
		}
	}
	return ''
}

fn (g &Parser) indexed_array_operand_type(tokens []FastcExpressionToken, inferred_type string) ?string {
	if tokens.len < 3 || !fastc_expression_tokens_contain(tokens, .lsbr)
		|| tokens.last().tok != .rsbr {
		return none
	}
	return g.array_element_type(inferred_type)
}

fn (g &Parser) infer_member_access_type(tokens []FastcExpressionToken) ?string {
	if tokens.len < 3 || tokens[0].tok != .name {
		return none
	}
	mut current_type := ''
	if local := g.locals[tokens[0].lit] {
		current_type = local.typ
	} else if global_type := g.global_types[fastc_global_key(g.module_name, tokens[0].lit)] {
		current_type = global_type
	} else {
		return none
	}
	mut index := 1
	for index < tokens.len {
		if tokens[index].tok == .lsbr {
			close := fastc_matching_delimiter(tokens, index, .lsbr, .rsbr) or { return none }
			if fastc_expression_tokens_contain(tokens[index + 1..close], .dotdot) {
				if current_type.trim_right('*') != 'string'
					&& g.array_element_type(current_type) == none {
					return none
				}
			} else if current_type.trim_right('*') == 'string' {
				current_type = 'u8'
			} else if current_type.trim_right('*').starts_with('Map_') {
				_, value_type := fastc_map_key_value_types(current_type) or { return none }
				current_type = value_type
			} else if current_type.ends_with('*') {
				current_type = current_type.trim_right('*')
			} else {
				current_type = g.array_element_type(current_type) or { return none }
			}
			index = close + 1
			continue
		}
		if index + 1 >= tokens.len || tokens[index].tok != .dot || tokens[index + 1].tok != .name {
			return none
		}
		field_name := tokens[index + 1].lit
		mut layout_type := current_type.trim_right('*')
		if layout_type.starts_with('Array_') {
			layout_type = 'array'
		} else if layout_type.starts_with('Map_') {
			layout_type = 'map'
		}
		if layout_type !in g.struct_fields {
			return none
		}
		fields := g.struct_fields[layout_type].clone()
		current_type = fields[field_name] or { return none }
		index += 2
	}
	if index != tokens.len {
		return none
	}
	return current_type
}

fn fastc_matching_delimiter(tokens []FastcExpressionToken, open_index int, open token.Token, close token.Token) ?int {
	mut depth := 0
	for i in open_index .. tokens.len {
		if tokens[i].tok == open {
			depth++
		} else if tokens[i].tok == close {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return none
}

fn (g &Parser) semantic_type_key(c_type string) string {
	base := c_type.trim_right('*')
	for key in g.declared_types.keys() {
		if fastc_c_declared_type_name(key) == base {
			return key
		}
	}
	return base
}

fn fastc_number_expression_type(literal string) string {
	clean := literal.replace('_', '')
	if clean.contains('.') || (!(clean.starts_with('0x') || clean.starts_with('0X'))
		&& clean.contains_any('eE')) {
		return 'float literal'
	}
	if clean.starts_with('-') {
		return 'negative integer literal'
	}
	return 'integer literal'
}

fn fastc_common_arithmetic_type(left string, right string) string {
	if left == right && fastc_is_numeric_expression_type(left) {
		return left
	}
	if left == 'negative integer literal' && fastc_is_unsigned_integer_type(right) {
		return ''
	}
	if right == 'negative integer literal' && fastc_is_unsigned_integer_type(left) {
		return ''
	}
	if fastc_is_integer_literal_expression_type(left) && fastc_is_integer_type(right) {
		return right
	}
	if fastc_is_integer_literal_expression_type(right) && fastc_is_integer_type(left) {
		return left
	}
	if fastc_is_integer_literal_expression_type(left)
		&& fastc_is_integer_literal_expression_type(right) {
		return if left == 'negative integer literal' || right == 'negative integer literal' {
			'negative integer literal'
		} else {
			'integer literal'
		}
	}
	if left == 'float literal' && right in ['f32', 'f64'] {
		return right
	}
	if right == 'float literal' && left in ['f32', 'f64'] {
		return left
	}
	return ''
}

fn fastc_is_numeric_expression_type(typ string) bool {
	return fastc_is_integer_literal_expression_type(typ) || typ in ['float literal', 'f32', 'f64']
		|| fastc_is_integer_type(typ)
}

fn fastc_is_integer_expression_type(typ string) bool {
	return fastc_is_integer_literal_expression_type(typ) || fastc_is_integer_type(typ)
}

fn fastc_is_integer_literal_expression_type(typ string) bool {
	return typ in ['integer literal', 'negative integer literal']
}

fn fastc_call_types_are_compatible(actual string, expected string) bool {
	if actual == expected {
		return true
	}
	if actual == 'integer literal' {
		return fastc_is_integer_type(expected)
	}
	if actual == 'negative integer literal' {
		return fastc_is_integer_type(expected) && !fastc_is_unsigned_integer_type(expected)
	}
	if actual == 'float literal' {
		return expected in ['f32', 'f64']
	}
	if actual == 'nil' {
		return expected.ends_with('*') || expected in ['voidptr', 'byteptr', 'charptr']
	}
	return false
}

fn fastc_selfhost_types_are_compatible(actual string, expected string) bool {
	if (actual == 'byteptr' && expected == 'u8*')
		|| (expected == 'byteptr' && actual == 'u8*')
		|| (actual == 'charptr' && expected == 'char*')
		|| (expected == 'charptr' && actual == 'char*') {
		return true
	}
	if actual == expected + '*' || expected == actual + '*' {
		return true
	}
	actual_base := actual.trim_right('*')
	expected_base := expected.trim_right('*')
	if (actual_base == 'array' && expected_base.starts_with('Array_'))
		|| (expected_base == 'array' && actual_base.starts_with('Array_'))
		|| (actual_base == 'map' && expected_base.starts_with('Map_'))
		|| (expected_base == 'map' && actual_base.starts_with('Map_')) {
		return true
	}
	if actual == 'negative integer literal' && fastc_is_unsigned_integer_type(expected) {
		return false
	}
	if fastc_is_integer_expression_type(actual) && fastc_is_integer_type(expected) {
		return true
	}
	if expected == 'voidptr' && fastc_is_pointer_type(actual) {
		return true
	}
	if actual == 'voidptr' && fastc_is_pointer_type(expected) {
		return true
	}
	return false
}

fn (g &Parser) selfhost_types_are_compatible(actual string, expected string) bool {
	if fastc_selfhost_types_are_compatible(actual, expected) {
		return true
	}
	if fastc_is_numeric_expression_type(actual)
		&& g.declared_kinds[g.semantic_type_key(expected)] == .alias_ {
		return true
	}
	if fastc_is_numeric_expression_type(expected)
		&& g.declared_kinds[g.semantic_type_key(actual)] == .alias_ {
		return true
	}
	return false
}

fn fastc_is_pointer_type(typ string) bool {
	return typ.ends_with('*') || typ in ['voidptr', 'byteptr', 'charptr']
}

fn fastc_array_element_type(typ string) ?string {
	base := typ.trim_right('*')
	if base.starts_with('Array_') && base.len > 'Array_'.len {
		element := base['Array_'.len..]
		return if element == 'char_ptr' { 'char*' } else { element }
	}
	if base.starts_with('FixedArray_') && base.len > 'FixedArray_'.len {
		if element_type := fastc_fixed_array_element_type(base) {
			return element_type
		}
		return base['FixedArray_'.len..]
	}
	return none
}

fn (g &Parser) array_element_type(typ string) ?string {
	if element_type := fastc_array_element_type(typ) {
		return element_type
	}
	layout_type := typ.trim_right('*')
	if layout_type !in g.struct_fields {
		return none
	}
	fields := g.struct_fields[layout_type].clone()
	element_type := fields['__fastc_element_type'] or { return none }
	return element_type
}

fn fastc_is_integer_type(typ string) bool {
	return typ in ['byte', 'char', 'i8', 'i16', 'i32', 'i64', 'int', 'isize', 'rune', 'u8', 'u16',
		'u32', 'u64', 'unsigned int', 'usize']
}

fn fastc_is_unsigned_integer_type(typ string) bool {
	return typ in ['byte', 'u8', 'u16', 'u32', 'u64', 'unsigned int', 'usize']
}

fn fastc_nondecimal_literal_is_type_sensitive(literal string) bool {
	clean := literal.replace('_', '')
	if clean.len <= 2 || clean[0] != `0` {
		return false
	}
	digits := clean[2..].trim_left('0')
	if clean[1] in [`x`, `X`] {
		if digits.len > 8 {
			return true
		}
		return digits.len == 8 && ((digits[0] >= `8` && digits[0] <= `9`)
			|| (digits[0] >= `a` && digits[0] <= `f`)
			|| (digits[0] >= `A` && digits[0] <= `F`))
	}
	if clean[1] in [`b`, `B`] {
		return digits.len >= 32
	}
	if clean[1] in [`o`, `O`] {
		return digits.len > 11 || (digits.len == 11 && digits[0] >= `2`)
	}
	return false
}

fn fastc_decimal_literal_is_type_sensitive(literal string) bool {
	clean := literal.replace('_', '')
	if clean.len == 0 || clean.contains_any('.eE') {
		return false
	}
	for digit in clean {
		if !digit.is_digit() {
			return false
		}
	}
	digits := clean.trim_left('0')
	int_max_literal := '2147483647'
	if digits.len != int_max_literal.len {
		return digits.len > int_max_literal.len
	}
	for i in 0 .. digits.len {
		if digits[i] != int_max_literal[i] {
			return digits[i] > int_max_literal[i]
		}
	}
	return false
}

fn fastc_c_number(literal string) !string {
	clean := literal.replace('_', '')
	if fastc_decimal_literal_is_type_sensitive(literal) {
		// C assigns oversized decimal tokens a wider type before any surrounding
		// operation. Reject them until the direct parser can preserve V inference.
		return error('fastc parser does not support oversized decimal literal expressions')
	}
	if fastc_nondecimal_literal_is_type_sensitive(literal) {
		return error('fastc parser does not support high-bit nondecimal literals')
	}
	if clean.len > 2 && clean[0] == `0` && clean[1] in [`o`, `O`] {
		// V spells octal integers with an explicit 0o prefix. GNU C uses a
		// leading zero, so translate the prefix before emitting the token.
		return '0' + clean[2..]
	}
	if clean.len < 2 || clean[0] != `0` || !clean[1].is_digit() || clean.contains_any('.eE') {
		return clean
	}
	mut first_digit := 0
	for first_digit < clean.len - 1 && clean[first_digit] == `0` {
		first_digit++
	}
	return clean[first_digit..]
}

fn fastc_c_selfhost_number(literal string) string {
	clean := literal.replace('_', '')
	if clean.len > 2 && clean[0] == `0` && clean[1] in [`o`, `O`] {
		return '0${clean[2..]}${if fastc_nondecimal_literal_is_type_sensitive(literal) {
			'ULL'
		} else {
			''
		}}'
	}
	if fastc_decimal_literal_is_type_sensitive(literal)
		|| fastc_nondecimal_literal_is_type_sensitive(literal) {
		return clean + 'ULL'
	}
	if clean.len < 2 || clean[0] != `0` || !clean[1].is_digit() || clean.contains_any('.eE') {
		return clean
	}
	mut first_digit := 0
	for first_digit < clean.len - 1 && clean[first_digit] == `0` {
		first_digit++
	}
	return clean[first_digit..]
}

fn (g &Parser) token_source() string {
	if g.lit.len > 0 {
		return g.lit
	}
	return g.tok.str()
}

fn (mut g Parser) write_line(line string) {
	if g.capturing_defer {
		indented_line := '\t'.repeat(g.indent) + line
		g.captured_defer_lines << indented_line
		return
	}
	for _ in 0 .. g.indent {
		g.out.write_u8(`\t`)
	}
	g.out.writeln(line)
}

fn fastc_needs_space(last u8, next string) bool {
	if next.len == 0 {
		return false
	}
	return (last.is_alnum() || last == `_`) && (next[0].is_alnum() || next[0] == `_`)
}

fn fastc_c_string(literal string) !string {
	if literal.len < 2 {
		return error('invalid fastc string literal')
	}
	mut raw := literal
	mut is_raw := false
	if raw[0] == `r` && raw.len >= 3 {
		is_raw = true
		raw = raw[1..]
	}
	quote := raw[0]
	if quote !in [`'`, `"`] || raw[raw.len - 1] != quote {
		return error('interpolated or unfinished fastc string literal')
	}
	content := raw[1..raw.len - 1]
	if fastc_string_contains_nul(content, is_raw) {
		return error('fastc parser does not support embedded NUL string literals')
	}
	mut result := strings.new_builder(raw.len + 2)
	result.write_u8(`"`)
	mut i := 1
	for i < raw.len - 1 {
		c := raw[i]
		if c == `\\` && !is_raw && i + 1 < raw.len - 1 {
			if raw[i + 1] == `\n` {
				i += 2
				for i < raw.len - 1 && raw[i] in [` `, `\t`, `\r`] {
					i++
				}
				continue
			}
			if raw[i + 1] == `\r` && i + 2 < raw.len - 1 && raw[i + 2] == `\n` {
				i += 3
				for i < raw.len - 1 && raw[i] in [` `, `\t`] {
					i++
				}
				continue
			}
			if raw[i + 1] == `x` {
				if i + 3 >= raw.len - 1 {
					return error('invalid fastc hex escape')
				}
				high := fastc_hex_digit_value(raw[i + 2])!
				low := fastc_hex_digit_value(raw[i + 3])!
				value := (high << 4) | low
				// V consumes exactly two hexadecimal digits. C consumes every
				// following hex digit, so use a full three-digit octal escape to
				// terminate the encoded byte unambiguously.
				result.write_u8(`\\`)
				result.write_u8(`0` + (value >> 6))
				result.write_u8(`0` + ((value >> 3) & 7))
				result.write_u8(`0` + (value & 7))
				i += 4
				continue
			}
			if raw[i + 1] >= `0` && raw[i + 1] <= `7` && (i + 3 >= raw.len - 1
				|| raw[i + 2] < `0` || raw[i + 2] > `7` || raw[i + 3] < `0`
				|| raw[i + 3] > `7`) {
				// V only decodes three-digit octal escapes. Preserve a shorter
				// spelling as a literal backslash and digits instead of letting C
				// consume it as a one- or two-digit octal escape.
				result.write_string('\\\\')
				i++
				continue
			}
			result.write_u8(c)
			result.write_u8(raw[i + 1])
			i += 2
			continue
		} else if c == `"` {
			result.write_string('\\"')
		} else if c == `\\` && is_raw {
			result.write_string('\\\\')
		} else if c == `\n` {
			result.write_string('\\n')
		} else if c == `\r` {
			result.write_string('\\r')
		} else if c == `\t` {
			result.write_string('\\t')
		} else {
			result.write_u8(c)
		}
		i++
	}
	result.write_u8(`"`)
	return result.str()
}

fn fastc_c_rune(literal string) !string {
	if literal.len == 0 {
		return error('invalid fastc rune literal')
	}
	content := if literal.len >= 3 && literal[0] == 96 && literal[literal.len - 1] == 96 {
		literal[1..literal.len - 1]
	} else {
		literal
	}
	if content.len == 1 {
		return '((rune)${content[0]})'
	}
	if content.len >= 2 && content[0] == `\\` {
		if content.len == 2 {
			value := match content[1] {
				`0` { 0 }
				`a` { 7 }
				`b` { 8 }
				`t` { 9 }
				`n` { 10 }
				`v` { 11 }
				`f` { 12 }
				`r` { 13 }
				`\\` { 92 }
				96 { 96 }
				`'` { 39 }
				`"` { 34 }
				else { return error('unsupported fastc rune escape') }
			}
			return '((rune)${value})'
		}
		if content.len == 4 && content[1] == `x` {
			high := fastc_hex_digit_value(content[2])!
			low := fastc_hex_digit_value(content[3])!
			value := (high << 4) | low
			return '((rune)${value})'
		}
	}
	first := content[0]
	mut value := u32(0)
	mut needed := 0
	if first & 0xe0 == 0xc0 {
		value = u32(first & 0x1f)
		needed = 1
	} else if first & 0xf0 == 0xe0 {
		value = u32(first & 0x0f)
		needed = 2
	} else if first & 0xf8 == 0xf0 {
		value = u32(first & 0x07)
		needed = 3
	} else {
		return error('invalid fastc UTF-8 rune literal')
	}
	if content.len != needed + 1 {
		return error('invalid fastc rune literal length')
	}
	for i in 1 .. content.len {
		if content[i] & 0xc0 != 0x80 {
			return error('invalid fastc UTF-8 rune literal')
		}
		value = (value << 6) | u32(content[i] & 0x3f)
	}
	return '((rune)${value})'
}

fn fastc_hex_digit_value(c u8) !u8 {
	if c >= `0` && c <= `9` {
		return u8(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return u8(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return u8(c - `A` + 10)
	}
	return error('invalid fastc hex digit `${c.ascii_str()}`')
}

fn fastc_string_contains_nul(content string, is_raw bool) bool {
	for byte_index in 0 .. content.len {
		if content[byte_index] == 0 {
			return true
		}
	}
	if is_raw {
		return false
	}
	mut i := 0
	for i + 1 < content.len {
		if content[i] != `\\` {
			i++
			continue
		}
		escape := content[i + 1]
		if escape == `\\` {
			i += 2
			continue
		}
		if escape >= `0` && escape <= `7` && i + 3 < content.len && content[i + 2] >= `0`
			&& content[i + 2] <= `7` && content[i + 3] >= `0` && content[i + 3] <= `7` {
			high := int(escape - `0`)
			middle := int(content[i + 2] - `0`)
			low := int(content[i + 3] - `0`)
			value := high * 64 + middle * 8 + low
			// V stores three-digit octal escapes in a byte, including wrapping
			// values such as \400 to NUL.
			if u8(value) == 0 {
				return true
			}
			i += 4
			continue
		}
		if escape == `0`
			|| (escape == `x` && i + 3 < content.len && content[i + 2..i + 4] == '00')
			|| (escape == `u` && i + 5 < content.len && content[i + 2..i + 6] == '0000')
			|| (escape == `U` && i + 9 < content.len && content[i + 2..i + 10] == '00000000') {
			return true
		}
		i += 2
	}
	return false
}
