module v

import os
import v3.parser
import v3.pref
import v3.flat

// parse_source parses `src` on its own and returns the flat AST plus the id of
// its trailing `.file` node (the one carrying the top-level declarations).
fn parse_source(name string, src string) (&flat.FlatAst, flat.NodeId) {
	path := os.join_path(os.temp_dir(), 'v3_vfmt_${name}_${os.getpid()}.v')
	os.write_file(path, src) or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	prefs.preserve_comptime_conditionals = true
	prefs.supports_inline_asm = true
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(path)
	os.rm(path) or {}
	mut fid := flat.empty_node
	for id in a.file_node_ids {
		n := a.nodes[id]
		if n.kind == .file && n.children_count > 0 {
			fid = flat.NodeId(id)
		}
	}
	return a, fid
}

// vfmt formats `src` and returns the generated V source.
fn vfmt(name string, src string) string {
	a, fid := parse_source(name, src)
	return format_file(a, fid)
}

fn vfmt_with_options(name string, src string, options FormatOptions) string {
	a, _ := parse_source(name, src)
	return format_with_options(a, options)
}

fn vfmt_file_with_json_migration(path string) string {
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	prefs.migrate_json2 = true
	prefs.preserve_comptime_conditionals = true
	prefs.supports_inline_asm = true
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(path)
	return format(a)
}

fn vfmt_with_json_migration(name string, source string) string {
	path := os.join_path(os.temp_dir(), 'v3_vfmt_json_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	return vfmt_file_with_json_migration(path)
}

// reparse_diagnostics reports how many parse diagnostics `src` produces.
fn reparse_diagnostics(name string, src string) int {
	path := os.join_path(os.temp_dir(), 'v3_vfmt_rp_${name}_${os.getpid()}.v')
	os.write_file(path, src) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_file(path)
	os.rm(path) or {}
	return p.diagnostics.len
}

fn test_fn_and_method() {
	out := vfmt('method',
		'module m\npub fn (mut p Point) inc(dx int) int {\n\tp.n += dx\n\treturn p.n\n}\n')
	assert out == 'module m

pub fn (mut p Point) inc(dx int) int {
	p.n += dx
	return p.n
}
'
}

fn test_formatter_preserves_blank_lines_between_statements() {
	source := "fn spaced() {\n\tprintln('a')\n\tprintln('b')\n\n\tprintln('c')\n\n\tif true {\n\t\tprintln('d')\n\t}\n\n\tdump('e')\n}\n"
	out := vfmt('statement_blank_lines', source)
	assert out == source, out
	assert vfmt('statement_blank_lines_twice', out) == out
}

fn test_formatter_preserves_gated_slices() {
	source := 'fn slices(body string, items []int, start int, end int) {\n\t_ := body#[start..end]\n\t_ := items#[..end]\n\t_ := items#[start..]\n\t_ := items#[..]\n}\n'
	out := vfmt('gated_slices', source)
	assert out == source, out
	assert vfmt('gated_slices_twice', out) == out
}

fn test_formatter_preserves_mutability_for_each_multi_decl_target() {
	source := 'fn pairs() {\n\tmut first, mut second := 1, 2\n\t_ = first\n\t_ = second\n}\n'
	out := vfmt('multi_decl_mutability', source)
	assert out == source, out
	assert vfmt('multi_decl_mutability_twice', out) == out
}

fn test_formatter_preserves_compact_function_and_expression_bodies() {
	source := 'fn empty() {}\n\nfn comment_only() {\n\t// keep inside\n}\n\nfn compact_expressions() {\n\t_ := if true { 1 } else { 2 }\n\t_ := match 10 {\n\t\t10 { 10 }\n\t\t5 {}\n\t\telse { 2 }\n\t}\n\tmatch 1 {\n\t\telse {\n\t\t\t// keep inside\n\t\t}\n\t}\n}\n'
	out := vfmt('compact_bodies', source)
	assert out == source, out
	assert vfmt('compact_bodies_twice', out) == out
}

fn test_formatter_keeps_trailing_array_comments_inside_literal() {
	source := 'fn array_comments() {\n\t_ := [\n\t\t// before\n\t\t6,\n\t\t// after\n\t]\n\t_ := [\n\t\t7, // inline after\n\t]\n}\n'
	out := vfmt('trailing_array_comments', source)
	assert out == source, out
	assert vfmt('trailing_array_comments_twice', out) == out
}

fn test_formatter_keeps_trailing_block_and_struct_update_comments_inside() {
	source := "struct Item {\n\tvalue int\n}\n\nfn comment_boundaries() {\n\t{\n\t\tprintln('first')\n\t\t// trailing block\n\t}\n\t{\n\t\tprintln('second')\n\n\t\t// trailing after blank\n\t}\n\titem := Item{}\n\t_ := Item{\n\t\t...item // inline spread\n\t\t// trailing spread\n\t}\n\t_ := Item{\n\t\t...item\n\t\tvalue: 1\n\t\t// trailing field\n\t}\n}\n"
	out := vfmt('trailing_block_update_comments', source)
	assert out == source, out
	assert vfmt('trailing_block_update_comments_twice', out) == out
}

fn test_formatter_preserves_unsafe_and_defer_source_layout() {
	source := 'fn foo() {}\n\nfn block_layouts() {\n\tunsafe { 6 }\n\tunsafe {}\n\tunsafe {\n\t}\n\tx := unsafe {\n\t\t5\n\t}\n\ty := unsafe { 7 }\n\tdefer {}\n\tdefer { foo() }\n\tdefer {\n\t\tfoo()\n\t}\n\t_ = x\n\t_ = y\n}\n'
	out := vfmt('unsafe_defer_layout', source)
	assert out == source, out
	assert vfmt('unsafe_defer_layout_twice', out) == out
}

fn test_formatter_keeps_anon_fn_body_statements_expanded_inside_init() {
	// Regression: an anonymous fn used as a struct init field value kept the
	// `in_init` flag set while emitting its body, collapsing the body statements
	// (and any nested `or {}` block) onto a single line, e.g.
	// `c := codem := msg_ = c_ = m}`, which broke compilation.
	source := 'struct Events {\n\ton_error fn (code int, msg string)\n\ton_close fn (id int)\n}\n\nstruct Registry {\nmut:\n\tclosed bool\n}\n\nfn (r &Registry) find(id int) ?int {\n\treturn id\n}\n\nfn handlers() {\n\tmut reg := Registry{}\n\te := Events{\n\t\ton_error: fn (code int, msg string) {\n\t\t\tc := code\n\t\t\tm := msg\n\t\t\t_ = c\n\t\t\t_ = m\n\t\t}\n\t\ton_close: fn (id int) {\n\t\t\tn := reg.find(id) or { return }\n\t\t\t_ = n\n\t\t}\n\t}\n\t_ = e\n\t_ = reg\n}\n'
	out := vfmt('anon_fn_body_inside_init', source)
	assert out == source, out
	assert vfmt('anon_fn_body_inside_init_twice', out) == out
}

fn test_formatter_preserves_compact_empty_literals_and_declarations() {
	source := 'interface Compact {}\n\nstruct Between {}\n\ninterface Expanded {\n}\n\nenum CompactEnum {}\n\nstruct Between2 {}\n\nenum ExpandedEnum {\n}\n\nfn literal_layouts() {\n\tcompact := fn (_s string) {}\n\texpanded := fn (_s string) {\n\t}\n\t_ = compact\n\t_ = expanded\n}\n'
	out := vfmt('compact_empty_literals_declarations', source)
	assert out == source, out
	assert vfmt('compact_empty_literals_declarations_twice', out) == out
}

fn test_formatter_preserves_loop_labels_debugger_and_enum_groups() {
	source := 'enum Grouped {\n\taa = 1\n\tbbb\n\n\tcccc  = 5\n\tddddd = 10\n\n\t// final group\n\tee  = 20\n\tfff = 30\n}\n\nfn labelled_debugger() {\n\tL1: for {\n\t\t\$dbg;\n\t\tbreak L1\n\t}\n}\n'
	out := vfmt('loop_label_debugger_enum_groups', source)
	assert out == source, out
	assert vfmt('loop_label_debugger_enum_groups_twice', out) == out
}

fn test_formatter_preserves_or_block_layout_and_lock_comments() {
	source := "fn block_boundaries() {\n\tempty_or_block() or {}\n\tempty_or_block() or {\n\t}\n\tfn_with_option() or { return }\n\tfn_with_option() or {\n\t\treturn\n\t}\n\tlock value {\n\t\tprintln('inside')\n\t\t// trailing lock\n\t}\n\tlock value {\n\t\t// comment only\n\t}\n}\n"
	out := vfmt('or_layout_lock_comments', source)
	assert out == source, out
	assert vfmt('or_layout_lock_comments_twice', out) == out
}

fn test_formatter_keeps_trailing_loop_comments_inside_body() {
	source := "fn loop_comments(items []int) {\n\tfor {\n\t\tprintln('regular')\n\t\t// trailing regular\n\t}\n\tfor {\n\t\t// comment-only regular\n\t}\n\tfor i := 0; i < 1; i++ {\n\t\tprintln(i)\n\t\t// trailing C-style\n\t}\n\tfor item in items {\n\t\tprintln(item)\n\t\t// trailing for-in\n\t}\n\tfor _ in items {\n\t\t// comment-only for-in\n\t}\n}\n"
	out := vfmt('trailing_loop_comments', source)
	assert out == source, out
	assert vfmt('trailing_loop_comments_twice', out) == out
}

fn test_formatter_keeps_trailing_comptime_for_comments_inside_body() {
	source := 'fn comptime_loop_comments[T]() {\n\t\$for field in T.fields {\n\t\tprintln(field.name)\n\t\t// trailing comptime loop\n\t}\n\t\$for method in T.methods {\n\t\t// comment-only comptime loop\n\t}\n}\n'
	out := vfmt('trailing_comptime_for_comments', source)
	assert out == source, out
	assert vfmt('trailing_comptime_for_comments_twice', out) == out
}

fn test_formatter_rewrites_c_string_selectors_by_backend() {
	source := "fn string_selector() {\n\ts := 'abc'.str\n}\n"
	c_expected := "fn string_selector() {\n\ts := c'abc'\n}\n"
	c_out := vfmt('c_string_selector', source)
	assert c_out == c_expected, c_out
	assert vfmt('c_string_selector_twice', c_out) == c_out
	for backend in ['js', 'js_node', 'js_browser', 'js_freestanding'] {
		js_out := vfmt_with_options('${backend}_string_selector', source, FormatOptions{
			backend: backend
		})
		assert js_out == source, '${backend}: ${js_out}'
		assert vfmt_with_options('${backend}_string_selector_twice', js_out, FormatOptions{
			backend: backend
		}) == js_out
	}
}

fn test_formatter_preserves_aggregate_member_blank_lines() {
	source := 'struct Grouped {\n\ta int\n\n\tbb string\n\tcc bool\n}\n\ninterface Contract {\n\ta int\n\n\tbb string\n\n\tfirst()\n\tsecond()\n}\n'
	out := vfmt('aggregate_member_blank_lines', source)
	assert out == source, out
	assert vfmt('aggregate_member_blank_lines_twice', out) == out
}

fn test_formatter_keeps_blank_lines_between_consecutive_enums() {
	source := 'enum First {\n\tone\n}\n\nenum Second {\n\ttwo\n}\n'
	out := vfmt('consecutive_enum_blank_lines', source)
	assert out == source, out
	assert vfmt('consecutive_enum_blank_lines_twice', out) == out
}

fn test_formatter_keeps_trailing_positional_struct_init_comments_inside() {
	source := 'struct Pair {\n\tfirst  int\n\tsecond int\n}\n\nfn positional() {\n\t_ := Pair{\n\t\t1,\n\t\t2,\n\t\t// trailing positional\n\t}\n}\n'
	expected := 'struct Pair {\n\tfirst  int\n\tsecond int\n}\n\nfn positional() {\n\t_ := Pair{1, 2,\n\t\t// trailing positional\n\t}\n}\n'
	out := vfmt('trailing_positional_struct_init_comment', source)
	assert out == expected, out
	assert vfmt('trailing_positional_struct_init_comment_twice', out) == out
}

fn test_formatter_preserves_compact_struct_updates() {
	source := 'struct Position {\n\tpos int\n\tlen int\n}\n\nfn compact(field Position, name_len int) {\n\t_ := Position{ ...field }\n\t_ := Position{ ...field, len: name_len }\n}\n'
	out := vfmt('compact_struct_updates', source)
	assert out == source, out
	assert vfmt('compact_struct_updates_twice', out) == out
}

fn test_formatter_expands_grouped_consts_and_keeps_trailing_global_comments_inside() {
	source := 'const (\n\t// first docs\n\tfirst = 1\n\tsecond = 2\n)\n\npub const (\n\tthird = 3\n)\n\n__global (\n\tvalue = 4\n\t// trailing global\n)\n'
	expected := '// first docs\nconst first = 1\nconst second = 2\n\npub const third = 3\n\n__global (\n\tvalue = 4\n\t// trailing global\n)\n'
	out := vfmt('grouped_consts_trailing_global_comment', source)
	assert out == expected, out
	second := vfmt('grouped_consts_trailing_global_comment_twice', out)
	assert second == out, second
}

fn test_formatter_keeps_trailing_array_initializer_comments_inside() {
	source := 'fn f() {\n\ta := []int{len: 1\n\t\t/* trailing initializer */\n\t}\n\t_ = a\n}\n'
	out := vfmt('trailing_array_initializer_comment', source)
	assert out == source, out
	assert vfmt('trailing_array_initializer_comment_twice', out) == out
}

// A `const` with a trailing `//` comment used to emit an extra newline after the
// comment, so every `v fmt -w` pass added one more blank line after it.
fn test_formatter_keeps_consts_adjacent_after_trailing_comment() {
	source := 'const first = u64(0x06)\nconst second = u64(0x08) // low 3 bits\nconst third = u64(0x10)\n\nconst fourth = 1 // spaced out\n\nconst fifth = 2\n\nfn main() {}\n'
	out := vfmt('const_trailing_comment_blank_lines', source)
	assert out == source, out
	assert vfmt('const_trailing_comment_blank_lines_twice', out) == out
}

fn test_formatter_keeps_singleton_grouped_const_comments_before_declaration() {
	source := 'const (\n\t// only docs\n\tonly = 1\n)\n'
	expected := '// only docs\nconst only = 1\n'
	out := vfmt('singleton_grouped_const_comment', source)
	assert out == expected, out
	assert vfmt('singleton_grouped_const_comment_twice', out) == out
}

fn test_formatter_preserves_declaration_attribute_groups() {
	source := "@[deprecated: 'use bar() instead']\n@[foo: bar]\n@[if debug; inline]\nfn keep_attributes() {}\n\n@[deprecated(msg: 'use foo_v2() instead', after: '2026-06-01')]\n@[inline]\nfn call_syntax() {}\n\n@[inline]\n@[export: 'symbol']\n@[unsafe]\n@[tom: 'jerry']\nfn normalized() {}\n"
	expected := "@[deprecated: 'use bar() instead']\n@[foo: bar]\n@[if debug; inline]\nfn keep_attributes() {}\n\n@[deprecated(msg: 'use foo_v2() instead', after: '2026-06-01')]\n@[inline]\nfn call_syntax() {}\n\n@[export: 'symbol']\n@[tom: 'jerry']\n@[inline; unsafe]\nfn normalized() {}\n"
	out := vfmt('declaration_attribute_groups', source)
	assert out == expected, out
	assert vfmt('declaration_attribute_groups_twice', out) == out
}

fn test_formatter_keeps_comptime_branch_and_selective_import_comments_inside() {
	source := "import sample {\n\tOne,\n\tTwo,\n\t// trailing import\n}\n\n\$if linux {\n\tprintln('first')\n\t// trailing first\n} \$else \$if windows {\n\t// comment-only second\n} \$else {\n\tprintln('last')\n\t// trailing last\n}\n"
	out := vfmt('comptime_branch_selective_import_comments', source)
	assert out == source, out
	assert vfmt('comptime_branch_selective_import_comments_twice', out) == out
}

fn test_formatter_keeps_trailing_interface_comments_inside_body() {
	source := 'interface Speaker {
	// first
	speak() string
	// last
}
'
	out := vfmt('interface_trailing_comment', source)
	assert out == source, out
	assert vfmt('interface_trailing_comment_twice', out) == out
}

fn test_formatter_preserves_multiline_strings_and_trailing_struct_comments() {
	source := "const text = 'first\nsecond\nthird'\n\nstruct User {\n\tname string\n\t// trailing one\n\t// trailing two\n}\n"
	out := vfmt('multiline_string_struct_comments', source)
	assert out == source, out
	assert vfmt('multiline_string_struct_comments_twice', out) == out
}

fn test_formatter_preserves_declaration_list_layout() {
	input := 'fn wrapped(first_parameter string,\n\tsecond_parameter int, third_parameter bool) {\n\tprintln(first_parameter)\n}\n\ntype Long = FirstVeryLongVariant | SecondVeryLongVariant | ThirdVeryLongVariant | FourthVeryLongVariant | FifthVeryLongVariant\n\ntype Commented = First // first\n\t| Second\n\t// disabled\n\t| Third\n\nenum Code {\n\ta = 1\n\tlong_name = 2\n\t// trailing\n}\n'
	expected := 'fn wrapped(first_parameter string,\n\tsecond_parameter int, third_parameter bool) {\n\tprintln(first_parameter)\n}\n\ntype Long = FirstVeryLongVariant\n\t| SecondVeryLongVariant\n\t| ThirdVeryLongVariant\n\t| FourthVeryLongVariant\n\t| FifthVeryLongVariant\n\ntype Commented = First // first\n\t| Second\n\t// disabled\n\t| Third\n\nenum Code {\n\ta         = 1\n\tlong_name = 2\n\t// trailing\n}\n'
	out := vfmt('declaration_list_layout', input)
	assert out == expected, out
	assert vfmt('declaration_list_layout_twice', out) == out
}

fn test_formatter_keeps_comments_inside_fn_literals() {
	source := "fn main() {\n\tcomment_only := fn () {\n\t\t// only comment\n\t}\n\twith_statement := fn () {\n\t\tprintln('inside')\n\t\t// trailing comment\n\t}\n\t_ = comment_only\n\t_ = with_statement\n}\n"
	out := vfmt('fn_literal_comments', source)
	assert out == source, out
	assert vfmt('fn_literal_comments_twice', out) == out
}

fn test_struct_access_sections() {
	out := vfmt('struct', 'struct Point {\n\tid int\npub mut:\n\tx int\n\ty int = 3\n}\n')
	assert out == 'struct Point {
	id int
pub mut:
	x int
	y int = 3
}
'
}

fn test_formatter_preserves_selective_import_layout() {
	source := 'import math { max, min }
import os {
	file_ext,
	user_os,
}
'
	out := vfmt('selective_import_layout', source)
	assert out == source, out
	assert vfmt('selective_import_layout_twice', out) == out
}

fn test_formatter_aligns_aggregate_fields() {
	source := 'interface Hex {
	a     int
	ab    int
	abc   int
	abcd  int
	abcde int
mut:
	aaaaaaaaaaaaaaaaaa string
	b                  f64
}

struct Hex2 {
	a     int
	ab    int
	abc   int
	abcd  int
	abcde int
}
'
	out := vfmt('aggregate_field_alignment', source)
	assert out == source, out
	assert vfmt('aggregate_field_alignment_twice', out) == out
}

fn test_formatter_preserves_global_struct_sections() {
	source := 'struct State {
	local int
__global:
	shared_value int
}
'
	out := vfmt('global_struct_section', source)
	assert out.contains('__global:\n\tshared_value int'), out
	assert !out.contains('pub mut:'), out
	assert vfmt('global_struct_section_twice', out) == out
}

fn test_formatter_normalizes_legacy_const_decl_assign() {
	out := vfmt('legacy_const_decl_assign', 'const answer := 42\n')
	assert out == 'const answer = 42\n', out
	assert vfmt('legacy_const_decl_assign_twice', out) == out
}

fn test_formatter_honors_new_int_option() {
	c_source := 'module main

fn C.abc(a int, b []int, foreign C.int, foreign_values []C.int) int

fn C.foreign(value C.int) C.int

fn abc(a int) int
'
	c_out := vfmt_with_options('new_int_c_decl', c_source, is_new_int: true)
	assert c_out.contains('fn C.abc(a i32, b []i32, foreign C.int, foreign_values []C.int) i32'), c_out
	assert c_out.contains('fn C.foreign(value C.int) C.int'), c_out
	assert !c_out.contains('C.i32'), c_out
	assert c_out.contains('fn abc(a int) int'), c_out

	translated_source := '@[translated]
module translated

fn convert(value int) int {
	return int(value)
}
'
	translated_out := vfmt_with_options('new_int_translated', translated_source, is_new_int: true)
	assert translated_out.contains('fn convert(value i32) i32'), translated_out
	assert translated_out.contains('return i32(value)'), translated_out
}

fn test_formatter_preserves_function_exit_defer() {
	source := "fn cleanup() {\n\tdefer\n\t(fn)\n\t{\n\t\tprintln('done')\n\t}\n\tprintln('after')\n}\n"
	expected := "fn cleanup() {\n\tdefer(fn) {\n\t\tprintln('done')\n\t}\n\tprintln('after')\n}\n"
	out := vfmt('function_exit_defer', source)
	assert out == expected, out
	assert vfmt('function_exit_defer_twice', out) == out
}

fn test_formatter_reescapes_control_bytes() {
	source := "fn main() {\n\tprint('\\a\\b\\f\\v\\x01\\x1b\\x7f')\n}\n"
	out := vfmt('escaped_control_bytes', source)
	assert out == source, out
	assert vfmt('escaped_control_bytes_twice', out) == out
}

fn test_formatter_does_not_import_lexical_binders() {
	source := 'struct Item {\n\tname string\n}\n\nfn scan(items []Item, ch chan Item) {\n\tfor flag in items {\n\t\tprintln(flag.name)\n\t}\n\tselect {\n\t\ttime := <-ch {\n\t\t\tprintln(time.name)\n\t\t}\n\t}\n}\n\nfn fields[T]() {\n\t\$for json in T.fields {\n\t\tprintln(json.name)\n\t}\n}\n'
	out := vfmt('lexical_binder_imports', source)
	assert !out.contains('import flag'), out
	assert !out.contains('import time'), out
	assert !out.contains('import json'), out
	assert vfmt('lexical_binder_imports_twice', out) == out
}

fn test_formatter_resolves_implied_imports_in_selector_scope() {
	source := 'struct Clock {\n\tnow int\n}\n\nfn shadow(time Clock) {\n\tprintln(time.now)\n}\n\nfn use_module() {\n\tprintln(time.now())\n}\n'
	out := vfmt('scope_aware_implied_import', source)
	assert out.contains('import time'), out
	assert out.contains('fn shadow(time Clock)'), out
	assert out.contains('println(time.now)'), out
	assert out.contains('println(time.now())'), out
	assert vfmt('scope_aware_implied_import_twice', out) == out
}

fn test_formatter_preserves_isreftype_spelling() {
	source := 'fn check[T](value T) {\n\t_ = isreftype(T)\n\t_ = isreftype[T]()\n\t_ = isreftype(value)\n\t_ = isreftype(sizeof(T))\n}\n'
	out := vfmt('isreftype_spelling', source)
	assert !out.contains('__v3_isreftype'), out
	assert out.contains('isreftype(T)'), out
	assert out.contains('isreftype[T]()'), out
	assert out.contains('isreftype(value)'), out
	assert out.contains('isreftype(sizeof(T))'), out
	assert vfmt('isreftype_spelling_twice', out) == out
}

fn test_formatter_preserves_anonymous_aggregate_types() {
	source := "struct Holder {\n\titem []struct {\n\t\t// keep anonymous field comment\n\t\tfoo string\n\t}\n\tchoice union { number int }\n}\n\nfn accept(value struct{ foo string }) {\n\tprintln(value.foo)\n}\n\nfn main() {\n\taccept(struct { foo: 'ok' })\n}\n"
	out := vfmt('anonymous_aggregate_types', source)
	assert !out.contains('AnonStruct_'), out
	assert !out.contains('AnonUnion_'), out
	assert out.contains('item   []struct {'), out
	assert out.count('// keep anonymous field comment') == 1, out
	assert out.contains('choice union { number int }'), out
	assert out.contains('value struct{ foo string }'), out
	assert out.contains("accept(struct { foo: 'ok' })"), out
	assert vfmt('anonymous_aggregate_types_twice', out) == out
}

fn test_formatter_preserves_mutable_match_subjects() {
	source := 'fn update(mut value int) {\n\tmatch mut value {\n\t\tint {\n\t\t\tvalue++\n\t\t}\n\t}\n}\n'
	out := vfmt('mutable_match_subject', source)
	assert out.contains('match mut value {'), out
	assert vfmt('mutable_match_subject_twice', out) == out
}

fn test_formatter_accepts_remaining_repository_syntax() {
	source := "module main\n\nimport underscore as _abc\n\nfn accepts[T]() bool { return true }\n\nfn check() {\n\tassert kind == .fn\n\tassert accepts[atomic fn (int) int]()\n\tassert sizeof(`€`) == 4\n\tassert sizeof(c'hello') == 6\n\tassert sizeof(r'hello') > 0\n}\n"
	out := vfmt('remaining_repository_syntax', source)
	assert out.contains('import underscore as _abc'), out
	assert out.contains('assert kind == .fn'), out
	assert out.contains('accepts[atomic fn (int) int]()'), out
	assert out.contains('sizeof(`€`)'), out
	assert out.contains("sizeof(c'hello')"), out
	assert out.contains("sizeof(r'hello')"), out
	assert vfmt('remaining_repository_syntax_twice', out) == out
}

fn test_attributes_and_pub() {
	out := vfmt('attrs', '@[heap]\npub struct Foo {\n\tx int\n}\n')
	assert out.starts_with('@[heap]\npub struct Foo {')
}

fn test_enum_and_types() {
	out := vfmt('enum',
		'pub enum Color as u8 {\n\tred = 1\n\tgreen\n}\n\ntype MyInt = int\ntype Sum = Foo | Bar\n')
	assert out.contains('enum Color as u8 {')
	assert out.contains('red = 1')
	assert out.contains('type MyInt = int')
	assert out.contains('type Sum = Foo | Bar')
}

fn test_control_flow() {
	out := vfmt('flow', 'fn f(xs []int) {
	for i, x in xs {
		if x > 0 {
			println(x)
		} else {
			continue
		}
	}
	for j := 0; j < 3; j++ {
	}
}
')
	assert out.contains('for i, x in xs {')
	// C-style loop var must not gain a spurious `mut`
	assert out.contains('for j := 0; j < 3; j++ {')
	assert !out.contains('for mut j := 0')
	assert out.contains('} else {')
}

fn test_match() {
	out := vfmt('match',
		'fn f(x int) string {\n\treturn match x {\n\t\t1, 2 { "a" }\n\t\telse { "b" }\n\t}\n}\n')
	assert out.contains('match x {')
	assert out.contains('1, 2 {')
	assert out.contains('else {')
}

fn test_string_escaping() {
	// a literal `$` must be escaped so it is not read back as interpolation
	out := vfmt('stresc', "fn f() {\n\ta := 'price: \$5'\n\tb := '\${x}y'\n}\n")
	assert out.contains("'price: \\\$5'")
	assert out.contains("'\${x}y'")
}

fn test_c_string_escaping() {
	source := 'fn f() {\n\tmessage := c"Unknown game version \'%s\'"\n\tquoted := c\'He said "it\\\'s done"\'\n}\n'
	out := vfmt('c_string_escaping', source)
	assert out.contains('message := c"Unknown game version \'%s\'"'), out
	assert out.contains('quoted := c\'He said "it\\\'s done"\''), out
	assert vfmt('c_string_escaping_twice', out) == out
}

fn test_rune_literal_escaping() {
	source := 'fn f() {\n\tprintln(`\\n`)\n\tprintln(`\\``)\n}\n'
	out := vfmt('rune_literal_escaping', source)
	assert out == source, out
	assert escape_string('\n', `\``) == '\\n'
	assert escape_string('`', `\``) == '\\`'
	assert vfmt('rune_literal_escaping_twice', out) == out
}

fn test_formatter_preserves_source_only_syntax() {
	out := vfmt('source_only',
		'// docs\nfn f() {\n\tx := c\' \'\n\ty := r"raw \\\\ text"\n\t\$if windows {\n\t\tprintln(\'windows\')\n\t} \$else {\n\t\tprintln(\'other\')\n\t}\n\tasm amd64 {\n\t\tnop\n\t}\n\tprintln(@FN) // inline\n\t_ = x\n\t_ = y\n}\n')
	assert out.starts_with('// docs\nfn f() {'), out
	assert out.contains("x := c' '"), out
	assert out.contains('y := r"raw \\\\ text"'), out
	assert out.contains('\$if windows {'), out
	assert out.contains("println('windows')"), out
	assert out.contains("println('other')"), out
	assert out.contains('asm amd64 {\n\t\tnop\n\t}'), out
	assert out.contains('println(@FN) // inline'), out
}

fn test_formatter_preserves_comptime_calls_attributes_and_volatile_fields() {
	out := vfmt('more_source_only',
		"@[if missing_flag ?]\nfn guarded() {\n\ta := \$embed_file('./missing.txt')\n\tb := \$env('VFMT_SECRET')\n\tc := \$tmpl('./missing.html')\n\tprintln(@FILE)\n\t_ = a\n\t_ = b\n\t_ = c\n}\n\nstruct Counter {\n\tvolatile value u64\n}\n\nfn main() {\n\tmut volatile counter := u64(0)\n\t_ = counter\n}\n")
	assert out.contains('@[if missing_flag ?]'), out
	assert out.contains("\$embed_file('./missing.txt')"), out
	assert out.contains("\$env('VFMT_SECRET')"), out
	assert out.contains("\$tmpl('./missing.html')"), out
	assert out.contains('println(@FILE)'), out
	assert out.contains('volatile value u64'), out
	assert out.contains('mut volatile counter := u64(0)'), out
	assert vfmt('more_source_only_twice', out) == out
}

fn test_formatter_preserves_fixed_array_literal_prefixes() {
	out := vfmt('fixed_array_literal_prefixes',
		'fn main() {\n\ta := [4]f32[1, 2, 3, 4]\n\tb := [..]f32[1, 2, 3, 4]\n\t_ = a\n\t_ = b\n}\n')
	assert out.contains('a := [4]f32[1, 2, 3, 4]'), out
	assert out.contains('b := [..]f32[1, 2, 3, 4]\n'), out
}

fn test_formatter_preserves_and_wraps_array_layout() {
	vertical := 'fn f() {
	values := [
		[1, 2],
		[3, 4],
	]
	_ = values
}
'
	vertical_out := vfmt('vertical_array_layout', vertical)
	assert vertical_out == vertical, vertical_out

	wrapped_source := "const supported_platforms = ['windows', 'macos', 'linux', 'freebsd', 'openbsd', 'netbsd', 'dragonfly', 'android', 'js', 'solaris', 'haiku']\n"
	wrapped := vfmt('wrapped_array_layout', wrapped_source)
	assert wrapped.contains("'netbsd', 'dragonfly',\n\t'android', 'js'"), wrapped
	assert vfmt('wrapped_array_layout_twice', wrapped) == wrapped
}

fn test_formatter_preserves_multiline_map_layout() {
	source := "numbers := {'one': 1, 'twentytwo': 22}\n"
	out := vfmt('multiline_map_layout', source)
	assert out == "numbers := {\n\t'one':       1\n\t'twentytwo': 22\n}\n", out
	assert vfmt('multiline_map_layout_twice', out) == out

	unicode := vfmt('unicode_map_alignment', "values := {'ß': 1, 'abc': 2}\n")
	assert unicode.contains("\t'ß':   1\n\t'abc': 2"), unicode

	comments := "fn f() {
	values := {
		'a': 1 // after
		// between
		'b': 2
		// post
	}
}
"
	assert vfmt('map_comments', comments) == comments
}

fn test_formatter_keeps_comments_inside_construct_boundaries() {
	source := 'struct Foo {
	value int
}

fn comments(base Foo, condition bool) {
	updated := Foo{
		// before
		...base // after
	}
	empty := Foo{
		// inside struct init
	}
	values := [
		// inside array
	]
	if condition {
		// inside branch
	} else {
		// inside else branch
	}
	_ = updated
	_ = empty
	_ = values
}

fn only_comments() {
	// abc
}
'
	out := vfmt('construct_comment_boundaries', source)
	assert out == source, out
	assert vfmt('construct_comment_boundaries_twice', out) == out
}

fn test_formatter_emits_comments_before_struct_init_field_names() {
	source := 'struct Config {
	one int
	two int
}

fn config() Config {
	return Config{
		// before one
		one: 1 // after one

		// before two
		two: 2 // after two
	}
}
'
	out := vfmt('struct_init_field_comments', source)
	assert out == source, out
	assert vfmt('struct_init_field_comments_twice', out) == out
}

fn test_formatter_preserves_three_value_if_guard_bindings() {
	source := "fn create() ?(int, string, bool) {
	return 5, 'value', true
}

fn check() {
	if mut r1, mut r2, r3 := create() {
		_ = r1
		_ = r2
		_ = r3
	}
}
"
	out := vfmt('three_value_if_guard', source)
	assert out == source, out
	assert vfmt('three_value_if_guard_twice', out) == out
}

fn test_formatter_preserves_mut_if_guard_binding() {
	source := 'struct Item {
mut:
	value int
}

struct Holder {
mut:
	items map[u32]&Item
}

fn (mut holder Holder) update(id u32) {
	if mut item := holder.items[id] {
		item.value++
	}
}
'
	out := vfmt('mut_if_guard_binding', source)
	assert out == source, out
	assert vfmt('mut_if_guard_binding_twice', out) == out
}

fn test_formatter_keeps_or_block_leading_comment_inside_block() {
	source := 'fn read_frame() ?int {
	return 1
}

fn process() {
	frame := read_frame() or {
		// Treat a clean transport close as end of session.
		return
	}
	println(frame)
}
'
	out := vfmt('or_block_leading_comment', source)
	assert out == source, out
	assert vfmt('or_block_leading_comment_twice', out) == out
}

fn test_formatter_retains_expanded_call_argument_layout() {
	source := 'fn many(a int, b int, c int) int {
	return a + b + c
}

fn configure(config Config) {
}

fn calls() {
	x := many(
		1,
		// before two
		2, 3, // after three
	)
	configure(
		// before one
		one: 1 // after one
		two: 2
	)
	_ = x
}
'
	out := vfmt('expanded_call_arguments', source)
	assert out == source, out
	assert vfmt('expanded_call_arguments_twice', out) == out
}

fn test_formatter_expands_long_single_line_named_call_arguments() {
	source := "fn calls() {\n\tbar_func(x: 'a very long content should cause vfmt to use multiple lines instead of one.', y: 123456789)\n}\n"
	expected := "fn calls() {\n\tbar_func(\n\t\tx: 'a very long content should cause vfmt to use multiple lines instead of one.'\n\t\ty: 123456789\n\t)\n}\n"
	out := vfmt('long_single_line_named_call_arguments', source)
	assert out == expected, out
	assert vfmt('long_single_line_named_call_arguments_twice', out) == out
}

fn test_formatter_removes_redundant_parentheses() {
	source := 'fn predicate(char int) bool {\n\treturn (char >= 65 && char <= 90)\n}\n\nfn checks() {\n\tx := 3\n\t_ := &(((x)))\n\t_, _ := (((22 > 11))), (43 > 22)\n\t_ := ((10 + 11))\n\t_ := (cond1 && cond2) || (single_ident)\n\t_ := (\n\t\t// keep grouping\n\t\tx\n\t)\n\tassert (((((1 + 2) == 3))))\n\tassert (((true)))\n}\n'
	expected := 'fn predicate(char int) bool {\n\treturn char >= 65 && char <= 90\n}\n\nfn checks() {\n\tx := 3\n\t_ := &x\n\t_, _ := (22 > 11), (43 > 22)\n\t_ := (10 + 11)\n\t_ := (cond1 && cond2) || single_ident\n\t_ := (\n\t\t// keep grouping\n\t\tx\n\t)\n\tassert (1 + 2) == 3\n\tassert true\n}\n'
	out := vfmt('redundant_parentheses', source)
	assert out == expected, out
	assert vfmt('redundant_parentheses_twice', out) == out
}

fn test_formatter_emits_hash_directive_attributes() {
	source := '@[use_once] #include "header.h"
@[custom_tag; use_once] #flag -I @VMODROOT/c
'
	expected := '@[use_once]
#include "header.h"

@[custom_tag; use_once]
#flag -I @VMODROOT/c
'
	out := vfmt('hash_directive_attributes', source)
	assert out == expected, out
	assert vfmt('hash_directive_attributes_twice', out) == out
}

fn test_formatter_preserves_boolean_compound_assignment_spelling() {
	source := 'fn update() {
	mut flag := true
	flag ||= false
	flag &&= true
	mut flags := [true]
	flags[0] ||= false
	flags[0] &&= true
	mut bits := 1
	bits |= 2
	bits &= 1
}
'
	out := vfmt('boolean_compound_assignments', source)
	assert out == source, out
	assert vfmt('boolean_compound_assignments_twice', out) == out
}

fn test_formatter_preserves_capture_and_shared_parameter_qualifiers() {
	out := vfmt('capture_and_shared_qualifiers',
		'struct St {}\n\nfn (shared receiver St) use(shared value St) {}\n\nfn consume[T](value T) {}\n\nfn main() {\n\tatomic counter := 0\n\tcallback := fn [mut item, atomic counter, shared state] () {}\n\tconsume[[]int]([]int{})\n\t_ = callback\n}\n')
	assert out.contains('fn (shared receiver St) use(shared value St)'), out
	assert out.contains('atomic counter := 0'), out
	assert out.contains('fn [mut item, atomic counter, shared state] ()'), out
	assert out.contains('consume[[]int]([]int{})'), out
	assert vfmt('capture_and_shared_qualifiers_twice', out) == out
}

fn test_formatter_preserves_atomic_parameter_qualifiers() {
	source := 'fn update(atomic value u64) {\n\t_ = value\n}\n'
	out := vfmt('atomic_parameter_qualifier', source)
	assert out == source, out
	assert vfmt('atomic_parameter_qualifier_twice', out) == out
}

fn test_formatter_preserves_comment_only_files() {
	source := '/*\nmodule acommentedmodule\n*/\n'
	out := vfmt_with_options('comment_only_file', source, FormatOptions{})
	assert out == source, out
	assert vfmt_with_options('comment_only_file_twice', out, FormatOptions{}) == out
}

fn test_formatter_preserves_comptime_match() {
	source := "\$match @OS {\n\t'linux' {\n\t\tconst platform = 'linux'\n\t}\n\t\$else {\n\t\tconst platform = 'other'\n\t}\n}\n\nfn main() {\n\tvalue := \$match @OS {\n\t\t'linux' { 'linux' }\n\t\t\$else { 'other' }\n\t}\n\t_ = value\n}\n"
	out := vfmt('comptime_match', source)
	assert out.count('\$match @OS') == 2, out
	assert out.contains("const platform = 'linux'"), out
	assert out.contains("const platform = 'other'"), out
	assert vfmt('comptime_match_twice', out) == out
}

fn test_formatter_preserves_inclusive_match_ranges() {
	source := "fn classify(value int) string {\n\treturn match value {\n\t\t32...126 { 'printable' }\n\t\telse { 'other' }\n\t}\n}\n"
	out := vfmt('inclusive_match_range', source)
	assert out.contains('32...126 {'), out
	assert !out.contains('32 .. 126'), out
	assert vfmt('inclusive_match_range_twice', out) == out
}

fn test_formatter_preserves_lifetime_annotations() {
	source := 'interface Reader {\n\tread[^a](value &^a string) &^a string\n}\n\nstruct Borrowed[^a, T] {\n\tvalue &^a T\n}\n\nfn Borrowed.new[^a, T](value &^a T) Borrowed[^a, T] {\n\treturn Borrowed[^a, T]{\n\t\tvalue: value\n\t}\n}\n\nfn (borrowed &^a Borrowed[^a, T]) get[^a]() &^a T {\n\treturn borrowed.value\n}\n'
	out := vfmt('lifetime_annotations', source)
	assert out.contains('read[^a](value &^a string) &^a string'), out
	assert out.contains('struct Borrowed[^a, T]'), out
	assert out.contains('value &^a T'), out
	assert out.contains('fn Borrowed.new[^a, T](value &^a T) Borrowed[^a, T]'), out
	assert out.contains('return Borrowed[^a, T]{'), out
	assert out.contains('fn (borrowed &^a Borrowed[^a, T]) get[^a]() &^a T'), out
	assert vfmt('lifetime_annotations_twice', out) == out
}

fn test_formatter_ignores_vfmt_directives_inside_strings() {
	out := vfmt('vfmt_directives_in_strings',
		"fn main(){\n\toff := '// vfmt off'\n\ton := '// vfmt on'\n\tprintln(off + on)\n}\n\nfn format_me(){println('yes')}\n")
	assert out.contains("off := '// vfmt off'"), out
	assert out.contains("on := '// vfmt on'"), out
	assert out.contains("fn format_me() {\n\tprintln('yes')\n}"), out
}

fn test_formatter_preserves_go_legacy_dollar_builtins_and_bodyless_functions() {
	bodyless := vfmt('bodyless_functions',
		'fn plain(value usize) usize\nfn C.c_call(value int) int\nfn JS.js_call(value int) int\n')
	assert bodyless.contains('fn plain(value usize) usize'), bodyless
	assert bodyless.contains('fn C.c_call(value int) int'), bodyless
	assert bodyless.contains('fn JS.js_call(value int) int'), bodyless
	assert !bodyless.contains('fn C.plain'), bodyless
	assert vfmt('bodyless_functions_twice', bodyless) == bodyless

	concurrency := vfmt('go_and_spawn',
		'fn work() {}\n\nfn main() {\n\tgo work()\n\tspawn work()\n}\n')
	assert concurrency.contains('go work()'), concurrency
	assert concurrency.contains('spawn work()'), concurrency
	assert vfmt('go_and_spawn_twice', concurrency) == concurrency

	dollar := vfmt('legacy_dollar_builtins',
		"fn main() {\n\t// vfmt off\n\tn := 1\n\tassert \$typeof(n).name == 'int'\n\tassert \$sizeof(n) > 0\n\tassert !\$isreftype[int]()\n\tassert \$dump(n) == n\n\t// vfmt on\n}\n")
	assert dollar.contains('\$typeof(n).name'), dollar
	assert dollar.contains('\$sizeof(n)'), dollar
	assert dollar.contains('\$isreftype[int]()'), dollar
	assert dollar.contains('\$dump(n)'), dollar
	assert dollar.ends_with('\t// vfmt on\n}\n'), dollar
	assert vfmt('legacy_dollar_builtins_twice', dollar) == dollar
}

fn test_formatter_preserves_mixed_lock_modes() {
	source := 'struct St {}

fn f(shared a St, shared b St, shared c St) {
	rlock a; lock b; rlock c {
		println(1)
	}
}
'
	out := vfmt('mixed_lock_modes', source)
	assert out.contains('lock b; rlock a, c {'), out
	assert vfmt('mixed_lock_modes_twice', out) == out
}

fn test_json_migration_rejects_declaration_collisions() {
	const_source := 'import json

const json2 = 1

fn f() {
	println(json.encode(1))
}
'
	const_out := vfmt_with_json_migration('const_collision', const_source)
	assert const_out.contains('import json\n'), const_out
	assert const_out.contains('json.encode('), const_out
	assert !const_out.contains('import json2'), const_out

	global_source := 'import json

__global (
	json2 = 1
)

fn f() {
	println(json.encode(1))
}
'
	global_out := vfmt_with_json_migration('global_collision', global_source)
	assert global_out.contains('import json\n'), global_out
	assert global_out.contains('json.encode('), global_out
	assert !global_out.contains('import json2'), global_out

	fn_source := 'import json

fn json2() {}

fn f() {
	println(json.encode(1))
}
'
	fn_out := vfmt_with_json_migration('function_collision', fn_source)
	assert fn_out.contains('import json\n'), fn_out
	assert fn_out.contains('json.encode('), fn_out
	assert !fn_out.contains('import json2'), fn_out
}

fn test_json_migration_rejects_declaration_collisions_in_comptime_branches() {
	declarations := {
		'const':    'const json2 = 1'
		'global':   '__global json2 = 1'
		'function': 'fn json2() {}'
	}
	for name, declaration in declarations {
		source := 'import json

\$if custom ? {
	${declaration}
}

fn f() {
	println(json.encode(1))
}
'
		out := vfmt_with_json_migration('comptime_${name}_collision', source)
		assert out.contains('import json\n'), out
		assert out.contains('json.encode('), out
		assert !out.contains('import json2'), out
		assert vfmt_with_json_migration('comptime_${name}_collision_twice', out) == out
	}
}

fn test_json_migration_rejects_aliased_qualifier_local_collisions() {
	source := 'import json
import json2 as j2

struct Foo {}

fn already_new() {
	println(j2.encode(1))
}

fn encode_legacy() string {
	j2 := Foo{}
	return json.encode(j2)
}
'
	out := vfmt_with_json_migration('aliased_qualifier_local_collision', source)
	assert out.contains('import json\n'), out
	assert out.contains('import json2 as j2'), out
	assert out.contains('j2 := Foo{}'), out
	assert out.contains('return json.encode(j2)'), out
	assert !out.contains('return j2.encode(j2'), out
	assert vfmt_with_json_migration('aliased_qualifier_local_collision_twice', out) == out
}

fn test_json_migration_skips_vfmt_disabled_regions() {
	source := 'import json

fn f() {
	// vfmt off
	println(json.encode(1))
	// vfmt on
}
'
	out := vfmt_with_json_migration('disabled_json_migration', source)
	assert out.contains('import json\n'), out
	assert out.contains('json.encode(1)'), out
	assert !out.contains('import json2'), out
	assert !out.contains('json2.encode'), out
	assert vfmt_with_json_migration('disabled_json_migration_twice', out) == out
}

fn test_formatter_expands_grouped_const_fields_with_comments() {
	source := 'const (
	// pi documents pi
	pi = 3.14
	// phi documents phi
	phi = 1.618
)
'
	out := vfmt('grouped_const_comments', source)
	assert out.contains('// pi documents pi\nconst pi = 3.14'), out
	assert out.contains('// phi documents phi\nconst phi = 1.618'), out
	assert !out.contains('const ('), out
	assert !out.contains('pi =\n'), out
	second := vfmt('grouped_const_comments_twice', out)
	assert second == out, second
}

fn test_formatter_preserves_typed_map_entries_and_typeof_array_init() {
	map_source := "fn f() {\n\tm := map[string]int{'a': 1}\n\tprintln(m)\n}\n"
	map_out := vfmt('typed_map_entries', map_source)
	assert map_out.contains("map[string]int{\n\t\t'a': 1\n\t}"), map_out
	assert vfmt('typed_map_entries_twice', map_out) == map_out

	array_source := 'fn f() {
	fixed := [1, 2, 3]!
	dyn := []typeof(fixed[0]){}
	println(dyn)
}
'
	array_out := vfmt('typeof_array_init', array_source)
	assert array_out.contains('[]typeof(fixed[0]){}'), array_out
	assert vfmt('typeof_array_init_twice', array_out) == array_out
}

fn test_formatter_rewrites_legacy_it_only_in_array_init_expression() {
	source := 'fn f() {
	it := 3
	a := []int{len: it, cap: it + 1, init: it}
	println(a)
}
'
	out := vfmt('array_init_legacy_it_scope', source)
	assert out.contains('[]int{len: it, cap: it + 1, init: index}'), out
	assert vfmt('array_init_legacy_it_scope_twice', out) == out
}

fn test_formatter_preserves_multi_variable_c_style_loop_headers() {
	source := 'fn f() {
	L4: for a, b := 0, 10; a < 4; a++, b-- {
		if a < 2 {
			continue L4
		}
		break L4
	}
}
'
	out := vfmt('multi_variable_c_style_loop', source)
	assert out.contains('L4: for a, b := 0, 10; a < 4; a++, b-- {'), out
	assert !out.contains('\n\t{\n\t\tmut a, b := 0, 10'), out
	assert vfmt('multi_variable_c_style_loop_twice', out) == out
}

fn test_formatter_preserves_postfix_assignment_attributes() {
	source := 'fn f() {
	x := [1, 2, 3] @[freed]
	unsafe {
		x.free()
	}
}
'
	out := vfmt('postfix_assignment_attribute', source)
	assert out.contains('x := [1, 2, 3] @[freed]'), out
	assert vfmt('postfix_assignment_attribute_twice', out) == out
}

fn test_formatter_preserves_branch_prediction_builtins() {
	source := 'fn f(value int) bool {
	if _likely_(value > 0) {
		return true
	}
	return _unlikely_(value < 0)
}
'
	out := vfmt('branch_prediction_builtins', source)
	assert out.contains('if _likely_(value > 0) {'), out
	assert out.contains('return _unlikely_(value < 0)'), out
	assert vfmt('branch_prediction_builtins_twice', out) == out
}

fn test_formatter_emits_mut_before_static_declarations() {
	source := '@[unsafe]
fn next() int {
	mut static value := 1
	value++
	return value
}
'
	out := vfmt('mutable_static_declaration', source)
	assert out.contains('mut static value := 1'), out
	assert !out.contains('static mut value'), out
	assert vfmt('mutable_static_declaration_twice', out) == out
}

fn test_formatter_preserves_global_grouping_and_field_comments() {
	source := '@[c_extern]
__global errno C.int

__global enabled = bool(true)

__global (
	// typed global docs
	typed int
	// initialized global docs
	initialized = int(2)
)
'
	out := vfmt('global_grouping_and_comments', source)
	assert out.contains('@[c_extern]\n__global errno C.int'), out
	assert out.contains('__global enabled = bool(true)'), out
	assert out.contains('__global (\n\t// typed global docs\n\ttyped       int'), out
	assert out.contains('\t// initialized global docs\n\tinitialized = int(2)'), out
	assert !out.contains('__global (\n\terrno C.int'), out
	assert !out.contains('initialized =\n'), out
	assert vfmt('global_grouping_and_comments_twice', out) == out
}

fn test_formatter_preserves_js_string_prefixes() {
	source := "fn f() {\n\ts := js'hello V'\n\tassert s == js'hello V'\n}\n"
	out := vfmt('js_string_prefixes', source)
	assert out.contains("s := js'hello V'"), out
	assert out.contains("s == js'hello V'"), out
	assert vfmt('js_string_prefixes_twice', out) == out
}

fn test_formatter_preserves_sql_body() {
	out := vfmt('sql_body',
		'struct User {\n\tid int\n}\n\nfn f(db DB) {\n\t_ := sql db {\n\t\tselect from User where id == 1\n\t}\n}\n')
	assert out.contains('sql db {\n\t\tselect from User where id == 1\n\t}'), out
}

fn test_formatter_preserves_mut_type_check() {
	out := vfmt('mut_type_check',
		'fn f(mut writer io.Writer) {\n\tif mut writer is os.File {\n\t\twriter.flush()\n\t}\n}\n')
	assert out.contains('if mut writer is os.File {'), out
}

fn test_json_migration_keeps_unsafe_legacy_uses() {
	fixture_dir := os.join_path(@VEXEROOT, 'vlib/v/fmt/tests')
	mut files := os.walk_ext(fixture_dir, '_keep.vv')
	files = files.filter(os.file_name(it).starts_with('json_migrate_'))
	assert files.len > 0
	for path in files {
		source := os.read_file(path) or { panic(err) }
		out := vfmt_file_with_json_migration(path)
		if source.contains('import json\n') {
			assert out.contains('import json\n'), os.file_name(path)
		}
		if source.contains('json.encode') {
			assert out.contains('json.encode'), os.file_name(path)
		}
		if source.contains('json.decode') {
			assert out.contains('json.decode'), os.file_name(path)
		}
	}
}

fn test_json_migration_matches_formatter_fixtures() {
	fixture_dir := os.join_path(@VEXEROOT, 'vlib/v/fmt/tests')
	mut inputs := os.walk_ext(fixture_dir, '_input.vv')
	inputs = inputs.filter(os.file_name(it).starts_with('json_migrate_'))
	assert inputs.len > 0
	for input in inputs {
		expected_path := input.replace('_input.vv', '_expected.vv')
		expected := os.read_file(expected_path) or { panic(err) }
		actual := vfmt_file_with_json_migration(input)
		assert actual == expected, os.file_name(input)
	}
}

fn test_source_preservation_matches_formatter_fixtures() {
	fixture_dir := os.join_path(@VEXEROOT, 'vlib/v/fmt/tests')
	for name in ['conditional_import', 'struct_decl_with_comments', 'vfmt_off_vfmt_on_with_crlf'] {
		input := os.join_path(fixture_dir, '${name}_input.vv')
		expected_path := os.join_path(fixture_dir, '${name}_expected.vv')
		expected := os.read_file(expected_path) or { panic(err) }
		actual := vfmt_file_with_json_migration(input)
		assert actual == expected, name
	}
	for name in ['global_keep.vv', 'language_prefixes_keep.vv', 'string_raw_and_cstr_keep.vv'] {
		path := os.join_path(fixture_dir, name)
		expected := os.read_file(path) or { panic(err) }
		actual := vfmt_file_with_json_migration(path)
		assert actual == expected, name
	}
}

fn test_channel_ops() {
	out := vfmt('chan', 'fn f() {\n\tx := <-ch\n\tch <- 5\n}\n')
	assert out.contains('x := <-ch')
	assert out.contains('ch <- 5')
	assert !out.contains('->')
}

fn test_comptime_selector() {
	out := vfmt('ctsel', 'fn f[T](owned T) {\n\tdrop_owned(owned.\$(field.name))\n}\n')
	assert out.contains('owned.\$(field.name)')
}

fn test_comptime_method_shorthand_selector() {
	source := 'struct Dummy {}\n\nfn (d Dummy) sample(x int) int {\n\treturn x + 1\n}\n\nfn main() {\n\t\$for method in Dummy.methods {\n\t\tDummy{}.\$method(1)\n\t}\n}\n'
	out := vfmt('comptime_method_shorthand', source)
	assert out.contains('Dummy{}.\$method(1)'), out
	assert !out.contains('Dummy{}.\$(method)(1)'), out
	assert vfmt('comptime_method_shorthand_twice', out) == out
}

fn test_select_compound_receive_preserves_operator() {
	// A compound receive (`x += <-ch`) must keep its operator; emitting `=` would
	// silently change program semantics.
	out := vfmt('selcompound', 'fn f(ch chan int) {
	mut x := 0
	select {
		x += <-ch {
			println(x)
		}
	}
}
')
	assert out.contains('x += <-ch'), out
	assert !out.contains('x = <-ch'), out
}

fn test_select_receive_forms() {
	out := vfmt('selforms', 'fn f(ch chan int) {
	mut x := 0
	select {
		y := <-ch {
			println(y)
		}
		x = <-ch {
			println(x)
		}
	}
}
')
	assert out.contains('y := <-ch'), out
	assert out.contains('x = <-ch'), out
}

fn test_formatter_keeps_trailing_select_comments_inside_body() {
	source := 'fn receive(ch chan int) {
	mut n := 0
	select {
		n = <-ch {
			n++
		}
		// post comment
	}
	assert n >= 0
}
'
	out := vfmt('select_trailing_comment', source)
	assert out == source, out
	assert vfmt('select_trailing_comment_twice', out) == out
}

fn test_formatter_renders_comptime_if_expressions() {
	source := 'const enable_debug = \$if debug { true } \$else { false }

fn enabled() bool {
	return \$if prod { false } \$else \$if debug { true } \$else { false }
}
'
	out := vfmt('comptime_if_expressions', source)
	assert out == source, out
	assert !out.contains('/* comptime_if */'), out
	assert vfmt('comptime_if_expressions_twice', out) == out
}

fn test_generics_and_interface() {
	out := vfmt('gen',
		'pub struct Stack[T] {\nmut:\n\tdata []T\n}\n\ninterface Reader {\n\tread(mut buf []u8) !int\nmut:\n\tpos int\n}\n')
	assert out.contains('struct Stack[T] {')
	assert out.contains('interface Reader {')
	assert out.contains('read(mut buf []u8) !int')
}

fn test_output_is_valid_and_idempotent() {
	src := 'module main

import os { join_path }

@[heap]
pub struct Node[T] {
pub mut:
	value T
	next  &Node[T] = unsafe { nil }
}

pub fn (mut n Node[T]) push(v T) {
	names := ["a", "b"]
	m := {"k": 1}
	for i, name in names {
		println("\${i}: \${name}")
	}
	x := if v == n.value { 1 } else { 2 }
	r := os.join_path("a", "b") or { panic(err) }
	match x {
		1, 2 { println("low") }
		else {}
	}
	assert x > 0, "positive"
}
'
	out1 := vfmt('rt', src)
	// the generated source must itself parse without diagnostics
	assert reparse_diagnostics('rt_valid', out1) == 0
	// and formatting is a fixed point
	out2 := vfmt('rt2', out1)
	assert out1 == out2
}

fn test_formatter_preserves_for_in_binder_mutability() {
	source := 'fn loops(mut values []int) {
	for mut index, _ in values {
		index++
	}
	for index, mut value in values {
		value++
		_ = index
	}
}
'
	out := vfmt('for_in_binder_mutability', source)
	assert out == source, out
	assert vfmt('for_in_binder_mutability_twice', out) == out
}

fn test_formatter_preserves_function_parameter_comments() {
	source := 'fn describe(
	// documents a
	a int // explains a
	// documents b
	b int // explains b
	// closes parameter list
) {
}
'
	expected := 'fn describe(
	// documents a
	a int, // explains a
	// documents b
	b int // explains b
	// closes parameter list
) {
}
'
	out := vfmt('function_parameter_comments', source)
	assert out == expected, out
	assert vfmt('function_parameter_comments_twice', out) == out
}

fn test_formatter_demangles_function_local_aggregate_types() {
	source := "fn local_types() {
	struct Tick {
		next  &Tick = unsafe { nil }
		value int
	}
	union Number {
		integer int
		decimal f64
	}
	struct Wrapper {
		Tick
		numbers map[string]Number
	}

	mut ticks := []Tick{}
	first := Tick{
		value: 1
	}
	ticks << Tick{
		...first
		value: 2
	}
	wrapper := Wrapper{
		Tick: first
		numbers: {
			'one': Number{
				integer: 1
			}
		}
	}
	_ = ticks
	_ = wrapper
}
"
	out := vfmt('function_local_aggregate_types', source)
	assert out == source, out
	assert !out.contains('@local@'), out
	assert vfmt('function_local_aggregate_types_twice', out) == out
}

fn test_formatter_keeps_comma_expression_branches_unbraced() {
	source := 'fn pick(var1 string, var2 string) (int, int) {
	shorter, longer := if var1.len <= var2.len {
		var1.len, var2.len
	} else {
		var2.len, var1.len
	}
	return shorter, longer
}
'
	out := vfmt('comma_expression_branches', source)
	assert out == source, out
	assert vfmt('comma_expression_branches_twice', out) == out
}

fn test_formatter_keeps_comma_expressions_in_or_block() {
	source := "fn split(attr string) (string, string) {
	name, value := attr.split_once(':') or { '', '' }
	return name, value
}
"
	out := vfmt('comma_expressions_or_block', source)
	assert out == source, out
}

fn test_formatter_keeps_multiline_interpolated_string() {
	source := "fn query(table string) string {
	return '
		SELECT
			*
		FROM
			\${table}
		;
	'
}
"
	out := vfmt('multiline_interpolated_string', source)
	assert out == source, out
	assert vfmt('multiline_interpolated_string_twice', out) == out
}

fn test_formatter_reflows_interpolation_wrapped_across_lines() {
	source := "fn log(a string, b string, c string) {
	println('prefix \${join(a,
		b, c)}')
}
"
	expected := "fn log(a string, b string, c string) {
	println('prefix \${join(a, b, c)}')
}
"
	out := vfmt('interpolation_wrapped_across_lines', source)
	assert out == expected, out
}

fn test_formatter_keeps_blank_line_after_leading_comment() {
	source := '// Copyright header

module abc

const answer = 42
'
	out := vfmt_with_options('blank_line_after_leading_comment', source, FormatOptions{})
	assert out == source, out
}

fn test_formatter_keeps_doc_comments_below_access_specifiers() {
	source := 'pub struct Foo {
	a int
	// section comment before the specifier

pub:
	// docs for b
	b int
	c int
	// docs for d
mut:
	d int
}
'
	out := vfmt('doc_comments_below_access_specifiers', source)
	assert out == source, out
	assert vfmt('doc_comments_below_access_specifiers_twice', out) == out
}

fn test_formatter_keeps_logical_operator_line_breaks() {
	source := 'fn check(found Rec, actual Rec) {
	assert found.some_long_field_name_lalalala1 == actual.some_long_field_name_lalalala1
		&& found.some_long_field_name_lalalala2 == actual.some_long_field_name_lalalala2
}
'
	out := vfmt('logical_operator_line_breaks', source)
	assert out == source, out
	assert vfmt('logical_operator_line_breaks_twice', out) == out
}

fn test_formatter_keeps_logical_chain_joined_when_source_is_one_line() {
	source := "fn check(name string, sel bool) bool {
	return sel && name !in [
		'main',
		'init',
	] && name.len > 0
}
"
	out := vfmt('logical_chain_joined', source)
	assert out == source, out
}

fn test_formatter_keeps_assignment_value_on_its_own_line() {
	source := 'fn build(a []int) []int {
	expected :=
		a.map(it * 2).filter(it > 3).map(it + 1).filter(it < 100).map(it * 3).filter(it != 7)
	return expected
}
'
	out := vfmt('assignment_value_on_own_line', source)
	assert out == source, out
	assert vfmt('assignment_value_on_own_line_twice', out) == out
}

fn test_formatter_keeps_comments_inside_multiline_interpolation_once() {
	source := "fn describe(a int, b int) string {
	x := 'line1
line2 \${a + /* keep */ b}
line3'
	return x
}
"
	out := vfmt('comments_inside_multiline_interpolation', source)
	assert out == source, out
	assert out.count('/* keep */') == 1, out
	assert vfmt('comments_inside_multiline_interpolation_twice', out) == out
}

fn test_formatter_keeps_comment_after_multiline_interpolation() {
	source := "fn describe(table string) string {
	q := '
		SELECT *
		FROM \${table}
	' // trailing comment
	return q
}
"
	out := vfmt('comment_after_multiline_interpolation', source)
	assert out == source, out
}

fn test_formatter_reflows_interpolation_wrap_when_text_only_has_escaped_newline() {
	source := "fn describe(a int, b int) string {
	return 'a\\nb \${add(a,
		b)}'
}
"
	expected := "fn describe(a int, b int) string {
	return 'a\\nb \${add(a, b)}'
}
"
	out := vfmt('escaped_newline_interpolation_wrap', source)
	assert out == expected, out
}

fn test_formatter_keeps_expanded_array_after_a_single_line_sibling() {
	source := 'fn build() []Outer {
	return [
		Outer{
			list: [Elem{
				a: 1
			}]
		},
		Outer{
			list: [
				Elem{
					a: 2
				},
			]
		},
	]
}
'
	out := vfmt('expanded_array_after_single_line_sibling', source)
	assert out == source, out
	assert vfmt('expanded_array_after_single_line_sibling_twice', out) == out
}

fn test_formatter_keeps_rows_of_small_nested_arrays_on_one_line() {
	source := 'fn rows() [][]int {
	return [[1, 2], [3, 4], [5, 6]]
}
'
	out := vfmt('rows_of_small_nested_arrays', source)
	assert out == source, out
}

// A block comment's continuation lines already carry their own leading whitespace, so the
// formatter must write them through as they are. Indenting them again added one level per run:
// `v fmt` was not a fixed point, and each further run pushed the body one tab deeper.
fn test_formatter_keeps_block_comment_body_indentation_stable() {
	source := "fn probe() {\n\t/*\n\tfirst line\n\tsecond line\n\t*/\n\tprintln('x')\n}\n"
	out := vfmt('block_comment_indent', source)
	assert out == source, out
	twice := vfmt('block_comment_indent_twice', out)
	assert twice == out, twice
	thrice := vfmt('block_comment_indent_thrice', twice)
	assert thrice == out, thrice
}

// A comment after a match branch's closing brace belongs to that branch. Emitting it as the next
// branch's leading comment moved it onto its own line, and the following run then read it as a
// commented branch and inserted a blank line before it — so formatting twice differed from once.
fn test_formatter_keeps_a_trailing_comment_on_a_match_branch() {
	source := "fn pick(n int) string {\n\tmatch n {\n\t\t1 {\n\t\t\treturn 'a'\n\t\t} // leave it blank\n\t\telse {\n\t\t\treturn ''\n\t\t}\n\t}\n}\n"
	out := vfmt('match_branch_trailing_comment', source)
	assert out == source, out
	assert vfmt('match_branch_trailing_comment_twice', out) == out
}

// A blank separator line must carry no indentation. Writing it left a line of whitespace, which
// V source never carries and which the next run read back differently, so the formatter was not a
// fixed point.
fn test_formatter_writes_blank_lines_without_indentation() {
	source := 'fn probe() {\n\tassert 1 == 1 // first\n\tassert 2 == 2\n}\n'
	out := vfmt('blank_line_indentation', source)
	for line in out.split('\n') {
		assert line.trim_space() != '' || line == '', 'a blank line must carry no whitespace, got `${line}`'
	}
	assert vfmt('blank_line_indentation_twice', out) == out
}

// An `assert` whose condition carries the statement's trailing comment is already terminated by
// that comment, so terminating it again left a stray separator line after every commented
// `assert`. Every other statement kind already guarded this.
fn test_formatter_does_not_separate_a_commented_assert_from_the_next_statement() {
	source := 'fn p() {\n\tassert 1 == 1 // c\n\tassert 2 == 2\n}\n'
	out := vfmt('commented_assert', source)
	assert out == source, out
	assert vfmt('commented_assert_twice', out) == out

	// a blank line the source did have is still kept
	spaced := 'fn p() {\n\tassert 1 == 1 // c\n\n\tassert 2 == 2\n}\n'
	spaced_out := vfmt('commented_assert_spaced', spaced)
	assert spaced_out == spaced, spaced_out
}

// A `match` range pattern took the parser's position when its node was built, which by then was
// already past the branch's `{`. The formatter then read a comment written after that brace as
// directly following the pattern and pulled it above the brace — and the next run re-split the
// pattern list around it.
fn test_formatter_keeps_a_trailing_comment_on_a_range_match_branch() {
	source := "fn f(n int) string {\n\tmatch n {\n\t\t48...57, 97...122 { // 0-9a-z\n\t\t\treturn 'alnum'\n\t\t}\n\t\telse {\n\t\t\treturn ''\n\t\t}\n\t}\n}\n"
	out := vfmt('range_branch_comment', source)
	assert out == source, out
	assert vfmt('range_branch_comment_twice', out) == out
}

// A bare `return` closing an inline block took the parser's position when its node was built,
// which by then was past that block's `}`. The formatter read the statement's trailing comment as
// directly following the `return` and moved it inside the braces, leaving the `}` inside the
// comment and the file unparseable.
fn test_formatter_keeps_a_trailing_comment_outside_an_inline_or_block() {
	source := "fn get(k string) !bool {\n\treturn k != ''\n}\n\nfn probe() {\n\thelp_enabled := get('help') or { return } // ignore the error\n\tif help_enabled {\n\t\tprintln('yes')\n\t}\n}\n"
	out := vfmt('inline_or_trailing_comment', source)
	assert out == source, out
	assert !out.contains('return // ignore the error }'), out
	assert vfmt('inline_or_trailing_comment_twice', out) == out
}

// V spells a closure `fn [captures] [T](params)`. The formatter wrote the generic list first,
// producing `fn[T] [captures] (params)`, which does not parse; the run after that read the
// captures as the generic list and dropped whatever the two disagreed on, so a `mut` capture was
// silently lost.
fn test_formatter_keeps_closure_capture_and_generic_list_order() {
	source := 'fn run[T](items []T) {\n\tch := chan T{}\n\tmut total := 0\n\tspawn fn [ch, mut total] [T]() {\n\t\tfor _ in ch {\n\t\t\ttotal++\n\t\t}\n\t}()\n\tfor item in items {\n\t\tch <- item\n\t}\n}\n'
	out := vfmt('closure_capture_generics', source)
	assert out == source, out
	assert !out.contains('fn[T]'), out
	assert out.contains('mut total'), out
	assert vfmt('closure_capture_generics_twice', out) == out
}

// `break` and `continue` had the same wrong span as `return`: the node took the parser's position,
// already past the `}` of an inline block, so the statement's trailing comment was moved inside
// the braces and the `}` ended up commented out.
fn test_formatter_keeps_a_trailing_comment_outside_an_inline_break_or_continue() {
	source := 'fn f(items []int) int {\n\tmut n := 0\n\tfor i in items {\n\t\tw := pick(i) or { continue } // only numeric\n\t\tif w == 0 {\n\t\t\tbreak\n\t\t}\n\t\tn += w\n\t}\n\treturn n\n}\n\nfn pick(i int) ?int {\n\treturn i\n}\n'
	out := vfmt('inline_continue_comment', source)
	assert out == source, out
	assert !out.contains('continue // only numeric }'), out
	assert vfmt('inline_continue_comment_twice', out) == out
}

// Implied imports matched the module directory through `os.is_dir`, which answers the filesystem.
// On a case-insensitive one (macOS, Windows) `vlib/Time` is the `time` module, so a static call on
// a type named `Time` implied `import Time` — and the formatter wrote that bogus import into the
// file, breaking the build. On a case-sensitive filesystem this never fired, so the assert below
// only bites where the bug lived.
fn test_formatter_does_not_imply_an_import_from_a_type_named_like_a_module() {
	source := 'module time\n\npub struct Time {\n\tunix i64\n}\n\npub fn Time.new(unix i64) Time {\n\treturn Time{\n\t\tunix: unix\n\t}\n}\n\npub fn now() Time {\n\treturn Time.new(0)\n}\n'
	out := vfmt('implied_import_type_name', source)
	assert !out.contains('import Time'), out
	assert out == source, out
}

// A struct embed and a field whose name matches its type (`thread thread`) are stored the same
// way — `value == typ` — so the formatter collapsed the second into the first and wrote a bare
// `thread` embed, which no longer compiles. A user attribute must not be mistaken for that parser
// metadata either.
fn test_formatter_keeps_a_field_named_like_its_type() {
	source := 'struct Base {\n\tid int\n}\n\nstruct Child {\n\tBase\n\tname string\n}\n\nstruct Named {\n\tthread thread\n}\n\nstruct AttributedNamed {\n\tthread thread @[__v3_embedded_field]\n}\n'
	out := vfmt('field_named_like_type', source)
	assert out == source, out
	assert vfmt('field_named_like_type_twice', out) == out
}

// V spells a function type with a space before the parameter list. The type system stores it
// without one (`fn(int) int`), so a type rendered straight from the internal name reformatted
// every function type in the file — in declarations, fields, parameters and return positions
// alike.
fn test_formatter_keeps_the_space_in_a_function_type() {
	source := 'pub type Cb = fn (a int, b string) !

struct Holder {
	cb     fn (int) int = unsafe { nil }
	cbs    []fn (int)
	m      map[string]fn (int) int
	opt    ?fn (int)
	nested fn (cb fn (int) int) fn (int) int
}

fn takes(cb fn (int) int) fn (int) int {
	return cb
}
'
	out := vfmt('fn_type_space', source)
	assert out == source, out
	assert !out.contains('fn('), out
}

// The optional-flag marker of a `$if` is written detached (`$if flag ? {`). The condition is
// stored without that space so flag lookups match on it, so the formatter has to put it back.
fn test_formatter_keeps_the_space_before_a_comptime_flag_marker() {
	source := "fn probe() {\n\t\$if !v3_no_parallel ? {\n\t\tprintln('a')\n\t}\n\t\$if linux {\n\t\tprintln('b')\n\t}\n}\n"
	out := vfmt('comptime_flag_marker', source)
	assert out == source, out
	assert !out.contains('parallel?'), out
	assert vfmt('comptime_flag_marker_twice', out) == out
}

// A NUL byte must be written as `\x00`, not `\0`. V's octal escape absorbs the digits that
// follow, so `\0` before an octal digit re-parses as a single escape and the formatter would
// silently rewrite the string's bytes: `'x\x0041y'` (x, NUL, `4`, `1`, y) came back as
// `'x\041y'`, which is `x!y`.
fn test_formatter_keeps_nul_escape_unambiguous() {
	source := "fn main() {\n\ta := 'x\\x0041y'\n\tb := 'p\\x00q'\n\tc := `\\x00`\n\tprintln(a + b + c.str())\n}\n"
	out := vfmt('nul_escape', source)
	assert out.contains("'x\\x0041y'"), out
	assert !out.contains('\\041'), out
	assert out.contains("'p\\x00q'"), out
	assert !out.contains("'p\\0q'"), out
	assert out.contains('`\\x00`'), out
	assert vfmt('nul_escape_twice', out) == out
}
