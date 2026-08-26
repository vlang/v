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

fn C.abc(a int, b []int) int

fn abc(a int) int
'
	c_out := vfmt_with_options('new_int_c_decl', c_source, is_new_int: true)
	assert c_out.contains('fn C.abc(a i32, b []i32) i32'), c_out
	assert c_out.contains('fn abc(a int) int'), c_out

	translated_source := '@[translated]
module translated

fn convert(value int) int {
	return int(value)
}
'
	translated_out := vfmt_with_options('new_int_translated', translated_source,
		is_new_int: true)
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
	source := "struct Item {\n\tname string\n}\n\nfn scan(items []Item, ch chan Item) {\n\tfor flag in items {\n\t\tprintln(flag.name)\n\t}\n\tselect {\n\t\ttime := <-ch {\n\t\t\tprintln(time.name)\n\t\t}\n\t}\n}\n\nfn fields[T]() {\n\t\$for json in T.fields {\n\t\tprintln(json.name)\n\t}\n}\n"
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
	assert out.contains('item []struct {'), out
	assert out.count('// keep anonymous field comment') == 1, out
	assert out.contains('choice union { number int }'), out
	assert out.contains('value struct{ foo string }'), out
	assert out.contains("accept(struct { foo: 'ok' })"), out
	assert vfmt('anonymous_aggregate_types_twice', out) == out
}

fn test_formatter_preserves_mutable_match_subjects() {
	source := "fn update(mut value int) {\n\tmatch mut value {\n\t\tint {\n\t\t\tvalue++\n\t\t}\n\t}\n}\n"
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

fn test_rune_literal_escaping() {
	source := "fn f() {\n\tprintln(`\\n`)\n\tprintln(`\\``)\n}\n"
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

	comments := 'fn f() {
	values := {
		\'a\': 1 // after
		// between
		\'b\': 2
		// post
	}
}
'
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
	source := 'fn classify(value int) string {\n\treturn match value {\n\t\t32...126 { \'printable\' }\n\t\telse { \'other\' }\n\t}\n}\n'
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
		'fn main() {\n\t// vfmt off\n\tn := 1\n\tassert \$typeof(n).name == \'int\'\n\tassert \$sizeof(n) > 0\n\tassert !\$isreftype[int]()\n\tassert \$dump(n) == n\n\t// vfmt on\n}\n')
	assert dollar.contains('\$typeof(n).name'), dollar
	assert dollar.contains('\$sizeof(n)'), dollar
	assert dollar.contains('\$isreftype[int]()'), dollar
	assert dollar.contains('\$dump(n)'), dollar
	assert dollar.ends_with("\t// vfmt on\n}\n"), dollar
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

fn test_formatter_keeps_comments_before_grouped_const_fields() {
	source := 'const (
	// pi documents pi
	pi = 3.14
	// phi documents phi
	phi = 1.618
)
'
	out := vfmt('grouped_const_comments', source)
	assert out.contains('\t// pi documents pi\n\tpi = 3.14'), out
	assert out.contains('\t// phi documents phi\n\tphi = 1.618'), out
	assert !out.contains('pi =\n'), out
	assert vfmt('grouped_const_comments_twice', out) == out
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
	assert out.contains('L4:\n\tfor a, b := 0, 10; a < 4; a++, b-- {'), out
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
