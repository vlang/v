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

fn test_formatter_preserves_capture_and_shared_parameter_qualifiers() {
	out := vfmt('capture_and_shared_qualifiers',
		'struct St {}\n\nfn (shared receiver St) use(shared value St) {}\n\nfn consume[T](value T) {}\n\nfn main() {\n\tatomic counter := 0\n\tcallback := fn [mut item, atomic counter, shared state] () {}\n\tconsume[[]int]([]int{})\n\t_ = callback\n}\n')
	assert out.contains('fn (shared receiver St) use(shared value St)'), out
	assert out.contains('atomic counter := 0'), out
	assert out.contains('fn [mut item, atomic counter, shared state] ()'), out
	assert out.contains('consume[[]int]([]int{})'), out
	assert vfmt('capture_and_shared_qualifiers_twice', out) == out
}

fn test_formatter_preserves_comptime_match() {
	source := "\$match @OS {\n\t'linux' {\n\t\tconst platform = 'linux'\n\t}\n\t\$else {\n\t\tconst platform = 'other'\n\t}\n}\n\nfn main() {\n\tvalue := \$match @OS {\n\t\t'linux' { 'linux' }\n\t\t\$else { 'other' }\n\t}\n\t_ = value\n}\n"
	out := vfmt('comptime_match', source)
	assert out.count('\$match @OS') == 2, out
	assert out.contains("const platform = 'linux'"), out
	assert out.contains("const platform = 'other'"), out
	assert vfmt('comptime_match_twice', out) == out
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
	for name in ['language_prefixes_keep.vv', 'string_raw_and_cstr_keep.vv'] {
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
