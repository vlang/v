import os

const vexe = @VEXE
const vfmt_test_tdir = os.join_path(os.vtmp_dir(), 'vfmt_test_25005')

fn testsuite_begin() {
	os.rmdir_all(vfmt_test_tdir) or {}
	os.mkdir_all(vfmt_test_tdir)!
}

fn testsuite_end() {
	os.rmdir_all(vfmt_test_tdir) or {}
}

fn test_fmt_keeps_invalid_assert_source_unchanged() {
	source_path := os.join_path(vfmt_test_tdir, 'invalid_assert_message.v')
	original := "fn main() {\n\tassert false 'bye'\n}\n"
	os.write_file(source_path, original)!

	res := os.execute('${os.quoted_path(vexe)} fmt -w ${os.quoted_path(source_path)}')

	assert res.exit_code != 0, res.output
	assert res.output.contains('unexpected string `bye`, expecting `,`'), res.output
	assert os.read_file(source_path)! == original
}

fn test_fmt_preferences_respect_vflags() {
	source_path := os.join_path(vfmt_test_tdir, 'vflags_backend_js.v')
	os.write_file(source_path, "fn main() {\n\tx := 'abc'.str\n}\n")!

	old_vflags := os.getenv('VFLAGS')
	defer {
		if old_vflags == '' {
			os.unsetenv('VFLAGS')
		} else {
			os.setenv('VFLAGS', old_vflags, true)
		}
	}

	os.unsetenv('VFLAGS')
	warmup_res := os.execute('${os.quoted_path(vexe)} fmt -help')
	assert warmup_res.exit_code == 0, warmup_res.output

	os.setenv('VFLAGS', '-b js', true)
	res := os.execute('${os.quoted_path(vexe)} fmt ${os.quoted_path(source_path)}')

	assert res.exit_code == 0, res.output
	assert res.output.contains("x := 'abc'.str"), res.output
	assert !res.output.contains("x := c'abc'"), res.output
}

fn test_fmt_uses_v3_formatter() {
	source_path := os.join_path(vfmt_test_tdir, 'v3_formatter.v')
	os.write_file(source_path, 'fn main(){println("v3")}\n')!

	res := os.execute('${os.quoted_path(vexe)} fmt -verbose ${os.quoted_path(source_path)}')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v3.gen.v over file:'), res.output
	assert res.output.contains("fn main() {\n\tprintln('v3')\n}"), res.output
}

fn test_fmt_debug_reports_v3_node_kinds() {
	source_path := os.join_path(vfmt_test_tdir, 'v3_formatter_debug.v')
	os.write_file(source_path, 'fn main() { println(1) }\n')!

	res := os.execute('${os.quoted_path(vexe)} fmt -debug ${os.quoted_path(source_path)}')

	assert res.exit_code == 0, res.output
	assert res.output.contains('stmt fn_decl'), res.output
	assert res.output.contains('expr call'), res.output
}

fn run_vfmt_write(name string, source string, extra_args string) (os.Result, string) {
	source_path := os.join_path(vfmt_test_tdir, '${name}.v')
	os.write_file(source_path, source) or { panic(err) }
	res :=
		os.execute('${os.quoted_path(vexe)} fmt -w -verbose ${extra_args} ${os.quoted_path(source_path)}')
	formatted := os.read_file(source_path) or { panic(err) }
	return res, formatted
}

fn test_fmt_preserves_comments_with_v3() {
	source := '// vfmt off\nfn main(){println("keep this") }\n// vfmt on\nfn format_me(){println("yes")}\n'
	res, formatted := run_vfmt_write('comments', source, '')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v3.gen.v over file:'), res.output
	assert formatted.contains('// vfmt off')
	assert formatted.contains('// vfmt on')
	assert formatted.contains('println("keep this")')
	assert formatted.contains("fn format_me() {\n\tprintln('yes')\n}"), formatted
}

fn test_fmt_keeps_regular_comments_attached_with_v3() {
	source := '// docs\nfn main(){\n\tx := 1 // inline\n\t_ = x\n}\n'
	res, formatted := run_vfmt_write('regular_comments', source, '')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v3.gen.v over file:'), res.output
	assert formatted.starts_with('// docs\nfn main() {')
	assert formatted.contains('x := 1 // inline')
}

fn test_fmt_preserves_comment_only_files_with_v3() {
	source := '/*\nmodule acommentedmodule\n*/\n'
	res, formatted := run_vfmt_write('comment_only_file', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('comment_only_file_twice', formatted,
		'')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_atomic_parameter_qualifiers_with_v3() {
	source := 'fn update(atomic value u64) {\n\t_ = value\n}\n'
	res, formatted := run_vfmt_write('atomic_parameter_qualifier', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('atomic_parameter_qualifier_twice',
		formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_comptime_if_with_v3() {
	source := "fn main(){\n\t\$if windows {\n\t\tprintln('windows')\n\t} \$else {\n\t\tprintln('other')\n\t}\n}\n"
	res, formatted := run_vfmt_write('comptime_if', source, '')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v3.gen.v over file:'), res.output
	assert formatted.contains('$if windows')
	assert formatted.contains("println('windows')")
	assert formatted.contains("println('other')")
}

fn test_fmt_preserves_comptime_calls_and_pseudo_variables_with_v3() {
	source := "fn main(){\n\ta := \$embed_file('./missing.txt')\n\tb := \$env('VFMT_SECRET')\n\tc := \$tmpl('./missing.html')\n\tprintln(@FILE)\n\tprintln(@LINE)\n\tprintln(@VEXE)\n\t_ = a\n\t_ = b\n\t_ = c\n}\n"
	res, formatted := run_vfmt_write('comptime_calls', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains("\$embed_file('./missing.txt')"), formatted
	assert formatted.contains("\$env('VFMT_SECRET')"), formatted
	assert formatted.contains("\$tmpl('./missing.html')"), formatted
	assert formatted.contains('println(@FILE)'), formatted
	assert formatted.contains('println(@LINE)'), formatted
	assert formatted.contains('println(@VEXE)'), formatted
}

fn test_fmt_preserves_conditional_attributes_with_v3() {
	source := "@[if formatter_missing_flag ?]\nfn guarded(){\n\tprintln('kept')\n}\n"
	res, formatted := run_vfmt_write('conditional_attribute', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('@[if formatter_missing_flag ?]'), formatted
	assert formatted.contains("println('kept')"), formatted
}

fn test_fmt_preserves_volatile_fields_with_v3() {
	source := 'struct Counter {\n\tvolatile value u64\n}\n\nfn main() {\n\tmut volatile counter := u64(0)\n\t_ = counter\n}\n'
	res, formatted := run_vfmt_write('volatile_field', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('volatile value u64'), formatted
	assert formatted.contains('mut volatile counter := u64(0)'), formatted
	second_res, formatted_twice := run_vfmt_write('volatile_field_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_fixed_array_literal_prefixes_with_v3() {
	source := 'fn main() {\n\ta := [4]f32[1, 2, 3, 4]\n\tb := [..]f32[1, 2, 3, 4]\n\t_ = a\n\t_ = b\n}\n'
	res, formatted := run_vfmt_write('fixed_array_literal_prefixes', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('a := [4]f32[1, 2, 3, 4]'), formatted
	assert formatted.contains('b := [..]f32[1, 2, 3, 4]\n'), formatted
}

fn test_fmt_preserves_closure_capture_qualifiers_with_v3() {
	source := 'fn consume[T](value T) {}\n\nfn main() {\n\tatomic counter := 0\n\tcallback := fn [mut value, atomic counter, shared state] () {}\n\tconsume[[]int]([]int{})\n\t_ = callback\n}\n'
	res, formatted := run_vfmt_write('closure_capture_qualifiers', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('atomic counter := 0'), formatted
	assert formatted.contains('fn [mut value, atomic counter, shared state] ()'), formatted
	assert formatted.contains('consume[[]int]([]int{})'), formatted
	second_res, formatted_twice := run_vfmt_write('closure_capture_qualifiers_twice', formatted,
		'')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_places_shared_before_receiver_and_parameter_names_with_v3() {
	source := 'struct St {}\n\nfn (shared receiver St) use(shared value St) {}\n'
	res, formatted := run_vfmt_write('shared_receiver_and_parameter', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('fn (shared receiver St) use(shared value St)'), formatted
}

fn test_fmt_preserves_comptime_match_with_v3() {
	source := "\$match @OS {\n\t'linux' {\n\t\tconst platform = 'linux'\n\t}\n\t\$else {\n\t\tconst platform = 'other'\n\t}\n}\n\nfn main() {\n\tvalue := \$match @OS {\n\t\t'linux' { 'linux' }\n\t\t\$else { 'other' }\n\t}\n\t_ = value\n}\n"
	res, formatted := run_vfmt_write('comptime_match', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains("\$match @OS"), formatted
	assert formatted.contains("const platform = 'linux'"), formatted
	assert formatted.contains("const platform = 'other'"), formatted
	assert formatted.count('\$match @OS') == 2, formatted
	second_res, formatted_twice := run_vfmt_write('comptime_match_twice', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_ignores_vfmt_directives_inside_strings_with_v3() {
	source := "fn main(){\n\toff := '// vfmt off'\n\ton := '// vfmt on'\n\tprintln(off + on)\n}\n\nfn format_me(){println('yes')}\n"
	res, formatted := run_vfmt_write('vfmt_directives_in_strings', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains("off := '// vfmt off'"), formatted
	assert formatted.contains("on := '// vfmt on'"), formatted
	assert formatted.contains("fn format_me() {\n\tprintln('yes')\n}"), formatted
}

fn test_fmt_preserves_go_and_spawn_keywords_with_v3() {
	source := 'fn work() {}\n\nfn main() {\n\tgo work()\n\tspawn work()\n}\n'
	res, formatted := run_vfmt_write('go_and_spawn', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('go work()'), formatted
	assert formatted.contains('spawn work()'), formatted
}

fn test_fmt_preserves_legacy_dollar_builtins_with_v3() {
	source := 'fn main() {\n\t// vfmt off\n\tn := 1\n\tassert \$typeof(n).name == \'int\'\n\tassert \$sizeof(n) > 0\n\tassert !\$isreftype[int]()\n\tassert \$dump(n) == n\n\t// vfmt on\n}\n'
	res, formatted := run_vfmt_write('legacy_dollar_builtins', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('\$typeof(n).name'), formatted
	assert formatted.contains('\$sizeof(n)'), formatted
	assert formatted.contains('\$isreftype[int]()'), formatted
	assert formatted.contains('\$dump(n)'), formatted
	assert formatted.ends_with("\t// vfmt on\n}\n"), formatted
}

fn test_fmt_preserves_bodyless_function_prefixes_with_v3() {
	source := 'fn plain(value usize) usize\nfn C.c_call(value int) int\nfn JS.js_call(value int) int\n'
	res, formatted := run_vfmt_write('bodyless_function_prefixes', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('fn plain(value usize) usize'), formatted
	assert formatted.contains('fn C.c_call(value int) int'), formatted
	assert formatted.contains('fn JS.js_call(value int) int'), formatted
	assert !formatted.contains('fn C.plain'), formatted
}

fn test_fmt_preserves_mixed_lock_modes_with_v3() {
	source := 'struct St {}

fn f(shared a St, shared b St, shared c St) {
	rlock a; lock b; rlock c {
		println(1)
	}
}
'
	res, formatted := run_vfmt_write('mixed_lock_modes', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('lock b; rlock a, c {'), formatted
	second_res, formatted_twice := run_vfmt_write('mixed_lock_modes', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_avoids_json2_declaration_collisions_with_v3() {
	source := 'import json

const json2 = 1

fn f() {
	println(json.encode(1))
}
'
	res, formatted := run_vfmt_write('json2_declaration_collision', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('import json\n'), formatted
	assert formatted.contains('json.encode('), formatted
	assert !formatted.contains('import json2'), formatted
}

fn test_fmt_keeps_comments_before_grouped_const_fields_with_v3() {
	source := 'const (
	// pi documents pi
	pi = 3.14
	// phi documents phi
	phi = 1.618
)
'
	res, formatted := run_vfmt_write('grouped_const_comments', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('\t// pi documents pi\n\tpi = 3.14'), formatted
	assert formatted.contains('\t// phi documents phi\n\tphi = 1.618'), formatted
	assert !formatted.contains('pi =\n'), formatted
}

fn test_fmt_preserves_typed_map_entries_and_typeof_array_init_with_v3() {
	source := "fn f() {\n\tm := map[string]int{'a': 1}\n\tfixed := [1, 2, 3]!\n\tdyn := []typeof(fixed[0]){}\n\tprintln(m)\n\tprintln(dyn)\n}\n"
	res, formatted := run_vfmt_write('typed_map_and_typeof_array', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains("map[string]int{'a': 1}"), formatted
	assert formatted.contains('[]typeof(fixed[0]){}'), formatted
	second_res, formatted_twice := run_vfmt_write('typed_map_and_typeof_array', formatted,
		'')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_js_string_prefixes_with_v3() {
	source_path := os.join_path(vfmt_test_tdir, 'js_string_prefixes.js.v')
	source := "fn f() {\n\ts := js'hello V'\n\tassert s == js'hello V'\n}\n"
	os.write_file(source_path, source)!

	res := os.execute('${os.quoted_path(vexe)} fmt -w -verbose ${os.quoted_path(source_path)}')
	formatted := os.read_file(source_path)!
	assert res.exit_code == 0, res.output
	assert formatted.contains("s := js'hello V'"), formatted
	assert formatted.contains("s == js'hello V'"), formatted

	second_res := os.execute('${os.quoted_path(vexe)} fmt -w ${os.quoted_path(source_path)}')
	assert second_res.exit_code == 0, second_res.output
	assert os.read_file(source_path)! == formatted
}

fn test_fmt_preserves_global_struct_sections_with_v3() {
	source := 'struct State {
	local int
__global:
	shared_value int
}
'
	res, formatted := run_vfmt_write('global_struct_section', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('__global:\n\tshared_value int'), formatted
	assert !formatted.contains('pub mut:'), formatted
	second_res, formatted_twice := run_vfmt_write('global_struct_section', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_normalizes_legacy_const_decl_assign_with_v3() {
	res, formatted := run_vfmt_write('legacy_const_decl_assign', 'const answer := 42\n', '')

	assert res.exit_code == 0, res.output
	assert formatted == 'const answer = 42\n', formatted
}

fn test_fmt_honors_new_int_with_v3() {
	c_source := 'module main

fn C.abc(a int, b []int) int

fn abc(a int) int
'
	c_res, c_formatted := run_vfmt_write('new_int_c_decl', c_source, '-new_int')
	assert c_res.exit_code == 0, c_res.output
	assert c_formatted.contains('fn C.abc(a i32, b []i32) i32'), c_formatted
	assert c_formatted.contains('fn abc(a int) int'), c_formatted

	translated_source := '@[translated]
module translated

fn convert(value int) int {
	return int(value)
}
'
	translated_res, translated_formatted := run_vfmt_write('new_int_translated',
		translated_source, '-new_int')
	assert translated_res.exit_code == 0, translated_res.output
	assert translated_formatted.contains('fn convert(value i32) i32'), translated_formatted
	assert translated_formatted.contains('return i32(value)'), translated_formatted
}

fn test_fmt_preserves_function_exit_defer_with_v3() {
	source := "fn cleanup() {\n\tdefer\n\t(fn)\n\t{\n\t\tprintln('done')\n\t}\n\tprintln('after')\n}\n"
	expected := "fn cleanup() {\n\tdefer(fn) {\n\t\tprintln('done')\n\t}\n\tprintln('after')\n}\n"
	res, formatted := run_vfmt_write('function_exit_defer', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == expected, formatted
	second_res, formatted_twice := run_vfmt_write('function_exit_defer', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_reescapes_control_bytes_with_v3() {
	source := "fn main() {\n\tprint('\\a\\b\\f\\v\\x01\\x1b\\x7f')\n}\n"
	res, formatted := run_vfmt_write('escaped_control_bytes', source, '')

	assert res.exit_code == 0, res.output
	assert formatted == source, formatted
	second_res, formatted_twice := run_vfmt_write('escaped_control_bytes', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_does_not_import_for_in_binders_with_v3() {
	source := "struct Item {\n\tname string\n}\n\nfn scan(items []Item) {\n\tfor flag in items {\n\t\tprintln(flag.name)\n\t}\n}\n"
	res, formatted := run_vfmt_write('for_in_binder_import', source, '')

	assert res.exit_code == 0, res.output
	assert !formatted.contains('import flag'), formatted
	second_res, formatted_twice := run_vfmt_write('for_in_binder_import', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_anonymous_aggregate_types_with_v3() {
	source := "struct Holder {\n\titem []struct { foo string }\n}\n\nfn accept(value struct{ foo string }) {\n\tprintln(value.foo)\n}\n\nfn main() {\n\taccept(struct { foo: 'ok' })\n}\n"
	res, formatted := run_vfmt_write('anonymous_aggregate_types', source, '')

	assert res.exit_code == 0, res.output
	assert !formatted.contains('AnonStruct_'), formatted
	assert formatted.contains('item []struct { foo string }'), formatted
	assert formatted.contains('value struct{ foo string }'), formatted
	assert formatted.contains("accept(struct { foo: 'ok' })"), formatted
	second_res, formatted_twice := run_vfmt_write('anonymous_aggregate_types', formatted,
		'')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_mutable_match_subjects_with_v3() {
	source := "fn update(mut value int) {\n\tmatch mut value {\n\t\tint {\n\t\t\tvalue++\n\t\t}\n\t}\n}\n"
	res, formatted := run_vfmt_write('mutable_match_subject', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('match mut value {'), formatted
	second_res, formatted_twice := run_vfmt_write('mutable_match_subject', formatted, '')
	assert second_res.exit_code == 0, second_res.output
	assert formatted_twice == formatted
}

fn test_fmt_preserves_c_string_prefix_with_v3() {
	source := "fn main(){\n\tx := c' '\n\t_ = x\n}\n"
	res, formatted := run_vfmt_write('c_string', source, '')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v3.gen.v over file:'), res.output
	assert formatted.contains("x := c' '")
}

fn test_fmt_preserves_raw_string_prefix_with_v3() {
	source := 'fn main(){\n\tx := r"raw \\ value"\n\t_ = x\n}\n'
	res, formatted := run_vfmt_write('raw_string', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('x := r"raw \\ value"')
}

fn test_fmt_accepts_inline_asm_with_v3() {
	source := 'fn main(){\n\tasm amd64 {\n\t\tnop\n\t}\n}\n'
	res, formatted := run_vfmt_write('inline_asm', source, '')

	assert res.exit_code == 0, res.output
	assert res.output.contains('vfmt running v3.gen.v over file:'), res.output
	assert formatted.contains('asm amd64 {')
	assert formatted.contains('nop')
}

fn test_fmt_preserves_json_migration_options_with_v3() {
	source := 'import json\n\nfn main(){\n\tprintln(json.encode(1))\n}\n'
	migrate_res, migrated := run_vfmt_write('json_migrate', source, '')

	assert migrate_res.exit_code == 0, migrate_res.output
	assert migrate_res.output.contains('vfmt running v3.gen.v over file:'), migrate_res.output
	assert migrated.contains('import json2')
	assert migrated.contains('json2.encode(')

	keep_res, kept := run_vfmt_write('json_keep', source, '-no-migrate-json2')
	assert keep_res.exit_code == 0, keep_res.output
	assert kept.contains('import json\n')
	assert kept.contains('json.encode(')
}
