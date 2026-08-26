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
	source := 'struct Counter {\n\tvolatile value u64\n}\n'
	res, formatted := run_vfmt_write('volatile_field', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('volatile value u64'), formatted
}

fn test_fmt_preserves_fixed_array_literal_prefixes_with_v3() {
	source := 'fn main() {\n\ta := [4]f32[1, 2, 3, 4]\n\tb := [..]f32[1, 2, 3, 4]\n\t_ = a\n\t_ = b\n}\n'
	res, formatted := run_vfmt_write('fixed_array_literal_prefixes', source, '')

	assert res.exit_code == 0, res.output
	assert formatted.contains('a := [4]f32[1, 2, 3, 4]'), formatted
	assert formatted.contains('b := [..]f32[1, 2, 3, 4]\n'), formatted
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
