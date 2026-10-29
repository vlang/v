module c

import os
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn test_comptime_values_are_static_without_initialization_code() {
	path := os.join_path(os.vtmp_dir(), 'static_comptime_${os.getpid()}.v')
	os.write_file(path, '@[comptime]\nfn table() [3]u32 { mut a := [3]u32{} for i in 0 .. 3 { a[i] = u32(i * i) } return a }\nconst squares = table()\n@[comptime]\nfn label() string { return "hello" + " world" }\nconst name = label()\n@[comptime]\nfn word() u32 { return 0xffffffff }\nconst value = word()\nfn main() { assert squares[2] == 4 assert name.len == 11 }')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.comptime_const_values.len == 3, tc.comptime_const_values.keys().str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used, _ := markused.mark_all_used_with_generic_usage(a, tc, [])
	mut g := FlatGen.new()
	generated := g.gen_with_used_options(a, used, &tc, true)
	assert generated.contains('const u32 main__squares[3] = {0LL, 1LL, 4LL};'), generated.all_after('string label(void);')
	assert generated.contains('const string main__name = {(u8*)"hello world", 11, 1};'), generated.all_after('string label(void);')
	assert generated.contains('#define main__value ((u32)(4294967295LL))'), generated.all_after('string label(void);')
	assert !generated.contains('memmove(main__squares,'), generated.all_after('string label(void);')
	assert !generated.contains('main__name = label('), generated.all_after('string label(void);')
	for casts in [false, true] {
		mut checked := FlatGen.new()
		checked.set_check_overflow(!casts)
		checked.set_check_casts(casts)
		_ = checked.gen_with_used_options(a, used, &tc, true)
		assert checked.output_error.contains('required evaluation with -check-overflow or -check-casts'), checked.output_error
	}
}

fn comptime_cleanup_program(declarations string, body string) string {
	return '${declarations}\n@[comptime]\nfn compute() int {\n${body}\n}\nconst answer = compute()\nfn main() { println(answer) }\n'
}

fn test_cli_comptime_values_reject_implicit_cleanup() {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_cleanup_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	suffix := if os.user_os() == 'windows' { '.exe' } else { '' }
	for mode in ['-ownership', '-autofree'] {
		declarations := if mode == '-ownership' {
			'struct Resource implements Drop { value int }\nfn (mut resource Resource) drop() { panic("cleanup reached") }'
		} else {
			'struct Resource { value int }\nfn (resource &Resource) free() { panic("cleanup reached") }'
		}
		args := [@VEXE, '-new-compiler', '-nocache', '-gc', 'none', mode]
		for i, body in [
			'resource := Resource{value: 7}\nreturn resource.value',
			'resources := [Resource{value: 7}]!\nreturn resources[0].value',
			'wrapper := Wrapper{resource: Resource{value: 7}}\nreturn wrapper.resource.value',
			'resource := ResourceAlias{value: 7}\nreturn resource.value',
		] {
			program := comptime_cleanup_program('${declarations}\nstruct Wrapper { resource Resource }\ntype ResourceAlias = Resource', body)
			os.write_file(source, program)!
			output := os.join_path(root, '${mode[1..]}_${i}${suffix}')
			result := os.exec([...args, '-o', output, source])
			assert result.exit_code != 0, result.output
			assert result.output.contains('cannot evaluate const `answer`'), result.output
			assert result.output.contains('implicit cleanup'), result.output
		}
		body := 'resource := Resource{value: 7}\nreturn resource.value'
		os.write_file(source, comptime_cleanup_program(declarations, body).replace('@[comptime]\n', ''))!
		control := os.join_path(root, '${mode[1..]}_runtime${suffix}')
		build_control := os.exec([...args, '-o', control, source])
		assert build_control.exit_code == 0, build_control.output
		runtime := os.exec([control])
		assert runtime.exit_code != 0, runtime.output
		assert runtime.output.contains('cleanup reached'), runtime.output
		os.write_file(source, comptime_cleanup_program('struct Resource { value int }', body))!
		plain := os.join_path(root, '${mode[1..]}_plain${suffix}')
		build_plain := os.exec([...args, '-o', plain, source])
		assert build_plain.exit_code == 0, build_plain.output
		plain_result := os.exec([plain])
		assert plain_result.exit_code == 0, plain_result.output
		assert plain_result.output.trim_space() == '7', plain_result.output
	}
}

fn test_cli_comptime_values_remain_static_after_parent_index_refresh() {
	root := os.join_path(os.vtmp_dir(), 'cli_static_comptime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, '@[comptime]\nfn table() [3]u32 { return [u32(0), 1, 4]! }\nconst squares = table()\n@[comptime]\nfn label() string { return "hello" + " world" }\nconst name = label()\nfn main() {\nassert squares[2] == 4\nassert name.len == 11\n}')!
	for flags in [[]string{}, ['-no-parallel']] {
		output := os.join_path(root, 'main.c')
		result := os.exec([@VEXE, '-nocache', '-gc', 'none', ...flags, '-o', output, source])
		assert result.exit_code == 0, result.output
		generated := os.read_file(output)!
		assert generated.contains('const u32 main__squares[3] = {0LL, 1LL, 4LL};')
		assert generated.contains('const string main__name = {(u8*)"hello world", 11, 1};')
		assert !generated.contains('memmove(main__squares,')
		assert !generated.contains('main__name = label(')
	}
}
