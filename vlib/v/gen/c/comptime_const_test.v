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

fn test_comptime_integer_lengths_survive_transform() {
	path := os.join_path(os.vtmp_dir(), 'comptime_lengths_${os.getpid()}.v')
	os.write_file(path, '@[comptime]\nfn seed() int { return 256 }\nconst source = seed()\nconst indirect = u8(source) + 1\nconst alias = indirect\n@[comptime]\nfn measure() int { a := [indirect + 1]u8{} b := [(alias)]u8{} return a.len + b.len }\nconst measured = measure()\nfn main() { _ = measured }')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	for stage in ['before transform', 'after transform'] {
		if stage == 'after transform' {
			transform.transform(mut a, &tc)
			tc.annotate_types()
		}
		for expression, expected in {
			'indirect':     1
			'(alias)':      1
			'indirect + 1': 2
		} {
			actual := tc.const_int_value(expression, []) or { -1 }
			if actual != expected {
				detail := if value := tc.eval_comptime_const(tc.const_exprs['indirect']!) {
					value.str()
				} else {
					err.msg()
				}
				assert false, '${stage}: ${expression} = ${actual}; ${detail}'
			}
		}
	}
}

fn test_cli_comptime_rejects_translated_evaluation_but_keeps_runtime_calls() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_translated_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	program := '@[comptime]\nfn negate(a u8) int { value := -a return int(value) }\nconst result = negate(250)\nfn main() { println(result) println(negate(250)) }'
	args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none', '-cc',
		'tcc']
	os.write_file(source, program)!
	native := os.exec([...args, 'run', source])
	assert native.exit_code == 0, native.output
	assert native.output.trim_space() == '6\n6', native.output
	global := os.exec([...args, '-translated', '-o', os.join_path(root, 'global'), source])
	assert global.exit_code != 0, global.output
	assert global.output.contains('required evaluation of translated code is not supported'), global.output
	os.write_file(source, '@[translated]\nmodule main\n' + program)!
	per_file := os.exec([...args, '-o', os.join_path(root, 'per_file'), source])
	assert per_file.exit_code != 0, per_file.output
	assert per_file.output.contains('required evaluation of translated code is not supported'), per_file.output
	runtime_program := program.replace('const result = negate(250)\n', '').replace('fn main() {',
		'fn main() { result := negate(250)\n')
	os.write_file(source, runtime_program)!
	runtime := os.exec([...args, '-translated', 'run', source])
	assert runtime.exit_code == 0, runtime.output
	assert runtime.output.trim_space() == '-250\n-250', runtime.output
}

fn test_cli_comptime_rejects_operator_methods_but_keeps_runtime_calls() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_operator_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	program := 'type Count = int\nfn (a Count) + (b Count) Count { return Count(int(a) + int(b) + 10) }\n@[comptime]\nfn compute() int { return int(Count(1) + Count(2)) }\nconst result = compute()\nfn main() { println(result) println(compute()) }'
	args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none', '-cc',
		'tcc']
	os.write_file(source, program)!
	required := os.exec([...args, '-o', os.join_path(root, 'required'), source])
	assert required.exit_code != 0, required.output
	assert required.output.contains('cannot evaluate const `result`'), required.output
	assert required.output.contains('overloaded operator'), required.output
	runtime_program := program.replace('const result = compute()\n', '').replace('fn main() {',
		'fn main() { result := compute()\n')
	os.write_file(source, runtime_program)!
	runtime := os.exec([...args, 'run', source])
	assert runtime.exit_code == 0, runtime.output
	assert runtime.output.trim_space() == '13\n13', runtime.output
	plain := program.replace('fn (a Count) + (b Count) Count { return Count(int(a) + int(b) + 10) }\n', '')
	os.write_file(source, plain)!
	builtin := os.exec([...args, 'run', source])
	assert builtin.exit_code == 0, builtin.output
	assert builtin.output.trim_space() == '3\n3', builtin.output
}

fn test_cli_comptime_mutation_targets_emit_static_values() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_lvalue_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'const answer = compute()\n@[comptime]\nfn compute() u32 { mut a := [3]int{} mut i := 0 a[if true { i += 1 i } else { 0 }] += 7 return u32(a[1] * 100 + i) }\nfn main() { assert answer == 701 }')!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-o', output, source])
	assert result.exit_code == 0, result.output
	generated := os.read_file(output)!
	assert generated.contains('#define main__answer ((u32)(701LL))'), generated.all_after('compute(void);')
	assert !generated.contains('main__answer = compute('), generated.all_after('compute(void);')
}

fn test_cli_comptime_conditional_index_ordering_is_explicit() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_index_order_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	suffix := if os.user_os() == 'windows' { '.exe' } else { '' }
	program := 'const result = compute()\n@[comptime]\nfn compute() int { mut a := [3]int{} mut order := 0 a[if true { order = order * 10 + 1 1 } else { 0 }] = if true { order = order * 10 + 2 7 } else { 0 } return order * 100 + a[1] }\nfn main() { println(result) println(compute()) }'
	args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none', '-cc',
		'tcc']
	os.write_file(source, program)!
	required := os.exec([...args, '-o', os.join_path(root, 'required'), source])
	assert required.exit_code != 0, required.output
	assert required.output.contains('cannot evaluate const `result`'), required.output
	assert required.output.contains('conditional index ordering'), required.output
	runtime_program := program.replace('const result = compute()\n', '').replace('fn main() {',
		'fn main() { result := compute()\n')
	os.write_file(source, runtime_program)!
	runtime_path := os.join_path(root, 'runtime${suffix}')
	runtime_build := os.exec([...args, '-o', runtime_path, source])
	assert runtime_build.exit_code == 0, runtime_build.output
	runtime := os.exec([runtime_path])
	assert runtime.exit_code == 0, runtime.output
	assert runtime.output.trim_space() == '1207\n1207', runtime.output
	workaround := program.replace('a[if true { order = order * 10 + 1 1 } else { 0 }] =',
		'index := if true { order = order * 10 + 1 1 } else { 0 }\na[index] =')
	os.write_file(source, workaround)!
	explicit_path := os.join_path(root, 'explicit_order${suffix}')
	explicit_build := os.exec([...args, '-o', explicit_path, source])
	assert explicit_build.exit_code == 0, explicit_build.output
	explicit_order := os.exec([explicit_path])
	assert explicit_order.exit_code == 0, explicit_order.output
	assert explicit_order.output.trim_space() == '1207\n1207', explicit_order.output
}

struct ComptimeAccessCase {
	declarations string
	body         string
	diagnostic   string
	expected     string
}

fn test_cli_comptime_restrictions_leave_ordinary_calls_available() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_access_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none', '-cc',
		'tcc']
	cases := [
		ComptimeAccessCase{
			declarations: 'type Pair = [2]int\nfn (a Pair) [] (i int) int { _ = i return 7 }\nconst pair = Pair([1, 2]!)'
			body:         'return pair[0]'
			diagnostic:   'overloaded indexing'
			expected:     '7'
		},
		ComptimeAccessCase{
			declarations: 'type Pair = [2]int\nfn (mut a Pair) []= (i int, value int) { _ = i a = Pair([value + 10, 2]!) }\nstruct Box {\nmut:\npair Pair\n}'
			body:         'mut a := Box{pair: Pair([1, 2]!)}\na.pair[0] = 3\nplain := [2]int(a.pair)\nreturn plain[0]'
			diagnostic:   'overloaded indexing'
			expected:     '13'
		},
		ComptimeAccessCase{
			declarations: 'type Word = string\nfn (a Word) == (b Word) bool { _ = b return false }'
			body:         's := Word(Word("a") + Word("b"))\nreturn if s == Word("ab") { 1 } else { 0 }'
			diagnostic:   'overloaded operator'
			expected:     '0'
		},
		ComptimeAccessCase{
			body:       'mut a := [1, 2]!\nreturn a#[if true { a[0] = 9 0 } else { 1 }]'
			diagnostic: 'gated indexing'
			expected:   '1'
		},
		ComptimeAccessCase{
			declarations: 'const n = 1'
			body:         'n := 255\nvalue := n + u8(1)\nreturn value'
			diagnostic:   'integer promotion of a local that shadows a constant'
			expected:     '0'
		},
		ComptimeAccessCase{
			body:       'return if true { 255 + u8(1) } else { 0 }'
			diagnostic: 'narrowing conditional integer values'
			expected:   '256'
		},
	]
	for i, case in cases {
		program := '${case.declarations}\nconst result = compute()\n@[comptime]\nfn compute() int { ${case.body} }\nfn main() { println(result) println(compute()) }'
		os.write_file(source, program)!
		required := os.exec([...args, '-o', os.join_path(root, 'required_${i}'), source])
		assert required.exit_code != 0, required.output
		assert required.output.contains('cannot evaluate const `result`'), required.output
		assert required.output.contains(case.diagnostic), required.output
		runtime_program := program.replace('const result = compute()\n', '').replace('fn main() {',
			'fn main() { result := compute()\n')
		os.write_file(source, runtime_program)!
		executable := os.join_path(root, 'runtime_${i}')
		built := os.exec([...args, '-o', executable, source])
		assert built.exit_code == 0, built.output
		runtime := os.exec([executable])
		assert runtime.exit_code == 0, runtime.output
		assert runtime.output.trim_space() == '${case.expected}\n${case.expected}', runtime.output
	}
	// A plain string operand makes concatenation return string, so no alias
	// equality method is reached by either the evaluator or the runtime call.
	mixed := 'type Word = string\nfn (a Word) == (b Word) bool { _ = b return false }\nconst result = compute()\n@[comptime]\nfn compute() [2]bool { left := Word("a") + "b" right := "a" + Word("b") return [left == Word("ab"), right == Word("ab")]! }\nfn main() {\nassert result == [true, true]!\nassert result == compute()\nprintln("ok")\n}'
	os.write_file(source, mixed)!
	mixed_executable := os.join_path(root, 'mixed')
	mixed_built := os.exec([...args, '-o', mixed_executable, source])
	assert mixed_built.exit_code == 0, mixed_built.output
	builtin := os.exec([mixed_executable])
	assert builtin.exit_code == 0, builtin.output
	assert builtin.output.trim_space() == 'ok', builtin.output
}

fn test_cli_comptime_local_promotion_and_live_bounds_emit_static_values() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_live_bounds_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'const initial = local_value()\n@[comptime]\nfn local_value() int { value := 1 return value + u8(1) }\nconst counts = range_counts()\n@[comptime]\nfn range_counts() [3]u32 { mut n := 3 mut live := u32(0) for _ in 0 .. n { n = 1 live++ } n = 3 mut snapshot := u32(0) for _ in 0 .. (n + 0) { n = 1 snapshot++ } return [u32(initial), live, snapshot]! }\nfn main() { assert counts == [u32(2), 1, 3]! }')!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-o', output, source])
	assert result.exit_code == 0, result.output
	generated := os.read_file(output)!
	assert generated.contains('const u32 main__counts[3] = {2LL, 1LL, 3LL};'), generated.all_after('range_counts(void);')
	assert !generated.contains('memmove(main__counts,'), generated.all_after('range_counts(void);')
	assert !generated.contains('main__counts = range_counts('), generated.all_after('range_counts(void);')
}

fn test_cli_comptime_array_conversions_preserve_nominal_operators() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_array_alias_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none', '-cc',
		'tcc']
	program := 'type Count = int\nfn (a Count) + (b Count) Count { return Count(int(a) + int(b) + 10) }\n@[comptime]\nfn values() [2]int { return [1, 2]! }\n@[comptime]\nfn total(items [2]Count) int { return int(items[0] + items[1]) }\nconst answer = total(values())\nfn main() { println(answer) println(total(values())) }'
	os.write_file(source, program)!
	required := os.exec([...args, '-o', os.join_path(root, 'required'), source])
	assert required.exit_code != 0, required.output
	assert required.output.contains('cannot evaluate const `answer`'), required.output
	assert required.output.contains('overloaded operator'), required.output
	runtime_program := program.replace('const answer = total(values())\n', '').replace('fn main() {', 'fn main() { answer := total(values())\n')
	os.write_file(source, runtime_program)!
	executable := os.join_path(root, 'runtime')
	built := os.exec([...args, '-o', executable, source])
	assert built.exit_code == 0, built.output
	runtime := os.exec([executable])
	assert runtime.exit_code == 0, runtime.output
	assert runtime.output.trim_space() == '13\n13', runtime.output
	reverse := 'type Count = int\nfn (a Count) + (b Count) Count { return Count(int(a) + int(b) + 10) }\n@[comptime]\nfn values() [2]Count { return [Count(1), Count(2)]! }\n@[comptime]\nfn total(items [2]int) int { return items[0] + items[1] }\nconst answer = total(values())\nfn main() { println(answer) println(total(values())) }'
	os.write_file(source, reverse)!
	reverse_path := os.join_path(root, 'reverse')
	reverse_built := os.exec([...args, '-o', reverse_path, source])
	assert reverse_built.exit_code == 0, reverse_built.output
	reverse_runtime := os.exec([reverse_path])
	assert reverse_runtime.exit_code == 0, reverse_runtime.output
	assert reverse_runtime.output.trim_space() == '3\n3', reverse_runtime.output
}

fn test_cli_comptime_generic_structs_reject_required_values_and_preserve_runtime_defaults() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_generic_struct_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none', '-cc',
		'tcc']
	program := 'struct Box[T] { value T\ntag int = 7 }\n@[comptime]\nfn make_box() Box[int] { return Box[int]{} }\nconst boxed = make_box()\nfn main() { println(boxed.tag) println(make_box().tag) }'
	os.write_file(source, program)!
	required := os.exec([...args, '-o', os.join_path(root, 'required'), source])
	assert required.exit_code != 0, required.output
	assert required.output.contains('cannot evaluate const `boxed`'), required.output
	assert required.output.contains('generic structs are not supported'), required.output
	runtime_program := program.replace('const boxed = make_box()\n', '').replace('fn main() {',
		'fn main() { boxed := make_box()\n')
	os.write_file(source, runtime_program)!
	executable := os.join_path(root, 'runtime')
	built := os.exec([...args, '-o', executable, source])
	assert built.exit_code == 0, built.output
	runtime := os.exec([executable])
	assert runtime.exit_code == 0, runtime.output
	assert runtime.output.trim_space() == '7\n7', runtime.output
}

fn test_cli_comptime_integer_staging_rejects_misfolds_and_keeps_runtime_calls() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_integer_staging_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	suffix := if os.user_os() == 'windows' { '.exe' } else { '' }
	args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none', '-cc',
		'tcc']
	mut global_rows := ['[7]!']
	for _ in 0 .. 255 {
		global_rows << '[0]!'
	}
	global_rows << '[9]!'
	global_table := 'const table = [${global_rows.join(', ')}]!'
	cases := [
		ComptimeAccessCase{
			body:     'return int((a + b) + (if true { u8(1) } else { u8(1) }))'
			expected: '5'
		},
		ComptimeAccessCase{
			body:     'return int((if true { u8(1) } else { u8(1) }) + (a + b))'
			expected: '5'
		},
		ComptimeAccessCase{
			declarations: '@[comptime]\nfn add_args(x int, y int) int { return x + y }'
			body:         'return add_args(a + b, if true { u8(1) } else { u8(1) })'
			expected:     '5'
		},
		ComptimeAccessCase{
			declarations: 'struct Pair { x int\ny int }'
			body:         'pair := Pair{x: a + b, y: if true { u8(1) } else { u8(1) }}\nreturn pair.x + pair.y'
			expected:     '5'
		},
		ComptimeAccessCase{
			body:     'return if (1 << 40) < (if true { i64(-1) } else { i64(-1) }) { 1 } else { 0 }'
			expected: '0'
		},
		ComptimeAccessCase{
			body:     'mut table := [300][1]int{}\ntable[4][0] = 7\ntable[260][0] = 9\nreturn table[u8(250) + u8(10)][if true { 0 } else { 0 }]'
			expected: '7'
		},
		ComptimeAccessCase{
			declarations: global_table
			body:         'return table[u8(255) + u8(1)][if true { 0 } else { 0 }]'
			expected:     '7'
		},
	]
	for i, case in cases {
		program := '${case.declarations}\n@[comptime]\nfn compute(a u8, b u8) int { ${case.body} }\nconst result = compute(250, 10)\nfn main() { println(result) println(compute(250, 10)) }'
		os.write_file(source, program)!
		required := os.exec([...args, '-o', os.join_path(root, 'required_${i}${suffix}'), source])
		assert required.exit_code != 0, '${i}: ${required.output}'
		assert required.output.contains('cannot evaluate const `result`'), '${i}: ${required.output}'
		assert required.output.contains('integer operand staging'), '${i}: ${required.output}'
		runtime_program := program.replace('const result = compute(250, 10)\n', '').replace('fn main() {',
			'fn main() { result := compute(250, 10)\n')
		os.write_file(source, runtime_program)!
		executable := os.join_path(root, 'runtime_${i}${suffix}')
		built := os.exec([...args, '-o', executable, source])
		assert built.exit_code == 0, '${i}: ${built.output}'
		runtime := os.exec([executable])
		assert runtime.exit_code == 0, '${i}: ${runtime.output}'
		assert runtime.output.trim_space() == '${case.expected}\n${case.expected}', '${i}: ${runtime.output}'
	}
	workaround := '@[comptime]\nfn compute(a u8, b u8) int { sum := a + b return int(sum + (if true { u8(1) } else { u8(1) })) }\nconst result = compute(250, 10)\nfn main() { println(result) println(compute(250, 10)) }'
	os.write_file(source, workaround)!
	explicit_path := os.join_path(root, 'explicit_local${suffix}')
	explicit_build := os.exec([...args, '-o', explicit_path, source])
	assert explicit_build.exit_code == 0, explicit_build.output
	explicit_local := os.exec([explicit_path])
	assert explicit_local.exit_code == 0, explicit_local.output
	assert explicit_local.output.trim_space() == '5\n5', explicit_local.output
}

fn test_cli_integer_constant_macros_and_expansions_preserve_declared_types() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_integer_macros_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	suffix := if os.user_os() == 'windows' { '.exe' } else { '' }
	program := 'const word_value = u16(65530) + u16(10)\nconst widened = int(word_value)\nconst halved = word_value / u16(2)\nconst values = [int(word_value)]!\nstruct Record { value int }\nconst record = Record{value: int(word_value)}\nconst addressed = u16(65530) + u16(10)\nconst shifted = 1 << 40\n@[comptime]\nfn compute() [6]int { return [int(word_value), widened, int(halved), values[0], record.value, int(addressed)]! }\n@[comptime]\nfn compare() bool { return shifted < i64(-1) }\nconst folded = compute()\nconst comparison = compare()\nfn read_pointer(value &u16) u16 { return *value }\nfn main() {\nassert folded == [4, 4, 2, 4, 4, 4]!\nassert folded == compute()\nassert !comparison\nassert comparison == compare()\nassert read_pointer(&addressed) == 4\nassert sizeof(word_value) == sizeof(u16)\nprintln("ok")\n}'
	os.write_file(source, program)!
	executable := os.join_path(root, 'main${suffix}')
	args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none', '-cc',
		'tcc']
	built := os.exec([...args, '-o', executable, source])
	assert built.exit_code == 0, built.output
	runtime := os.exec([executable])
	assert runtime.exit_code == 0, runtime.output
	assert runtime.output.trim_space() == 'ok', runtime.output
	output := os.join_path(root, 'main.c')
	generated_result := os.exec([...args, '-o', output, source])
	assert generated_result.exit_code == 0, generated_result.output
	generated := os.read_file(output)!
	assert generated.contains('#define main__word_value (((u16)('), generated.all_after('void main__main(void);')
	assert generated.contains('static const u16 main__addressed ='), generated.all_after('void main__main(void);')
}

struct ComptimeConstantOperationCase {
	expression string
	value_type string
	expected   string
}

fn test_cli_comptime_constant_integer_operations_match_runtime() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_constant_operations_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	suffix := if os.user_os() == 'windows' { '.exe' } else { '' }
	args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none', '-cc',
		'tcc']
	cases := [
		ComptimeConstantOperationCase{
			expression: '(u16(65530) + u16(10)) / u16(2)'
			value_type: 'u16'
			expected:   '2'
		},
		ComptimeConstantOperationCase{
			expression: '(u16(65530) + u16(10)) % u16(3)'
			value_type: 'u16'
			expected:   '1'
		},
		ComptimeConstantOperationCase{
			expression: 'u64(0xffffffffffffffff) > i64(-1)'
			value_type: 'bool'
			expected:   'true'
		},
		ComptimeConstantOperationCase{
			expression: 'int(i8(-128) / i8(-1))'
			value_type: 'int'
			expected:   '-128'
		},
		ComptimeConstantOperationCase{
			expression: 'u64(0xffffffffffffffff) == 0xffffffffffffffff'
			value_type: 'bool'
			expected:   'true'
		},
	]
	for i, case in cases {
		for aggregate in [false, true] {
			name := '${i}_${aggregate}'
			initializer := if aggregate { '[${case.expression}]!' } else { case.expression }
			value_type := if aggregate { '[1]${case.value_type}' } else { case.value_type }
			value := if aggregate { 'value[0]' } else { 'value' }
			index := if aggregate { '[0]' } else { '' }
			result_type := if case.value_type == 'bool' { 'bool' } else { 'int' }
			returned := if case.value_type == 'bool' { value } else { 'int(${value})' }
			// Compare a folded read, an ordinary read, an annotated initializer,
			// and the direct ordinary helper call for both scalars and arrays.
			program := 'const value = ${initializer}\n@[comptime]\nfn make_value() ${value_type} { return ${initializer} }\nconst made_value = make_value()\n@[comptime]\nfn compute() ${result_type} { return ${returned} }\nconst result = compute()\nfn main() { println(result) println(compute()) println(made_value${index}) println(make_value()${index}) }'
			os.write_file(source, program)!
			executable := os.join_path(root, 'runtime_${name}${suffix}')
			built := os.exec([...args, '-o', executable, source])
			assert built.exit_code == 0, '${name}: ${built.output}'
			runtime := os.exec([executable])
			assert runtime.exit_code == 0, '${name}: ${runtime.output}'
			assert runtime.output.trim_space() == '${case.expected}\n${case.expected}\n${case.expected}\n${case.expected}', '${name}: ${runtime.output}'
		}
	}
	// Runtime array elements must evaluate call operands once, including when a
	// signed operand is negative and the comparison can choose its result early.
	control := 'struct Counter {\nmut:\ncalls int\n}\nfn unsigned_value(mut counter Counter) u64 { counter.calls++ return u64(0xffffffffffffffff) }\nfn signed_value(mut counter Counter) i64 { counter.calls++ return i64(-1) }\nfn dividend(mut counter Counter) u16 { counter.calls++ return u16(65530) }\nfn main() {\nmut counter := Counter{}\ncomparison := [unsigned_value(mut counter) > signed_value(mut counter)]!\nassert comparison[0]\nassert counter.calls == 2\nquotient := [(dividend(mut counter) + u16(10)) / u16(2)]!\nassert quotient[0] == 2\nassert counter.calls == 3\nprintln("ok")\n}'
	os.write_file(source, control)!
	control_path := os.join_path(root, 'runtime_calls${suffix}')
	control_build := os.exec([...args, '-o', control_path, source])
	assert control_build.exit_code == 0, control_build.output
	control_run := os.exec([control_path])
	assert control_run.exit_code == 0, control_run.output
	assert control_run.output.trim_space() == 'ok', control_run.output
}

fn test_cli_comptime_constant_shifts_match_runtime() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_constant_shifts_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	suffix := if os.user_os() == 'windows' { '.exe' } else { '' }
	args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none', '-cc',
		'tcc']
	cases := [
		ComptimeConstantOperationCase{
			expression: '(u8(200) << 1) >> 1'
			value_type: 'u8'
			expected:   '72'
		},
		ComptimeConstantOperationCase{
			expression: '(u16(50000) << 1) >> 1'
			value_type: 'u16'
			expected:   '17232'
		},
		ComptimeConstantOperationCase{
			expression: '(i8(100) << 1) >> 1'
			value_type: 'i8'
			expected:   '-28'
		},
		ComptimeConstantOperationCase{
			expression: 'i8(-2) >> 1'
			value_type: 'i8'
			expected:   '-1'
		},
		ComptimeConstantOperationCase{
			expression: 'i8(-2) >>> 1'
			value_type: 'u8'
			expected:   '127'
		},
		ComptimeConstantOperationCase{
			expression: 'u8(1) << u8(8)'
			value_type: 'u8'
			expected:   '0'
		},
		ComptimeConstantOperationCase{
			expression: 'u8(200) >> i8(-1)'
			value_type: 'u8'
			expected:   '0'
		},
		ComptimeConstantOperationCase{
			expression: 'i8(-2) >>> u8(8)'
			value_type: 'u8'
			expected:   '0'
		},
		ComptimeConstantOperationCase{
			expression: 'u32(1) << u32(32)'
			value_type: 'u32'
			expected:   '0'
		},
		ComptimeConstantOperationCase{
			expression: 'i64(-1) >> i64(64)'
			value_type: 'i64'
			expected:   '0'
		},
		ComptimeConstantOperationCase{
			expression: 'u64(1) << u64(63)'
			value_type: 'u64'
			expected:   '9223372036854775808'
		},
		ComptimeConstantOperationCase{
			expression: 'u64(0xffffffffffffffff) >>> -1'
			value_type: 'u64'
			expected:   '0'
		},
		ComptimeConstantOperationCase{
			expression: '1 << 40'
			value_type: 'int'
			expected:   '1099511627776'
		},
		ComptimeConstantOperationCase{
			expression: '(1 << 40) < i64(-1)'
			value_type: 'bool'
			expected:   'true'
		},
		ComptimeConstantOperationCase{
			expression: '(1 << 63) >> 63'
			value_type: 'int'
			expected:   '-1'
		},
		ComptimeConstantOperationCase{
			expression: '1 << 64'
			value_type: 'int'
			expected:   '0'
		},
		ComptimeConstantOperationCase{
			expression: '-5 >>> 1'
			value_type: 'u32'
			expected:   '2147483645'
		},
		ComptimeConstantOperationCase{
			expression: '(Tiny(200) << 1) >> 1'
			value_type: 'Tiny'
			expected:   '72'
		},
	]
	for i, case in cases {
		for depth in 0 .. 3 {
			name := '${i}_${depth}'
			initializer := match depth {
				0 { case.expression }
				1 { '[${case.expression}]!' }
				else { '[[${case.expression}]!]!' }
			}
			value_type := '[1]'.repeat(depth) + case.value_type
			index := '[0]'.repeat(depth)
			// Each shape exercises both ordinary constant expansion and the
			// fixed-literal path inside an ordinary call to the annotated helper.
			program := 'type Tiny = u8\nconst value = ${initializer}\n@[comptime]\nfn make_value() ${value_type} { return ${initializer} }\nconst made_value = make_value()\n@[comptime]\nfn compute() ${case.value_type} { return value${index} }\nconst result = compute()\nfn main() { println(result) println(compute()) println(made_value${index}) println(make_value()${index}) }'
			os.write_file(source, program)!
			executable := os.join_path(root, 'runtime_${name}${suffix}')
			built := os.exec([...args, '-o', executable, source])
			assert built.exit_code == 0, '${name}: ${built.output}'
			runtime := os.exec([executable])
			assert runtime.exit_code == 0, '${name}: ${runtime.output}'
			assert runtime.output.trim_space() == '${case.expected}\n${case.expected}\n${case.expected}\n${case.expected}', '${name}: ${runtime.output}'
		}
	}
	// Invalid counts must still evaluate both runtime operands exactly once.
	// The legacy bound query must use the unsigned width of literal >>> too.
	control := 'const n = 1 + int((-5 >>> 1) % u32(3))\n@[comptime]\nfn bound_count() int { return n }\nconst folded_n = bound_count()\nstruct Counter {\nmut:\ncalls int\n}\nfn byte_value(mut counter Counter) u8 { counter.calls++ return u8(200) }\nfn negative_count(mut counter Counter) int { counter.calls++ return -1 }\nfn wide_count(mut counter Counter) u8 { counter.calls++ return u8(8) }\nfn one(mut counter Counter) int { counter.calls++ return 1 }\nfn main() {\nassert n == 3\nassert folded_n == n\nvalues := [n]u8{}\nassert values.len == n\nmut counter := Counter{}\nleft := [byte_value(mut counter) << negative_count(mut counter)]!\nassert left[0] == 0\nassert counter.calls == 2\nright := [[byte_value(mut counter) >> wide_count(mut counter)]!]!\nassert right[0][0] == 0\nassert counter.calls == 4\nlogical := [byte_value(mut counter) >>> one(mut counter)]!\nassert logical[0] == 100\nassert counter.calls == 6\nchained := [(byte_value(mut counter) << 1) >> 1]!\nassert chained[0] == 72\nassert counter.calls == 7\nlarge := u128(1) << 100\nassert large >> 100 == u128(1)\nprintln("ok")\n}'
	os.write_file(source, control)!
	control_path := os.join_path(root, 'runtime_calls${suffix}')
	control_build := os.exec([...args, '-o', control_path, source])
	assert control_build.exit_code == 0, control_build.output
	control_run := os.exec([control_path])
	assert control_run.exit_code == 0, control_run.output
	assert control_run.output.trim_space() == 'ok', control_run.output
}

fn test_cli_comptime_fixed_literals_keep_expected_context() ! {
	root := os.join_path(os.vtmp_dir(), 'cli_comptime_literal_context_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	suffix := if os.user_os() == 'windows' { '.exe' } else { '' }
	args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none']
	helpers := '@[comptime]\nfn make_bytes() [2]u8 { return [1, 2]! }\n@[comptime]\nfn make_nested() [2][2]u16 { return [[3, 4]!, [5, 6]!]! }\n@[comptime]\nfn take_bytes(values [2]u8) [2]u8 { return values }\n@[comptime]\nfn choose_bytes(first bool) [2]u8 { return if first { [9, 10]! } else { [11, 12]! } }\n@[comptime]\nfn parenthesized_bytes() [2]u8 { return ([13, 14]!) }'
	constants := 'const bytes = make_bytes()\nconst nested = make_nested()\nconst passed = take_bytes([7, 8]!)\nconst chosen = choose_bytes(false)\nconst parenthesized = parenthesized_bytes()'
	main_body := 'fn main() {\nassert bytes == [u8(1), 2]!\nassert bytes == make_bytes()\nassert nested == [[u16(3), 4]!, [u16(5), 6]!]!\nassert nested == make_nested()\nassert passed == [u8(7), 8]!\nassert passed == take_bytes([7, 8]!)\nassert chosen == [u8(11), 12]!\nassert chosen == choose_bytes(false)\nassert parenthesized == [u8(13), 14]!\nassert parenthesized == parenthesized_bytes()\nprintln("ok")\n}'
	for before in [false, true] {
		program := if before {
			'${constants}\n${helpers}\n${main_body}\n'
		} else {
			'${helpers}\n${constants}\n${main_body}\n'
		}
		os.write_file(source, program)!
		executable := os.join_path(root, 'literal_context_${before}${suffix}')
		built := os.exec([...args, '-o', executable, source])
		assert built.exit_code == 0, '${before}: ${built.output}'
		runtime := os.exec([executable])
		assert runtime.exit_code == 0, '${before}: ${runtime.output}'
		assert runtime.output.trim_space() == 'ok', '${before}: ${runtime.output}'
		for flags in [[]string{}, ['-no-parallel']] {
			output := os.join_path(root, 'literal_context.c')
			generated_result := os.exec([...args, ...flags, '-o', output, source])
			assert generated_result.exit_code == 0, '${before}: ${generated_result.output}'
			generated := os.read_file(output)!
			assert generated.contains('const u8 main__bytes[2] = {1LL, 2LL};')
			assert generated.contains('const u16 main__nested[2][2] = {{3LL, 4LL}, {5LL, 6LL}};')
			assert generated.contains('const u8 main__passed[2] = {7LL, 8LL};')
			assert generated.contains('const u8 main__chosen[2] = {11LL, 12LL};')
			assert generated.contains('const u8 main__parenthesized[2] = {13LL, 14LL};')
			for name in ['bytes', 'nested', 'passed', 'chosen', 'parenthesized'] {
				assert !generated.contains('memmove(main__${name},')
			}
		}
	}
}
