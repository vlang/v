import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_type_checker_errors_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0
	return v3_bin
}

fn run_bad(v3_bin string, name string, src string, expected string) {
	bad_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	result := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert result.exit_code != 0
	assert result.output.contains(expected)
	assert !result.output.contains('C compilation failed')
}

fn run_good(v3_bin string, name string, src string) string {
	good_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0
	assert !compile.output.contains('C compilation failed')
	run := os.execute(good_bin)
	assert run.exit_code == 0
	return run.output.trim_space()
}

fn write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn run_bad_project(v3_bin string, name string, files map[string]string, input string, expected string) {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	result := os.execute('${v3_bin} ${input_path} -b c -o ${bad_bin}')
	assert result.exit_code != 0
	assert result.output.contains(expected)
	assert !result.output.contains('C compilation failed')
}

fn run_good_project(v3_bin string, name string, files map[string]string, input string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${input_path} -b c -o ${good_bin}')
	assert compile.exit_code == 0
	assert !compile.output.contains('C compilation failed')
	run := os.execute(good_bin)
	assert run.exit_code == 0
	return run.output.trim_space()
}

fn test_type_checker_reports_core_semantic_errors() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'bad_assignment', "fn main() {\n\tmut x := 1\n\tx = 'bad'\n}\n",
		'cannot assign `string` to `int`')
	run_bad(v3_bin, 'bad_return', "fn f() int {\n\treturn 'bad'\n}\nfn main() {}\n",
		'cannot return `string` as `int`')
	run_bad(v3_bin, 'bad_call_arg', "fn takes_int(x int) {}\nfn main() {\n\ttakes_int('bad')\n}\n",
		'cannot use `string` as argument 1 to `takes_int`; expected `int`')
	run_bad(v3_bin, 'bad_field',
		'struct Foo {\n\tx int\n}\nfn main() {\n\tf := Foo{}\n\t_ := f.y\n}\n',
		'unknown field `y` on `Foo`')
	run_bad(v3_bin, 'bad_index', 'fn main() {\n\tx := 1\n\t_ := x[0]\n}\n', 'cannot index `int`')
	run_bad(v3_bin, 'bad_condition', "fn main() {\n\tif 'bad' {}\n}\n",
		'if condition must be `bool`, not `string`')
	run_bad(v3_bin, 'bad_zero_arg_call', 'fn f() {}\nfn main() {\n\tf(1)\n}\n',
		'argument count mismatch for `f`: expected 0, got 1')
	run_bad(v3_bin, 'bad_int_condition', "fn main() {\n\tif 1 {\n\t\tprintln('bad')\n\t}\n}\n",
		'if condition must be `bool`, not `int`')
	run_bad(v3_bin, 'bad_missing_return',
		'fn f(x bool) int {\n\tif x {\n\t\treturn 1\n\t}\n}\nfn main() {}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_interface_method_set',
		'interface Speaker {\n\tspeak() string\n}\nstruct Person {}\nfn takes_speaker(s Speaker) {}\nfn main() {\n\ttakes_speaker(Person{})\n}\n',
		'cannot use `Person` as argument 1 to `takes_speaker`; expected `Speaker`')
	run_bad(v3_bin, 'bad_fn_value_call',
		"fn add(a int, b int) int {\n\treturn a + b\n}\nfn main() {\n\tf := add\n\t_ := f('bad', 4)\n}\n",
		'cannot use `string` as argument 1 to `f`; expected `int`')
	run_bad(v3_bin, 'bad_struct_default', "struct Foo {\n\tx int = 'bad'\n}\nfn main() {}\n",
		'cannot initialize field `x` with `string`; expected `int`')
	run_bad(v3_bin, 'bad_enum_value', "enum Color {\n\tred = 'bad'\n}\nfn main() {}\n",
		'enum field `red` value must be integer, not `string`')
	run_bad(v3_bin, 'bad_enum_shorthand',
		'enum Color {\n\tred\n}\nfn paint(c Color) {}\nfn main() {\n\tpaint(.blue)\n}\n',
		'unknown enum field `blue` for `Color`')
	run_bad(v3_bin, 'bad_interface_field',
		'interface Named {\n\tname string\n}\nstruct Person {}\nfn takes_named(n Named) {}\nfn main() {\n\ttakes_named(Person{})\n}\n',
		'cannot use `Person` as argument 1 to `takes_named`; expected `Named`')
	run_bad(v3_bin, 'bad_sum_missing_shared_field',
		'struct A {\n\tid int\n}\nstruct B {\n\tname string\n}\ntype Node = A | B\nfn main() {\n\tn := Node(A{\n\t\tid: 1\n\t})\n\t_ := n.id\n}\n',
		'unknown field `id` on `Node`')
	run_bad(v3_bin, 'bad_sum_and_smartcast_rhs',
		'struct Cat {\n\tage int\n}\nstruct Dog {\n\ttricks int\n}\ntype Animal = Cat | Dog\nfn main() {\n\ta := Animal(Cat{\n\t\tage: 2\n\t})\n\tif a is Cat && a.tricks == 2 {}\n}\n',
		'unknown field `tricks` on `Cat`')
	run_bad(v3_bin, 'bad_sum_is_variant',
		'struct Cat {\n\tage int\n}\nstruct Dog {\n\ttricks int\n}\nstruct Bird {\n\twings int\n}\ntype Animal = Cat | Dog\nfn main() {\n\ta := Animal(Cat{\n\t\tage: 2\n\t})\n\tif a is Bird {}\n}\n',
		'`Bird` is not a variant of sum type `Animal`')
	run_bad(v3_bin, 'bad_sum_match_variant',
		'struct Cat {\n\tage int\n}\nstruct Dog {\n\ttricks int\n}\nstruct Bird {\n\twings int\n}\ntype Animal = Cat | Dog\nfn main() {\n\ta := Animal(Cat{\n\t\tage: 2\n\t})\n\tmatch a {\n\t\tBird {}\n\t\telse {}\n\t}\n}\n',
		'`Bird` is not a variant of sum type `Animal`')
	run_bad(v3_bin, 'bad_unknown_decl_type', 'fn f(x Missing) {}\nfn main() {}\n',
		'unknown type `Missing`')
	run_bad(v3_bin, 'bad_generic_param',
		'fn id[T](x T) T {\n\treturn x\n}\nfn main() {\n\t_ := id(1)\n}\n',
		'unsupported generic type parameter `T`')
	run_bad(v3_bin, 'bad_generic_type_application', 'fn takes_box(x Box[int]) {}\nfn main() {}\n',
		'unsupported generic type application `Box[int]`')
	run_bad_project(v3_bin, 'bad_bare_imported_call', {
		'main.v':      'module main\n\nimport moda\n\nfn main() {\n\t_ := answer()\n}\n'
		'moda/moda.v': 'module moda\n\nfn answer() int {\n\treturn 7\n}\n'
	}, 'main.v', 'unknown function `answer`')
	run_bad_project(v3_bin, 'bad_import_leak', {
		'main.v':      'module main\n\nimport moda\n\nfn main() {}\n'
		'other.v':     'module main\n\nfn use_it() int {\n\treturn moda.answer()\n}\n'
		'moda/moda.v': 'module moda\n\nfn answer() int {\n\treturn 7\n}\n'
	}, '', 'unknown function `moda.answer`')
	alias_out := run_good(v3_bin, 'alias_method',
		'type UserId = int\n\nfn (id UserId) str() string {\n\treturn int_str(int(id))\n}\n\nfn main() {\n\tid := UserId(1)\n\tprintln(id.str())\n}\n')
	assert alias_out == '1'
	map_mutation_out := run_good(v3_bin, 'map_mutation_lowering',
		"fn main() {\n\tmut m := map[string]int{}\n\tm['a'] = 1\n\tm['a'] += 2\n\tm['a']++\n\tm['a'] -= 1\n\tprintln(int_str(m['a']))\n}\n")
	assert map_mutation_out == '3'
	map_array_append_out := run_good(v3_bin, 'map_array_append_lowering',
		"fn main() {\n\tmut m := map[string][]int{}\n\tm['a'] << 1\n\tm['a'] << 2\n\tprintln(int_str(m['a'].len))\n}\n")
	assert map_array_append_out == '2'
	assoc_selector_out := run_good(v3_bin, 'assoc_selector_type',
		'struct Point {\n\tx int\n\ty int\n}\n\nfn main() {\n\tp := Point{\n\t\tx: 10\n\t\ty: 20\n\t}\n\tq := Point{\n\t\t...p\n\t\tx: 99\n\t}\n\tprintln(int_str(q.x + q.y))\n}\n')
	assert assoc_selector_out == '119'
	array_literal_push_many_out := run_good(v3_bin, 'array_literal_push_many',
		'fn main() {\n\tmut xs := [1, 2]\n\tys := [3, 4]\n\txs << ys\n\txs << [5, 6]\n\tprintln(int_str(xs.len))\n}\n')
	assert array_literal_push_many_out == '6'
	const_forward_out := run_good(v3_bin, 'const_forward',
		'const first_value = second_value\nconst second_value = 2\nfn main() {\n\tprintln(int_str(first_value))\n}\n')
	assert const_forward_out == '2'
	imported_call_out := run_good_project(v3_bin, 'qualified_import_call', {
		'main.v':      'module main\n\nimport moda\n\nfn main() {\n\tprintln(int_str(moda.answer()))\n}\n'
		'moda/moda.v': 'module moda\n\nfn answer() int {\n\treturn 7\n}\n'
	}, 'main.v')
	assert imported_call_out == '7'
	comptime_type_chain_out := run_good_project(v3_bin, 'comptime_type_chain_keeps_later_decls', {
		'main.v':      'module main\n\nimport moda\n\nfn main() {\n\tprintln(int_str(moda.after_chain()))\n}\n'
		'moda/moda.v': 'module moda\n\nfn choose[T](x T) T {\n\t$if T is u8 {\n\t\treturn x\n\t} $else $if T is int {\n\t\treturn x\n\t} $else {}\n\treturn x\n}\n\nfn after_chain() int {\n\treturn 7\n}\n'
	}, 'main.v')
	assert comptime_type_chain_out == '7'
	builder_out := run_good(v3_bin, 'strings_builder_from_vlib',
		"import strings\n\nfn main() {\n\tmut sb := strings.new_builder(16)\n\tsb.write_string('ok')\n\tsb.write_u8(u8(33))\n\tprintln(sb.last_n(3))\n\tprintln(sb.str())\n}\n")
	assert builder_out == 'ok!\nok!'
}
