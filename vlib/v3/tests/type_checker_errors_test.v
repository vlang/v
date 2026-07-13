import os
import rand
import strings

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// build_v3 builds v3 data for v3 tests.
fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(),
		'v3_type_checker_errors_test_${os.getpid()}_${rand.ulid()}')
	build := os.execute('${vexe} -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0
	return v3_bin
}

fn unique_temp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}_${rand.ulid()}')
}

fn error_index(output string, needle string) int {
	idx := output.index(needle) or { -1 }
	assert idx >= 0, 'missing `${needle}` in\n${output}'
	return idx
}

// run_bad supports run bad handling for v3 tests.
fn run_bad(v3_bin string, name string, src string, expected string) {
	run_bad_with_flags(v3_bin, name, src, expected, '')
}

fn run_bad_selfhost(v3_bin string, name string, src string, expected string) {
	run_bad_with_flags(v3_bin, name, src, expected, '-selfhost')
}

fn run_bad_with_flags(v3_bin string, name string, src string, expected string, flags string) {
	out := unique_temp_path(name)
	bad_src := out + '.v'
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := out
	result := os.execute('${v3_bin} ${bad_src} ${flags} -b c -o ${bad_bin}')
	assert result.exit_code != 0, '${name}: expected compile failure, got success: ${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in ${result.output}'
	assert !result.output.contains('C compilation failed'), '${name}: C compilation failed: ${result.output}'
}

// run_good supports run good handling for v3 tests.
fn run_good(v3_bin string, name string, src string) string {
	out := unique_temp_path(name)
	good_src := out + '.v'
	os.write_file(good_src, src) or { panic(err) }
	good_bin := out
	compile := os.execute('${v3_bin} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
}

// write_project_file writes project file output for v3 tests.
fn write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

// run_bad_project supports run bad project handling for v3 tests.
fn run_bad_project(v3_bin string, name string, files map[string]string, input string, expected string) {
	root := unique_temp_path('${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	bad_bin := unique_temp_path(name)
	result := os.execute('${v3_bin} ${input_path} -b c -o ${bad_bin}')
	assert result.exit_code != 0, '${name}: expected compile failure, got success: ${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in ${result.output}'
	assert !result.output.contains('C compilation failed'), '${name}: C compilation failed: ${result.output}'
}

// run_good_project supports run good project handling for v3 tests.
fn run_good_project(v3_bin string, name string, files map[string]string, input string) string {
	root := unique_temp_path('${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	good_bin := unique_temp_path(name)
	compile := os.execute('${v3_bin} ${input_path} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
}

// gen_c_project emits c project output for v3 tests.
fn gen_c_project(v3_bin string, name string, files map[string]string, input string) string {
	root := unique_temp_path('${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	c_out := unique_temp_path(name) + '.c'
	os.rm(c_out) or {}
	compile := os.execute('${v3_bin} ${input_path} -o ${c_out}')
	assert compile.exit_code == 0
	assert os.exists(c_out)
	return os.read_file(c_out) or { panic(err) }
}

fn test_parallel_checker_preserves_diagnostic_order() {
	v3_bin := build_v3()
	out := unique_temp_path('parallel_checker_error_order')
	src_path := out + '.v'
	mut src := strings.new_builder(32_000)
	for i in 0 .. 270 {
		src.writeln('fn f_${i}() int {')
		src.writeln('\treturn missing_${i}')
		src.writeln('}')
	}
	src.writeln('fn main() {}')
	os.write_file(src_path, src.str()) or { panic(err) }
	old_vjobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '4', true)
	defer {
		if value := old_vjobs {
			os.setenv('VJOBS', value, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	result := os.execute('${v3_bin} ${src_path} -b c -o ${out}')
	assert result.exit_code != 0, result.output
	first := error_index(result.output, 'unknown identifier `missing_0`')
	second := error_index(result.output, 'unknown identifier `missing_1`')
	third := error_index(result.output, 'unknown identifier `missing_2`')
	assert first < second
	assert second < third
}

// test_type_checker_reports_core_semantic_errors validates this v3 regression case.
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
	run_bad(v3_bin, 'bad_comma_index', 'fn main() {\n\txs := [1]\n\t_ := xs[0, 0]\n}\n',
		'index expression accepts one index')
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
	run_bad(v3_bin, 'bad_interface_match_unrelated_sum_variant',
		'interface Shape {\n\tarea() int\n}\nstruct Rect {\n\tw int\n}\nfn (r Rect) area() int {\n\treturn r.w\n}\nstruct Other {\n\tx int\n}\ntype Unrelated = Other\nfn describe(s Shape) int {\n\treturn match s {\n\t\tOther { 1 }\n\t\telse { s.area() }\n\t}\n}\nfn main() {}\n',
		'`Other` is not compatible with interface `Shape`')
	run_bad(v3_bin, 'bad_interface_is_unrelated_sum_variant',
		'interface Shape {\n\tarea() int\n}\nstruct Rect {\n\tw int\n}\nfn (r Rect) area() int {\n\treturn r.w\n}\nstruct Other {\n\tx int\n}\ntype Unrelated = Other\nfn check(s Shape) bool {\n\treturn s is Other\n}\nfn main() {}\n',
		'`Other` is not compatible with interface `Shape`')
	run_bad(v3_bin, 'bad_interface_match_unresolved_pattern',
		'interface Shape {\n\tarea() int\n}\nstruct Rect {\n\tw int\n}\nfn (r Rect) area() int {\n\treturn r.w\n}\nfn describe(s Shape) int {\n\treturn match s {\n\t\tMissingType { 1 }\n\t\telse { 0 }\n\t}\n}\nfn main() {}\n',
		'unknown type `MissingType`')
	run_bad(v3_bin, 'bad_interface_is_unresolved_pattern',
		'interface Shape {\n\tarea() int\n}\nstruct Rect {\n\tw int\n}\nfn (r Rect) area() int {\n\treturn r.w\n}\nfn check(s Shape) bool {\n\treturn s is MissingType\n}\nfn main() {}\n',
		'unknown type `MissingType`')
	alias_interface_out := run_good(v3_bin, 'alias_receiver_implements_interface',
		"type Text = string\n\nfn (t Text) display() string {\n\treturn t\n}\n\ninterface Displayable {\n\tdisplay() string\n}\n\nfn print_displayable(ds ...Displayable) {\n\tfor d in ds {\n\t\tprintln(d.display())\n\t}\n}\n\nfn main() {\n\tprint_displayable(Text('test'), Text('hehe'))\n}\n")
	assert alias_interface_out == 'test\nhehe'
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
	run_bad(v3_bin, 'bad_sum_constructor_extra_arg',
		'struct Empty {}\nstruct Node[T] {\n\tvalue T\n}\ntype Tree[T] = Empty | Node[T]\nfn side_effect() Node[int] {\n\treturn Node[int]{\n\t\tvalue: 1\n\t}\n}\nfn main() {\n\t_ := Tree[int](Empty{}, side_effect())\n}\n',
		'argument count mismatch for `Tree[int]`: expected 1, got 2')
	run_bad(v3_bin, 'bad_sum_constructor_extra_arg_as_call_arg',
		'struct Empty {}\nstruct Node[T] {\n\tvalue T\n}\ntype Tree[T] = Empty | Node[T]\nfn side_effect() Node[int] {\n\treturn Node[int]{\n\t\tvalue: 1\n\t}\n}\nfn use(t Tree[int]) {}\nfn main() {\n\tuse(Tree[int](Empty{}, side_effect()))\n}\n',
		'argument count mismatch for `Tree[int]`: expected 1, got 2')
	run_bad_project(v3_bin, 'bad_imported_sum_constructor_extra_arg', {
		'trees/trees.v': 'module trees\n\npub struct Empty {}\n\npub struct Node[T] {\n\tpub:\n\tvalue T\n}\n\npub type Tree[T] = Empty | Node[T]\n\npub fn side_effect() Node[int] {\n\treturn Node[int]{\n\t\tvalue: 1\n\t}\n}\n'
		'main.v':        'import trees { Empty, Tree, side_effect }\n\nfn main() {\n\t_ := Tree[int](Empty{}, side_effect())\n}\n'
	}, 'main.v', 'argument count mismatch for `trees.Tree[int]`: expected 1, got 2')
	run_bad_project(v3_bin, 'bad_aliased_sum_constructor_extra_arg', {
		'trees/trees.v': 'module trees\n\npub struct Empty {}\n\npub struct Node[T] {\n\tpub:\n\tvalue T\n}\n\npub type Tree[T] = Empty | Node[T]\n\npub fn side_effect() Node[int] {\n\treturn Node[int]{\n\t\tvalue: 1\n\t}\n}\n'
		'main.v':        'import trees as tr\n\nfn main() {\n\t_ := tr.Tree[int](tr.Empty{}, tr.side_effect())\n}\n'
	}, 'main.v', 'argument count mismatch for `trees.Tree[int]`: expected 1, got 2')
	run_bad(v3_bin, 'bad_unknown_decl_type', 'fn f(x Missing) {}\nfn main() {}\n',
		'unknown type `Missing`')
	run_bad(v3_bin, 'bad_unknown_generic_application_base',
		'fn f(x Missing[int]) {}\nfn main() {}\n', 'unknown type `Missing`')
	run_bad(v3_bin, 'bad_unknown_generic_application_arg',
		'struct Box[T] {\n\tvalue T\n}\nfn f(x Box[Missing]) {}\nfn main() {}\n',
		'unknown type `Missing`')
	run_bad(v3_bin, 'bad_single_letter_struct_arg',
		'struct A {}\nfn takes_a(a A) {}\nfn main() {\n\ttakes_a(1)\n}\n',
		'cannot use `int` as argument 1 to `takes_a`; expected `A`')
	run_bad(v3_bin, 'bad_unknown_single_letter_type', 'fn f(x Z) {}\nfn main() {}\n',
		'unknown type `Z`')
	run_bad(v3_bin, 'bad_repeated_unknown_single_letter_type',
		'fn f(x Z) Z {\n\treturn x\n}\nfn main() {}\n', 'unknown type `Z`')
	run_bad(v3_bin, 'bad_undeclared_generic_fn_param', 'fn f[T](x T, y Z) {}\nfn main() {}\n',
		'unknown type `Z`')
	run_bad(v3_bin, 'bad_undeclared_generic_struct_field',
		'struct Box[T] {\n\tother U\n}\nfn main() {}\n', 'unknown type `U`')
	run_bad_selfhost(v3_bin, 'bad_generic_param',
		'fn id[T](x T) T {\n\treturn x\n}\nfn main() {\n\t_ := id(1)\n}\n',
		'unsupported generic type parameter `T`')
	run_bad_selfhost(v3_bin, 'bad_generic_type_application',
		'fn takes_box(x Box[int]) {}\nfn main() {}\n',
		'unsupported generic type application `Box[int]`')
	run_bad_project(v3_bin, 'bad_bare_imported_call', {
		'main.v':      'module main\n\nimport moda\n\nfn main() {\n\t_ := answer()\n}\n'
		'moda/moda.v': 'module moda\n\nfn answer() int {\n\treturn 7\n}\n'
	}, 'main.v', 'unknown function `answer`')
	run_bad_project(v3_bin, 'bad_import_leak', {
		'main.v':      'module main\n\nimport moda\n\nfn main() {}\n'
		'other.v':     'module main\n\nfn use_it() int {\n\treturn moda.answer()\n}\n'
		'moda/moda.v': 'module moda\n\nfn answer() int {\n\treturn 7\n}\n'
	}, '', 'unknown identifier `moda`')
	alias_out := run_good(v3_bin, 'alias_method',
		'type UserId = int\n\nfn (id UserId) str() string {\n\treturn int_str(int(id))\n}\n\nfn main() {\n\tid := UserId(1)\n\tprintln(id.str())\n}\n')
	assert alias_out == '1'
	err_local_out := run_good(v3_bin, 'local_err_binding',
		'fn main() {\n\terr := 2\n\tprintln(int_str(err + 1))\n}\n')
	assert err_local_out == '3'
	args_contains_out := run_good(v3_bin, 'local_args_contains',
		"fn main() {\n\targs := [1, 2]\n\tif args.contains(2) {\n\t\tprintln('ok')\n\t}\n}\n")
	assert args_contains_out == 'ok'
	run_bad(v3_bin, 'required_options_param',
		"struct Options {\n\tlimit int\n}\n\nfn connect(url string, opts Options) int {\n\treturn opts.limit\n}\n\nfn main() {\n\t_ := connect('db')\n}\n",
		'argument count mismatch')
	custom_codec_out := run_good(v3_bin, 'custom_codec_methods',
		'struct Codec {}\n\nfn (c Codec) decode() int {\n\treturn 7\n}\n\nfn (c Codec) encode() int {\n\treturn 8\n}\n\nfn (c Codec) use(x int) int {\n\treturn x + 1\n}\n\nfn (c Codec) run_at(x int) int {\n\treturn x + 2\n}\n\nfn main() {\n\tc := Codec{}\n\tprintln(int_str(c.decode()))\n\tprintln(int_str(c.encode()))\n\tprintln(int_str(c.use(9)))\n\tprintln(int_str(c.run_at(5)))\n}\n')
	assert custom_codec_out == '7\n8\n10\n7'
	item_with_user_out := run_good(v3_bin, 'item_with_user_initializer',
		'struct ItemWithUserFoo {\n\titem int\n}\n\nfn main() {\n\tx := ItemWithUserFoo{\n\t\titem: 7\n\t}\n\tprintln(int_str(x.item))\n}\n')
	assert item_with_user_out == '7'
	struct_default_out := run_good(v3_bin, 'struct_default_literals',
		'type GetFn = fn () int\n\nfn default_num() int {\n\treturn 6\n}\n\nstruct Defaults {\n\tx int = 5\n\tget GetFn = default_num\n}\n\nfn main() {\n\ta := Defaults{}\n\tb := &Defaults{}\n\tprintln(int_str(a.x))\n\tprintln(int_str(b.x))\n\tprintln(int_str(a.get()))\n}\n')
	assert struct_default_out == '5\n5\n6'
	nested_embedded_method_out := run_good(v3_bin, 'nested_embedded_method',
		'struct Leaf {}\n\nfn (leaf Leaf) value() int {\n\treturn 9\n}\n\nstruct Middle {\n\tLeaf\n}\n\nstruct Outer {\n\tMiddle\n}\n\nfn main() {\n\touter := Outer{}\n\tprintln(int_str(outer.value()))\n}\n')
	assert nested_embedded_method_out == '9'
	nested_embedded_field_out := run_good(v3_bin, 'nested_embedded_field',
		'struct Leaf {\n\tx int\n}\n\nstruct Middle {\n\tLeaf\n}\n\nstruct Outer {\n\tMiddle\n}\n\nfn main() {\n\touter := Outer{\n\t\tMiddle: Middle{\n\t\t\tLeaf: Leaf{\n\t\t\t\tx: 12\n\t\t\t}\n\t\t}\n\t}\n\tprintln(int_str(outer.x))\n}\n')
	assert nested_embedded_field_out == '12'
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
	nested_array_push_out := run_good(v3_bin, 'nested_array_push',
		'fn main() {\n\tmut xs := [][]int{}\n\ty := [1, 2]\n\txs << y\n\tprintln(int_str(xs.len))\n\tprintln(int_str(xs[0][1]))\n}\n')
	assert nested_array_push_out == '1\n2'
	mut_array_for_out := run_good(v3_bin, 'mut_array_for_in',
		'struct Item {\nmut:\n\tn int\n}\n\nfn main() {\n\tmut xs := []Item{}\n\txs << Item{n: 1}\n\tfor mut item in xs {\n\t\titem.n = 7\n\t}\n\tprintln(int_str(xs[0].n))\n}\n')
	assert mut_array_for_out == '7'
	mut_array_for_scalar_out := run_good(v3_bin, 'mut_array_for_scalar_updates',
		'fn main() {\n\tmut xs := []int{}\n\txs << 1\n\txs << 2\n\tfor mut x in xs {\n\t\tx++\n\t\tx += 10\n\t}\n\tprintln(int_str(xs[0]))\n\tprintln(int_str(xs[1]))\n}\n')
	assert mut_array_for_scalar_out == '12\n13'
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
	init_order_out := run_good_project(v3_bin, 'module_init_order', {
		'main.v':      'module main\n\nimport moda\n\n__global seen int\n\nfn init() {\n\tseen = moda.value()\n}\n\nfn main() {\n\tprintln(int_str(seen))\n\tprintln(int_str(moda.value()))\n}\n'
		'moda/moda.v': 'module moda\n\n__global flag int\n\nfn init() {\n\tflag = 41\n}\n\nfn value() int {\n\treturn flag\n}\n'
	}, 'main.v')
	assert init_order_out == '41\n41'
	hier_init_order_out := run_good_project(v3_bin, 'hierarchical_module_init_order', {
		'main.v':               'module main\n\nimport parent.child\n\n__global seen int\n\nfn init() {\n\tseen = child.value()\n}\n\nfn main() {\n\tprintln(int_str(seen))\n\tprintln(int_str(child.value()))\n}\n'
		'parent/child/child.v': 'module child\n\n__global flag int\n\nfn init() {\n\tflag = 41\n}\n\npub fn value() int {\n\treturn flag\n}\n'
	}, 'main.v')
	assert hier_init_order_out == '41\n41'
	dotted_collision_init_order_out := run_good_project(v3_bin,
		'dotted_collision_module_init_order', {
		'main.v':          'module main\n\nimport foo.user as user\nimport bar as shortbar\n\nfn main() {\n\tprintln(int_str(user.value()))\n\tprintln(int_str(shortbar.value()))\n}\n'
		'foo/user/user.v': 'module user\n\nimport foo.bar as foobar\n\n__global seen int\n\nfn init() {\n\tseen = foobar.value() + 1\n}\n\npub fn value() int {\n\treturn seen\n}\n'
		'foo/bar/bar.v':   'module bar\n\n__global flag int\n\nfn init() {\n\tflag = 40\n}\n\npub fn value() int {\n\treturn flag\n}\n'
		'bar/bar.v':       'module bar\n\n__global flag int\n\nfn init() {\n\tflag = 3\n}\n\npub fn value() int {\n\treturn flag\n}\n'
	}, 'main.v')
	assert dotted_collision_init_order_out == '41\n3'
	transitive_init_order_out := run_good_project(v3_bin, 'transitive_module_init_order', {
		'main.v':      'module main\n\nimport moda\n\n__global seen int\n\nfn init() {\n\tseen = moda.value()\n}\n\nfn main() {\n\tprintln(int_str(seen))\n\tprintln(int_str(moda.value()))\n}\n'
		'moda/moda.v': 'module moda\n\nimport modb\n\npub fn value() int {\n\treturn modb.value()\n}\n'
		'modb/modb.v': 'module modb\n\n__global flag int\n\nfn init() {\n\tflag = 41\n}\n\npub fn value() int {\n\treturn flag\n}\n'
	}, 'main.v')
	assert transitive_init_order_out == '41\n41'
	global_amp_out := run_good(v3_bin, 'global_amp_initializers',
		'struct Point {\n\tx int\n\ty int\n}\n\ninterface Reader {\n\tn int\n\tread() int\n}\n\nstruct Box {\n\tn int\n}\n\nfn (b Box) read() int {\n\treturn b.n + 1\n}\n\n__global (\n\tbase_point = Point{x: 1, y: 2}\n\tassoc_point = &Point{...base_point, y: 5}\n\treader_box = Box{n: 7}\n\treader_ref = &Reader(reader_box)\n)\n\nfn main() {\n\tprintln(int_str(assoc_point.y))\n\tprintln(int_str(reader_ref.n))\n\tprintln(int_str(reader_ref.read()))\n}\n')
	assert global_amp_out == '5\n7\n8'
	disabled_if_out := run_good(v3_bin, 'disabled_if_call_elides_args',
		'__global hit int\n\n@[if trace ?]\nfn trace(x int) {}\n\nfn side_effect() int {\n\thit = 99\n\treturn 1\n}\n\nfn main() {\n\ttrace(side_effect())\n\tprintln(int_str(hit))\n}\n')
	assert disabled_if_out == '0'
	disabled_if_alias_out := run_good_project(v3_bin, 'disabled_if_alias_call_elides_args', {
		'main.v':      'module main\n\nimport moda as m\n\n__global hit int\n\nfn side_effect() int {\n\thit = 99\n\treturn 1\n}\n\nfn main() {\n\tm.trace(side_effect())\n\tprintln(int_str(hit))\n}\n'
		'moda/moda.v': 'module moda\n\n@[if trace ?]\npub fn trace(x int) {}\n'
	}, 'main.v')
	assert disabled_if_alias_out == '0'
	disabled_if_method_out := run_good_project(v3_bin, 'disabled_if_method_call_elides_args', {
		'main.v':      'module main\n\nimport moda\n\nfn main() {\n\tprintln(int_str(moda.run()))\n}\n'
		'moda/moda.v': 'module moda\n\n__global hit int\n\nstruct Tracer {}\n\n@[if trace ?]\nfn (t Tracer) trace(x int) {}\n\nfn side_effect() int {\n\thit = 99\n\treturn 1\n}\n\npub fn run() int {\n\tt := Tracer{}\n\tt.trace(side_effect())\n\treturn hit\n}\n'
	}, 'main.v')
	assert disabled_if_method_out == '0'
	disabled_operator_out := run_good(v3_bin, 'disabled_if_operator_call_elides_args',
		'__global hit int\n\nstruct Number {\n\tn int\n}\n\n@[if trace ?]\nfn (a Number) + (b Number) Number {\n\treturn Number{n: a.n + b.n}\n}\n\nfn side_effect() Number {\n\thit = 99\n\treturn Number{n: 2}\n}\n\nfn main() {\n\ta := Number{n: 1}\n\t_ := a + side_effect()\n\tprintln(int_str(hit))\n}\n')
	assert disabled_operator_out == '0'
	disabled_if_non_fn_out := run_good(v3_bin, 'disabled_if_non_fn_decl_skipped',
		'@[if trace ?]\nstruct DisabledStruct {\n\tbad MissingDisabledType\n}\n\nstruct EnabledStruct {\n\tvalue int\n}\n\nfn main() {\n\tprintln(int_str(EnabledStruct{value: 7}.value))\n}\n')
	assert disabled_if_non_fn_out == '7'
	function_defer_loop_out := run_good(v3_bin, 'function_defer_runs_each_loop_execution',
		'__global hit int\n\nfn run() {\n\tfor _ in 0 .. 3 {\n\t\tdefer(fn) {\n\t\t\thit += 100\n\t\t}\n\t}\n}\n\nfn main() {\n\trun()\n\tprintln(int_str(hit))\n}\n')
	assert function_defer_loop_out == '300'
	cross_module_array_append_c := gen_c_project(v3_bin, 'array_append_distinct_module_types', {
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tmut xs := []moda.Foo{}\n\tys := []modb.Foo{}\n\txs << ys\n}\n'
		'moda/moda.v': 'module moda\n\nstruct Foo {\n\ta int\n}\n'
		'modb/modb.v': 'module modb\n\nstruct Foo {\n\tb int\n}\n'
	}, 'main.v')
	assert !cross_module_array_append_c.contains('array_push_many(&xs')
	assert cross_module_array_append_c.contains('array_push(&xs')
}

fn test_review_generic_call_diagnostics() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'bad_nongeneric_unknown_is_pattern', 'fn check(err IError) bool {
	return err is T
}

fn main() {}
',
		'not compatible with `IError`')
	run_bad(v3_bin, 'bad_nongeneric_unknown_receiver_call', 'fn main() {
	missing.method()
}
',
		'unknown identifier `missing`')
	run_bad(v3_bin, 'bad_nongeneric_placeholder_call', 'fn main() {
	missing[T]()
}
',
		'unknown function `missing[T]`')
	run_bad(v3_bin, 'bad_generic_missing_explicit_call', 'fn invoke[T]() {
	missing[T]()
}

fn main() {
	invoke[int]()
}
',
		'unknown function `missing[T]`')
	run_bad(v3_bin, 'bad_generic_missing_receiver_method', 'fn invoke[T](value T) {
	value.no_such()
}

fn main() {
	invoke(1)
}
',
		'unknown function `value.no_such`')
	run_bad(v3_bin, 'bad_generic_missing_array_receiver_method', 'fn invoke[T](value T) {
	value.no_such()
}

fn main() {
	invoke([]int{})
}
',
		'unknown function `value.no_such`')
	run_bad(v3_bin, 'bad_generic_array_pop_argument_count', 'fn invoke[T](mut value T) {
	value.pop(1)
}

fn main() {
	mut values := [1, 2]
	invoke(mut values)
}
',
		'argument count mismatch for `value.pop`: expected 0, got 1')
	run_bad(v3_bin, 'bad_generic_array_trim_argument_type', 'fn invoke[T](mut value T) {
	value.trim("bad")
}

fn main() {
	mut values := [1, 2]
	invoke(mut values)
}
',
		'cannot use `string` as argument 1 to `value.trim`; expected `int`')
	valid_array_methods := run_good(v3_bin, 'generic_cgen_array_method_arguments', 'fn invoke[T](mut value T) {
	value.trim(1)
	value.pop()
}

fn main() {
	mut values := [1, 2]
	invoke(mut values)
	println(int_str(values.len))
}
')
	assert valid_array_methods == '0'
	run_bad(v3_bin, 'bad_generic_is_on_concrete_non_sum', 'fn matches[T](value int) bool {
	return value is T
}

fn main() {
	_ := matches[string](1)
}
',
		'`is` can only be used with sum type or interface values, not `int`')
	run_bad(v3_bin, 'bad_generic_is_pattern_for_sum', 'struct Cat {}
struct Dog {}

type Animal = Cat | Dog

fn matches[T](value Animal) bool {
	return value is T
}

fn main() {
	_ := matches[string](Animal(Cat{}))
}
',
		'`string` is not a variant of sum type `Animal`')
	run_bad(v3_bin, 'bad_generic_is_pattern_for_interface', 'interface Shape {
	area() int
}

struct Rect {}

fn (r Rect) area() int {
	return 1
}

struct Other {}

fn matches[T](value Shape) bool {
	return value is T
}

fn main() {
	_ := matches[Other](Rect{})
}
',
		'`Other` is not compatible with interface `Shape`')
	run_bad(v3_bin, 'bad_generic_is_pattern_for_ierror', 'struct NotError {}

fn matches[T](value IError) bool {
	return value is T
}

fn main() {
	_ := matches[NotError](error("x"))
}
',
		'`NotError` is not compatible with `IError`')
	run_bad(v3_bin, 'bad_generic_receiver_method_for_concrete_type', 'struct HasValue {}

fn (v HasValue) value() int {
	return 1
}

struct NoValue {}

fn read[T](value T) int {
	return value.value()
}

fn main() {
	_ := read(NoValue{})
}
',
		'unknown function `value.value`')
	run_bad(v3_bin, 'bad_generic_receiver_method_argument_count', 'struct HasValue {}

fn (v HasValue) value() int {
	return 1
}

fn read[T](value T) int {
	return value.value(1)
}

fn main() {
	_ := read(HasValue{})
}
',
		'argument count mismatch for `value.value`: expected 0, got 1')
	run_bad(v3_bin, 'bad_generic_receiver_method_argument_type', 'struct HasValue {}

fn (v HasValue) value(n int) int {
	return n
}

fn read[T](value T) int {
	return value.value("bad")
}

fn main() {
	_ := read(HasValue{})
}
',
		'cannot use `string` as argument 1 to `value.value`; expected `int`')
	out := run_good(v3_bin, 'good_generic_receiver_method_for_concrete_type', 'struct HasValue {}

fn (v HasValue) value() int {
	return 7
}

fn read[T](value T) int {
	return value.value()
}

fn main() {
	println(int_str(read(HasValue{})))
}
')
	assert out == '7'
	arg_out := run_good(v3_bin, 'good_generic_receiver_method_arguments', 'struct HasValue {}

fn (v HasValue) value(n int) int {
	return n
}

fn read[T](value T) int {
	return value.value(8)
}

fn main() {
	println(int_str(read(HasValue{})))
}
')
	assert arg_out == '8'
	params_out := run_good(v3_bin, 'good_generic_receiver_params_struct_arguments', '@[params]
struct ValueConfig {
	a int
	b int
}

struct HasValue {}

fn (v HasValue) value(n int, cfg ValueConfig) int {
	return n + cfg.a + cfg.b
}

fn read[T](value T) int {
	return value.value(1, a: 2, b: 3)
}

fn read_defaults[T](value T) int {
	return value.value(4)
}

fn main() {
	println(int_str(read(HasValue{})))
	println(int_str(read_defaults(HasValue{})))
}
')
	assert params_out == '6\n4'
	run_bad(v3_bin, 'bad_generic_receiver_method_return_context', 'struct HasValue {}

fn (v HasValue) value() int {
	return 1
}

fn read[T](value T) bool {
	return value.value()
}

fn main() {
	_ := read(HasValue{})
}
',
		'cannot return `int` as `bool`')
	run_bad(v3_bin, 'bad_generic_receiver_method_condition_context', 'struct HasValue {}

fn (v HasValue) value() int {
	return 1
}

fn read[T](value T) {
	if value.value() {}
}

fn main() {
	read(HasValue{})
}
',
		'cannot use `int` as `bool`')
	run_bad(v3_bin, 'bad_generic_receiver_method_negated_condition', 'struct HasValue {}

fn (v HasValue) value() int {
	return 1
}

fn read[T](value T) {
	if !(value.value()) {}
}

fn main() {
	read(HasValue{})
}
',
		'cannot use `int` as `bool`')
	run_bad(v3_bin, 'bad_generic_receiver_method_logical_condition', 'struct HasValue {}

fn (v HasValue) value() int {
	return 1
}

fn read[T](value T) {
	if true && value.value() {}
}

fn main() {
	read(HasValue{})
}
',
		'cannot use `int` as `bool`')
	run_bad(v3_bin, 'bad_generic_receiver_method_comparison_operand', 'struct HasValue {}

fn (v HasValue) value() int {
	return 1
}

fn read[T](value T) bool {
	return value.value() == true
}

fn main() {
	_ := read(HasValue{})
}
',
		'cannot use `int` as `bool`')
	comparison_out := run_good(v3_bin, 'good_generic_receiver_method_numeric_comparison', 'struct HasValue {}

fn (v HasValue) value() u8 {
	return 1
}

fn read[T](value T) bool {
	return value.value() == 1
}

fn main() {
	println(read(HasValue{}))
}
')
	assert comparison_out == 'true'
	run_bad(v3_bin, 'bad_generic_receiver_method_assignment_context', 'struct HasValue {}

fn (v HasValue) value() int {
	return 1
}

fn read[T](value T) {
	mut ok := false
	ok = value.value()
}

fn main() {
	read(HasValue{})
}
',
		'cannot use `int` as `bool`')
	run_bad(v3_bin, 'bad_generic_function_field_argument', 'struct HasCallback {
	cb fn (int)
}

fn invoke[T](value T) {
	value.cb("bad")
}

fn main() {
	invoke(HasCallback{
		cb: fn (n int) {}
	})
}
',
		'cannot use `string` as argument 1 to `value.cb`; expected `int`')
	run_bad(v3_bin, 'bad_generic_function_field_argument_count', 'struct HasCallback {
	cb fn (int)
}

fn invoke[T](value T) {
	value.cb()
}

fn main() {
	invoke(HasCallback{
		cb: fn (n int) {}
	})
}
',
		'argument count mismatch for `value.cb`: expected 1, got 0')
	fn_field_out := run_good(v3_bin, 'good_generic_function_field_argument', 'struct HasCallback {
	cb fn (int) int
}

fn invoke[T](value T) int {
	return value.cb(3)
}

fn main() {
	println(int_str(invoke(HasCallback{
		cb: fn (n int) int {
			return n + 1
		}
	})))
}
')
	assert fn_field_out == '4'
	run_bad(v3_bin, 'bad_generic_receiver_params_field_type', '@[params]
struct OpenOptions {
	limit int
}

struct Service {}

fn (s Service) open(opts OpenOptions) {}

fn invoke[T](service T) {
	service.open(limit: "bad")
}

fn main() {
	invoke(Service{})
}
',
		'cannot initialize field `limit` with `string`; expected `int`')
	run_bad(v3_bin, 'bad_generic_receiver_params_unknown_field', '@[params]
struct OpenOptions {
	limit int
}

struct Service {}

fn (s Service) open(opts OpenOptions) {}

fn invoke[T](service T) {
	service.open(missing: 1)
}

fn main() {
	invoke(Service{})
}
',
		'unknown field `missing` in `OpenOptions`')
	run_bad(v3_bin, 'bad_generic_receiver_narrow_unsigned_literal', 'struct Sink {}

fn (s Sink) take(value u8) {}

fn invoke[T](sink T) {
	sink.take(300)
}

fn main() {
	invoke(Sink{})
}
',
		'cannot use `int` as argument 1 to `sink.take`; expected `u8`')
	run_bad(v3_bin, 'bad_generic_receiver_narrow_signed_literal', 'struct Sink {}

fn (s Sink) take(value i8) {}

fn invoke[T](sink T) {
	sink.take(128)
}

fn main() {
	invoke(Sink{})
}
',
		'cannot use `int` as argument 1 to `sink.take`; expected `i8`')
	literal_out := run_good(v3_bin, 'good_generic_receiver_narrow_literal_boundaries', 'struct Sink {}

fn (s Sink) take_unsigned(value u8) {}
fn (s Sink) take_signed(value i8) {}

fn invoke[T](sink T) {
	sink.take_unsigned(255)
	sink.take_signed(-128)
}

fn main() {
	invoke(Sink{})
	println("ok")
}
')
	assert literal_out == 'ok'
	run_bad(v3_bin, 'bad_generic_receiver_none_for_result', 'struct Sink {}

fn (s Sink) take(value !int) {}

fn invoke[T](sink T) {
	sink.take(none)
}

fn main() {
	invoke(Sink{})
}
',
		'cannot use `none` as argument 1 to `sink.take`; expected `!int`')
	result_error_out := run_good(v3_bin, 'good_generic_receiver_ierror_for_result', 'struct Sink {}

fn (s Sink) take(value !int) {}

fn invoke[T](sink T, err IError) {
	sink.take(error("literal"))
	sink.take(err)
}

fn main() {
	invoke(Sink{}, error("value"))
	println("ok")
}
')
	assert result_error_out == 'ok'
	option_out := run_good(v3_bin, 'good_generic_receiver_none_for_option', 'struct Sink {}

fn (s Sink) take(value ?int) {}

fn invoke[T](sink T) {
	sink.take(none)
}

fn main() {
	invoke(Sink{})
	println("ok")
}
')
	assert option_out == 'ok'
	run_bad(v3_bin, 'bad_generic_receiver_variadic_spread_type', 'struct Sink {}

fn (s Sink) take(values ...int) {}

fn invoke[T](sink T, values []string) {
	sink.take(...values)
}

fn main() {
	invoke(Sink{}, ["bad"])
}
',
		'cannot use `[]string` as argument 1 to `sink.take`; expected `[]int`')
	variadic_out := run_good(v3_bin, 'good_generic_receiver_variadic_spread_type', 'struct Sink {}

fn (s Sink) take(values ...int) {}

fn invoke[T](sink T, values []int) {
	sink.take(...values)
}

fn main() {
	invoke(Sink{}, [1, 2])
	println("ok")
}
')
	assert variadic_out == 'ok'
}

// Regression tests for the post-PR review fixes: fixed-array literals must match
// the expected fixed length, and genuine fixed-array if-branches of different
// lengths must mismatch — while bare array literals stay length-agnostic.
fn test_fixed_array_length_checks() {
	v3_bin := build_v3()
	// A literal passed where a fixed array is expected must have the exact length.
	run_bad(v3_bin, 'bad_fixed_array_arg_len',
		'fn take4(a [4]int) int {\n\treturn a[0]\n}\nfn main() {\n\t_ := take4([1, 2])\n}\n',
		'expected `int[4]`')
	// Genuine fixed arrays of different lengths in if-branches must mismatch.
	run_bad(v3_bin, 'bad_if_branch_fixed_array_len',
		'fn main() {\n\ta := [2]int{}\n\tb := [3]int{}\n\tc := true\n\t_ := if c { a } else { b }\n}\n',
		'if-expression branch type mismatch')
	// Correct fixed-array length is accepted and round-trips.
	good4 := run_good(v3_bin, 'good_fixed_array_arg_len',
		'fn take4(a [4]int) int {\n\treturn a[0] + a[1] + a[2] + a[3]\n}\nfn main() {\n\tprintln(int_str(take4([10, 20, 30, 40])))\n}\n')
	assert good4 == '100'
	// Bare array literals are dynamic `[]T`, so different-length branches are fine.
	good_lit := run_good(v3_bin, 'good_if_branch_array_literal_len',
		'fn main() {\n\tc := true\n\tx := if c { [1, 2, 3] } else { [4, 5] }\n\tprintln(int_str(x.len))\n}\n')
	assert good_lit == '3'
	// `return if cond { ... } else { ... }` with fixed-array-const branches must
	// coerce each branch to the dynamic `[]T` return type (like a plain return and
	// the `return match` path), not return the bare C fixed array.
	good_ret_const := run_good(v3_bin, 'good_return_if_fixed_const',
		'const wp3 = [1, 2, 3]\nconst wp2 = [4, 5]\nfn pick(c bool) []int {\n\treturn if c { wp3 } else { wp2 }\n}\nfn main() {\n\tprintln(int_str(pick(true).len + pick(false).len))\n}\n')
	assert good_ret_const == '5'
	good_ret_lit := run_good(v3_bin, 'good_return_if_array_literal',
		'fn pick(c bool) []int {\n\treturn if c { [1, 2, 3] } else { [4, 5] }\n}\nfn main() {\n\tprintln(int_str(pick(true).len + pick(false).len))\n}\n')
	assert good_ret_lit == '5'
	mixed_const_src := 'const xs = [1, 2, 3]\nfn first() int {\n\treturn xs[0]\n}\nfn length_score() int {\n\treturn xs.len\n}\nfn all() []int {\n\treturn xs\n}\nfn main() {\n\tys := all()\n\tprintln(int_str(first() + length_score() + ys.len + ys[2]))\n}\n'
	mixed_const := run_good(v3_bin, 'good_indexed_const_returned_dynamic_array', mixed_const_src)
	assert mixed_const == '10'
	mixed_const_c := gen_c_project(v3_bin, 'good_indexed_const_returned_dynamic_array_c', {
		'main.v': mixed_const_src
	}, 'main.v')
	mixed_const_compact := mixed_const_c.replace('\t', '').replace(' ', '').replace('\n', '')
	assert mixed_const_compact.contains('Arraymain__xs;'), mixed_const_c
	assert mixed_const_compact.contains('main__xs=new_array_from_c_array(3,3,sizeof(int),(int[]){1,2,3});'), mixed_const_c

	assert mixed_const_compact.contains('return(*(int*)array_get(main__xs,0));'), mixed_const_c
	assert mixed_const_compact.contains('intlength_score(void){returnmain__xs.len;}'), mixed_const_c
	assert mixed_const_compact.contains('Arrayall(void){returnmain__xs;}'), mixed_const_c
	assert !mixed_const_compact.contains('returnnew_array_from_c_array(3,3,sizeof(int),&main__xs);'), mixed_const_c

	shadowed := run_good_project(v3_bin, 'good_shadowed_const_fixed_storage', {
		'main.v':       'module main\n\nimport fixture\n\nconst xs = [10, 20, 30]\n\nfn all() []int {\n\treturn xs\n}\n\nfn main() {\n\tys := all()\n\tprintln(int_str(fixture.first() + ys.len + ys[2]))\n}\n'
		'fixture/fi.v': 'module fixture\n\npub const xs = [1, 2, 3]\n\npub fn first() int {\n\treturn xs[0]\n}\n'
	}, 'main.v')
	assert shadowed == '34'
	shadowed_c := gen_c_project(v3_bin, 'good_shadowed_const_fixed_storage_c', {
		'main.v':       'module main\n\nimport fixture\n\nconst xs = [10, 20, 30]\n\nfn all() []int {\n\treturn xs\n}\n\nfn main() {\n\tys := all()\n\tprintln(int_str(fixture.first() + ys.len + ys[2]))\n}\n'
		'fixture/fi.v': 'module fixture\n\npub const xs = [1, 2, 3]\n\npub fn first() int {\n\treturn xs[0]\n}\n'
	}, 'main.v')
	shadowed_compact := shadowed_c.replace('\t', '').replace(' ', '').replace('\n', '')
	assert shadowed_compact.contains('Arraymain__xs;'), shadowed_c
	assert shadowed_compact.contains('returnmain__xs;'), shadowed_c
	assert shadowed_compact.contains('fixture__xs[3]'), shadowed_c
	assert shadowed_compact.contains('returnfixture__xs[0];'), shadowed_c
}

fn test_statement_if_branch_tails_are_not_value_checked() {
	v3_bin := build_v3()
	statement_if := run_good(v3_bin, 'statement_if_mixed_tail_exprs',
		"fn main() {\n\tmut n := 0\n\tmut errors := []string{}\n\tif true {\n\t\tn++\n\t} else {\n\t\terrors << 'bad'\n\t}\n\tprintln(int_str(n + errors.len))\n}\n")
	assert statement_if == '1'
	run_bad(v3_bin, 'bad_if_branch_primitive_mismatch',
		"fn main() {\n\tc := true\n\t_ := if c { 1 } else { 'bad' }\n}\n",
		'if-expression branch type mismatch')
}

fn test_multi_return_if_tail_infers_common_type() {
	v3_bin := build_v3()
	result_error := run_good(v3_bin, 'good_result_multi_return_error_return',
		"fn pair(fail bool) !(int, int) {\n\tif fail {\n\t\treturn error('x')\n\t}\n\treturn 1, 2\n}\nfn main() {\n\ta, b := pair(false) or { panic(err.msg()) }\n\tprintln(int_str(a + b))\n\t_, _ := pair(true) or {\n\t\tprintln(err.msg())\n\t\treturn\n\t}\n}\n")
	assert result_error == '3\nx'
	if_tail := run_good(v3_bin, 'good_multi_return_if_common_pointer_tail',
		'fn main() {\n\tx := 7\n\tp, n := if false {\n\t\tnil\n\t\t0\n\t} else {\n\t\t&x\n\t\t1\n\t}\n\tprintln(typeof(p).name)\n\tprintln(int_str(n))\n}\n')
	assert if_tail == '&void\n1'
	numeric_tail := run_good(v3_bin, 'good_multi_return_if_promoted_numeric_tail',
		'fn main() {\n\tcond := false\n\tx, _ := if cond {\n\t\t1\n\t\t0\n\t} else {\n\t\t1.5\n\t\t0\n\t}\n\tprintln(typeof(x).name)\n\tprintln((x > 1.4).str())\n}\n')
	assert numeric_tail == 'f64\ntrue'
	call_decl_assign := run_good(v3_bin, 'good_multi_return_if_call_decl_assign',
		'fn pair(n int) (int, int) {\n\treturn n, n + 1\n}\nfn main() {\n\tflag := false\n\ta, b := if flag {\n\t\tpair(1)\n\t} else {\n\t\tpair(3)\n\t}\n\tprintln(int_str(a + b))\n}\n')
	assert call_decl_assign == '7'
	call_assign := run_good(v3_bin, 'good_multi_return_if_call_assign',
		'fn pair(n int) (int, int) {\n\treturn n, n + 1\n}\nfn main() {\n\tflag := false\n\tmut a := 0\n\tmut b := 0\n\ta, b = if flag {\n\t\tpair(1)\n\t} else {\n\t\tpair(3)\n\t}\n\tprintln(int_str(a + b))\n}\n')
	assert call_assign == '7'
	run_bad(v3_bin, 'bad_multi_return_if_call_mixed_tuple_tail_decl_assign',
		'fn pair(n int) (int, int) {\n\treturn n, n + 1\n}\nfn main() {\n\tflag := true\n\ta, b := if flag {\n\t\tpair(1)\n\t} else {\n\t\t3\n\t\t4\n\t}\n\tprintln(int_str(a + b))\n}\n',
		'multi-return assignment mismatch')
	run_bad(v3_bin, 'bad_multi_return_if_void_tail_decl_assign',
		'fn side() {}\nfn main() {\n\tflag := true\n\ta, b := if flag {\n\t\tside()\n\t\t1\n\t} else {\n\t\tside()\n\t\t2\n\t}\n\tprintln(int_str(b))\n}\n',
		'multi-return assignment mismatch')
}

fn test_multi_return_if_assignment_uses_lhs_context() {
	v3_bin := build_v3()
	enum_tail := run_good(v3_bin, 'good_multi_return_if_assign_enum_and_none_tail',
		"enum Color {\n\tred\n\tblue\n}\n\nfn main() {\n\tcond := false\n\tmut c := Color.red\n\tmut n := 0\n\tc, n = if cond {\n\t\t.red\n\t\t1\n\t} else {\n\t\t.blue\n\t\t2\n\t}\n\tmut opt := ?int(0)\n\tmut label := ''\n\topt, label = if cond {\n\t\tnone\n\t\t'none'\n\t} else {\n\t\t?int(7)\n\t\t'some'\n\t}\n\tvalue := opt or { 0 }\n\tif c == .blue && label == 'some' {\n\t\tprintln(int_str(n + value))\n\t}\n}\n")
	assert enum_tail == '9'
	swap_tail := run_good(v3_bin, 'good_multi_return_if_assign_stages_tail_values',
		"fn main() {\n\tcond := true\n\tmut a := 1\n\tmut b := 2\n\ta, b = if cond {\n\t\tb\n\t\ta\n\t} else {\n\t\ta\n\t\tb\n\t}\n\tprintln(int_str(a) + ':' + int_str(b))\n}\n")
	assert swap_tail == '2:1'
}

fn test_nested_if_tuple_tail_multi_return_lowers_each_value() {
	v3_bin := build_v3()
	nested_if_tail := run_good(v3_bin, 'good_nested_if_tail_decl_assign',
		'fn main() {\n\tc := true\n\td := false\n\ta, b := if c {\n\t\tif d {\n\t\t\t1\n\t\t\t2\n\t\t} else {\n\t\t\t3\n\t\t\t4\n\t\t}\n\t} else {\n\t\t5\n\t\t6\n\t}\n\tprintln(int_str(a + b))\n}\n')
	assert nested_if_tail == '7'
	prefixed_block_tail := run_good(v3_bin, 'good_prefixed_nested_block_tail_decl_assign',
		"fn main() {\n\tc := true\n\ta, b := if c {\n\t\t{\n\t\t\tprintln('side')\n\t\t\t1\n\t\t\t2\n\t\t}\n\t} else {\n\t\t3\n\t\t4\n\t}\n\tprintln(int_str(a + b))\n}\n")
	assert prefixed_block_tail == 'side\n3'
}

fn test_multi_return_if_returning_branches_do_not_produce_assignment_values() {
	v3_bin := build_v3()
	return_values := run_good(v3_bin, 'good_multi_return_if_branch_return_values',
		"fn choose(cond bool) (string, string) {\n\ta, b := if cond {\n\t\treturn 'x', 'y'\n\t} else {\n\t\t1\n\t\t2\n\t}\n\treturn int_str(a), int_str(b)\n}\n\nfn main() {\n\ta, b := choose(false)\n\tprintln(a + ':' + b)\n\tc, d := choose(true)\n\tprintln(c + ':' + d)\n}\n")
	assert return_values == '1:2\nx:y'
	bare_return := run_good(v3_bin, 'good_multi_return_if_branch_bare_return',
		"fn emit(cond bool) {\n\ta, b := if cond {\n\t\treturn\n\t} else {\n\t\t3\n\t\t4\n\t}\n\tprintln(int_str(a + b))\n}\n\nfn main() {\n\temit(false)\n\tprintln('done')\n}\n")
	assert bare_return == '7\ndone'
	panic_branch := run_good(v3_bin, 'good_multi_return_if_branch_panic_tail',
		"fn main() {\n\tbad := false\n\ta, b := if bad {\n\t\tpanic('x')\n\t} else {\n\t\t5\n\t\t6\n\t}\n\tprintln(int_str(a + b))\n}\n")
	assert panic_branch == '11'
}

fn test_multi_rhs_if_expr_is_not_multi_return() {
	v3_bin := build_v3()
	decl_out := run_good(v3_bin, 'good_multi_rhs_if_expr_decl_assign',
		'fn main() {\n\tcond := true\n\ta, b := if cond { 1 } else { 2 }, 3\n\tprintln(int_str(a + b))\n}\n')
	assert decl_out == '4'
	assign_out := run_good(v3_bin, 'good_multi_rhs_if_expr_assign',
		'fn main() {\n\tcond := false\n\tmut a := 0\n\tmut b := 0\n\ta, b = if cond { 1 } else { 2 }, 3\n\tprintln(int_str(a + b))\n}\n')
	assert assign_out == '5'
}

fn test_match_multi_return_tails_require_explicit_tuple() {
	v3_bin := build_v3()
	match_call := run_good(v3_bin, 'good_multi_return_match_call_return',
		'fn pair(n int) (int, int) {\n\treturn n, n + 1\n}\nfn pick(flag bool) (int, int) {\n\treturn match flag {\n\t\ttrue {\n\t\t\tpair(1)\n\t\t}\n\t\tfalse {\n\t\t\tpair(3)\n\t\t}\n\t}\n}\nfn main() {\n\ta, b := pick(false)\n\tprintln(int_str(a + b))\n}\n')
	assert match_call == '7'
	match_decl_assign := run_good(v3_bin, 'good_multi_return_match_call_decl_assign',
		'fn pair(n int) (int, int) {\n\treturn n, n + 1\n}\nfn main() {\n\tflag := false\n\ta, b := match flag {\n\t\ttrue {\n\t\t\tpair(1)\n\t\t}\n\t\tfalse {\n\t\t\tpair(3)\n\t\t}\n\t}\n\tprintln(int_str(a + b))\n}\n')
	assert match_decl_assign == '7'
	match_assign := run_good(v3_bin, 'good_multi_return_match_call_assign',
		'fn pair(n int) (int, int) {\n\treturn n, n + 1\n}\nfn main() {\n\tflag := false\n\tmut a := 0\n\tmut b := 0\n\ta, b = match flag {\n\t\ttrue {\n\t\t\tpair(1)\n\t\t}\n\t\tfalse {\n\t\t\tpair(3)\n\t\t}\n\t}\n\tprintln(int_str(a + b))\n}\n')
	assert match_assign == '7'
	match_tuple_decl_assign := run_good(v3_bin, 'good_multi_return_match_tuple_decl_assign',
		'fn main() {\n\tflag := false\n\ta, b := match flag {\n\t\ttrue { 1, 2 }\n\t\tfalse { 3, 4 }\n\t}\n\tprintln(int_str(a + b))\n}\n')
	assert match_tuple_decl_assign == '7'
	match_tuple_assign := run_good(v3_bin, 'good_multi_return_match_tuple_assign',
		'fn main() {\n\tflag := true\n\tmut a := 0\n\tmut b := 0\n\ta, b = match flag {\n\t\ttrue { 1, 2 }\n\t\tfalse { 3, 4 }\n\t}\n\tprintln(int_str(a + b))\n}\n')
	assert match_tuple_assign == '3'
	run_bad(v3_bin, 'bad_multi_return_match_overlong_tuple_tail_decl_assign',
		'fn main() {\n\tflag := true\n\ta, b := match flag {\n\t\ttrue { 1, 2, 3 }\n\t\tfalse { 4, 5 }\n\t}\n\tprintln(int_str(a + b))\n}\n',
		'match expression branches cannot produce multiple assignment values')
	run_bad(v3_bin, 'bad_multi_return_match_call_non_exhaustive_decl_assign',
		'fn pair(n int) (int, int) {\n\treturn n, n + 1\n}\nfn main() {\n\tflag := true\n\ta, b := match flag {\n\t\ttrue {\n\t\t\tpair(1)\n\t\t}\n\t}\n\tprintln(int_str(a + b))\n}\n',
		'match expression must be exhaustive')
	run_bad(v3_bin, 'bad_multi_return_match_call_non_exhaustive_assign',
		'fn pair(n int) (int, int) {\n\treturn n, n + 1\n}\nfn main() {\n\tflag := true\n\tmut a := 0\n\tmut b := 0\n\ta, b = match flag {\n\t\ttrue {\n\t\t\tpair(1)\n\t\t}\n\t}\n\tprintln(int_str(a + b))\n}\n',
		'match expression must be exhaustive')
	run_bad(v3_bin, 'bad_multi_return_match_call_mixed_item_types',
		'fn pair_int() (int, int) {\n\treturn 1, 2\n}\nfn pair_f64() (f64, int) {\n\treturn 1.5, 2\n}\nfn main() {\n\tflag := true\n\ta, b := match flag {\n\t\ttrue {\n\t\t\tpair_int()\n\t\t}\n\t\tfalse {\n\t\t\tpair_f64()\n\t\t}\n\t}\n\tprintln(int_str(b))\n\tprintln(a)\n}\n',
		'multi-return assignment mismatch')
	run_bad(v3_bin, 'bad_multi_return_match_tail_decl_assign',
		'fn main() {\n\tflag := true\n\ta, b := match flag {\n\t\ttrue {\n\t\t\t1\n\t\t\t2\n\t\t}\n\t\tfalse {\n\t\t\t3\n\t\t\t4\n\t\t}\n\t}\n\tprintln(int_str(a + b))\n}\n',
		'match expression branches cannot produce multiple assignment values')
	run_bad(v3_bin, 'bad_multi_return_match_call_mixed_tuple_tail_decl_assign',
		'fn pair(n int) (int, int) {\n\treturn n, n + 1\n}\nfn main() {\n\tflag := true\n\ta, b := match flag {\n\t\ttrue {\n\t\t\tpair(1)\n\t\t}\n\t\tfalse {\n\t\t\t3\n\t\t\t4\n\t\t}\n\t}\n\tprintln(int_str(a + b))\n}\n',
		'match expression branches cannot produce multiple assignment values')
	run_bad(v3_bin, 'bad_multi_return_match_preceding_expr_decl_assign',
		"fn log() int {\n\tprintln('log')\n\treturn 9\n}\nfn main() {\n\tflag := true\n\ta, b := match flag {\n\t\ttrue {\n\t\t\tlog()\n\t\t\t1\n\t\t}\n\t\tfalse { 2, 3 }\n\t}\n\tprintln(int_str(a + b))\n}\n",
		'match expression branches cannot produce multiple assignment values')
	run_bad(v3_bin, 'bad_multi_return_match_preceding_expr_assign',
		"fn log() int {\n\tprintln('log')\n\treturn 9\n}\nfn main() {\n\tflag := true\n\tmut a := 0\n\tmut b := 0\n\ta, b = match flag {\n\t\ttrue {\n\t\t\tlog()\n\t\t\t1\n\t\t}\n\t\tfalse { 2, 3 }\n\t}\n\tprintln(int_str(a + b))\n}\n",
		'match expression branches cannot produce multiple assignment values')
	run_bad(v3_bin, 'bad_multi_return_match_tail_return',
		'fn pair(flag bool) (int, int) {\n\treturn match flag {\n\t\ttrue {\n\t\t\t1\n\t\t\t2\n\t\t}\n\t\tfalse {\n\t\t\t3\n\t\t\t4\n\t\t}\n\t}\n}\nfn main() {\n\ta, b := pair(true)\n\tprintln(int_str(a + b))\n}\n',
		'match expression branches cannot produce multiple return values')
}

fn test_return_if_tuple_tail_multi_return_is_rejected() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'bad_multi_return_if_tail_return',
		'fn pair(flag bool) (int, int) {\n\treturn if flag {\n\t\t1\n\t\t2\n\t} else {\n\t\t3\n\t\t4\n\t}\n}\nfn main() {\n\ta, b := pair(true)\n\tprintln(int_str(a + b))\n}\n',
		'if expression branches cannot produce multiple return values')
}

fn test_pr_review_struct_sum_scope_and_gated_regressions() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'bad_non_variadic_array_struct_field_args',
		'struct Point {\n\tx int\n\ty int\n}\n\nfn total(points []Point) int {\n\treturn points.len\n}\n\nfn main() {\n\t_ := total(x: 1, y: 2)\n}\n',
		'cannot use `key: value` arguments as `[]Point`')
	variadic_struct := run_good(v3_bin, 'good_variadic_struct_field_args',
		'struct Point {\n\tx int\n\ty int\n}\n\nfn total(points ...Point) int {\n\treturn points[0].x + points[0].y\n}\n\nfn main() {\n\tprintln(int_str(total(x: 3, y: 4)))\n}\n')
	assert variadic_struct == '7'
	run_bad_project(v3_bin, 'bad_module_type_does_not_bind_main_type', {
		'v.mod':  "Module { name: 'module_unknown_scope' }\n"
		'main.v': 'module main\n\nimport m\n\nstruct Foo {}\n\nfn main() {\n\t_ = m.consume(Foo{})\n}\n'
		'm/m.v':  'module m\n\npub const value = 1\n\npub fn consume(x Foo) int {\n\treturn value\n}\n'
	}, 'main.v', 'expected `m.Foo`')
	aliased_sum := run_good_project(v3_bin, 'good_aliased_sum_uses_full_suffix', {
		'v.mod':           "Module { name: 'aliased_sum_full_suffix' }\n"
		'main.v':          'module main\n\nimport other as _\nimport sub.tast as tast\n\nfn main() {\n\tvalue := tast.make_beta()\n\tif value is tast.Beta {\n\t\tprintln(int_str(value.n))\n\t} else {\n\t\tprintln("wrong")\n\t}\n}\n'
		'other/other.v':   'module other\n\npub struct Alpha {\npub:\n\tn int\n}\n\npub struct Beta {\npub:\n\tn int\n}\n\npub type Value = Beta | Alpha\n'
		'sub/tast/tast.v': 'module tast\n\npub struct Alpha {\npub:\n\tn int\n}\n\npub struct Beta {\npub:\n\tn int\n}\n\npub type Value = Alpha | Beta\n\npub fn make_beta() Value {\n\treturn Beta{\n\t\tn: 9\n\t}\n}\n'
	}, 'main.v')
	assert aliased_sum == '9'
	gated := run_good(v3_bin, 'good_gated_scalar_and_fixed_array_indexes',
		'struct Box {\n\titems [10]int\n}\n\nfn main() {\n\ta := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]\n\tprintln(int_str(a#[-1]))\n\tprintln(int_str(a#[-11] or { 42 }))\n\tprintln(a#[-2..].str())\n\tprintln("0123456789"#[-1].ascii_str())\n\tfixed := [10]int{init: index}\n\tprintln(int_str(fixed#[-1]))\n\tprintln(fixed#[-2..].str())\n\tbox := Box{\n\t\titems: fixed\n\t}\n\tprintln(int_str(box.items#[-1]))\n}\n')
	assert gated == '9\n42\n[8, 9]\n9\n9\n[8, 9]\n9'
}

fn test_local_type_names_include_nested_block_scope() {
	v3_bin := build_v3()
	block_rows := run_good(v3_bin, 'good_local_struct_sibling_block_scope',
		'fn main() {\n\tmut total := 0\n\tif true {\n\t\tstruct Row {\n\t\t\ta int\n\t\t}\n\t\tr := Row{\n\t\t\ta: 1\n\t\t}\n\t\ttotal += r.a\n\t}\n\tif true {\n\t\tstruct Row {\n\t\t\tb int\n\t\t}\n\t\tr := Row{\n\t\t\tb: 2\n\t\t}\n\t\ttotal += r.b\n\t}\n\tprintln(int_str(total))\n}\n')
	assert block_rows == '3'
	embedded_local := run_good(v3_bin, 'good_local_embedded_struct_field',
		'fn main() {\n\tstruct Inner {\n\t\tn int\n\t}\n\tstruct Outer {\n\t\tInner\n\t}\n\touter := Outer{\n\t\tInner{\n\t\t\tn: 12\n\t\t}\n\t}\n\tprintln(int_str(outer.n))\n}\n')
	assert embedded_local == '12'
	local_generic_arg := run_good(v3_bin, 'good_local_generic_struct_init_local_arg',
		"fn main() {\n\tstruct Inner {}\n\tstruct Box[T] {}\n\t_ := Box[Inner]{}\n\tprintln('ok')\n}\n")
	assert local_generic_arg == 'ok'
	mutual_local := run_good(v3_bin, 'good_local_mutual_struct_pointer_fields',
		"fn main() {\n\tstruct A {\n\t\tb &B\n\t}\n\tstruct B {\n\t\ta &A\n\t}\n\ta := A{}\n\t_ := B{\n\t\ta: &a\n\t}\n\tprintln('ok')\n}\n")
	assert mutual_local == 'ok'
}

fn test_module_local_error_type_shadows_builtin_error() {
	v3_bin := build_v3()
	out := run_good_project(v3_bin, 'good_module_local_error_type', {
		'main.v':       'module main\n\nimport fixture\n\nfn main() {\n\terr := fixture.make_error()\n\tprintln(err.msg())\n}\n'
		'fixture/fi.v': "module fixture\n\npub struct Error {\n\tmessage string\n}\n\npub fn make_error() Error {\n\treturn Error{\n\t\tmessage: 'local'\n\t}\n}\n\npub fn (err Error) msg() string {\n\treturn err.message\n}\n"
	}, 'main.v')
	assert out == 'local'
}

fn test_bool_match_single_branch_is_exhaustive() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'good_bool_match_single_branch_exhaustive',
		'fn f(b bool) int {\n\tmatch b {\n\t\ttrue, false {\n\t\t\treturn 1\n\t\t}\n\t}\n}\n\nfn main() {\n\tprintln(int_str(f(true) + f(false)))\n}\n')
	assert out == '2'
	alias_out := run_good(v3_bin, 'good_bool_alias_match_single_branch_exhaustive',
		'type Flag = bool\nfn f(b Flag) int {\n\tmatch b {\n\t\ttrue, false {\n\t\t\treturn 1\n\t\t}\n\t}\n}\n\nfn main() {\n\tprintln(int_str(f(true) + f(false)))\n}\n')
	assert alias_out == '2'
	run_bad(v3_bin, 'bad_bool_pointer_match_not_exhaustive',
		'fn f(b &bool) int {\n\tmatch b {\n\t\ttrue, false {\n\t\t\treturn 1\n\t\t}\n\t}\n}\n\nfn main() {\n\tmut b := true\n\tprintln(int_str(f(&b)))\n}\n',
		'missing return')
}

fn test_voidptr_variadic_spread_requires_voidptr_array() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'bad_voidptr_variadic_int_spread',
		'fn sink(args ...voidptr) int {\n\treturn args.len\n}\n\nfn main() {\n\txs := [1, 2]\n\tprintln(int_str(sink(...xs)))\n}\n',
		'cannot use `[]int` as argument 1 to `sink`')
	run_bad(v3_bin, 'bad_voidptr_variadic_void_arg',
		'fn side() {}\n\nfn sink(args ...voidptr) int {\n\treturn args.len\n}\n\nfn main() {\n\tprintln(int_str(sink(side())))\n}\n',
		'cannot use `void`')
	run_bad(v3_bin, 'bad_voidptr_variadic_none_arg',
		'fn sink(args ...voidptr) int {\n\treturn args.len\n}\n\nfn main() {\n\tprintln(int_str(sink(none)))\n}\n',
		'cannot use `?void`')
	run_bad(v3_bin, 'bad_voidptr_variadic_multi_return_arg',
		'fn pair() (int, int) {\n\treturn 1, 2\n}\n\nfn sink(args ...voidptr) int {\n\treturn args.len\n}\n\nfn main() {\n\tprintln(int_str(sink(pair())))\n}\n',
		'cannot use `(int, int)`')
	run_bad(v3_bin, 'bad_voidptr_variadic_enum_shorthand_arg',
		'enum Color {\n\tred\n}\n\nfn sink(args ...voidptr) int {\n\treturn args.len\n}\n\nfn main() {\n\tprintln(int_str(sink(.red)))\n}\n',
		'cannot use `int`')
	good := run_good(v3_bin, 'good_voidptr_variadic_voidptr_spread',
		'fn sink(args ...voidptr) int {\n\treturn args.len\n}\n\nfn main() {\n\tx := 7\n\txs := [voidptr(&x)]\n\tprintln(int_str(sink(...xs)))\n\tprintln(int_str(sink(1)))\n}\n')
	assert good == '1\n1'
}

// Regression tests for the post-PR review fixes around generic struct receivers
// and generic heap struct literals.
fn test_generic_struct_receiver_and_heap_init() {
	v3_bin := build_v3()
	// Two *different concrete* instantiations are incompatible: `Box[string]` must
	// not satisfy an expected `Box[int]` (value or pointer form).
	run_bad(v3_bin, 'bad_generic_concrete_value_arg',
		"struct Box[T] {\n\tval T\n}\nfn use_int(b Box[int]) int {\n\treturn b.val\n}\nfn main() {\n\tbs := Box[string]{\n\t\tval: 'hi'\n\t}\n\t_ := use_int(bs)\n}\n",
		'expected `Box[int]`')
	run_bad(v3_bin, 'bad_generic_concrete_ptr_arg',
		"struct Box[T] {\n\tval T\n}\nfn use_int(b &Box[int]) int {\n\treturn b.val\n}\nfn main() {\n\tbs := &Box[string]{\n\t\tval: 'hi'\n\t}\n\t_ := use_int(bs)\n}\n",
		'expected `&Box[int]`')
	// A generic method on the open `Box[T]` receiver works for every instantiation.
	good_method := run_good(v3_bin, 'good_generic_method_instances',
		"struct Box[T] {\n\tval T\n}\nfn (b Box[T]) get() T {\n\treturn b.val\n}\nfn main() {\n\tbi := Box[int]{\n\t\tval: 42\n\t}\n\tbs := Box[string]{\n\t\tval: 'hi'\n\t}\n\tprintln(int_str(bi.get()))\n\tprintln(bs.get())\n}\n")
	assert good_method == '42\nhi'
	generic_return := run_good(v3_bin, 'good_generic_return_open_struct_param',
		'struct Match[T] {\n\tvalue T\n\thas   bool\n}\nfn choose[T](use_second bool, first Match[T], second Match[T]) Match[T] {\n\tif use_second {\n\t\treturn second\n\t}\n\treturn first\n}\nfn (m Match[T]) or[T](other Match[T]) Match[T] {\n\tif m.has {\n\t\treturn m\n\t}\n\treturn other\n}\nfn main() {\n\ta := Match[int]{\n\t\tvalue: 1\n\t\thas: true\n\t}\n\tb := Match[int]{\n\t\tvalue: 2\n\t}\n\tprintln(int_str(choose(true, a, b).value))\n\tprintln(int_str(b.or(a).value))\n}\n')
	assert generic_return == '2\n1'
	// A bare generic literal (`Box{..}` / `&Box{..}`) specializes to the expected
	// concrete instance — and the heap literal must materialize the same concrete
	// C type (`Box_int`) as the value literal, not the bare template.
	good_heap := run_good(v3_bin, 'good_generic_bare_value_and_heap_args',
		'struct Box[T] {\n\tval T\n}\nfn take_val(b Box[int]) int {\n\treturn b.val\n}\nfn take_heap(b &Box[int]) int {\n\treturn b.val\n}\nfn main() {\n\tprintln(int_str(take_val(Box{\n\t\tval: 7\n\t}) + take_heap(&Box{\n\t\tval: 9\n\t})))\n}\n')
	assert good_heap == '16'
}

// Regression tests for the post-PR review fixes: const-expression fixed-array
// lengths must be folded to a literal (not c_name-mangled into `segs_+_1`), and the
// `[]thread T.wait()` helper name must sanitize a pointer payload C type (`Foo*`).
fn test_const_expr_fixed_array_and_thread_ptr_wait() {
	v3_bin := build_v3()
	// `[segs + 1]f32` must emit `[5]` for both the array dimension and `.len`.
	good_len := run_good(v3_bin, 'good_const_expr_fixed_array_len',
		'const segs = 4\nfn main() {\n\tmut x := [segs + 1]f32{}\n\tx[0] = 1.5\n\tx[segs] = 2.5\n\tprintln(int_str(x.len))\n\tprintln(int_str(int(x[0] + x[segs])))\n}\n')
	assert good_len == '5\n4'
	// `[]thread &Foo.wait()` returns `[]&Foo`; the wait helper symbol must not contain
	// the `*` from the `Foo*` payload C type.
	good_thread := run_good(v3_bin, 'good_thread_ptr_payload_wait',
		'struct Foo {\n\tx int\n}\nfn work(n int) &Foo {\n\treturn &Foo{\n\t\tx: n\n\t}\n}\nfn main() {\n\tmut threads := []thread &Foo{}\n\tthreads << spawn work(40)\n\tthreads << spawn work(2)\n\tresults := threads.wait()\n\tmut sum := 0\n\tfor r in results {\n\t\tsum += r.x\n\t}\n\tprintln(int_str(sum))\n}\n')
	assert good_thread == '42'
}

// Regression tests: a const-expression fixed-array length must work as a STRUCT
// FIELD type (not only as a literal) and must fold whether or not the operators
// are spaced (`segs+1` as well as `segs + 1`).
fn test_const_expr_fixed_array_field_and_spacing() {
	v3_bin := build_v3()
	// Struct field `[segs + 1]f32` was emitted as `void verts[4]` (element lost,
	// size = segs); it must be `[5]f32` with `.len == 5`.
	good_field := run_good(v3_bin, 'good_const_expr_fixed_array_field',
		'const segs = 4\nstruct Mesh {\n\tverts [segs + 1]f32\n}\nfn main() {\n\tm := Mesh{}\n\tprintln(int_str(m.verts.len))\n}\n')
	assert good_field == '5'
	// Spaced, unspaced, and nested const-expression sizes all fold to a literal.
	good_forms := run_good(v3_bin, 'good_const_expr_fixed_array_forms',
		'const segs = 3\nconst mult = 2\nfn main() {\n\ta := [segs + 1]int{}\n\tb := [segs+1]int{}\n\tc := [segs * mult]int{}\n\td := [segs - 1]int{}\n\te := [segs * mult + 1]int{}\n\tprintln(int_str(a.len + b.len + c.len + d.len + e.len))\n}\n')
	assert good_forms == '23'
}

// Regression tests for the second PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_two() {
	v3_bin := build_v3()
	// or-block binds `err` (IError) only inside the or-body; an outer `err` keeps its
	// type afterwards (was emitted as `err.message` on an int).
	or_err := run_good(v3_bin, 'good_or_block_err_scope',
		'fn maybe() ?int {\n\treturn 1\n}\nfn main() {\n\terr := 7\n\t_ := maybe() or { 0 }\n\tprintln(int_str(err))\n}\n')
	assert or_err == '7'
	// Generic receiver params are substituted by declared name: `Pair[L, R].right()`
	// returns `R` (int), not the first arg `L` (string).
	pair := run_good(v3_bin, 'good_generic_receiver_param_by_name',
		"struct Pair[L, R] {\n\tleft  L\n\tright R\n}\nfn (p Pair[L, R]) right_val() R {\n\treturn p.right\n}\nfn main() {\n\tp := Pair[string, int]{\n\t\tleft:  'hi'\n\t\tright: 42\n\t}\n\tprintln(int_str(p.right_val()))\n}\n")
	assert pair == '42'
	// `[flag]` enum stringification renders combined values as `Enum{.a | .b}`.
	flag := run_good(v3_bin, 'good_flag_enum_str',
		'@[flag]\nenum Perm {\n\tread\n\twrite\n\texec\n}\nfn main() {\n\ta := Perm.read | Perm.write\n\tprintln(a.str())\n\tb := Perm.read\n\tprintln(b.str())\n}\n')
	assert flag == 'Perm{.read | .write}\nPerm{.read}'
	// spawn of an option-returning fn stores/reads the `Optional_T` ABI layout.
	spawn_opt := run_good(v3_bin, 'good_spawn_option_return',
		"fn work() ?string {\n\treturn 'hello'\n}\nfn main() {\n\tmut ts := []thread ?string{}\n\tts << spawn work()\n\trs := ts.wait()\n\tx := rs[0] or { 'none' }\n\tprintln(x)\n}\n")
	assert spawn_opt == 'hello'
	// A global V function passed to a method `fn ()` param keeps the function-pointer
	// typedef (not `(void*)`).
	cb := run_good(v3_bin, 'good_method_fn_ptr_arg',
		"struct S {\n\tn int\n}\nfn (s S) run(cb fn ()) {\n\tcb()\n}\nfn greet() {\n\tprintln('hi')\n}\nfn main() {\n\ts := S{\n\t\tn: 1\n\t}\n\ts.run(greet)\n}\n")
	assert cb == 'hi'
}

// Regression tests for the third PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_three() {
	v3_bin := build_v3()
	// A `[flag]` enum match that lists only single-field branches is NOT exhaustive
	// (combined/zero values fall through), so a non-void fn needs `else`/missing-return.
	run_bad(v3_bin, 'bad_flag_enum_match_not_exhaustive',
		'@[flag]\nenum Perm {\n\tread\n\twrite\n}\nfn f(p Perm) int {\n\tmatch p {\n\t\t.read { return 1 }\n\t\t.write { return 2 }\n\t}\n}\nfn main() {\n\tprintln(int_str(f(Perm.read)))\n}\n',
		'missing return')
	// A bound method value returning an option, passed to a V `fn () ?string`
	// parameter, must emit a wrapper with the `Optional_string` ABI return and a
	// function-pointer (not `(void*)`) cast.
	mv := run_good(v3_bin, 'good_method_value_option_return',
		"struct G {\n\tn int\n}\nfn (g G) make() ?string {\n\treturn 'hi'\n}\nfn run(cb fn () ?string) {\n\ts := cb() or { 'none' }\n\tprintln(s)\n}\nfn main() {\n\tg := G{\n\t\tn: 1\n\t}\n\trun(g.make)\n}\n")
	assert mv == 'hi'
}

// Regression tests: a bare generic struct literal adopts a matching concrete
// expected instance (value and heap), and a field-type mismatch is rejected.
fn test_bare_generic_literal_adopts_expected_instance() {
	v3_bin := build_v3()
	val := run_good(v3_bin, 'good_bare_generic_literal_return',
		'struct Box[T] {\n\tv T\n}\nfn make() Box[int] {\n\treturn Box{\n\t\tv: 7\n\t}\n}\nfn main() {\n\tprintln(int_str(make().v))\n}\n')
	assert val == '7'
	heap := run_good(v3_bin, 'good_bare_generic_literal_heap_return',
		'struct Box[T] {\n\tv T\n}\nfn make() &Box[int] {\n\treturn &Box{\n\t\tv: 9\n\t}\n}\nfn main() {\n\tprintln(int_str(make().v))\n}\n')
	assert heap == '9'
	pair := run_good(v3_bin, 'good_bare_generic_literal_multi_param',
		"struct Pair[L, R] {\n\tl L\n\tr R\n}\nfn make() Pair[string, int] {\n\treturn Pair{\n\t\tl: 'hi'\n\t\tr: 5\n\t}\n}\nfn main() {\n\tp := make()\n\tprintln(p.l)\n\tprintln(int_str(p.r))\n}\n")
	assert pair == 'hi\n5'
	// A field whose type does not match the concrete instantiation is rejected by the
	// checker (rather than adopting the type and emitting broken C).
	run_bad(v3_bin, 'bad_bare_generic_literal_field_mismatch',
		"struct Box[T] {\n\tv T\n}\nfn make() Box[int] {\n\treturn Box{\n\t\tv: 'str'\n\t}\n}\nfn main() {\n\t_ := make()\n}\n",
		'cannot return `Box` as `Box[int]`')
}

// Regression tests for the fourth PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_four() {
	v3_bin := build_v3()
	// A `&Box{...}` value must not be accepted where a `Box[int]` value is expected
	// (pointer shape must match in the generic-base relaxation).
	run_bad(v3_bin, 'bad_generic_pointer_shape_arg',
		'struct Box[T] {\n\tv T\n}\nfn take(b Box[int]) int {\n\treturn b.v\n}\nfn main() {\n\t_ := take(&Box{\n\t\tv: 1\n\t})\n}\n',
		'cannot use')
	// A method value on a concrete generic receiver resolves the open `Box[T].get`,
	// types the value, and codegen materialises the `Box_int__get` wrapper.
	mval := run_good(v3_bin, 'good_generic_receiver_method_value',
		'struct Box[T] {\n\tv T\n}\nfn (b Box[T]) get() T {\n\treturn b.v\n}\nfn run(cb fn () int) int {\n\treturn cb()\n}\nfn main() {\n\tb := Box[int]{\n\t\tv: 7\n\t}\n\tprintln(int_str(run(b.get)))\n}\n')
	assert mval == '7'
	// A V function passed to a parameter whose type is a `fn`-type *alias* keeps the
	// function pointer (not `(void*)`).
	alias_cb := run_good(v3_bin, 'good_fn_type_alias_callback',
		'type Cb = fn () int\nfn run(cb Cb) int {\n\treturn cb()\n}\nfn five() int {\n\treturn 5\n}\nfn main() {\n\tprintln(int_str(run(five)))\n}\n')
	assert alias_cb == '5'
}

// Regression tests for the fifth PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_five() {
	v3_bin := build_v3()
	// A const-expression fixed-array length using `/` and `%` (and mixed precedence)
	// must fold to the right literal, not mangle the raw expression into an identifier:
	// `segs / 2` = 4, `segs % 3` = 2, `segs * 2 + 1` = 17, `segs / 2 * 2` = 8.
	divmod := run_good(v3_bin, 'good_const_expr_fixed_array_divmod',
		'const segs = 8\nstruct Buf {\n\ta [segs / 2]u8\n\tb [segs % 3]u8\n\tc [segs * 2 + 1]u8\n\td [segs / 2 * 2]u8\n}\nfn main() {\n\tx := Buf{}\n\tprintln(int_str(x.a.len + x.b.len + x.c.len + x.d.len))\n}\n')
	// 4 + 2 + 17 + 8 = 31
	assert divmod == '31'
	// A bare `[]thread` (threads with no return value) joins to `void`: `.wait()` is a
	// valid statement that runs every spawned thread to completion.
	void_wait := run_good(v3_bin, 'good_bare_thread_void_wait',
		"fn work() {\n\tprintln('w')\n}\nfn main() {\n\tmut threads := []thread{}\n\tthreads << spawn work()\n\tthreads << spawn work()\n\tthreads.wait()\n\tprintln('done')\n}\n")
	assert void_wait.contains('done')
	// Because a bare `[]thread`.wait() is `void` (not the receiver `[]thread`), its
	// result cannot be bound to a variable — guards against the old fallback that typed
	// the call as the receiver array.
	run_bad(v3_bin, 'bad_bare_thread_void_wait_assign',
		"fn work() {\n\tprintln('w')\n}\nfn main() {\n\tmut threads := []thread{}\n\tthreads << spawn work()\n\tx := threads.wait()\n\tprintln(x)\n}\n",
		'unknown identifier `x`')
}

// Regression tests for the sixth PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_six() {
	v3_bin := build_v3()
	// A bare generic struct literal with POSITIONAL fields (`Box{'str'}`) must validate
	// each value against the struct field order before adopting the expected concrete
	// instance; a wrong positional type is a definite mismatch, not a silent adoption
	// that codegen would init an `int` field from a string.
	run_bad(v3_bin, 'bad_positional_generic_literal_field_mismatch',
		"struct Box[T] {\n\tv T\n}\nfn make() Box[int] {\n\treturn Box{'str'}\n}\nfn main() {\n\t_ := make()\n}\n",
		'cannot return `Box` as `Box[int]`')
	// A positional bare generic literal whose value matches the concrete field type is
	// accepted and round-trips.
	pos_good := run_good(v3_bin, 'good_positional_generic_literal',
		'struct Box[T] {\n\tv T\n}\nfn make() Box[int] {\n\treturn Box{5}\n}\nfn main() {\n\tprintln(int_str(make().v))\n}\n')
	assert pos_good == '5'
	// A function returning a nested fixed array (`[2][3]int`, whose element is itself a
	// fixed array) must get a C return wrapper (functions cannot return raw arrays) and
	// the caller must type the result as the full nested array (`int[3][2]` round-trips
	// through parse_type), so the values survive the wrapper memcpy.
	nested_ret := run_good(v3_bin, 'good_nested_fixed_array_return',
		'fn make() [2][3]int {\n\tmut m := [2][3]int{}\n\tm[0][0] = 1\n\tm[0][1] = 2\n\tm[0][2] = 3\n\tm[1][0] = 4\n\tm[1][1] = 5\n\tm[1][2] = 6\n\treturn m\n}\nfn main() {\n\tr := make()\n\tprintln(int_str(r[0][0] + r[0][1] + r[0][2] + r[1][0] + r[1][1] + r[1][2]))\n}\n')
	assert nested_ret == '21'
}

// Regression tests for the seventh PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_seven() {
	v3_bin := build_v3()
	// A parenthesized const fixed-array length must fold by ignoring operators inside the
	// parentheses: `2 * (segs + 1)` is 10 (not split at the inner `+`), `(segs + 2) / 2`
	// is 5, so the array dimensions are real numbers, not mangled raw text.
	paren := run_good(v3_bin, 'good_paren_const_fixed_array_len',
		'const segs = 4\nfn main() {\n\ta := [2 * (segs + 1)]int{}\n\tb := [(segs + 2) / 2]int{}\n\tprintln(int_str(a.len + b.len))\n}\n')
	// 10 + 3 = 13
	assert paren == '13'
	// A generic method whose signature mentions the receiver type (`Box[T].clone() Box[T]`)
	// must resolve, on `Box[int]`, to the concrete `Box[int]` return (not the bare `Box`
	// the collapsed open signature would yield), so the caller types and codegens correctly.
	clone := run_good(v3_bin, 'good_generic_method_returns_receiver',
		'struct Box[T] {\n\tv T\n}\nfn (b Box[T]) clone() Box[T] {\n\treturn Box[T]{\n\t\tv: b.v\n\t}\n}\nfn (b Box[T]) get() T {\n\treturn b.v\n}\nfn main() {\n\tb := Box[int]{\n\t\tv: 9\n\t}\n\tc := b.clone()\n\tprintln(int_str(c.get()))\n}\n')
	assert clone == '9'
	// An operator overload on a generic struct is specialized only for instances whose
	// operator is actually used: `Box[NoPlus]` is merely stored (its `+` is never applied),
	// so the unused `Box_NoPlus__plus` body — which would do `NoPlus + NoPlus` — is not
	// emitted, while `Box[int] + Box[int]` is.
	op_gate := run_good(v3_bin, 'good_operator_specialization_gated',
		"struct Box[T] {\n\tv T\n}\nfn (a Box[T]) + (b Box[T]) Box[T] {\n\treturn Box[T]{\n\t\tv: a.v + b.v\n\t}\n}\nstruct NoPlus {\n\tname string\n}\nfn main() {\n\tx := Box[int]{\n\t\tv: 1\n\t}\n\ty := Box[int]{\n\t\tv: 2\n\t}\n\tz := x + y\n\tprintln(int_str(z.v))\n\tw := Box[NoPlus]{\n\t\tv: NoPlus{\n\t\t\tname: 'hi'\n\t\t}\n\t}\n\tprintln(w.v.name)\n}\n")
	assert op_gate == '3\nhi'
}

// Regression tests for the eighth PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_eight() {
	v3_bin := build_v3()
	// A bare *value* generic literal must not adopt a *pointer* expectation: `&Box[int]`
	// expected from a plain `Box{...}` is a definite mismatch (cgen would emit a `Box_int`
	// value where a `Box_int*` is required), so it is rejected rather than silently typed
	// as `&Box[int]`.
	run_bad(v3_bin, 'bad_value_literal_pointer_expectation',
		'struct Box[T] {\n\tv T\n}\nfn make() &Box[int] {\n\treturn Box{\n\t\tv: 1\n\t}\n}\nfn main() {\n\t_ := make()\n}\n',
		'cannot return `Box` as `&Box[int]`')
	// The pointer form `&Box{...}` still adopts the `&Box[int]` expectation and round-trips.
	heap := run_good(v3_bin, 'good_amp_generic_literal_pointer',
		'struct Box[T] {\n\tv T\n}\nfn make() &Box[int] {\n\treturn &Box{\n\t\tv: 7\n\t}\n}\nfn main() {\n\tprintln(int_str(make().v))\n}\n')
	assert heap == '7'
	// A method value passed through a function-type *alias* parameter (`type Cb = fn ()`)
	// must be cast to the generated `_fn_ptr_*` typedef, not `(void*)` (which strict C
	// rejects for a function pointer). `fn_type_from` unwraps the alias.
	mv_alias := run_good(v3_bin, 'good_method_value_fn_alias',
		"struct Game {\n\tscore int\n}\nfn (g Game) draw() {\n\tprintln('drawing')\n}\ntype Cb = fn ()\nfn run(cb Cb) {\n\tcb()\n}\nfn main() {\n\tg := Game{\n\t\tscore: 5\n\t}\n\trun(g.draw)\n}\n")
	assert mv_alias == 'drawing'
}

// Regression tests for the tenth PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_ten() {
	v3_bin := build_v3()
	// Const fixed-array lengths may use shifts and bitwise operators, in both the string
	// evaluator (recovered length text) and the AST const evaluator (`const n = 1 << 2`).
	// `1 << 2`=4, `8 >> 1`=4, `3 | 4`=7, `0xF & 6`=6, `(1 << 3) + 2`=10, `16 >> 2 << 1`=8.
	shifts := run_good(v3_bin, 'good_shift_bitwise_fixed_array_len',
		'const shift_amt = 1 << 2\nconst my_mask = 0xF & 6\nfn main() {\n\ta := [1 << 2]int{}\n\tb := [shift_amt]int{}\n\tc := [8 >> 1]int{}\n\td := [3 | 4]int{}\n\te := [my_mask]int{}\n\tf := [(1 << 3) + 2]int{}\n\tg := [16 >> 2 << 1]int{}\n\tprintln(int_str(a.len + b.len + c.len + d.len + e.len + f.len + g.len))\n}\n')
	// 4 + 4 + 4 + 7 + 6 + 10 + 8 = 43
	assert shifts == '43'
	// The fixed-array literal length guard evaluates a shift length: `[1, 2]` (two
	// elements) does not match an expected `[1 << 2]int` (four), so it is rejected.
	run_bad(v3_bin, 'bad_shift_fixed_array_literal_len',
		'fn take(a [1 << 2]int) int {\n\treturn a[0]\n}\nfn main() {\n\t_ := take([1, 2]!)\n}\n',
		'cannot use')
	// A function returning a fixed array of structs gets a C return wrapper (emitted after
	// the struct is defined), so it compiles and round-trips instead of emitting a raw
	// `Array_fixed_Foo_2` return type C rejects.
	fa_struct := run_good(v3_bin, 'good_fixed_array_struct_return',
		'struct Foo {\n\tx int\n}\nfn make() [2]Foo {\n\treturn [Foo{\n\t\tx: 1\n\t}, Foo{\n\t\tx: 2\n\t}]!\n}\nfn main() {\n\tr := make()\n\tprintln(int_str(r[0].x + r[1].x))\n}\n')
	assert fa_struct == '3'
	// Likewise a fixed array of `string` returns through a wrapper.
	fa_string := run_good(v3_bin, 'good_fixed_array_string_return',
		"fn make() [2]string {\n\treturn ['hi', 'yo']!\n}\nfn main() {\n\tr := make()\n\tprintln(r[0] + r[1])\n}\n")
	assert fa_string == 'hiyo'
}

// Regression tests for the eleventh PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_eleven() {
	v3_bin := build_v3()
	// A method value is backed by a per-evaluation-site static receiver, so storing it in
	// an array (where several instances from one site would share that slot and every
	// callback would use the last receiver) is rejected with a clear error instead of
	// reaching cgen and emitting an undefined helper. Append form:
	run_bad(v3_bin, 'bad_method_value_array_append',
		'struct Counter {\n\tid int\n}\nfn (c Counter) report() int {\n\treturn c.id\n}\nfn main() {\n\tmut cbs := []fn () int{}\n\tfor i in 0 .. 3 {\n\t\tc := Counter{\n\t\t\tid: i * 10\n\t\t}\n\t\tcbs << c.report\n\t}\n\tprintln(int_str(cbs.len))\n}\n',
		'cannot escape its call site')
	// Array-literal form is rejected too.
	run_bad(v3_bin, 'bad_method_value_array_literal',
		'struct Counter {\n\tid int\n}\nfn (c Counter) report() int {\n\treturn c.id\n}\nfn main() {\n\ta := Counter{\n\t\tid: 1\n\t}\n\tb := Counter{\n\t\tid: 2\n\t}\n\tcbs := [a.report, b.report]\n\tprintln(int_str(cbs.len))\n}\n',
		'cannot escape its call site')
	// The supported single-use forms still work: a method value passed directly as a
	// callback argument, and an `arr << int` append / `int << int` shift are not flagged.
	imm := run_good(v3_bin, 'good_method_value_immediate_after_guard',
		'struct Counter {\n\tid int\n}\nfn (c Counter) report() int {\n\treturn c.id\n}\nfn run(cb fn () int) int {\n\treturn cb()\n}\nfn main() {\n\tc := Counter{\n\t\tid: 7\n\t}\n\tmut xs := [1]\n\txs << (2 << 1)\n\tprintln(int_str(run(c.report) + xs.len))\n}\n')
	// 7 + 2 (xs has [1, 4]) = 9
	assert imm == '9'
}

// Regression tests for the twelfth PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_twelve() {
	v3_bin := build_v3()
	// A heap struct literal with POSITIONAL fields must emit positional C values, not a
	// `. = v` designator. For a *generic* heap literal the fields live under the concrete
	// instance key (`Box[int]`), so the per-index field lookup must use that, not the bare
	// `Box`; otherwise `arr << &Box[int]{1, 2}` generates invalid C like `(Box_int){. = 1}`.
	gpos := run_good(v3_bin, 'good_positional_generic_heap_struct',
		'struct Box[T] {\n\ta T\n\tb T\n}\nfn main() {\n\tmut arr := []&Box[int]{}\n\tarr << &Box[int]{1, 2}\n\tprintln(int_str(arr[0].a + arr[0].b))\n}\n')
	assert gpos == '3'
	// A positional generic heap literal that omits a default-initialized `[]T` field still
	// gets that field's default (`array_new(...)`) alongside the positional value.
	gposdef := run_good(v3_bin, 'good_positional_generic_heap_default',
		'struct Box[T] {\n\tv     T\n\titems []T\n}\nfn main() {\n\tb := &Box[int]{5}\n\tmut its := b.items\n\tits << 10\n\tprintln(int_str(b.v))\n\tprintln(int_str(its.len))\n}\n')
	assert gposdef == '5\n1'
	// Non-generic positional heap literals keep working (no regression).
	pos := run_good(v3_bin, 'good_positional_heap_struct',
		'struct Point {\n\tx int\n\ty int\n}\nfn main() {\n\tmut arr := []&Point{}\n\tarr << &Point{1, 2}\n\tprintln(int_str(arr[0].x + arr[0].y))\n}\n')
	assert pos == '3'
}

// Regression tests for the thirteenth PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_thirteen() {
	v3_bin := build_v3()
	// V's unsigned right shift `>>>` is a valid constant-length operator. It must fold in
	// the const evaluator (so the array dimension is emitted as a numeric literal, since
	// `>>>` has no C form) and in the literal-length guard. `8 >>> 1` = 4, `16 >>> 2` = 4,
	// `(1 << 5) >>> 2` = 8.
	ushift := run_good(v3_bin, 'good_unsigned_shift_fixed_array_len',
		'const shamt = 16 >>> 2\nfn main() {\n\ta := [8 >>> 1]int{}\n\tb := [shamt]int{}\n\tc := [(1 << 5) >>> 2]int{}\n\tprintln(int_str(a.len + b.len + c.len))\n}\n')
	// 4 + 4 + 8 = 16
	assert ushift == '16'
	// The fixed-array literal-length guard evaluates `>>>` too: `[1, 2]` (two elements)
	// does not match an expected `[8 >>> 1]int` (four), so it is rejected.
	run_bad(v3_bin, 'bad_unsigned_shift_fixed_array_literal_len',
		'fn take(a [8 >>> 1]int) int {\n\treturn a[0]\n}\nfn main() {\n\t_ := take([1, 2]!)\n}\n',
		'cannot use')
}

// Regression tests for the fourteenth PR-review batch (vlang/v#27557).
fn test_pr_review_codegen_batch_fourteen() {
	v3_bin := build_v3()
	// Non-decimal integer literals (hex `0x`, octal `0o`, binary `0b`) fold in const
	// fixed-array lengths: `0xF & 6` = 6, `0b1100 >> 1` = 6, `0o17 & 8` = 8.
	nondec := run_good(v3_bin, 'good_non_decimal_const_fixed_array_len',
		'const flags = 0b1010 | 0b0100\nfn main() {\n\ta := [0xF & 6]int{}\n\tb := [0b1100 >> 1]int{}\n\tc := [0o17 & 8]int{}\n\td := [flags]int{}\n\tprintln(int_str(a.len + b.len + c.len + d.len))\n}\n')
	// 6 + 6 + 8 + 14 = 34
	assert nondec == '34'
	// The length guard evaluates non-decimal too: `[1, 2]` does not match `[0xF & 6]int`.
	run_bad(v3_bin, 'bad_non_decimal_fixed_array_literal_len',
		'fn take(a [0xF & 6]int) int {\n\treturn a[0]\n}\nfn main() {\n\t_ := take([1, 2]!)\n}\n',
		'cannot use')
	// A generic-receiver method with a function-type parameter substitutes the type params
	// inside the signature, so `Box[string].apply` expects `fn (string) int`, and a matching
	// callback is accepted and emitted with the right fn-pointer type.
	apply := run_good(v3_bin, 'good_generic_method_fn_type_param',
		"struct Box[T] {\n\tv T\n}\nfn (b Box[T]) apply(cb fn (T) int) int {\n\treturn cb(b.v)\n}\nfn slen(s string) int {\n\treturn s.len\n}\nfn main() {\n\tb := Box[string]{\n\t\tv: 'hello'\n\t}\n\tprintln(int_str(b.apply(slen)))\n}\n")
	assert apply == '5'
	// The same substitution also handles named parameters inside the callback type:
	// `fn (x T) int` specializes the type part to `fn (int) int`.
	named_cb := run_good(v3_bin, 'good_generic_fn_named_callback_param',
		'fn run[T](cb fn (x T) int, v T) int {\n\treturn cb(v)\n}\nfn inc(x int) int {\n\treturn x + 1\n}\nfn main() {\n\tprintln(int_str(run(inc, 4)))\n}\n')
	assert named_cb == '5'
	// A method value that escapes its call site is rejected: returned from a factory, stored
	// in a struct field, or put in a map (the per-site static receiver can't keep several
	// instances distinct). Immediately-passed callbacks and local bindings still work.
	run_bad(v3_bin, 'bad_method_value_return_escape',
		'struct Counter {\n\tid int\n}\nfn (c Counter) report() int {\n\treturn c.id\n}\nfn bind(c Counter) fn () int {\n\treturn c.report\n}\nfn main() {\n\t_ := bind(Counter{\n\t\tid: 1\n\t})\n}\n',
		'cannot escape its call site')
	run_bad(v3_bin, 'bad_method_value_struct_field_escape',
		'struct Counter {\n\tid int\n}\nfn (c Counter) report() int {\n\treturn c.id\n}\nstruct Engine {\n\tcb fn () int\n}\nfn main() {\n\t_ := Engine{\n\t\tcb: Counter{\n\t\t\tid: 1\n\t\t}.report\n\t}\n}\n',
		'cannot escape its call site')
}

fn test_pr_review_codegen_batch_fifteen() {
	v3_bin := build_v3()
	// The string length evaluator folds with the same operator precedence as the v3 parser
	// and AST const evaluator: shifts bind looser than `+`, so `1 << 2 + 1` groups as
	// `1 << (2 + 1)` = 8 (not `(1 << 2) + 1` = 5) whether the length is recovered from a
	// const's source text (string evaluator) or a literal expression. Both must agree, else
	// the literal length check and the generated C dimension would diverge.
	prec := run_good(v3_bin, 'good_shift_add_precedence_fixed_array_len',
		'const seg_count = 1 << 2 + 1\nfn main() {\n\ta := [1 << 2 + 1]u8{}\n\tb := [seg_count]u8{}\n\tc := [1 + 2 << 1]u8{}\n\tprintln(int_str(a.len + b.len + c.len))\n}\n')
	// 8 + 8 + 6 = 22
	assert prec == '22'
	// The fixed-array literal-length guard uses the same folded length: a `[1 << 2 + 1]int`
	// parameter (length 8) rejects a 5-element literal.
	run_bad(v3_bin, 'bad_shift_add_precedence_literal_len',
		'fn take(a [1 << 2 + 1]int) int {\n\treturn a[0]\n}\nfn main() {\n\t_ := take([1, 2, 3, 4, 5]!)\n}\n',
		'cannot use')
	// An fn-pointer type whose return is a non-early fixed array (`string`/struct element) gets
	// a return-wrapper struct. The wrapper is forward-declared before the fn-pointer typedef and
	// completed after the element type, so the C compiles; the wrapped value is unwrapped to
	// `.ret_arr` at the indirect call, whether the callback is reached through a param or a
	// struct field.
	fp_str := run_good(v3_bin, 'good_fn_ptr_returns_fixed_string_array',
		"type MakeArr = fn () [2]string\nfn mk() [2]string {\n\treturn ['hi', 'there']!\n}\nfn run(f MakeArr) string {\n\tr := f()\n\treturn r[0]\n}\nstruct Holder {\n\tf MakeArr\n}\nfn main() {\n\th := Holder{\n\t\tf: mk\n\t}\n\tprintln(run(mk) + h.f()[1])\n}\n")
	assert fp_str == 'hithere'
	fp_struct := run_good(v3_bin, 'good_fn_ptr_returns_fixed_struct_array',
		'struct P {\n\tx int\n}\ntype MakeP = fn () [2]P\nfn mkp() [2]P {\n\treturn [P{\n\t\tx: 3\n\t}, P{\n\t\tx: 4\n\t}]!\n}\nfn runp(f MakeP) int {\n\tr := f()\n\treturn r[0].x + r[1].x\n}\nfn main() {\n\tprintln(int_str(runp(mkp)))\n}\n')
	assert fp_struct == '7'
}

fn test_pr_review_codegen_batch_sixteen() {
	v3_bin := build_v3()
	// A bare generic struct literal (`Box{..}`) inside an array literal whose expected type is
	// a concrete generic instance (`[]Box[int]`) must carry that concrete element type into the
	// array storage AND each element: the array literal emitter passes the element type as the
	// expected type per element so the element is emitted as `Box_int`, not the open `Box` (which
	// has no C type), and the storage and element agree. Covers the reviewer's return example plus
	// the argument and struct-field positions.
	out := run_good(v3_bin, 'good_generic_struct_array_literal_positions',
		"struct Box[T] {\n\tv T\n}\nfn first(a []Box[int]) int {\n\treturn a[0].v\n}\nstruct Holder {\n\titems []Box[int]\n}\nfn make() []Box[int] {\n\treturn [Box{\n\t\tv: 1\n\t}, Box{\n\t\tv: 2\n\t}]\n}\nfn main() {\n\tr := make()\n\ta := first([Box{\n\t\tv: 3\n\t}, Box{\n\t\tv: 4\n\t}])\n\th := Holder{\n\t\titems: [Box{\n\t\t\tv: 5\n\t\t}]\n\t}\n\tprintln(int_str(r[0].v + r[1].v) + ',' + int_str(a) + ',' + int_str(h.items[0].v))\n}\n")
	assert out == '3,3,5'
}

fn test_pr_review_codegen_batch_seventeen() {
	v3_bin := build_v3()
	// Static method `Type.method(...)` calls now lower their arguments through the ordinary
	// argument path, so a parameter that needs coercion is emitted against its expected type.
	// Here `Factory.describe` takes a sum type, so the bare `Circle{..}` argument is wrapped as
	// a `Shape` variant instead of being passed as the raw struct (which would be wrong C).
	// `Shader.build()` takes no parameters but shares the short name `build` with `Config.build`
	// (a method, hence has params); the lowering must not adopt that unrelated signature and
	// append a phantom argument to the 0-parameter static call.
	out := run_good(v3_bin, 'good_static_method_arg_lowering',
		"struct Config {\n\tlengths []int\n}\nfn (c Config) build() int {\n\treturn c.lengths.len\n}\nstruct Shader {\n\tid int\n}\nfn Shader.build() Shader {\n\treturn Shader{\n\t\tid: 42\n\t}\n}\ntype Shape = Circle | Square\nstruct Circle {\n\tr int\n}\nstruct Square {\n\ts int\n}\nstruct Factory {}\nfn Factory.describe(sh Shape) int {\n\treturn match sh {\n\t\tCircle { sh.r }\n\t\tSquare { sh.s }\n\t}\n}\nfn main() {\n\ts := Shader.build()\n\td := Factory.describe(Circle{r: 7})\n\tprintln(int_str(s.id) + ',' + int_str(d))\n}\n")
	assert out == '42,7'
}

fn test_pr_review_codegen_batch_eighteen() {
	v3_bin := build_v3()
	// A method value bound to a local (`cb := c.report`) shares the same per-site static receiver
	// as the bare selector, so aliasing it out — returned from a factory or appended to an array —
	// escapes just like `return c.report` and is rejected, not only the direct selector form.
	mv := 'struct Counter {\n\tid int\n}\nfn (c Counter) report() int {\n\treturn c.id\n}\n'
	run_bad(v3_bin, 'bad_method_value_local_return_escape', mv +
		'fn bind(c Counter) fn () int {\n\tcb := c.report\n\treturn cb\n}\nfn main() {\n\t_ := bind(Counter{\n\t\tid: 1\n\t})\n}\n',
		'cannot escape its call site')
	run_bad(v3_bin, 'bad_method_value_local_append_escape', mv +
		'fn collect(c Counter) []fn () int {\n\tmut arr := []fn () int{}\n\tcb := c.report\n\tarr << cb\n\treturn arr\n}\nfn main() {\n\t_ := collect(Counter{\n\t\tid: 1\n\t})\n}\n',
		'cannot escape its call site')
	// A method-value local passed straight to a callback parameter does not escape and stays valid.
	direct := run_good(v3_bin, 'good_method_value_local_direct_use', mv +
		'fn invoke(cb fn () int) int {\n\treturn cb()\n}\nfn main() {\n\tc := Counter{\n\t\tid: 7\n\t}\n\tcb := c.report\n\tprintln(int_str(invoke(cb)))\n}\n')
	assert direct == '7'
	// A bare heap generic literal (`&Box{v: 1}` where `&Box[int]` is expected) must read its
	// omitted-field defaults from the concrete instance key: monomorphization drops the bare `Box`
	// entry, so an omitted `items []T` needs its `array_new` default or the field has invalid
	// zeroed array metadata (here a later append would silently drop the elements).
	heap_default := run_good(v3_bin, 'good_heap_generic_omitted_array_default',
		"struct Box[T] {\n\tv     T\n\titems []T\n}\nfn make() &Box[int] {\n\treturn &Box{\n\t\tv: 1\n\t}\n}\nfn main() {\n\tmut b := make()\n\tb.items << 10\n\tb.items << 20\n\tprintln(int_str(b.v) + ',' + int_str(b.items.len) + ',' + int_str(b.items[0]) + ',' + int_str(b.items[1]))\n}\n")
	assert heap_default == '1,2,10,20'
	// `[]thread T.wait()` recovers the spawned return type from the thread name; a fixed-array
	// return with a non-decimal length (`[0x10]u8`) must reconstruct the same `_v_ret_*` wrapper
	// ABI type the spawn wrapper stored, not a bogus generic application of `u8`.
	thread_fixed := run_good(v3_bin, 'good_thread_wait_nondecimal_fixed_array',
		"fn work() [0x10]u8 {\n\tmut r := [0x10]u8{}\n\tr[0] = 42\n\tr[15] = 7\n\treturn r\n}\nfn main() {\n\tmut threads := []thread [0x10]u8{}\n\tthreads << spawn work()\n\tresults := threads.wait()\n\tprintln(int_str(results[0][0]) + ',' + int_str(results[0][15]))\n}\n")
	assert thread_fixed == '42,7'
}

fn test_pr_review_codegen_batch_nineteen() {
	v3_bin := build_v3()
	// A value local whose address escapes (`p := &v` with `p` returned) is moved to the heap at
	// its declaration, so a mutation between the alias and the return is observed by the caller —
	// matching V's auto-heap semantics. Copying eagerly at `p := &v` returned stale data (x == 1).
	escaped := run_good(v3_bin, 'good_escaped_pointer_alias_mutation',
		'struct Box {\nmut:\n\tx int\n}\nfn make() &Box {\n\tmut v := Box{\n\t\tx: 1\n\t}\n\tp := &v\n\tv.x = 2\n\treturn p\n}\nfn main() {\n\tb := make()\n\tprintln(int_str(b.x))\n}\n')
	assert escaped == '2'
	// A static-method call returning a fixed array (`Type.make() [N]T`) is lowered to a selector
	// whose base is a type, not a receiver value; its `_v_ret_*` wrapper must still be unwrapped to
	// `.ret_arr`, so the assignment/indexing below sees the array, not the wrapper struct.
	static_fixed := run_good(v3_bin, 'good_static_method_fixed_array_return',
		'struct Maker {}\nfn Maker.make() [3]int {\n\treturn [10, 20, 30]!\n}\nfn main() {\n\ta := Maker.make()\n\tprintln(int_str(a[0] + a[1] + a[2]))\n}\n')
	assert static_fixed == '60'
}

fn test_pr_review_codegen_batch_twenty() {
	v3_bin := build_v3()
	mv := 'struct Counter {\n\tid int\n}\nfn (c Counter) report() int {\n\treturn c.id\n}\n'
	// Storing a method value into a struct field is an escape: the per-site static receiver slot
	// is overwritten on the next evaluation, so previously-stored callbacks lose their receiver.
	run_bad(v3_bin, 'bad_method_value_struct_field_assign', mv +
		'struct Holder {\nmut:\n\tcb fn () int\n}\nfn main() {\n\tc := Counter{\n\t\tid: 5\n\t}\n\tmut h := Holder{}\n\th.cb = c.report\n\tprintln(int_str(h.cb()))\n}\n',
		'cannot escape its call site')
	// Same hazard storing into an array element.
	run_bad(v3_bin, 'bad_method_value_index_assign', mv +
		'fn main() {\n\tc := Counter{\n\t\tid: 5\n\t}\n\tmut cbs := []fn () int{len: 1}\n\tcbs[0] = c.report\n\tprintln(int_str(cbs[0]()))\n}\n',
		'cannot escape its call site')
	// A method value assigned to a local with `=` is still tracked, so a later escape is caught.
	run_bad(v3_bin, 'bad_method_value_local_eq_then_return_escape', mv +
		'fn dummy() int {\n\treturn 0\n}\nfn bind(c Counter) fn () int {\n\tmut cb := dummy\n\tcb = c.report\n\treturn cb\n}\nfn main() {\n\t_ := bind(Counter{\n\t\tid: 1\n\t})\n}\n',
		'cannot escape its call site')
	// Assigning a method value to a local (including reassignment) and passing it straight to a
	// callback parameter does not escape, so it stays valid.
	ok := run_good(v3_bin, 'good_method_value_local_assign_direct_use', mv +
		'fn invoke(cb fn () int) int {\n\treturn cb()\n}\nfn main() {\n\tc := Counter{\n\t\tid: 7\n\t}\n\tmut cb := c.report\n\tcb = c.report\n\tprintln(int_str(invoke(cb)))\n}\n')
	assert ok == '7'
}

fn test_pr_review_codegen_batch_twentyone() {
	v3_bin := build_v3()
	// An escaping pointer returned through a copy (`p := &v; q := p; return q`) must still move
	// `v` to the heap, so a mutation after the alias is seen by the caller (not a dangling stack
	// pointer). The pointer-alias chain is followed when deciding what escapes.
	alias := run_good(v3_bin, 'good_returned_pointer_alias_heap',
		'struct Box {\nmut:\n\tx int\n}\nfn make() &Box {\n\tmut v := Box{\n\t\tx: 1\n\t}\n\tp := &v\n\tq := p\n\tv.x = 2\n\treturn q\n}\nfn main() {\n\tb := make()\n\tprintln(int_str(b.x))\n}\n')
	assert alias == '2'
	// An interface method returning a fixed array gets its dispatch stub declared with the
	// `_v_ret_*` wrapper, not a bare `Array_fixed_*` (a C function cannot return an array). This
	// holds both when only the concrete implementer is called and through a real interface call.
	iface_concrete := run_good(v3_bin, 'good_iface_fixed_array_concrete_only',
		'interface I {\n\tmake() [3]int\n}\nstruct S {}\nfn (s S) make() [3]int {\n\treturn [1, 2, 3]!\n}\nfn main() {\n\ts := S{}\n\ta := s.make()\n\tprintln(int_str(a[0] + a[1] + a[2]))\n}\n')
	assert iface_concrete == '6'
	iface_dispatch := run_good(v3_bin, 'good_iface_fixed_array_dispatch',
		'interface I {\n\tmake() [3]int\n}\nstruct S {}\nfn (s S) make() [3]int {\n\treturn [4, 5, 6]!\n}\nfn use_iface(i I) int {\n\ta := i.make()\n\treturn a[0] + a[1] + a[2]\n}\nfn main() {\n\ts := S{}\n\tprintln(int_str(use_iface(s)))\n}\n')
	assert iface_dispatch == '15'
	// A pointer-receiver method value taken on an rvalue base (`Foo{..}.tick`) copies the receiver
	// into durable static storage instead of capturing `&(temporary)` that dies before the
	// callback runs.
	mv_rvalue := run_good(v3_bin, 'good_ptr_receiver_method_value_rvalue',
		'struct Foo {\nmut:\n\tn int\n}\nfn (f &Foo) tick() int {\n\treturn f.n\n}\nfn run(cb fn () int) int {\n\treturn cb()\n}\nfn main() {\n\tprintln(int_str(run(Foo{\n\t\tn: 5\n\t}.tick)))\n}\n')
	assert mv_rvalue == '5'
}

fn test_pr_review_codegen_batch_twentytwo() {
	v3_bin := build_v3()
	// A bare generic literal retargeted to a concrete instance must run the fixed-array-field
	// detection against that concrete key (the bare `Box` field table is dropped by
	// monomorphization), so a `[N]T` field is filled by memcpy instead of the invalid C
	// `(Box_int){.data = a}` (a C array member cannot be initialized from another array object).
	fa := run_good(v3_bin, 'good_concrete_generic_fixed_array_field',
		'struct Box[T] {\n\tdata [2]T\n}\nfn f(a [2]int) Box[int] {\n\treturn Box{\n\t\tdata: a\n\t}\n}\nfn main() {\n\tarr := [10, 20]!\n\tb := f(arr)\n\tprintln(int_str(b.data[0] + b.data[1]))\n}\n')
	assert fa == '30'
	// A generic method value used only in an unreachable function must not drive a concrete
	// specialization: `Box[Pair].doubled` (here `Pair + Pair`) would emit an invalid body and
	// fail C compilation. Only the reachable `Box[int].doubled` is specialized.
	dead := run_good(v3_bin, 'good_dead_generic_method_value_not_specialized',
		'struct Box[T] {\n\tv T\n}\nfn (b Box[T]) doubled() T {\n\treturn b.v + b.v\n}\nstruct Pair {\n\ta int\n}\nfn run_int(cb fn () int) int {\n\treturn cb()\n}\nfn run_pair(cb fn () Pair) Pair {\n\treturn cb()\n}\nfn dead_fn() {\n\tp := Box[Pair]{\n\t\tv: Pair{\n\t\t\ta: 1\n\t\t}\n\t}\n\t_ := run_pair(p.doubled)\n}\nfn main() {\n\tb := Box[int]{\n\t\tv: 5\n\t}\n\tprintln(int_str(run_int(b.doubled)))\n}\n')
	assert dead == '10'
	// An unused interface must not force every structurally matching generic receiver method to
	// specialize: `Box[NoPlus].run` satisfies `Runner.run`, but no value is boxed as `Runner` and
	// the method body is invalid for `NoPlus`.
	unused_iface := run_good(v3_bin, 'good_unused_interface_no_generic_method_specialization',
		"interface Runner {\n\trun() int\n}\nstruct Box[T] {\n\tv T\n}\nfn (b Box[T]) run() int {\n\treturn b.v + b.v\n}\nstruct NoPlus {\n\tvalue int\n}\nfn main() {\n\tb := Box[NoPlus]{\n\t\tv: NoPlus{\n\t\t\tvalue: 1\n\t\t}\n\t}\n\t_ = b\n\tprintln('ok')\n}\n")
	assert unused_iface == 'ok'
	// A reachable generic method value of any instance still specializes correctly.
	live := run_good(v3_bin, 'good_reachable_generic_method_value_specialized',
		'struct Box[T] {\n\tv T\n}\nfn (b Box[T]) first() T {\n\treturn b.v\n}\nstruct Pair {\n\ta int\n}\nfn run_pair(cb fn () Pair) Pair {\n\treturn cb()\n}\nfn main() {\n\tp := Box[Pair]{\n\t\tv: Pair{\n\t\t\ta: 7\n\t\t}\n\t}\n\tr := run_pair(p.first)\n\tprintln(int_str(r.a))\n}\n')
	assert live == '7'
	// Discarding a method value with `_ =` stores nothing, so it is not an escape.
	blank := run_good(v3_bin, 'good_method_value_blank_discard',
		'struct Counter {\n\tid int\n}\nfn (c Counter) report() int {\n\treturn c.id\n}\nfn main() {\n\tc := Counter{\n\t\tid: 5\n\t}\n\tcb := c.report\n\t_ = cb\n\tprintln(int_str(c.report()))\n}\n')
	assert blank == '5'
}

fn test_pr_review_codegen_batch_twentythree() {
	v3_bin := build_v3()
	// A concrete generic with multiple arguments including a later pointer argument
	// (`Pair[int, &Node]`) must parse as a generic instance, not a fixed array: the `&` only
	// leads a type argument because of the preceding comma, so a comma-bearing bracket is never
	// a fixed-array length (which is always a single integer expression).
	genptr := run_good(v3_bin, 'good_generic_multi_arg_pointer',
		'struct Node {\n\tval int\n}\nstruct Pair[A, B] {\n\tfirst  A\n\tsecond B\n}\nfn main() {\n\tn := Node{\n\t\tval: 5\n\t}\n\tp := Pair[int, &Node]{\n\t\tfirst:  1\n\t\tsecond: &n\n\t}\n\tprintln(int_str(p.first + p.second.val))\n}\n')
	assert genptr == '6'
	// A whole-value assignment between two heaped locals (both moved to `&T` because their
	// addresses escape) must copy the value through the pointers (`*v = *w`), not alias `w`'s
	// object. Here `v = w` then mutating `w` must leave the first returned pointer at w's old value.
	heap2 := run_good(v3_bin, 'good_heaped_local_whole_value_assign',
		"struct Box {\nmut:\n\tx int\n}\nfn split() (&Box, &Box) {\n\tmut v := Box{\n\t\tx: 1\n\t}\n\tmut w := Box{\n\t\tx: 2\n\t}\n\tp := &v\n\tq := &w\n\tv = w\n\tw.x = 100\n\treturn p, q\n}\nfn main() {\n\ta, b := split()\n\tprintln(int_str(a.x) + ',' + int_str(b.x))\n}\n")
	assert heap2 == '2,100'
	// A method value bound to a local and reassigned only on one branch still escapes on the
	// other path, so returning it is rejected (the marker survives a non-dominating reassignment).
	mv := 'struct Counter {\n\tid int\n}\nfn (c Counter) report() int {\n\treturn c.id\n}\nfn plain() int {\n\treturn 0\n}\n'
	run_bad(v3_bin, 'bad_method_value_conditional_reassign_escape', mv +
		'fn build_cb(c Counter, cond bool) fn () int {\n\tmut cb := c.report\n\tif cond {\n\t\tcb = plain\n\t}\n\treturn cb\n}\nfn main() {\n\t_ := build_cb(Counter{\n\t\tid: 1\n\t}, false)\n}\n',
		'cannot escape its call site')
	// An unconditional reassignment to a non-method value dominates the later use, so the marker
	// is cleared and the (now plain) callback is accepted.
	uncond := run_good(v3_bin, 'good_method_value_unconditional_reassign', mv +
		'fn build_cb(c Counter) int {\n\tmut cb := c.report\n\tcb = plain\n\treturn cb()\n}\nfn main() {\n\tprintln(int_str(build_cb(Counter{\n\t\tid: 1\n\t})))\n}\n')
	assert uncond == '0'
}

fn test_fn_pointer_return_type() {
	v3_bin := build_v3()
	// A function whose return type is itself a function pointer (`fn () fn (int) int`) must be
	// declared with the shared `_fn_ptr_N` typedef, not the internal `fn_ptr:...` encoding (which
	// would emit invalid C). Covers a plain fn return and a method returning a fn pointer.
	picker := run_good(v3_bin, 'good_fn_pointer_return',
		'fn add_one(x int) int {\n\treturn x + 1\n}\nfn picker() fn (int) int {\n\treturn add_one\n}\nfn main() {\n\tf := picker()\n\tprintln(int_str(f(41)))\n}\n')
	assert picker == '42'
	method_ret := run_good(v3_bin, 'good_method_fn_pointer_return',
		'struct Calc {\n\tbase int\n}\nfn dbl(x int) int {\n\treturn x * 2\n}\nfn (c Calc) op() fn (int) int {\n\treturn dbl\n}\nfn main() {\n\tc := Calc{\n\t\tbase: 5\n\t}\n\tf := c.op()\n\tprintln(int_str(f(21)))\n}\n')
	assert method_ret == '42'
	// A function returning a callback assembled from a reassigned method-value local compiles and
	// returns the (plain) callback through the fn-pointer return type.
	built := run_good(v3_bin, 'good_fn_pointer_return_reassigned_method_value',
		'struct Counter {\n\tid int\n}\nfn (c Counter) report() int {\n\treturn c.id\n}\nfn plain() int {\n\treturn 7\n}\nfn build_cb(c Counter) fn () int {\n\tmut cb := c.report\n\tcb = plain\n\treturn cb\n}\nfn main() {\n\tf := build_cb(Counter{\n\t\tid: 1\n\t})\n\tprintln(int_str(f()))\n}\n')
	assert built == '7'
}

fn test_const_length_fixed_array() {
	v3_bin := build_v3()
	// A fixed array whose length is a const round-trips through the checker as the postfix name
	// `int[seg_count]`. The transform's fixed-array predicate (now const-aware) folds `.len` to the
	// constant instead of emitting a struct field access on the C array, and parse_type treats the
	// builtin-base postfix as a fixed array (not a bogus generic), so the decl uses memcpy.
	direct := run_good(v3_bin, 'good_const_len_fixed_array_call_len',
		'const seg_count = 3\nfn f() [seg_count]int {\n\tmut r := [seg_count]int{}\n\treturn r\n}\nfn main() {\n\tprintln(int_str(f().len))\n}\n')
	assert direct == '3'
	decl := run_good(v3_bin, 'good_const_len_fixed_array_decl_len',
		"const seg_count = 3\nfn f() [seg_count]int {\n\tmut r := [seg_count]int{}\n\tr[0] = 10\n\treturn r\n}\nfn main() {\n\ta := f()\n\tprintln(int_str(a[0]) + ',' + int_str(a.len))\n}\n")
	assert decl == '10,3'
	// An expression length (`[segs + 1]int`) likewise folds, iterates, and indexes correctly.
	expr := run_good(v3_bin, 'good_expr_len_fixed_array',
		"const segs = 2\nfn f() [segs + 1]int {\n\tmut r := [segs + 1]int{}\n\tr[0] = 5\n\tr[1] = 6\n\tr[2] = 7\n\treturn r\n}\nfn main() {\n\ta := f()\n\tmut sum := 0\n\tfor x in a {\n\t\tsum += x\n\t}\n\tprintln(int_str(a.len) + ',' + int_str(a[1]) + ',' + int_str(sum))\n}\n")
	assert expr == '3,6,18'
}

fn test_pr_review_codegen_batch_twentyfive() {
	v3_bin := build_v3()
	// An interface-returning function with a pending `defer` saves its return value into a temp
	// before running the defers. That temp must hold the boxed interface value (`_typ`/`_object`),
	// not a zeroed `(Iface){0}`; otherwise the later dynamic dispatch panics. The boxing must match
	// the direct (defer-free) return path.
	defer_iface := run_good(v3_bin, 'good_interface_return_with_defer',
		'interface Speaker {\n\tspeak() int\n}\nstruct Dog {\n\tvolume int\n}\nfn (d Dog) speak() int {\n\treturn d.volume\n}\nfn make() Speaker {\n\tdefer {\n\t\t_ := 0\n\t}\n\treturn Dog{\n\t\tvolume: 42\n\t}\n}\nfn main() {\n\ts := make()\n\tprintln(int_str(s.speak()))\n}\n')
	assert defer_iface == '42'
	// A local whose address escapes (returned via a pointer) is moved to the heap, so the local is
	// now a `&T`. Its compound (`v += 1`) and postfix (`v++`) mutations must store through the
	// pointer (`*v += 1`, `(*v)++`), not perform pointer arithmetic; the returned pointer reflects
	// the mutated value.
	heap_compound := run_good(v3_bin, 'good_heaped_local_compound_mutation',
		"fn compound() int {\n\tmut v := 1\n\tp := &v\n\tv += 1\n\treturn *p\n}\nfn postfix() int {\n\tmut v := 5\n\tp := &v\n\tv++\n\tv++\n\treturn *p\n}\nfn main() {\n\tprintln(int_str(compound()) + ',' + int_str(postfix()))\n}\n")
	assert heap_compound == '2,7'
}

fn test_pr_review_codegen_batch_twentysix() {
	v3_bin := build_v3()
	// The escape prepass must only heap-move a local whose address is *actually returned*. A
	// pointer that merely appears in a consuming expression (comparison/boolean here, or a deref)
	// does not escape, so its source local must stay a plain value — otherwise codegen reads it as
	// an `int*` in the integer comparison and the result is wrong. `return p == p && v == 1` and
	// `return *p` both keep `v` on the stack.
	no_escape := run_good(v3_bin, 'good_escape_only_returned_pointer',
		"fn check() bool {\n\tmut v := 1\n\tp := &v\n\treturn p == p && v == 1\n}\nfn deref() int {\n\tmut v := 3\n\tp := &v\n\tv = 8\n\treturn *p\n}\nfn main() {\n\tprintln(check().str() + ',' + int_str(deref()))\n}\n")
	assert no_escape == 'true,8'
	// A pointer that *is* returned (directly, and inside a returned aggregate) must still heap-move
	// its source local, so the returned pointer observes mutations made after the address was taken.
	escapes := run_good(v3_bin, 'good_escape_returned_pointer_and_aggregate',
		"fn direct() &int {\n\tmut v := 10\n\tp := &v\n\tv += 5\n\treturn p\n}\nfn aggregate() []&int {\n\tmut v := 7\n\tp := &v\n\tv = 9\n\treturn [p]\n}\nfn main() {\n\tarr := aggregate()\n\tprintln(int_str(*direct()) + ',' + int_str(*arr[0]))\n}\n")
	assert escapes == '15,9'
	// An optional fixed-array return (`?[N]T`) with a pending `defer` routes through the deferred
	// return path; that path must apply the same temp + memcpy handling as the direct path (the
	// `.value` member is a fixed array and cannot be set via a compound literal), so the array value
	// is preserved instead of being dropped to `{.ok = false}`.
	defer_opt_arr := run_good(v3_bin, 'good_optional_fixed_array_return_with_defer',
		"fn f() ?[2]int {\n\tdefer {\n\t\t_ := 0\n\t}\n\treturn [1, 2]!\n}\nfn main() {\n\ta := f() or { [0, 0]! }\n\tprintln(int_str(a[0]) + ',' + int_str(a[1]))\n}\n")
	assert defer_opt_arr == '1,2'
}

fn test_pr_review_codegen_batch_twentyseven() {
	v3_bin := build_v3()
	// Local type names declared inside sibling function literals must include a closure
	// discriminator. Both closures declare `Row` with different fields; the outer local `Shared`
	// remains visible from both closure scopes.
	local_rows := run_good(v3_bin, 'good_fn_literal_local_struct_scope',
		'fn main() {\n\tstruct Shared {\n\t\tn int\n\t}\n\tfirst := fn () int {\n\t\tstruct Row {\n\t\t\ta int\n\t\t}\n\t\ts := Shared{\n\t\t\tn: 10\n\t\t}\n\t\tr := Row{\n\t\t\ta: 1\n\t\t}\n\t\treturn s.n + r.a\n\t}\n\tsecond := fn () int {\n\t\tstruct Row {\n\t\t\tb int\n\t\t}\n\t\ts := Shared{\n\t\t\tn: 20\n\t\t}\n\t\tr := Row{\n\t\t\tb: 2\n\t\t}\n\t\treturn s.n + r.b\n\t}\n\tprintln(int_str(first() + second()))\n}\n')
	assert local_rows == '33'
}

fn test_pr_review_codegen_batch_twentyeight() {
	v3_bin := build_v3()
	// `char` values passed through `...voidptr` use the same int-width storage as other small
	// integer varargs, so callees that read `%c`/small-integer slots do not read past the value.
	char_vararg := run_good(v3_bin, 'good_voidptr_variadic_char_promotes_to_int',
		'fn sink(args ...voidptr) int {\n\treturn unsafe { *(&int(args[0])) }\n}\nfn main() {\n\tch := char(66)\n\tprintln(int_str(sink(ch)))\n}\n')
	assert char_vararg == '66'
	// A local type's synthesized name uses an internal marker, so a user-written top-level type
	// that matched the old underscore-only spelling cannot overwrite the local declaration.
	local_collision := run_good(v3_bin, 'good_local_type_name_avoids_user_collision',
		'struct Row__local_make {\n\tglobal int\n}\nfn make() int {\n\tstruct Row {\n\t\tlocal int\n\t}\n\tlocal := Row{\n\t\tlocal: 3\n\t}\n\tglobal := Row__local_make{\n\t\tglobal: 4\n\t}\n\treturn local.local + global.global\n}\nfn main() {\n\tprintln(int_str(make()))\n}\n')
	assert local_collision == '7'
}

fn test_if_guard_rejects_or_handled_value() {
	v3_bin := build_v3()
	run_bad(v3_bin, 'bad_if_guard_or_handled_value',
		'fn maybe() ?int {\n\treturn 1\n}\nfn main() {\n\tif value := maybe() or { return } {\n\t\tprintln(int_str(value))\n\t}\n}\n',
		'if guard expression must be optional or result')
}
