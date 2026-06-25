import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// build_v3 builds v3 data for v3 tests.
fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_type_checker_errors_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0
	return v3_bin
}

// run_bad supports run bad handling for v3 tests.
fn run_bad(v3_bin string, name string, src string, expected string) {
	run_bad_with_flags(v3_bin, name, src, expected, '')
}

fn run_bad_selfhost(v3_bin string, name string, src string, expected string) {
	run_bad_with_flags(v3_bin, name, src, expected, '-selfhost')
}

fn run_bad_with_flags(v3_bin string, name string, src string, expected string, flags string) {
	bad_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	result := os.execute('${v3_bin} ${bad_src} ${flags} -b c -o ${bad_bin}')
	assert result.exit_code != 0
	assert result.output.contains(expected)
	assert !result.output.contains('C compilation failed')
}

// run_good supports run good handling for v3 tests.
fn run_good(v3_bin string, name string, src string) string {
	good_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
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

// run_good_project supports run good project handling for v3 tests.
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
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
}

// gen_c_project emits c project output for v3 tests.
fn gen_c_project(v3_bin string, name string, files map[string]string, input string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	c_out := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.rm(c_out) or {}
	compile := os.execute('${v3_bin} ${input_path} -o ${c_out}')
	assert compile.exit_code == 0
	assert os.exists(c_out)
	return os.read_file(c_out) or { panic(err) }
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
	}, '', 'unknown function `moda.answer`')
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
