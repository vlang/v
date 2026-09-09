import os

const selfhost_regression_vexe = @VEXE
const selfhost_regression_tests_dir = os.dir(@FILE)
const selfhost_regression_v3_dir = os.dir(selfhost_regression_tests_dir)
const selfhost_regression_vlib_dir = os.dir(selfhost_regression_v3_dir)
const selfhost_regression_v3_src = os.join_path(selfhost_regression_v3_dir, 'v3.v')

fn selfhost_regression_v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_selfhost_transform_regression_test')
}

fn testsuite_begin() {
	os.rm(selfhost_regression_v3_bin_path()) or {}
}

fn selfhost_regression_build_v3() string {
	v3_bin := selfhost_regression_v3_bin_path()
	if os.exists(v3_bin) {
		return v3_bin
	}
	build := os.execute('${selfhost_regression_vexe} -gc none -path "${selfhost_regression_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${selfhost_regression_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn selfhost_regression_run(name string, source string) string {
	v3_bin := selfhost_regression_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_selfhost_regression_${name}.v')
	bin := os.join_path(os.temp_dir(), 'v3_selfhost_regression_${name}')
	os.write_file(src, source) or { panic(err) }
	compile := os.execute('${v3_bin} -nocache -b c -o ${bin} ${src}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A value `if`/`match` call argument is materialized into a value temp, and the call is rebuilt
// over the materialized operands and re-dispatched. In a generic clone the checker has no
// recorded type for an enum-shorthand `if`, so the materialization falls back to plain
// `transform_expr`, which rebuilds the branch unchanged in shape. The re-dispatch used to see a
// changed-but-still-a-branch operand and recurse forever, overflowing the stack.
fn test_untyped_enum_branch_argument_in_generic_clone_terminates() {
	out := selfhost_regression_run('untyped_enum_branch_arg', 'enum Op {
	dot
	arrow
}

struct Sel {
mut:
	n int
}

fn (mut s Sel) make_selector_op(base int, field string, typ string, op Op) string {
	s.n++
	return "\${base}-\${field}-\${typ}-\${op}"
}

fn (mut s Sel) wrap[U](v U, expr_type string) string {
	base := s.n + 1
	field := "\${v}"
	field_typ := "x\${field}"
	return s.make_selector_op(base, field, field_typ, if expr_type.starts_with("&") {
		.arrow
	} else {
		.dot
	})
}

fn main() {
	mut s := Sel{}
	println(s.wrap(1, "&int"))
	println(s.wrap("s", "int"))
}
')
	assert out.split_into_lines() == ['1-1-x1-arrow', '2-s-xs-dot']
}

// A module-qualified callee (`os.abs_path(...)`) is a selector whose base names an import, not a
// value. When a later argument hoists a value branch, the source-order guards used to snapshot
// that base into a temp, emitting `unknown __order_snapshot_0 = os;`.
fn test_module_qualified_call_with_branch_argument() {
	out := selfhost_regression_run('module_qualified_branch_arg', 'import os

fn pick(name string) !string {
	if name.len == 0 {
		return error("empty")
	}
	return name
}

fn base_dir(name string) string {
	return os.abs_path(pick(name) or {
		if os.getenv("V3_SELFHOST_REGRESSION_UNSET") == "1" {
			"/tmp/a"
		} else {
			"/tmp/b"
		}
	})
}

fn main() {
	println(base_dir("") == os.abs_path("/tmp/b"))
	println(base_dir("/tmp/c") == os.abs_path("/tmp/c"))
}
')
	assert out.split_into_lines() == ['true', 'true']
}

// A static associated function and an instance method may intentionally share
// their source-level name. Their internal symbols and ABIs must remain distinct.
fn test_static_and_instance_method_with_same_name() {
	out := selfhost_regression_run('static_instance_same_name', 'struct Form {
	value string
}

struct Post {
	id int
}

@[params]
struct FormOptions {
	id     int
	action string
}

fn Post.form_for(opts FormOptions) Form {
	return Form{
		value: "static:\${opts.id}:\${opts.action}"
	}
}

fn (post Post) form_for(action string) Form {
	return Post.form_for(id: post.id, action: action)
}

fn cache__static__reset() string {
	return "ordinary"
}

struct Cache__static__State {}

fn Cache__static__State.reset__static__now() string {
	return "reversible"
}

@[markused]
fn int.tag() string {
	return "static-int"
}

fn int__static__tag__static__3() string {
	return "ordinary-int"
}

fn main() {
	println(Post{7}.form_for("edit").value)
	println(Post.form_for(id: 9, action: "new").value)
	println(cache__static__reset())
	println(Cache__static__State.reset__static__now())
	println(int__static__tag__static__3())
}
')
	assert out.split_into_lines() == ['static:7:edit', 'static:9:new', 'ordinary', 'reversible',
		'ordinary-int']
}

// A static call through a generic type parameter is resolved only after the
// generic body is cloned. Retarget it to the concrete encoded declaration.
fn test_generic_static_assoc_call_retargets_encoded_declaration() {
	out := selfhost_regression_run('generic_static_assoc_call', 'struct Parser {}

fn Parser.parse() string {
	return "parsed"
}

fn read[T]() string {
	return T.parse()
}

fn main() {
	println(read[Parser]())
}
')
	assert out == 'parsed'
}

fn test_generic_static_assoc_declaration_uses_encoded_lookup_key() {
	out := selfhost_regression_run('generic_static_assoc_declaration', 'struct Box[T] {
	value T
}

fn Box.new[T](value T) Box[T] {
	return Box[T]{
		value: value
	}
}

fn main() {
	explicit := Box.new[int](41)
	inferred := Box.new("ok")
	println(explicit.value + 1)
	println(inferred.value)
}
')
	assert out.split_into_lines() == ['42', 'ok']
}

fn test_static_method_pseudo_variables_use_source_name() {
	name := 'static_method_pseudo_vars'
	src := os.join_path(os.temp_dir(), 'v3_selfhost_regression_${name}.v')
	source_path_literal := src.replace('\\', '\\\\')
	out := selfhost_regression_run(name, 'struct Cache__static__State {}

fn Cache__static__State.report__static__now() {
	\$if @FN != \'report__static__now\' {
		\$compile_error(\'incorrect @FN\')
	}
	\$if @METHOD != \'Cache__static__State.report__static__now\' {
		\$compile_error(\'incorrect @METHOD\')
	}
	\$if @LOCATION != \'${source_path_literal}:10, main.Cache__static__State.report__static__now (static)\' {
		\$compile_error(\'incorrect @LOCATION\')
	}
	println(@FN)
	println(@METHOD)
	println(@LOCATION)
}

fn main() {
	Cache__static__State.report__static__now()
}
')
	lines := out.split_into_lines()
	assert lines[0] == 'report__static__now'
	assert lines[1] == 'Cache__static__State.report__static__now'
	assert lines[2].ends_with(', main.Cache__static__State.report__static__now (static)')
}

// An unresolved literal field shape may be shared by several declared anonymous
// structs. Keep the exact type selected from the call parameter context.
fn test_contextual_anonymous_struct_call_field_keeps_declared_type() {
	out := selfhost_regression_run('contextual_anonymous_struct_call_field', 'fn produce() string {
	return "contextual"
}

fn take_int(value struct {
	item int
}) string {
	return value.item.str()
}

fn take_string(value struct {
	item string
}) string {
	return value.item
}

fn main() {
	println(take_string(struct { item: produce() }))
}
')
	assert out == 'contextual'
}

// Literals whose call-valued fields resolve to the same semantic shape must
// share a concrete anonymous type when an enclosing expression unifies them.
fn test_inferred_anonymous_struct_call_fields_reuse_semantic_shape() {
	out := selfhost_regression_run('inferred_anonymous_struct_shape_reuse', 'fn produce(n int) int {
	return n
}

fn main() {
	values := [struct { item: produce(1) }, struct { item: produce(2) }]
	println(int_str(values.len))
}
')
	assert out == '2'
}

// A generic call result is still unknown when the template is first
// transformed. Revisit the literal after monomorphization gives the cloned
// call a concrete result type.
fn test_generic_inferred_anonymous_struct_is_materialized_after_specialization() {
	out := selfhost_regression_run('generic_inferred_anonymous_struct', 'fn produce[T](value T) T {
	return value
}

fn wrap[T](value T) T {
	result := struct { item: produce(value) }
	return result.item
}

fn main() {
	println(wrap(41))
	println(wrap("ok"))
}
')
	assert out.split_into_lines() == ['41', 'ok']
}

// A non-capturing fn literal passed to an imported generic must remain a cgen root.
// The large-project failure called this as `__anon_fn_0` without emitting its body.
fn test_imported_generic_keeps_non_capturing_fn_literal() {
	out := selfhost_regression_run('generic_fn_literal_root', 'import arrays

struct Table {
	name string
}

fn main() {
	tables := [Table{name: "users"}, Table{name: "posts"}]
	result := arrays.find_first(tables, fn (table Table) bool {
		return table.name == "posts"
	}) or { panic("missing") }
	println(result.name)
}
')
	assert out == 'posts'
}

// A generic fn literal can already spell the callback's full parameter list
// while its parameter types still need contextual specialization. Lift the
// concrete callback, not the unresolved template name left on the call.
fn test_imported_generic_specializes_generic_fn_literal() {
	out := selfhost_regression_run('generic_generic_fn_literal_root', 'import arrays

struct Table {
	name string
}

fn main() {
	tables := [Table{name: "users"}, Table{name: "posts"}]
	result := arrays.find_first(tables, fn [T](table T) bool {
		return table.name == "posts"
	}) or { panic("missing") }
	println(result.name)
}
')
	assert out == 'posts'
}

// A generic fn literal with explicit reference parameters keeps pointer identity after its
// parameters are specialized to the concrete callback signature. Only inferred pipe-lambda
// pointer parameters use auto-dereferenced value equality.
fn test_generic_fn_literal_reference_params_keep_identity() {
	out := selfhost_regression_run('generic_fn_literal_reference_identity', 'struct Data {
	value int
}

fn compare_data(compare fn (&Data, &Data) bool, a &Data, b &Data) bool {
	return compare(a, b)
}

fn main() {
	a := Data{}
	b := Data{}
	println(compare_data(fn [T](x &T, y &T) bool {
		return x == y
	}, a, b))
	println(compare_data(fn [T](x &T, y &T) bool {
		return x != y
	}, a, b))
	println(compare_data(fn [T](x &T, y &T) bool {
		return x == y
	}, a, a))
	println(compare_data(fn [T](x &T, y &T) bool {
		return x != y
	}, a, a))
}
')
	assert out.split_into_lines() == ['false', 'true', 'true', 'false']
}

// Only `for k, mut v in m` binds the map value by reference. A container that is merely a map
// reference (`m &map[string]bool`) still binds a plain value copy, so the binding must not be
// typed `&V` — that made every use of it emit a dereference of a non-pointer local.
fn test_for_in_over_map_reference_binds_value() {
	out := selfhost_regression_run('for_in_map_reference', 'fn count_used(used &map[string]bool) int {
	mut n := 0
	for name, is_used in used {
		if !is_used || name.len == 0 {
			continue
		}
		n++
	}
	return n
}

fn double_values(mut m map[string]int) {
	for _, mut v in m {
		v = v * 2
	}
}

fn main() {
	flags := {
		"a": true
		"b": false
		"c": true
	}
	println(count_used(&flags))
	mut counts := {
		"a": 1
		"b": 2
	}
	double_values(mut counts)
	println(counts["a"])
	println(counts["b"])
}
')
	assert out.split_into_lines() == ['2', '2', '4']
}

// `mod.Type.make(...)` is a static associated call: its callee base is the *selector* `mod.Type`,
// which names a type, not a value. `static_assoc_fn_name` recognizes that selector shape, so the
// namespace guard must consult it for selector bases too — otherwise the call is classified as a
// method and a value-branch argument makes the ordering guards snapshot the type name as a
// receiver (`void __order_snapshot_0 = shapes__Box;`).
fn test_module_qualified_static_assoc_call_with_branch_argument() {
	v3_bin := selfhost_regression_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_selfhost_regression_static_assoc_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'shapes')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'selfhost_regression_static_assoc' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'shapes/shapes.v'), 'module shapes

pub struct Box {
pub:
	w int
	h int
}

pub fn Box.make(w int) Box {
	return Box{
		w: w
		h: w * 2
	}
}
') or { panic(err) }
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, 'module main

import shapes

fn pick(n int) !int {
	if n < 0 {
		return error("neg")
	}
	return n
}

fn build(n int) shapes.Box {
	return shapes.Box.make(pick(n) or {
		if n == -1 {
			7
		} else {
			9
		}
	})
}

fn main() {
	b := build(3)
	println("\${b.w}-\${b.h}")
	c := build(-1)
	println("\${c.w}-\${c.h}")
}
') or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_selfhost_regression_static_assoc_bin_${os.getpid()}')
	compile := os.execute('${v3_bin} -nocache -b c -o ${bin} ${main_path}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['3-6', '7-14']
}

// `strings.new_builder(if cond { a } else { b })` — the shape `strconv.format_es_old` uses — is a
// module-qualified call whose argument is a value `if`. The ordering guards used to snapshot the
// `strings` base into a temp typed by whatever the checker recorded for a module identifier,
// emitting `strconv__unknown __order_snapshot_1 = strings;`.
//
// This pins the reported program against a regression in the namespace classifier, which is what
// resolves this shape. The backstop behind the classifier is covered directly by
// vlib/v3/transform/ordering_guard_test.v.
fn test_module_qualified_call_with_branch_argument_in_imported_module() {
	out := selfhost_regression_run('module_qualified_branch_arg_imported', 'import strconv

fn main() {
	x := 3.141516
	println(unsafe { strconv.v_sprintf("aaa %G", x) })
	println(unsafe { strconv.v_sprintf("bbb %08.3f", x) })
}
')
	assert out.split_into_lines() == ['aaa 3.141516', 'bbb 0003.142']
}
