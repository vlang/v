import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn review_v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_review_transform_regressions_test')
}

fn testsuite_begin() {
	v3_bin := review_v3_bin_path()
	if os.exists(v3_bin) {
		os.rm(v3_bin) or {}
	}
}

fn build_v3_review_transform() string {
	v3_bin := review_v3_bin_path()
	if os.exists(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn run_bad(v3_bin string, name string, src string, expected string) {
	bad_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	result := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
	assert !result.output.contains('C compilation failed'), '${name}: reached C compilation\n${result.output}'
}

fn run_good(v3_bin string, name string, src string) string {
	return run_good_with_flags(v3_bin, name, '', src)
}

fn run_good_with_flags(v3_bin string, name string, flags string, src string) string {
	good_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${flags} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
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
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn test_lifted_fn_literal_mut_param_interpolation_derefs_value() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'lifted_literal_mut_param_interpolation',
		'fn main() {\n\tmut n := 7\n\tf := fn (mut x int) {\n\t\tprintln("\${x}")\n\t}\n\tf(mut n)\n}\n')
	assert out == '7'
}

fn test_folded_string_constant_ifs_keep_branch_scopes() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'folded_string_constant_if_branch_scopes',
		"fn main() {\n\tif 'left' == 'left' {\n\t\tx := 20\n\t\tprintln(int_str(x))\n\t}\n\tif 'right' == 'right' {\n\t\tx := 22\n\t\tprintln(int_str(x))\n\t}\n}\n")
	assert out == '20\n22'
}

fn test_import_aliased_variadic_call_uses_exact_module() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'import_aliased_variadic_call', {
		'v.mod':         "Module { name: 'import_aliased_variadic_call' }\n"
		'a/http/http.v': 'module http\n\npub fn total(values []int) int {\n\treturn values.len\n}\n'
		'b/http/http.v': 'module http\n\npub fn total(values ...int) int {\n\treturn values.len\n}\n'
		'main.v':        'module main\n\nimport a.http as other_http\nimport b.http as http\n\nfn main() {\n\t_ := other_http.total([1, 2])\n\tprintln(int_str(http.total(3, 4, 5)))\n}\n'
	}, 'main.v')
	assert out == '3'
}

fn test_generic_specializations_keep_full_aliased_import_paths() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'generic_specialization_aliased_import_paths', {
		'v.mod':          "Module { name: 'generic_specialization_aliased_import_paths' }\n"
		'a/tast/value.v': 'module tast\n\npub struct Value {\npub:\n\tn int\n}\n'
		'b/tast/value.v': 'module tast\n\npub struct Value {\npub:\n\ttext string\n}\n'
		'main.v':         "module main\n\nimport a.tast as left\nimport b.tast as tast\n\nfn keep[T](value T) T {\n\treturn value\n}\n\nfn main() {\n\tleft_value := keep(left.Value{\n\t\tn: 41\n\t})\n\tright_value := keep(tast.Value{\n\t\ttext: 'ok'\n\t})\n\tprintln(int_str(left_value.n))\n\tprintln(right_value.text)\n}\n"
	}, 'main.v')
	assert out == '41\nok'
}

fn test_nested_inferred_fixed_array_literal_parses() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'nested_inferred_fixed_array_literal',
		'fn main() {\n\tvalues := [..][..]int[[1, 2], [3, 4]]\n\tprintln(int_str(values[0][0] + values[0][1] + values[1][0] + values[1][1]))\n}\n')
	assert out == '10'
	run_bad(v3_bin, 'ragged_nested_inferred_fixed_array_literal',
		'fn main() {\n\t_ := [..][..]int[[1], [2, 3]]\n}\n',
		'inferred fixed-array literal rows must have the same size')
}

fn test_shared_field_without_sync_import_compiles_and_locks() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'shared_field_without_sync_import',
		'struct S {\nmut:\n\ta shared int\n}\n\nfn main() {\n\tmut s := S{}\n\tlock s.a {\n\t\ts.a = 7\n\t\tprintln(int_str(s.a))\n\t}\n}\n')
	assert out == '7'
}

fn test_imported_shared_field_without_sync_import_compiles_and_locks() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'imported_shared_field_without_sync_import', {
		'v.mod':     'Module { name: "imported_shared_field_without_sync_import" }\n'
		'main.v':    'module main\n\nimport bag\n\nfn main() {\n\tprintln(int_str(bag.value()))\n}\n'
		'bag/bag.v': 'module bag\n\nstruct S {\nmut:\n\ta shared int\n}\n\npub fn value() int {\n\tmut s := S{}\n\tmut out := 0\n\tlock s.a {\n\t\ts.a = 9\n\t\tout = s.a\n\t}\n\treturn out\n}\n'
	}, 'main.v')
	assert out == '9'
}

fn test_reject_dynamic_arrays_for_fixed_array_expectations() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'bad_fixed_array_literal_len',
		'fn take3(a [3]int) int {\n\treturn a[0]\n}\nfn main() {\n\t_ := take3([1, 2])\n}\n',
		'cannot use')
	run_bad(v3_bin, 'bad_dynamic_array_for_fixed_array',
		'fn take3(a [3]int) int {\n\treturn a[0]\n}\nfn main() {\n\txs := [1, 2, 3]\n\t_ := take3(xs)\n}\n',
		'cannot use')
	out := run_good(v3_bin, 'good_exact_fixed_array_literal',
		'fn take3(a [3]int) int {\n\treturn a[0] + a[1] + a[2]\n}\nfn main() {\n\tprintln(int_str(take3([1, 2, 3])))\n}\n')
	assert out == '6'
	indexed := run_good(v3_bin, 'good_fixed_array_init_index',
		'fn main() {\n\ta := [4]int{init: index * index}\n\tprintln(int_str(a[0]) + "," + int_str(a[1]) + "," + int_str(a[2]) + "," + int_str(a[3]))\n}\n')
	assert indexed == '0,1,4,9'
	const_indexed := run_good(v3_bin, 'good_fixed_array_const_init_index',
		'const n = 4\n\nfn main() {\n\ta := [n]int{init: index * index}\n\tprintln(int_str(a[0]) + "," + int_str(a[1]) + "," + int_str(a[2]) + "," + int_str(a[3]))\n}\n')
	assert const_indexed == '0,1,4,9'
}

fn test_array_equality_uses_semantic_element_comparison() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'semantic_array_equality',
		"struct Child {\n\tlabel string\n}\n\nstruct Item {\n\tname string\n\tparts []string\n\tnested [][]string\n\tchildren []Child\n}\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tleft := [Item{\n\t\tname: 'hi'.clone()\n\t\tparts: ['ab'.clone()]\n\t\tnested: [[join('n', 'est')]]\n\t\tchildren: [Child{\n\t\t\tlabel: 'kid'.clone()\n\t\t}]\n\t}]\n\tright := [Item{\n\t\tname: join('h', 'i')\n\t\tparts: [join('a', 'b')]\n\t\tnested: [['nest'.clone()]]\n\t\tchildren: [Child{\n\t\t\tlabel: join('k', 'id')\n\t\t}]\n\t}]\n\tmaps_left := [{\n\t\t'k': 'value'.clone()\n\t}]\n\tmaps_right := [{\n\t\t'k': join('val', 'ue')\n\t}]\n\tnested_left := [[join('y', 'o')]]\n\tnested_right := [['yo'.clone()]]\n\tchild_map_left := {\n\t\t'items': [Child{\n\t\t\tlabel: 'mapkid'.clone()\n\t\t}]\n\t}\n\tchild_map_right := {\n\t\t'items': [Child{\n\t\t\tlabel: join('map', 'kid')\n\t\t}]\n\t}\n\tneedle := Item{\n\t\tname: join('h', 'i')\n\t\tparts: [join('a', 'b')]\n\t\tnested: [['nest'.clone()]]\n\t\tchildren: [Child{\n\t\t\tlabel: join('k', 'id')\n\t\t}]\n\t}\n\tprintln(left == right)\n\tprintln(left.equals(right))\n\tprintln(maps_left == maps_right)\n\tprintln(nested_left == nested_right)\n\tprintln(child_map_left == child_map_right)\n\tprintln(needle in left)\n\tprintln(int_str(left.index(needle)))\n}\n")
	assert out == 'true\ntrue\ntrue\ntrue\ntrue\ntrue\n0'
}

fn test_array_map_fn_value_uses_callback_return_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_map_fn_value_return_type',
		"fn main() {\n\ti_to_str := fn (i int) string {\n\t\treturn int_str(i)\n\t}\n\ta := [1, 2, 3].map(i_to_str)\n\tassert a == ['1', '2', '3']\n\tprintln(a[0] + a[1] + a[2])\n}\n")
	assert out == '123'
}

fn test_const_array_allows_newline_separators_with_line_comments() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'const_array_line_comments',
		'const xs = [\n\t1\n\t// one\n\t2\n\t// two\n\t3\n]\n\nfn main() {\n\tprintln(int_str(xs.len))\n\tprintln(int_str(xs[1]))\n}\n')
	assert out == '3\n2'
}

fn test_mut_pointer_capture_is_not_over_dereferenced() {
	v3_bin := build_v3_review_transform()
	// A `[mut p]` capture whose original type is already a pointer (`&S`) must stay a
	// genuine `&S` local: its rvalue uses must not be over-dereferenced, so a call that
	// expects the pointer still receives it (regression for gating the pointer-value
	// rvalue/lvalue flags on `capture_by_ref` instead of every `capture_mut`).
	out := run_good(v3_bin, 'mut_pointer_capture',
		'struct S {\n\tn int\n}\n\nfn takes_ptr(p &S) int {\n\treturn p.n\n}\n\nfn call(cb fn ()) {\n\tcb()\n}\n\nfn main() {\n\tmut p := &S{\n\t\tn: 5\n\t}\n\tcall(fn [mut p] () {\n\t\tprintln(int_str(takes_ptr(p)))\n\t})\n}\n')
	assert out == '5'
}

fn test_mut_value_capture_in_call_under_selector_base() {
	v3_bin := build_v3_review_transform()
	// A `[mut s]` value capture used as a call argument nested inside a selector base
	// (`wrap(s).s.n`) must still be lowered to its value; the selector-base deref
	// suppression applies only to the direct receiver ident, not to nested expressions.
	out := run_good(v3_bin, 'mut_capture_selector_base',
		'struct S {\n\tn int\n}\n\nstruct Box {\n\ts S\n}\n\nfn wrap(s S) Box {\n\treturn Box{\n\t\ts: s\n\t}\n}\n\nfn call(cb fn ()) {\n\tcb()\n}\n\nfn main() {\n\tmut s := S{\n\t\tn: 7\n\t}\n\tcall(fn [mut s] () {\n\t\tprintln(int_str(wrap(s).s.n))\n\t})\n}\n')
	assert out == '7'
}

fn test_mut_value_capture_parenthesized_selector_receiver() {
	v3_bin := build_v3_review_transform()
	// A `[mut s]` value capture is a `&S` local; a parenthesized direct receiver
	// (`(s).n`) is still the direct selector receiver and must keep the suppression so
	// the selector emits arrow access. Otherwise the inner `s` is auto-dereferenced to
	// `*s` while the selector still emits `->`, producing an invalid `(*s)->n`.
	out := run_good(v3_bin, 'mut_capture_paren_selector_base',
		'struct S {\n\tn int\n}\n\nfn call(cb fn ()) {\n\tcb()\n}\n\nfn main() {\n\tmut s := S{\n\t\tn: 7\n\t}\n\tcall(fn [mut s] () {\n\t\tprintln(int_str((s).n))\n\t})\n}\n')
	assert out == '7'
}

fn test_heap_escaping_amp_alias_keeps_heap_pointer() {
	v3_bin := build_v3_review_transform()
	// When a local `s` whose address escapes is moved to the heap, `s` becomes the `&S`
	// heap pointer and the alias `p := &s` must stay that pointer (`p := s`), NOT be
	// auto-dereferenced to `*s`. Over-dereferencing here initializes `p`'s `&S` decl from
	// an `S` value (a stale stack copy), reviving the escape/stale-mutation bug the heap
	// move avoids. A later `s = S{n: 2}` must be observable through the returned pointer.
	out := run_good(v3_bin, 'heap_escaping_amp_alias',
		'struct S {\n\tn int\n}\n\nfn leak() &S {\n\tmut s := S{\n\t\tn: 1\n\t}\n\tp := &s\n\ts = S{\n\t\tn: 2\n\t}\n\treturn p\n}\n\nfn main() {\n\tp := leak()\n\tprintln(int_str(p.n))\n}\n')
	assert out == '2'
}

fn test_imported_result_array_return_or_preserves_success_value() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'imported_result_array_return_or', {
		'v.mod':     "Module { name: 'imported_result_array_return_or' }\n"
		'main.v':    "module main\n\nimport pat\n\nfn main() {\n\tlines := pat.from_path()!\n\tassert lines == ['ok']\n\tprintln(lines[0])\n}\n"
		'pat/pat.v': "module pat\n\nfn source() ![]string {\n\treturn ['ok']\n}\n\npub fn from_path() ![]string {\n\treturn source() or {\n\t\treturn error(err.msg())\n\t}\n}\n"
	}, 'main.v')
	assert out == 'ok'
}

fn test_result_multi_return_match_branch_unwraps_payload_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'result_multi_return_match_branch_unwrap',
		"enum Kind {\n\tleft\n\tright\n}\n\nfn left_pair() !(int, string) {\n\treturn 1, 'left'\n}\n\nfn right_pair() !(int, string) {\n\treturn 2, 'right'\n}\n\nfn choose(kind Kind) !(int, string) {\n\treturn match kind {\n\t\t.left {\n\t\t\tleft_pair()!\n\t\t}\n\t\t.right {\n\t\t\tright_pair()!\n\t\t}\n\t}\n}\n\nfn main() {\n\tn, label := choose(.right)!\n\tprintln(int_str(n) + ':' + label)\n}\n")
	assert out == '2:right'
}

fn test_result_multi_return_match_branch_unwraps_imported_payload_type() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'result_multi_return_match_imported_payload', {
		'v.mod':  "Module { name: 'result_multi_return_match_imported_payload' }\n"
		'main.v': "module main\n\nimport m\n\nenum Kind {\n\tleft\n\tright\n}\n\nstruct Wrap {\n\tkind  Kind\n\tinner m.Inner\n}\n\nfn (wrap Wrap) choose() !(m.Match, []string) {\n\treturn match wrap.kind {\n\t\t.left {\n\t\t\twrap.inner.pair('left')!\n\t\t}\n\t\t.right {\n\t\t\twrap.inner.pair('right')!\n\t\t}\n\t}\n}\n\nfn main() {\n\twrap := Wrap{\n\t\tkind:  .right\n\t\tinner: m.Inner{\n\t\t\tn: 2\n\t\t}\n\t}\n\tmat, groups := wrap.choose()!\n\tprintln(int_str(mat.n) + ':' + groups[0])\n}\n"
		'm/m.v':  'module m\n\npub struct Match {\npub:\n\tn int\n}\n\npub struct Inner {\npub:\n\tn int\n}\n\npub fn (inner Inner) pair(label string) !(Match, []string) {\n\treturn Match{\n\t\tn: inner.n\n\t}, [label]\n}\n'
	}, 'main.v')
	assert out == '2:right'
}

fn test_string_to_owned_compiles_under_ownership_cgen() {
	v3_bin := build_v3_review_transform()
	out := run_good_with_flags(v3_bin, 'string_to_owned_ownership_cgen', '-ownership',
		"fn main() {\n\tname := 'owned'.to_owned()\n\tcopy := name.to_owned()\n\tprintln(copy)\n}\n")
	assert out == 'owned'
}

fn test_generic_interface_method_body_marks_log_debug_dispatch() {
	v3_bin := build_v3_review_transform()
	out := run_good_with_flags(v3_bin, 'generic_interface_log_debug_dispatch', '-ownership',
		"import log\n\ninterface Sink {\n\tbinary_data()\n}\n\nstruct Box[T] {}\n\nfn (mut b Box[T]) binary_data() {\n\t_ = b\n\tlog.debug('hidden')\n}\n\nstruct Runner {}\n\nfn (mut r Runner) run(mut s Sink) {\n\t_ = r\n\ts.binary_data()\n}\n\nstruct Worker {\nmut:\n\trunner Runner\n}\n\nfn main() {\n\tmut worker := Worker{\n\t\trunner: Runner{}\n\t}\n\tmut b := Box[int]{}\n\tworker.runner.run(mut b)\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_array_literal_separator_handling() {
	v3_bin := build_v3_review_transform()
	// Comma-, newline-, and blank-line-separated element lists parse with the
	// expected length. This parser is permissive (it never hard-errors on
	// malformed input), so the guarantee here is that a missing separator
	// (`[9 10]`) or a repeated comma (`[11,,12]`) is *not* merged/collapsed into
	// extra elements: only the leading element is kept, never `len == 2`.
	out := run_good(v3_bin, 'array_literal_separators',
		'const nl = [\n\t1\n\t2\n\t3\n]\nconst blank = [\n\t4\n\n\t5\n]\n\nfn main() {\n\tcommas := [6, 7, 8]\n\tmissing := [9 10]\n\tdoubled := [11,,12]\n\tprintln(int_str(nl.len) + ":" + int_str(blank.len) + ":" + int_str(commas.len) + ":" + int_str(missing.len) + ":" + int_str(doubled.len))\n}\n')
	assert out == '3:2:3:1:1'
}

fn test_container_wrapped_import_alias_type_resolves() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'container_import_alias_types', {
		'v.mod':     "Module { name: 'container_import_alias_types' }\n"
		'bar/bar.v': 'module bar\n\npub struct Baz {\npub:\n\tn int\n}\n'
		'foo/foo.v': 'module foo\n\nimport bar as b\n\npub struct Holder {\npub:\n\titems []b.Baz\n\tone   ?b.Baz\n}\n\npub fn make() Holder {\n\treturn Holder{\n\t\titems: [b.Baz{\n\t\t\tn: 1\n\t\t}, b.Baz{\n\t\t\tn: 2\n\t\t}]\n\t\tone: b.Baz{\n\t\t\tn: 7\n\t\t}\n\t}\n}\n'
		'main.v':    'module main\n\nimport foo\n\nfn main() {\n\th := foo.make()\n\tmut out := int_str(h.items[0].n) + int_str(h.items[1].n)\n\tif v := h.one {\n\t\tout += int_str(v.n)\n\t}\n\tprintln(out)\n}\n'
	}, 'main.v')
	assert out == '127'
}

fn test_nested_map_equality_uses_declared_value_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'nested_map_semantic_equality',
		"struct Item {\n\tname string\n\tparts []string\n}\n\nstruct Holder {\n\titems map[string][]Item\n}\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tmut left_map := map[string][]Item{}\n\tleft_map['items'] = [Item{\n\t\tname: 'ab'.clone()\n\t\tparts: ['xy'.clone()]\n\t}]\n\tmut right_map := map[string][]Item{}\n\tright_map['items'] = [Item{\n\t\tname: join('a', 'b')\n\t\tparts: [join('x', 'y')]\n\t}]\n\tleft_arr := [left_map]\n\tright_arr := [right_map]\n\tleft_holder := Holder{\n\t\titems: left_map\n\t}\n\tright_holder := Holder{\n\t\titems: right_map\n\t}\n\tprintln(left_arr == right_arr)\n\tprintln(left_holder == right_holder)\n}\n")
	assert out == 'true\ntrue'
}

fn test_pointer_array_equality_uses_pointer_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'pointer_array_equality',
		'struct Node {\n\tvalue int\n}\n\nfn main() {\n\tleft_node := Node{\n\t\tvalue: 5\n\t}\n\tright_node := Node{\n\t\tvalue: 5\n\t}\n\tleft_ptr := &left_node\n\tright_ptr := &right_node\n\tleft := [left_ptr]\n\tright := [right_ptr]\n\tsame := [left_ptr]\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == same)\n\tprintln(right_ptr in left)\n\tprintln(int_str(left.index(right_ptr)))\n}\n')
	assert out == 'false\ntrue\ntrue\nfalse\n-1'
}

fn test_struct_pointer_equality_is_semantic() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'struct_pointer_semantic_equality',
		"struct Person {\n\tname string\n\ttags []string\n}\n\nfn main() {\n\tleft := &Person{\n\t\tname: 'abc'.clone()\n\t\ttags: ['x'.clone()]\n\t}\n\tright := &Person{\n\t\tname: ('a' + 'bc')\n\t\ttags: [('x' + '')]\n\t}\n\tsame := left\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == same)\n}\n")
	assert out == 'true\nfalse\ntrue'
}

fn test_struct_equality_with_interface_field_compiles() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'struct_eq_interface_field',
		"interface Thing {\n\tvalue() int\n}\n\nstruct Item {\n\tn int\n}\n\nfn (i Item) value() int {\n\treturn i.n\n}\n\nstruct Box {\n\tthing Thing\n\tlabel string\n}\n\nfn main() {\n\titem := Item{\n\t\tn: 7\n\t}\n\tleft := Box{\n\t\tthing: item\n\t\tlabel: 'same'\n\t}\n\tright := left\n\tprintln(left == right)\n}\n")
	assert out == 'true'
}

fn test_array_pointer_equality_uses_pointer_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_pointer_equality',
		'fn main() {\n\tleft := [1, 2]\n\tright := [1, 2]\n\tleft_ptr := &left\n\tright_ptr := &right\n\tsame_ptr := left_ptr\n\tprintln(left_ptr == right_ptr)\n\tprintln(left_ptr != right_ptr)\n\tprintln(left_ptr == same_ptr)\n\tprintln(*left_ptr == *right_ptr)\n}\n')
	assert out == 'false\ntrue\ntrue\ntrue'
}

fn test_pointer_u8_array_bytestr_stays_in_cgen() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'pointer_u8_array_bytestr',
		'fn show(data &[]u8) string {\n\treturn data.bytestr()\n}\n\nfn main() {\n\tbytes := [u8(104), u8(105)]\n\tprintln(show(&bytes))\n}\n')
	assert out == 'hi'
}

fn test_map_pointer_equality_uses_pointer_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_pointer_equality',
		"fn main() {\n\tleft := {\n\t\t'x': 1\n\t}\n\tright := {\n\t\t'x': 1\n\t}\n\tleft_ptr := &left\n\tright_ptr := &right\n\tsame_ptr := left_ptr\n\tprintln(left_ptr == right_ptr)\n\tprintln(left_ptr != right_ptr)\n\tprintln(left_ptr == same_ptr)\n\tprintln(*left_ptr == *right_ptr)\n}\n")
	assert out == 'false\ntrue\ntrue\ntrue'
}

fn test_fixed_array_values_compare_semantically() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'fixed_array_semantic_equality',
		"fn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tleft := [[1]string{init: 'ab'.clone()}]\n\tright := [[1]string{init: join('a', 'b')}]\n\tmut map_left := map[string][1]string{}\n\tmap_left['k'] = [1]string{init: 'cd'.clone()}\n\tmut map_right := map[string][1]string{}\n\tmap_right['k'] = [1]string{init: join('c', 'd')}\n\tmut ints_left := map[string][2]i64{}\n\tints_left['k'] = [i64(1), i64(0)]!\n\tmut ints_right := map[string][2]i64{}\n\tints_right['k'] = [i64(2), i64(0)]!\n\tprintln(left == right)\n\tprintln(left.equals(right))\n\tprintln(map_left == map_right)\n\tprintln(ints_left == ints_right)\n}\n")
	assert out == 'true\ntrue\ntrue\nfalse'
}

fn test_const_length_fixed_array_map_values_compare_semantically() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'const_len_fixed_array_map_equality', {
		'main.v':        'module main\n\nimport store\n\nfn main() {\n\tprintln(store.check())\n}\n'
		'store/store.v': "module store\n\nconst n = 2\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\npub fn check() bool {\n\tmut left := map[string][n]string{}\n\tleft['k'] = [n]string{init: 'ab'.clone()}\n\tmut right := map[string][n]string{}\n\tright['k'] = [n]string{init: join('a', 'b')}\n\treturn left == right\n}\n"
	}, 'main.v')
	assert out == 'true'
}

fn test_interface_array_repeat_evaluates_receiver_once() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_repeat_side_effects',
		'interface Thing {\n\tvalue() int\n}\n\nstruct Item {\n\tn int\n}\n\nfn (i Item) value() int {\n\treturn i.n\n}\n\n__global calls int\n\nfn make_item() Thing {\n\tcalls++\n\treturn Item{\n\t\tn: calls\n\t}\n}\n\nfn main() {\n\titems := [make_item()].repeat(3)\n\tprintln(int_str(calls))\n\tprintln(int_str(items[0].value() + items[1].value() + items[2].value()))\n}\n')
	assert out == '1\n3'
}

fn test_negative_is_return_smartcasts_following_statements() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'negative_is_return_smartcast',
		'struct MapKind {\n\tkey_type int\n\tvalue_type int\n}\nstruct OtherKind {}\ntype Kind = MapKind | OtherKind\n\nfn passthrough(k Kind) Kind {\n\treturn k\n}\n\nfn score(k Kind) int {\n\tclean := passthrough(k)\n\tif clean !is MapKind {\n\t\treturn 0\n\t}\n\treturn clean.key_type + clean.value_type\n}\n\nfn main() {\n\tprintln(int_str(score(Kind(MapKind{\n\t\tkey_type: 2\n\t\tvalue_type: 5\n\t}))))\n\tprintln(int_str(score(Kind(OtherKind{}))))\n}\n')
	assert out == '7\n0'
}

fn test_if_expr_smartcast_selector_decl_does_not_smartcast_local() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'if_expr_selector_decl_smartcast_local',
		'struct Cat {\n\tage int\n}\nstruct Dog {\n\ttricks int\n}\ntype Animal = Cat | Dog\n\nstruct Ident {\n\tobj Animal\n}\n\nfn has_age(cat Cat) bool {\n\treturn cat.age == 3\n}\n\nfn main() {\n\tleft := Ident{\n\t\tobj: Animal(Cat{\n\t\t\tage: 2\n\t\t})\n\t}\n\tmut obj := if left.obj is Cat {\n\t\tleft.obj\n\t} else {\n\t\tCat{}\n\t}\n\tif true {\n\t\tobj = Cat{\n\t\t\tage: 3\n\t\t}\n\t}\n\tprintln(has_age(obj))\n}\n')
	assert out == 'true'
}

fn test_comptime_type_conditions_handle_logical_ops() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'comptime_type_condition_logical_ops',
		"fn classify[T](x T) int {\n\t_ := x\n\t\$if T !is string && T !is \$int && T !is []u8 {\n\t\treturn 1\n\t} \$else {\n\t\treturn 2\n\t}\n\treturn 0\n}\n\nfn grouped[T](x T) int {\n\t_ := x\n\t\$if (T is int || T is string) && T is bool {\n\t\treturn 1\n\t} \$else {\n\t\treturn 2\n\t}\n\treturn 0\n}\n\nfn main() {\n\tprintln(int_str(classify('abc')))\n\tprintln(int_str(classify(3)))\n\tprintln(int_str(classify([u8(1)])))\n\tprintln(int_str(classify(1.5)))\n\tprintln(int_str(grouped(3)))\n\tprintln(int_str(grouped('abc')))\n}\n")
	assert out == '2\n2\n2\n1\n2\n2'
}

fn test_comptime_type_conditions_keep_prefix_types_compact() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'comptime_type_condition_prefix_types',
		'struct Foo {}\n\nfn main() {\n\t\$if ?int is ?int {\n\t\tprintln("opt")\n\t} \$else {\n\t\tprintln("badopt")\n\t}\n\t\$if !Foo is !Foo {\n\t\tprintln("res")\n\t} \$else {\n\t\tprintln("badres")\n\t}\n}\n')
	assert out == 'opt\nres'
}

fn test_comptime_type_conditions_qualify_module_aliases() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'comptime_type_condition_module_alias', {
		'main.v':    'module main\n\nimport foo\n\nfn main() {\n\tprintln(foo.check())\n}\n'
		'foo/foo.v': "module foo\n\ntype ID = int\n\npub fn check() string {\n\t\$if ID is \$alias {\n\t\treturn 'alias'\n\t} \$else {\n\t\treturn 'not alias'\n\t}\n}\n"
	}, 'main.v')
	assert out == 'alias'
}

fn test_struct_equality_compares_pointer_fields_as_pointers() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'struct_eq_pointer_field',
		'struct Node {\n\tvalue int\n\tnext &Node\n}\n\nfn main() {\n\tleft := Node{\n\t\tvalue: 7\n\t\tnext: unsafe { nil }\n\t}\n\tright := Node{\n\t\tvalue: 7\n\t\tnext: unsafe { nil }\n\t}\n\tprintln([left] == [right])\n}\n')
	assert out == 'true'
}

fn test_single_module_test_file_skips_premodule_attributes() {
	v3_bin := build_v3_review_transform()
	root := os.join_path(os.temp_dir(), 'v3_premodule_attr_module_test')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'tar')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "premodule_attr_module_test" }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'tar', 'reader.v'),
		'module tar /* implementation module */\n\nfn reader_value() string {\n\treturn "reader"\n}\n') or {
		panic(err)
	}
	test_file := os.join_path(root, 'tar', 'reader_test.v')
	os.write_file(test_file,
		'@[has_globals]\n/* block comment before module */\nmodule tar // test module\n\nfn test_reader_value() {\n\tprintln(reader_value())\n}\n') or {
		panic(err)
	}
	bin_path := os.join_path(root, 'reader_test_bin')
	compile := os.execute('${v3_bin} ${test_file} -b c -o ${bin_path}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin_path)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'reader'
}

fn test_delete_last_empty_array_panics_before_tail_clear() {
	v3_bin := build_v3_review_transform()
	src := 'fn main() {\n\tmut values := []int{}\n\tvalues.delete_last()\n\tprintln("after")\n}\n'
	good_src := os.join_path(os.temp_dir(), 'v3_delete_last_empty.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_delete_last_empty')
	compile := os.execute('${v3_bin} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(good_bin)
	assert run.exit_code != 0, run.output
	assert run.output.contains('array.delete_last: array is empty'), run.output
}

fn test_delete_last_preserves_shared_slice_buffer() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'delete_last_preserves_shared_slice_buffer',
		"fn main() {\n\tmut a := [1, 2, 3, 4]\n\tb := unsafe { a[..a.len] }\n\told_data := a.data\n\ta.delete_last()\n\tassert a == [1, 2, 3]\n\tassert b == [1, 2, 3, 4]\n\tassert a.data != old_data\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_slice_element_assignment_writes_through() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'slice_element_assignment_writes_through',
		"fn main() {\n\tmut a := [1, 2, 3, 4]\n\tmut s := unsafe { a[1..3] }\n\ts[0] = 42\n\ts[1] += 5\n\tassert a == [1, 42, 8, 4]\n\tassert s == [42, 8]\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_string_pointer_comparisons_keep_pointer_semantics() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'string_pointer_comparison',
		"fn main() {\n\tleft := 'same'.clone()\n\tright := 'same'.clone()\n\tpleft := &left\n\tpright := &right\n\tprintln(pleft == pright)\n\tprintln(pleft != pright)\n\tprintln(*pleft == *pright)\n}\n")
	assert out == 'false\ntrue\ntrue'
}

fn test_map_keys_and_values_lower_to_runtime_methods() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_keys_values_lowering',
		"fn make_lookup() map[string]int {\n\treturn {\n\t\t'one': 1\n\t\t'two': 2\n\t}\n}\n\nfn main() {\n\tlookup := make_lookup()\n\tkeys := lookup.keys()\n\tvalues := make_lookup().values()\n\tmut total := 0\n\tfor value in values {\n\t\ttotal += value\n\t}\n\tsingle := {\n\t\t'only': 9\n\t}\n\tprintln(int_str(keys.len))\n\tprintln(int_str(values.len))\n\tprintln(int_str(total))\n\tprintln(int_str(single.keys().len))\n\tprintln(single.keys()[0])\n\tprintln(int_str(single.values().len))\n\tprintln(int_str(single.values()[0]))\n}\n")
	assert out == '2\n2\n3\n1\nonly\n1\n9'
}

fn test_map_str_preserves_signed_wide_entries() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_str_signed_wide_entries',
		"fn main() {\n\tvalue_map := {\n\t\t'x': i64(5000000000)\n\t}\n\tkey_map := {\n\t\ti64(-5000000000): 'x'\n\t}\n\tprintln(value_map.str())\n\tprintln(key_map.str())\n}\n")
	assert out == "{'x': 5000000000}\n{-5000000000: 'x'}"
}

fn test_map_str_normalizes_alias_key_and_value_types() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_str_alias_kinds',
		"type ID = int\n\ntype Amount = f64\n\nfn main() {\n\tids := {\n\t\tID(23): 'id'\n\t}\n\tamounts := {\n\t\t'price': Amount(1.25)\n\t}\n\tprintln('\${ids}')\n\tprintln('\${amounts}')\n}\n")
	assert out == "{23: 'id'}\n{'price': 1.25}"
}

fn test_chained_array_alias_stringification_uses_outer_alias_only() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'chained_array_alias_str',
		"type A = []int\n\ntype B = A\n\nfn main() {\n\tvalue := B([1, 2])\n\tprintln('\${value}')\n}\n")
	assert out == 'B([1, 2])'
}

fn test_alias_pointer_receiver_str_gets_addressable_value() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'alias_pointer_receiver_str',
		"type Number = int\n\nfn (n &Number) str() string {\n\treturn 'number:' + int_str(int(*n))\n}\n\nstruct Point {\n\tx int\n}\n\ntype NamedPoint = Point\n\nfn (p &NamedPoint) str() string {\n\treturn 'point:' + int_str(p.x)\n}\n\nfn main() {\n\tn := Number(7)\n\tp := NamedPoint(Point{\n\t\tx: 9\n\t})\n\tprintln('\${n}')\n\tprintln('\${Number(8)}')\n\tprintln('\${p}')\n\tprintln('\${NamedPoint(Point{x: 10})}')\n}\n")
	assert out == 'number:7\nnumber:8\npoint:9\npoint:10'
}

fn test_mut_map_param_interpolation_preserves_pointer() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'mut_map_param_interpolation',
		"type Scores = map[string]int\n\nfn show(mut m map[string]int) {\n\tprintln('\${m}')\n}\n\nfn show_alias(mut m Scores) {\n\tprintln('\${m}')\n}\n\nfn main() {\n\tmut m := map[string]int{}\n\tm['x'] = 3\n\tshow(mut m)\n\tmut scores := Scores(map[string]int{})\n\tscores['y'] = 4\n\tshow_alias(mut scores)\n}\n")
	assert out == "{'x': 3}\n{'y': 4}"
}

fn test_map_literal_stringification_evaluates_entries_once() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_literal_str_side_effects',
		"__global key_calls int\n__global val_calls int\n\nfn next_key() string {\n\tkey_calls++\n\treturn 'k' + int_str(key_calls)\n}\n\nfn next_val() int {\n\tval_calls++\n\treturn val_calls * 10\n}\n\nfn main() {\n\tprintln({\n\t\tnext_key(): next_val()\n\t})\n\tprintln(int_str(key_calls) + ',' + int_str(val_calls))\n}\n")
	assert out == "{'k1': 10}\n1,1"
}

fn test_map_literal_declaration_evaluates_entries_once() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_literal_decl_side_effects',
		"__global key_calls int\n__global val_calls int\n\nfn next_key() string {\n\tkey_calls++\n\treturn 'key'\n}\n\nfn next_val() int {\n\tval_calls++\n\treturn val_calls * 10\n}\n\nfn main() {\n\tvalues := {\n\t\tnext_key(): next_val()\n\t}\n\tprintln(int_str(values['key']))\n\tprintln(int_str(key_calls) + ',' + int_str(val_calls))\n}\n")
	assert out == '10\n1,1'
}

fn test_fn_literal_preserves_mut_param_string_interpolation() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'fn_literal_mut_param_interp',
		"fn show(mut x int) {\n\t_ := fn () {}\n\tprintln('\${x}')\n}\n\nfn main() {\n\tmut n := 42\n\tshow(mut n)\n}\n")
	assert out == '42'
}

fn test_shadowed_minmaxof_calls_are_not_rewritten() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'shadowed_minmaxof_calls', {
		'main.v':          'module main\n\nimport shadow { maxof, minof }\n\nfn main() {\n\tprintln(int_str(maxof[int]()))\n\tprintln(int_str(minof[int]()))\n}\n'
		'shadow/shadow.v': 'module shadow\n\npub fn maxof[T]() int {\n\treturn 7\n}\n\npub fn minof[T]() int {\n\treturn -7\n}\n'
	}, 'main.v')
	assert out == '7\n-7'
}

fn test_runes_iterator_index_is_loop_scoped() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'runes_iterator_index_scope',
		"fn main() {\n\tfor i, r in 'ab'.runes_iterator() {\n\t\t_ := r\n\t\tprintln(int_str(i))\n\t}\n\ti := 9\n\tprintln(int_str(i))\n}\n")
	assert out == '0\n1\n9'
}

fn test_array_last_index_uses_element_width() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_last_index_element_width',
		'fn main() {\n\twide := [i64(1), i64(5000000000), i64(2), i64(5000000000)]\n\tfloats := [1.25, 2.5, 1.25]\n\tflags := [true, false, true]\n\tprintln(int_str(wide.last_index(i64(5000000000))))\n\tprintln(int_str(floats.last_index(1.25)))\n\tprintln(int_str(flags.last_index(true)))\n}\n')
	assert out == '3\n2\n2'
}

fn test_array_last_index_uses_semantic_element_comparison() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_last_index_semantic_equality',
		"struct Item {\n\tname string\n\tparts []string\n}\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tnested := [['ab'.clone()], [join('x', 'y')], [join('a', 'b')]]\n\tnested_needle := ['ab'.clone()]\n\titems := [Item{\n\t\tname: 'ab'.clone()\n\t\tparts: ['xy'.clone()]\n\t}, Item{\n\t\tname: join('a', 'b')\n\t\tparts: [join('x', 'y')]\n\t}]\n\tneedle := Item{\n\t\tname: 'ab'.clone()\n\t\tparts: ['xy'.clone()]\n\t}\n\tprintln(int_str(nested.last_index(nested_needle)))\n\tprintln(int_str(items.last_index(needle)))\n}\n")
	assert out == '2\n1'
}

fn test_generic_string_literal_matching_typeof_marker_is_preserved() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'generic_marker_string_literal',
		"fn marker_and_type[T](value T) string {\n\tmarker := '__v3_generic_type_name:T'\n\treturn marker + '|' + typeof(value).name\n}\n\nfn main() {\n\tprintln(marker_and_type(7))\n}\n")
	assert out == '__v3_generic_type_name:T|int'
}

fn test_typeof_function_fixed_array_types_keep_function_shape() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'typeof_function_fixed_array_types', 'fn values() [3]int {
	return [1, 2, 3]!
}

fn first_two(input [3]int) [2]int {
	return [input[0], input[1]]!
}

const values_fn_type_name = typeof(values).name
const first_two_fn_type_name = typeof(first_two).name

fn main() {
	values_fn := values
	first_two_fn := first_two
	println(typeof(values_fn).name)
	println(typeof(first_two_fn).name)
	println(values_fn_type_name)
	println(first_two_fn_type_name)
}
')
	assert out == 'fn () [3]int\nfn ([3]int) [2]int\nfn () [3]int\nfn ([3]int) [2]int'
}

fn test_typeof_idx_uses_active_smartcast() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'typeof_idx_smartcast', 'struct Foo {}
struct Bar {}

type Value = Foo | Bar

fn show(value Value) {
	if value is Foo {
		println(typeof(value).name)
		println((typeof(value).idx == typeof[Foo]().idx).str())
	}
	match value {
		Bar {
			println(typeof(value).name)
			println((typeof(value).idx == typeof[Bar]().idx).str())
		}
		else {}
	}
}

fn main() {
	show(Foo{})
	show(Bar{})
}
')
	assert out == 'Foo\ntrue\nBar\ntrue'
}

fn test_optional_string_equality_uses_payload_equality() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_string_semantic_equality',
		"fn maybe_text(ok bool) ?string {\n\tif !ok {\n\t\treturn none\n\t}\n\tprefix := 'a'.clone()\n\treturn prefix + 'b'\n}\n\nfn main() {\n\tleft := maybe_text(true)\n\tright := maybe_text(true)\n\tmissing_left := maybe_text(false)\n\tmissing_right := maybe_text(false)\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == missing_left)\n\tprintln(missing_left == missing_right)\n\tprintln(missing_left != missing_right)\n}\n")
	assert out == 'true\nfalse\nfalse\ntrue\nfalse'
}

fn test_optional_nested_array_equality_guards_payload_work() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_nested_array_guarded_equality',
		"fn maybe_nested(ok bool) ?[][]string {\n\tif !ok {\n\t\treturn none\n\t}\n\treturn [['a'.clone()], ['b'.clone()]]\n}\n\nfn main() {\n\tleft := maybe_nested(true)\n\tright := maybe_nested(true)\n\tmissing_left := maybe_nested(false)\n\tmissing_right := maybe_nested(false)\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == missing_left)\n\tprintln(missing_left == missing_right)\n\tprintln(missing_left != missing_right)\n}\n")
	assert out == 'true\nfalse\nfalse\ntrue\nfalse'
}

fn test_wrapped_plus_minus_continuations_consume_auto_semicolon() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'wrapped_plus_minus_continuation',
		'fn add(total int, delta int) int {\n\treturn total\n\t\t+ delta\n}\n\nfn sub(total int, delta int) int {\n\treturn total\n\t\t- delta\n}\n\nfn main() {\n\tprintln(int_str(add(3, 4)))\n\tprintln(int_str(sub(9, 2)))\n}\n')
	assert out == '7\n7'
}

fn test_gated_optional_array_index_materializes_base_before_wrap() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'gated_optional_array_index_base_order',
		"fn get_arr(ok bool) ?[]int {\n\tprintln('get')\n\tif !ok {\n\t\treturn none\n\t}\n\treturn [3, 7, 11]\n}\n\nfn main() {\n\tprintln(int_str(get_arr(true)#[-1] or { 40 }))\n\tprintln(int_str(get_arr(true)#[9] or { 41 }))\n\tprintln(int_str(get_arr(false)#[-1] or { 42 }))\n}\n")
	assert out == 'get\n11\nget\n41\nget\n42'
}

fn test_normalized_option_result_fixed_array_names_keep_outer_wrapper() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'normalized_option_result_fixed_array',
		"struct Foo {\n\tn int\n}\n\nfn opt_values(ok bool) ?[2]int {\n\tif !ok {\n\t\treturn none\n\t}\n\treturn [1, 2]!\n}\n\nfn res_values(ok bool) ![2]Foo {\n\tif !ok {\n\t\treturn error('x')\n\t}\n\treturn [Foo{\n\t\tn: 3\n\t}, Foo{\n\t\tn: 4\n\t}]!\n}\n\nfn main() {\n\ta := opt_values(true) or { [0, 0]! }\n\tb := res_values(true) or { [Foo{\n\t\tn: 0\n\t}, Foo{\n\t\tn: 0\n\t}]! }\n\tmissing_a := opt_values(false) or { [5, 6]! }\n\tmissing_b := res_values(false) or { [Foo{\n\t\tn: 7\n\t}, Foo{\n\t\tn: 8\n\t}]! }\n\tprintln(int_str(a[0] + a[1] + b[0].n + b[1].n))\n\tprintln(int_str(missing_a[0] + missing_a[1] + missing_b[0].n + missing_b[1].n))\n}\n")
	assert out == '10\n26'
}

fn test_hierarchical_import_runtime_inits_before_importer_init() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'hierarchical_runtime_init_order', {
		'main.v':            'module main\n\nimport foo.user\nimport bar as shortbar\n\nfn main() {\n\t_ := shortbar.value()\n\tprintln(int_str(user.value()))\n}\n'
		'foo/user/user.v':   'module user\n\nimport foo.bar as foobar\n\n__global seen int\n\nfn init() {\n\tseen = foobar.value() + 1\n}\n\npub fn value() int {\n\treturn seen\n}\n'
		'foo/bar/bar.v':     'module bar\n\n__global flag = make_flag()\n\nfn make_flag() int {\n\treturn 40\n}\n\npub fn value() int {\n\treturn flag\n}\n'
		'bar/bar.v':         'module bar\n\n__global flag = make_flag()\n\nfn make_flag() int {\n\treturn 3\n}\n\npub fn value() int {\n\treturn flag\n}\n'
		'unrelated/other.v': 'module other\n\npub fn value() int {\n\treturn 0\n}\n'
	}, 'main.v')
	assert out == '41'
}
