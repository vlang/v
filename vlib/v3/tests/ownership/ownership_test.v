module ownership

import os

const ownership_tests_dir = os.dir(@FILE)
const ownership_v3_dir = os.dir(os.dir(ownership_tests_dir))
const ownership_vlib_dir = os.dir(ownership_v3_dir)
const ownership_v3_src = os.join_path(ownership_v3_dir, 'v3.v')
const ownership_vexe = @VEXE

fn ownership_build_v3() string {
	cache_path := os.join_path(os.temp_dir(), 'v3_ownership_test_${os.getpid()}.path')
	if cached := os.read_file(cache_path) {
		cached_bin := cached.trim_space()
		if cached_bin.len > 0 && os.exists(cached_bin) {
			return cached_bin
		}
	}
	v3_bin := os.join_path(os.temp_dir(), 'v3_ownership_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${ownership_vexe} -gc none -d ownership -path "${ownership_vlib_dir}" -o ${v3_bin} ${ownership_v3_src}')
	assert build.exit_code == 0, build.output
	os.write_file(cache_path, v3_bin) or {}
	return v3_bin
}

fn run_ownership_check(v3_bin string, name string, code string) os.Result {
	tmp_dir := os.join_path(os.temp_dir(), 'v3_ownership_${name}_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	src := os.join_path(tmp_dir, 'main.v')
	out := os.join_path(tmp_dir, 'out')
	os.write_file(src, code) or { panic(err) }
	return os.execute('${v3_bin} -ownership -b c -o ${out} ${src} 2>&1')
}

fn run_ownership_check_c_only(v3_bin string, name string, code string) os.Result {
	tmp_dir := os.join_path(os.temp_dir(), 'v3_ownership_${name}_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	src := os.join_path(tmp_dir, 'main.v')
	out := os.join_path(tmp_dir, 'out.c')
	os.write_file(src, code) or { panic(err) }
	return os.execute('${v3_bin} -ownership -b c -o ${out} ${src} 2>&1')
}

fn run_ownership_check_with_module(v3_bin string, name string, main_code string, module_name string, module_code string) os.Result {
	tmp_dir := os.join_path(os.temp_dir(), 'v3_ownership_${name}_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	module_dir := os.join_path(tmp_dir, module_name)
	os.mkdir_all(module_dir) or { panic(err) }
	src := os.join_path(tmp_dir, 'main.v')
	mod_src := os.join_path(module_dir, '${module_name}.v')
	out := os.join_path(tmp_dir, 'out')
	os.write_file(src, main_code) or { panic(err) }
	os.write_file(mod_src, module_code) or { panic(err) }
	return os.execute('${v3_bin} -ownership -b c -o ${out} ${src} 2>&1')
}

fn test_ownership_flag_does_not_define_target_ownership() {
	v3_bin := ownership_build_v3()
	ok := run_ownership_check(v3_bin, 'target_define_not_forwarded', r'
$if ownership ? {
	$compile_error("ownership define leaked into target")
}

fn main() {}
')
	assert ok.exit_code == 0, ok.output
}

fn test_ownership_move_on_assign_and_clone_escape() {
	v3_bin := ownership_build_v3()
	fail := run_ownership_check(v3_bin, 'move_assign', "
fn main() {
	s1 := 'hello'.to_owned()
	s2 := s1
	println(s1)
	_ = s2
}
")
	assert fail.exit_code != 0
	assert fail.output.contains('use of moved value: `s1`'), fail.output
	assert fail.output.contains('value moved to `s2`'), fail.output

	ok := run_ownership_check(v3_bin, 'clone_assign', "
fn main() {
	s1 := 'hello'.to_owned()
	s2 := s1.clone()
	println(s1)
	println(s2)
}
")
	assert ok.exit_code == 0, ok.output

	fail_clone_move := run_ownership_check(v3_bin, 'clone_move', "
fn main() {
	s1 := 'hello'.to_owned()
	s2 := s1.clone()
	s3 := s2
	println(s2)
	println(s1)
	_ = s3
}
")
	assert fail_clone_move.exit_code != 0
	assert fail_clone_move.output.contains('use of moved value: `s2`'), fail_clone_move.output

	fail_clone_owned_temporary := run_ownership_check(v3_bin, 'clone_owned_temporary', "
fn main() {
	a := 'hello'.to_owned().clone()
	b := a
	println(a)
	_ = b
}
")
	assert fail_clone_owned_temporary.exit_code != 0
	assert fail_clone_owned_temporary.output.contains('use of moved value: `a`'), fail_clone_owned_temporary.output

	fail_clone_owned_return_temporary := run_ownership_check(v3_bin,
		'clone_owned_return_temporary', "
fn main() {
	a := make_owned().clone()
	b := a
	println(a)
	_ = b
}

fn make_owned() string {
	return 'hello'.to_owned()
}
")
	assert fail_clone_owned_return_temporary.exit_code != 0
	assert fail_clone_owned_return_temporary.output.contains('use of moved value: `a`'), fail_clone_owned_return_temporary.output

	fail_paren_owned := run_ownership_check(v3_bin, 'paren_to_owned_move', "
fn main() {
	s := ('hello'.to_owned())
	t := s
	println(s)
	_ = t
}
")
	assert fail_paren_owned.exit_code != 0
	assert fail_paren_owned.output.contains('use of moved value: `s`'), fail_paren_owned.output

	fail_if_expr_owned := run_ownership_check(v3_bin, 'if_expr_owned_result', "
fn main() {
	cond := true
	s := 'hello'.to_owned()
	x := if cond { s } else { 'fallback'.to_owned() }
	println(s)
	y := x
	println(x)
	_ = y
}
")
	assert fail_if_expr_owned.exit_code != 0
	assert fail_if_expr_owned.output.contains('use of moved value: `s`'), fail_if_expr_owned.output
	assert fail_if_expr_owned.output.contains('use of moved value: `x`'), fail_if_expr_owned.output

	fail_match_expr_owned := run_ownership_check(v3_bin, 'match_expr_owned_result', "
fn main() {
	n := 0
	s := 'hello'.to_owned()
	x := match n {
		0 { s }
		else { 'fallback'.to_owned() }
	}
	println(s)
	y := x
	println(x)
	_ = y
}
")
	assert fail_match_expr_owned.exit_code != 0
	assert fail_match_expr_owned.output.contains('use of moved value: `s`'), fail_match_expr_owned.output
	assert fail_match_expr_owned.output.contains('use of moved value: `x`'), fail_match_expr_owned.output

	fail_or_fallback_owned := run_ownership_check(v3_bin, 'or_fallback_owned_result', "
fn maybe() ?string {
	return none
}

fn main() {
	s := 'hello'.to_owned()
	x := maybe() or { s }
	println(s)
	y := x
	println(x)
	_ = y
}
")
	assert fail_or_fallback_owned.exit_code != 0
	assert fail_or_fallback_owned.output.contains('use of moved value: `s`'), fail_or_fallback_owned.output
	assert fail_or_fallback_owned.output.contains('use of moved value: `x`'), fail_or_fallback_owned.output

	fail_or_fallback_side_effect := run_ownership_check(v3_bin, 'or_fallback_side_effect_branch', "
fn maybe() ?int {
	return 1
}

fn main() {
	mut s := 'hello'.to_owned()
	x := maybe() or {
		s = 'literal'
		0
	}
	t := s
	println(s)
	_ = x
	_ = t
}
")
	assert fail_or_fallback_side_effect.exit_code != 0
	assert fail_or_fallback_side_effect.output.contains('use of moved value: `s`'), fail_or_fallback_side_effect.output

	ok_plain_to_owned_method := run_ownership_check(v3_bin, 'plain_to_owned_method', '
struct Foo {}

fn (f Foo) to_owned() Foo {
	return f
}

fn main() {
	f := Foo{}
	g := f.to_owned()
	h := g
	println(g)
	_ = h
}
	')
	assert ok_plain_to_owned_method.exit_code == 0, ok_plain_to_owned_method.output

	ok_builtin_string_receiver := run_ownership_check(v3_bin, 'builtin_string_receiver_borrows', "
fn main() {
	s := 'hello'.to_owned()
	println(s.contains('h'))
	println(s.contains('e'))
}
	")
	assert ok_builtin_string_receiver.exit_code == 0, ok_builtin_string_receiver.output

	fail_user_string_receiver := run_ownership_check(v3_bin, 'user_string_receiver_moves', '
fn (s string) consume() {
	_ = s
}

fn main() {
	s := "hello".to_owned()
	s.consume()
	println(s)
}
	')
	assert fail_user_string_receiver.exit_code != 0
	assert fail_user_string_receiver.output.contains('use of moved value: `s`'), fail_user_string_receiver.output

	ok_non_owned_clone_return := run_ownership_check(v3_bin, 'owned_receiver_clone_returns_int', '
struct Resource implements Owned {
	id int
}

fn (r Resource) clone() int {
	return r.id
}

fn main() {
	r := Resource{id: 1}
	n := r.clone()
	m := n
	println(n)
	_ = m
}
	')
	assert ok_non_owned_clone_return.exit_code == 0, ok_non_owned_clone_return.output

	fail_plain_struct_clone := run_ownership_check(v3_bin, 'plain_struct_clone_unknown', '
struct Plain {
	id int
}

fn main() {
	_ := Plain{id: 1}.clone()
}
	')
	assert fail_plain_struct_clone.exit_code != 0
	assert fail_plain_struct_clone.output.contains('unknown function'), fail_plain_struct_clone.output

	ok_iclone_default_clone := run_ownership_check(v3_bin, 'iclone_default_clone', '
struct Resource implements IClone {
	id int
}

fn main() {
	r := Resource{id: 7}
	c := r.clone()
	println(c.id)
}
	')
	assert ok_iclone_default_clone.exit_code == 0, ok_iclone_default_clone.output

	ok_iclone_if_expr_clone := run_ownership_check(v3_bin, 'iclone_if_expr_clone', '
struct Resource implements IClone {
	id int
}

fn pick(cond bool, left Resource, right Resource) Resource {
	res := if cond {
		left.clone()
	} else {
		right.clone()
	}
	return res
}

fn main() {
	left := Resource{id: 7}
	right := Resource{id: 9}
	res := pick(true, left, right)
	println(res.id)
}
	')
	assert ok_iclone_if_expr_clone.exit_code == 0, ok_iclone_if_expr_clone.output

	fail_inferred_owned_global := run_ownership_check(v3_bin, 'inferred_owned_global', '
struct Resource implements Owned {
	id int
}

__global g = Resource{id: 1}

fn main() {
	x := g
	y := g
	_ = x
	_ = y
}
')
	assert fail_inferred_owned_global.exit_code != 0
	assert fail_inferred_owned_global.output.contains('cannot move owned global `g`'), fail_inferred_owned_global.output

	fail_owned_global_value_receiver := run_ownership_check(v3_bin, 'owned_global_value_receiver', '
struct Resource implements Owned {
	id int
}

fn (r Resource) consume() {
	_ = r
}

__global g = Resource{id: 1}

fn main() {
	g.consume()
}
')
	assert fail_owned_global_value_receiver.exit_code != 0
	assert fail_owned_global_value_receiver.output.contains('cannot move owned global `g`'), fail_owned_global_value_receiver.output

	fail_forward_owned_global := run_ownership_check(v3_bin, 'forward_owned_global', '
__global g Resource

struct Resource implements Owned {
	id int
}

fn main() {
	x := g
	y := g
	_ = x
	_ = y
}
')
	assert fail_forward_owned_global.exit_code != 0
	assert fail_forward_owned_global.output.contains('cannot move owned global `g`'), fail_forward_owned_global.output

	fail_to_owned_global := run_ownership_check(v3_bin, 'to_owned_global', '
__global g = "global".to_owned()

fn main() {
	x := g
	y := g
	_ = x
	_ = y
}
')
	assert fail_to_owned_global.exit_code != 0
	assert fail_to_owned_global.output.contains('cannot move owned global `g`'), fail_to_owned_global.output

	fail_owned_return_global := run_ownership_check(v3_bin, 'owned_return_global', '
__global g = make()

fn make() string {
	return "global".to_owned()
}

fn main() {
	x := g
	y := g
	_ = x
	_ = y
}
')
	assert fail_owned_return_global.exit_code != 0
	assert fail_owned_return_global.output.contains('cannot move owned global `g`'), fail_owned_return_global.output

	fail_module_owned_return_global := run_ownership_check_with_module(v3_bin,
		'module_owned_return_global', '
import foo

fn main() {
	x := foo.s
	y := foo.s
	_ = x
	_ = y
}
',
		'foo', '
module foo

pub __global s = make_owned()

fn make_owned() string {
	return "global".to_owned()
}
')
	assert fail_module_owned_return_global.exit_code != 0
	assert fail_module_owned_return_global.output.contains('cannot move owned global `foo.s`'), fail_module_owned_return_global.output

	fail_global_return_descendant := run_ownership_check(v3_bin,
		'global_call_return_owned_descendant', '
struct Holder {
	value string
}

__global h = make_holder()

fn make_holder() Holder {
	return Holder{value: "global".to_owned()}
}

fn main() {
	x := h.value
	y := h.value
	_ = x
	_ = y
}
')
	assert fail_global_return_descendant.exit_code != 0
	assert fail_global_return_descendant.output.contains('cannot move owned global `h.value`'), fail_global_return_descendant.output

	fail_global_aggregate_owned_descendant := run_ownership_check(v3_bin,
		'global_aggregate_owned_descendant_move', '
struct Holder {
	value string
}

__global h = Holder{value: "global".to_owned()}

fn main() {
	x := h
	y := x.value
	println(x.value)
	_ = y
}
')
	assert fail_global_aggregate_owned_descendant.exit_code != 0
	assert fail_global_aggregate_owned_descendant.output.contains('cannot move owned global `h`'), fail_global_aggregate_owned_descendant.output

	fail_global_array_owned_entry := run_ownership_check(v3_bin, 'global_array_owned_entry', '
__global g = ["global".to_owned()]

fn main() {
	x := g[0]
	y := g[0]
	_ = x
	_ = y
}
')
	assert fail_global_array_owned_entry.exit_code != 0
	assert fail_global_array_owned_entry.output.contains('cannot move owned global `g[0]`'), fail_global_array_owned_entry.output

	fail_global_assoc_owned_field := run_ownership_check(v3_bin, 'global_assoc_owned_field', '
struct Holder {
	value string
}

__global g = Holder{...Holder{}, value: "global".to_owned()}

fn main() {
	x := g.value
	y := g.value
	_ = x
	_ = y
}
')
	assert fail_global_assoc_owned_field.exit_code != 0
	assert fail_global_assoc_owned_field.output.contains('cannot move owned global `g.value`'), fail_global_assoc_owned_field.output

	fail_module_global_assoc_base := run_ownership_check_with_module(v3_bin,
		'module_global_assoc_base_owned_descendant', '
import foo

fn main() {
	x := foo.copy.value
	y := foo.copy.value
	_ = x
	_ = y
}
',
		'foo', '
module foo

pub struct Holder {
pub:
	value string
}

pub __global base = Holder{value: "global".to_owned()}
pub __global copy = Holder{...base}
')
	assert fail_module_global_assoc_base.exit_code != 0
	assert fail_module_global_assoc_base.output.contains('cannot move owned global `foo.copy.value`'), fail_module_global_assoc_base.output

	fail_qualified_owned_global := run_ownership_check_with_module(v3_bin,
		'qualified_owned_global', '
import foo

fn main() {
	x := foo.g
	y := foo.g
	_ = x
	_ = y
}
',
		'foo', '
module foo

pub struct Resource implements Owned {
	id int
}

pub __global g = Resource{id: 1}
')
	assert fail_qualified_owned_global.exit_code != 0
	assert fail_qualified_owned_global.output.contains('cannot move owned global `foo.g`'), fail_qualified_owned_global.output

	fail_aliased_owned_global := run_ownership_check_with_module(v3_bin,
		'aliased_qualified_owned_global', '
import foo as f

fn main() {
	x := f.g
	y := f.g
	_ = x
	_ = y
}
',
		'foo', '
module foo

pub struct Resource implements Owned {
	id int
}

pub __global g = Resource{id: 1}
')
	assert fail_aliased_owned_global.exit_code != 0
	assert fail_aliased_owned_global.output.contains('cannot move owned global `foo.g`'), fail_aliased_owned_global.output
}

fn test_ownership_plain_reassignment_clears_owned_state() {
	v3_bin := ownership_build_v3()
	ok := run_ownership_check(v3_bin, 'plain_reassign', "
fn main() {
	mut s := 'hello'.to_owned()
	s = 'literal'
	t := s
	println(s)
	_ = t
}
")
	assert ok.exit_code == 0, ok.output

	fail_zero_loop_preserves_owned := run_ownership_check(v3_bin, 'zero_loop_preserves_owned', "
fn main() {
	mut s := 'hello'.to_owned()
	for false {
		s = 'literal'
	}
	t := s
	println(s)
	_ = t
}
")
	assert fail_zero_loop_preserves_owned.exit_code != 0
	assert fail_zero_loop_preserves_owned.output.contains('use of moved value: `s`'), fail_zero_loop_preserves_owned.output

	fail_c_loop_post_after_body := run_ownership_check(v3_bin, 'c_loop_post_after_body', "
fn main() {
	mut s := 'hello'.to_owned()
	for i := 0; i < 1; s = 'literal' {
		t := s
		println(s)
		_ = t
		break
	}
}
")
	assert fail_c_loop_post_after_body.exit_code != 0
	assert fail_c_loop_post_after_body.output.contains('use of moved value: `s`'), fail_c_loop_post_after_body.output

	fail_empty_for_in_preserves_owned := run_ownership_check(v3_bin,
		'empty_for_in_preserves_owned', "
fn main() {
	mut s := 'hello'.to_owned()
	for _ in []int{} {
		s = 'literal'
	}
	t := s
	println(s)
	_ = t
}
")
	assert fail_empty_for_in_preserves_owned.exit_code != 0
	assert fail_empty_for_in_preserves_owned.output.contains('use of moved value: `s`'), fail_empty_for_in_preserves_owned.output

	fail_compound_preserves_owned := run_ownership_check(v3_bin, 'compound_assign_preserves_owned', "
fn main() {
	mut s := 'hello'.to_owned()
	s += ' world'
	t := s
	println(s)
	_ = t
}
")
	assert fail_compound_preserves_owned.exit_code != 0
	assert fail_compound_preserves_owned.output.contains('use of moved value: `s`'), fail_compound_preserves_owned.output

	fail_compound_checks_moved_lhs := run_ownership_check(v3_bin,
		'compound_assign_checks_moved_lhs', "
fn main() {
	mut s := 'hello'.to_owned()
	t := s
	s += ' world'
	_ = t
}
")
	assert fail_compound_checks_moved_lhs.exit_code != 0
	assert fail_compound_checks_moved_lhs.output.contains('use of moved value: `s`'), fail_compound_checks_moved_lhs.output

	fail_defer_assign_does_not_clear_early := run_ownership_check(v3_bin,
		'defer_assign_does_not_clear_early', "
fn main() {
	mut s := 'hello'.to_owned()
	defer {
		s = 'literal'
	}
	t := s
	println(s)
	_ = t
}
")
	assert fail_defer_assign_does_not_clear_early.exit_code != 0
	assert fail_defer_assign_does_not_clear_early.output.contains('use of moved value: `s`'), fail_defer_assign_does_not_clear_early.output

	fail_defer_read_sees_exit_state := run_ownership_check(v3_bin, 'defer_read_sees_exit_state', "
fn main() {
	s := 'hello'.to_owned()
	defer {
		println(s)
	}
	t := s
	_ = t
}
")
	assert fail_defer_read_sees_exit_state.exit_code != 0
	assert fail_defer_read_sees_exit_state.output.contains('use of moved value: `s`'), fail_defer_read_sees_exit_state.output

	ok_defer_read_after_reassign_at_exit := run_ownership_check(v3_bin,
		'defer_read_after_reassign_at_exit', "
fn main() {
	mut s := 'hello'.to_owned()
	t := s
	defer {
		println(s)
	}
	s = 'literal'
	_ = t
}
")
	assert ok_defer_read_after_reassign_at_exit.exit_code == 0, ok_defer_read_after_reassign_at_exit.output

	fail_defer_read_on_return_path := run_ownership_check(v3_bin, 'defer_read_on_return_path', "
fn main() {
	cond := true
	mut s := 'hello'.to_owned()
	defer {
		println(s)
	}
	if cond {
		t := s
		_ = t
		return
	}
	s = 'literal'
}
")
	assert fail_defer_read_on_return_path.exit_code != 0
	assert fail_defer_read_on_return_path.output.contains('use of moved value: `s`'), fail_defer_read_on_return_path.output

	fail_defer_after_return_move := run_ownership_check(v3_bin, 'defer_after_return_move', "
fn take(s string) {
	_ = s
}

fn make() string {
	s := 'hello'.to_owned()
	defer {
		take(s)
	}
	return s
}

fn main() {
	_ = make()
}
")
	assert fail_defer_after_return_move.exit_code != 0
	assert fail_defer_after_return_move.output.contains('use of moved value: `s`'), fail_defer_after_return_move.output

	ok_function_defer_return_branch := run_ownership_check(v3_bin,
		'function_defer_return_branch_does_not_leak', "
fn main() {
	cond := false
	s := 'hello'.to_owned()
	if cond {
		defer(fn) {
			println(s)
		}
		return
	}
	t := s
	_ = t
}
")
	assert ok_function_defer_return_branch.exit_code == 0, ok_function_defer_return_branch.output

	fail_function_defer_continuing_branch := run_ownership_check(v3_bin,
		'function_defer_continuing_branch_is_merged', "
fn main() {
	cond := true
	s := 'hello'.to_owned()
	if cond {
		defer(fn) {
			println(s)
		}
	}
	t := s
	_ = t
}
")
	assert fail_function_defer_continuing_branch.exit_code != 0
	assert fail_function_defer_continuing_branch.output.contains('use of moved value: `s`'), fail_function_defer_continuing_branch.output

	ok_function_defer_loop_return := run_ownership_check(v3_bin,
		'function_defer_loop_return_does_not_leak', "
fn main() {
	cond := false
	s := 'hello'.to_owned()
	for cond {
		defer(fn) {
			println(s)
		}
		return
	}
	t := s
	_ = t
}
")
	assert ok_function_defer_loop_return.exit_code == 0, ok_function_defer_loop_return.output

	fail_borrow_then_move := run_ownership_check(v3_bin, 'explicit_borrow_then_move_call', "
fn foo(a &string, b string) {
	_ = a
	_ = b
}

fn main() {
	s := 'hello'.to_owned()
	foo(&s, s)
}
")
	assert fail_borrow_then_move.exit_code != 0
	assert fail_borrow_then_move.output.contains('cannot move `s` because it is borrowed'), fail_borrow_then_move.output

	fail_move_then_borrow := run_ownership_check(v3_bin, 'move_then_explicit_borrow_call', "
fn foo(a string, b &string) {
	_ = a
	_ = b
}

fn main() {
	s := 'hello'.to_owned()
	foo(s, &s)
}
")
	assert fail_move_then_borrow.exit_code != 0
	assert fail_move_then_borrow.output.contains('use of moved value: `s`'), fail_move_then_borrow.output

	fail_blank_assign := run_ownership_check(v3_bin, 'blank_assign_consumes_owned', "
fn main() {
	s := 'hello'.to_owned()
	_ = s
	println(s)
}
")
	assert fail_blank_assign.exit_code != 0
	assert fail_blank_assign.output.contains('use of moved value: `s`'), fail_blank_assign.output

	fail_blank_decl := run_ownership_check(v3_bin, 'blank_decl_consumes_owned', "
fn main() {
	s := 'hello'.to_owned()
	_ := s
	println(s)
}
")
	assert fail_blank_decl.exit_code != 0
	assert fail_blank_decl.output.contains('use of moved value: `s`'), fail_blank_decl.output

	fail_conditional_blank_sink := run_ownership_check(v3_bin, 'conditional_blank_sink_move', "
fn main() {
	cond := true
	s := 'hello'.to_owned()
	_ = if cond { s } else { '' }
	println(s)
}
")
	assert fail_conditional_blank_sink.exit_code != 0
	assert fail_conditional_blank_sink.output.contains('use of moved value: `s`'), fail_conditional_blank_sink.output

	fail_conditional_blank_aggregate_sink := run_ownership_check(v3_bin,
		'conditional_blank_aggregate_sink_move', '
struct Holder {
	value string
}

fn main() {
	cond := true
	h := Holder{value: "hello".to_owned()}
	_ = if cond { h } else { Holder{value: "fallback".to_owned()} }
	t := h.value
	_ = t
}
')
	assert fail_conditional_blank_aggregate_sink.exit_code != 0
	assert fail_conditional_blank_aggregate_sink.output.contains('use of moved value: `h.value`'), fail_conditional_blank_aggregate_sink.output

	fail_blank_aggregate_sink := run_ownership_check(v3_bin, 'blank_aggregate_sink_move', '
struct Holder {
	value string
}

fn main() {
	h := Holder{value: "hello".to_owned()}
	_ = h
	t := h.value
	_ = t
}
')
	assert fail_blank_aggregate_sink.exit_code != 0
	assert fail_blank_aggregate_sink.output.contains('use of moved value: `h.value`'), fail_blank_aggregate_sink.output

	fail_conditional_assign_aggregate := run_ownership_check(v3_bin,
		'conditional_assign_aggregate_descendant', '
struct Holder {
	value string
}

fn main() {
	cond := true
	h := Holder{value: "hello".to_owned()}
	out := if cond { h } else { Holder{value: "fallback".to_owned()} }
	t := out.value
	println(out.value)
	_ = t
}
')
	assert fail_conditional_assign_aggregate.exit_code != 0
	assert fail_conditional_assign_aggregate.output.contains('use of moved value: `out.value`'), fail_conditional_assign_aggregate.output

	fail_conditional_channel_send := run_ownership_check(v3_bin, 'conditional_channel_send_move', "
fn main() {
	cond := true
	ch := chan string{cap: 1}
	s := 'hello'.to_owned()
	ch <- if cond { s } else { '' }
	println(s)
}
")
	assert fail_conditional_channel_send.exit_code != 0
	assert fail_conditional_channel_send.output.contains('use of moved value: `s`'), fail_conditional_channel_send.output

	fail_channel_aggregate_send := run_ownership_check(v3_bin, 'channel_aggregate_send_move', '
struct Holder {
	value string
}

fn main() {
	ch := chan Holder{cap: 1}
	h := Holder{value: "hello".to_owned()}
	ch <- h
	t := h.value
	_ = t
}
')
	assert fail_channel_aggregate_send.exit_code != 0
	assert fail_channel_aggregate_send.output.contains('use of moved value: `h.value`'), fail_channel_aggregate_send.output
}

fn test_ownership_fn_literal_uses_isolated_frame() {
	v3_bin := ownership_build_v3()
	ok_shadow_param := run_ownership_check(v3_bin, 'fn_literal_shadow_param_return', "
fn main() {
	s := 'hello'.to_owned()
	f := fn (s string) string {
		return s
	}
	println(s)
	_ = f
}
	")
	assert ok_shadow_param.exit_code == 0, ok_shadow_param.output

	fail_captured_owned := run_ownership_check(v3_bin, 'fn_literal_capture_moves_owned', "
fn take(s string) {
	_ = s
}

fn run(f fn ()) {
	f()
}

fn main() {
	s := 'hello'.to_owned()
	run(fn [s] () {
		take(s)
	})
	println(s)
}
")
	assert fail_captured_owned.exit_code != 0
	assert fail_captured_owned.output.contains('use of moved value: `s`'), fail_captured_owned.output

	fail_owned_fn_literal_param := run_ownership_check(v3_bin, 'fn_literal_owned_param_from_call', "
fn take(s string) {
	_ = s
}

fn main() {
	f := fn (s string) {
		take(s)
		println(s)
	}
	f('hello'.to_owned())
}
")
	assert fail_owned_fn_literal_param.exit_code != 0
	assert fail_owned_fn_literal_param.output.contains('use of moved value: `s`'), fail_owned_fn_literal_param.output

	ok_lambda_shadow_param := run_ownership_check_c_only(v3_bin, 'lambda_shadow_param_call', "
fn take(s string) {
	_ = s
}

fn keep(f fn (string)) {
	_ = f
}

fn main() {
	s := 'hello'.to_owned()
	keep(|s| take(s))
	println(s)
}
")
	assert ok_lambda_shadow_param.exit_code == 0, ok_lambda_shadow_param.output

	ok_lambda_if_guard_shadow := run_ownership_check_c_only(v3_bin, 'lambda_if_guard_shadow', "
fn take(s string) {
	_ = s
}

fn keep(f fn (int)) {
	_ = f
}

fn maybe() ?string {
	return 'inner'
}

fn main() {
	s := 'outer'.to_owned()
	keep(|_| if s := maybe() {
		take(s)
	})
	println(s)
}
")
	assert ok_lambda_if_guard_shadow.exit_code == 0, ok_lambda_if_guard_shadow.output

	ok_lambda_match_branch_shadow := run_ownership_check_c_only(v3_bin,
		'lambda_match_branch_shadow', "
fn keep(f fn ()) {
	_ = f
}

fn main() {
	s := 'outer'.to_owned()
	keep(|| {
		match 0 {
			0 {
				s := 'inner'.to_owned()
				println(s)
			}
			else {}
		}
	})
	println(s)
}
")
	assert ok_lambda_match_branch_shadow.exit_code == 0, ok_lambda_match_branch_shadow.output
}

fn test_ownership_shadowed_owned_local_does_not_poison_outer() {
	v3_bin := ownership_build_v3()
	ok := run_ownership_check(v3_bin, 'shadow_owned_local', "
fn main() {
	cond := true
	s := 'outer'.to_owned()
	if cond {
		s := 'inner'.to_owned()
		t := s
		println(t)
	}
	println(s)
}
")
	assert ok.exit_code == 0, ok.output

	ok_guard := run_ownership_check(v3_bin, 'if_guard_shadow_owned_local', "
fn maybe_string() ?string {
	return 'inner'
}

fn main() {
	s := 'outer'.to_owned()
	if s := maybe_string() {
		t := s
		println(t)
	}
	println(s)
}
	")
	assert ok_guard.exit_code == 0, ok_guard.output

	fail_guard_outer_owned := run_ownership_check(v3_bin, 'if_guard_shadow_preserves_outer_owned', "
fn maybe_string() ?string {
	return 'inner'
}

fn main() {
	s := 'outer'.to_owned()
	if s := maybe_string() {
		println(s)
	}
	t := s
	println(s)
	_ = t
}
")
	assert fail_guard_outer_owned.exit_code != 0
	assert fail_guard_outer_owned.output.contains('use of moved value: `s`'), fail_guard_outer_owned.output

	fail_guard_owned_option_source := run_ownership_check(v3_bin,
		'if_guard_moves_owned_option_source', '
struct Resource implements Owned {
	id int
}

fn maybe_resource() ?Resource {
	return Resource{id: 1}
}

fn main() {
	opt := maybe_resource()
	if r := opt {
		t := r
		_ = t
	}
	u := opt
	_ = u
}
')
	assert fail_guard_owned_option_source.exit_code != 0
	assert fail_guard_owned_option_source.output.contains('use of moved value: `opt`'), fail_guard_owned_option_source.output
}

fn test_ownership_reassigned_reference_releases_old_borrow() {
	v3_bin := ownership_build_v3()
	ok := run_ownership_check(v3_bin, 'reassign_ref_releases_borrow', "
fn main() {
	s := 's'.to_owned()
	t := 't'.to_owned()
	mut r := &s
	r = &t
	u := s
	println(u)
	_ = r
}
")
	assert ok.exit_code == 0, ok.output

	ok_block := run_ownership_check(v3_bin, 'block_borrow_released', "
fn main() {
	s := 'hello'.to_owned()
	{
		r := &s
		_ = r
	}
	u := s
	println(u)
}
")
	assert ok_block.exit_code == 0, ok_block.output

	fail_borrow_before_owned := run_ownership_check(v3_bin, 'borrow_before_owned_reassign', "
fn main() {
	mut s := ''
	r := &s
	s = 'hello'.to_owned()
	t := s
	_ = r
	_ = t
}
")
	assert fail_borrow_before_owned.exit_code != 0
	assert fail_borrow_before_owned.output.contains('cannot assign to `s` because it is borrowed by `r`'), fail_borrow_before_owned.output

	fail_paren_borrow := run_ownership_check(v3_bin, 'parenthesized_borrow_then_move', "
fn main() {
	s := 'hello'.to_owned()
	r := (&s)
	t := s
	_ = r
	_ = t
}
")
	assert fail_paren_borrow.exit_code != 0
	assert fail_paren_borrow.output.contains('cannot move `s` because it is borrowed by `r`'), fail_paren_borrow.output

	fail_array_stored_borrow := run_ownership_check(v3_bin, 'array_stored_borrow_then_move', "
fn main() {
	s := 'hello'.to_owned()
	arr := [&s]
	t := s
	_ = arr
	_ = t
}
")
	assert fail_array_stored_borrow.exit_code != 0
	assert fail_array_stored_borrow.output.contains('cannot move `s` because it is borrowed by `arr[0]`'), fail_array_stored_borrow.output

	fail_struct_stored_borrow := run_ownership_check(v3_bin, 'struct_stored_borrow_then_move', "
struct Holder {
	p &string
}

fn main() {
	s := 'hello'.to_owned()
	h := Holder{p: &s}
	t := s
	_ = h
	_ = t
}
")
	assert fail_struct_stored_borrow.exit_code != 0
	assert fail_struct_stored_borrow.output.contains('cannot move `s` because it is borrowed by `h.p`'), fail_struct_stored_borrow.output

	fail_alias := run_ownership_check(v3_bin, 'reference_alias_keeps_borrow', "
fn main() {
	s := 's'.to_owned()
	other := 'other'.to_owned()
	mut r := &s
	r2 := r
	r = &other
	u := s
	println(u)
	_ = r2
}
")
	assert fail_alias.exit_code != 0
	assert fail_alias.output.contains('cannot move `s` because it is borrowed by `r2`'), fail_alias.output
}

fn test_ownership_move_on_call_and_borrowed_call_ok() {
	v3_bin := ownership_build_v3()
	fail := run_ownership_check(v3_bin, 'move_call', "
fn take(s string) {
	_ = s
}

fn main() {
	s := 'hello'.to_owned()
	take(s)
	println(s)
}
")
	assert fail.exit_code != 0
	assert fail.output.contains('use of moved value: `s`'), fail.output
	assert fail.output.contains('value moved into function `take`'), fail.output

	ok := run_ownership_check(v3_bin, 'borrow_call', "
fn borrow(s &string) {
	_ = s
}

fn main() {
	s := 'hello'.to_owned()
	borrow(&s)
	println(s)
}
")
	assert ok.exit_code == 0, ok.output

	fail_returned_borrow := run_ownership_check(v3_bin, 'returned_borrow_keeps_alias', "
fn id(s &string) &string {
	return s
}

fn main() {
	s := 'hello'.to_owned()
	r := id(&s)
	t := s
	println(r)
	_ = t
}
")
	assert fail_returned_borrow.exit_code != 0
	assert fail_returned_borrow.output.contains('cannot move `s` because it is borrowed by `r`'), fail_returned_borrow.output

	fail_mut_borrow := run_ownership_check(v3_bin, 'mut_param_borrow_twice', "
fn both(mut a string, mut b string) {
	_ = a
	_ = b
}

fn main() {
	mut s := 'hello'.to_owned()
	both(mut s, mut s)
}
")
	assert fail_mut_borrow.exit_code != 0
	assert fail_mut_borrow.output.contains('cannot borrow `s` as mutable more than once'), fail_mut_borrow.output

	fail_imported_mut_receiver := run_ownership_check_with_module(v3_bin,
		'imported_method_mut_receiver', '
import foo

fn main() {
	mut r := foo.Resource{id: 1}
	r.use(&r)
}
',
		'foo', '
module foo

pub struct Resource implements Owned {
pub:
	id int
}

pub fn (mut r Resource) use(other &Resource) {
	_ = other
}
')
	assert fail_imported_mut_receiver.exit_code != 0
	assert fail_imported_mut_receiver.output.contains('cannot borrow `r` as immutable because it is borrowed as mutable'), fail_imported_mut_receiver.output

	fail_method_value_receiver := run_ownership_check(v3_bin, 'method_value_receiver_moves', '
struct Resource implements Owned {
	id int
}

fn (r Resource) consume() {
	_ = r
}

fn run(f fn ()) {
	f()
}

fn main() {
	r := Resource{id: 1}
	run(r.consume)
	println(r)
}
')
	assert fail_method_value_receiver.exit_code != 0
	assert fail_method_value_receiver.output.contains('use of moved value: `r`'), fail_method_value_receiver.output

	fail_conditional_value_receiver := run_ownership_check(v3_bin,
		'conditional_value_receiver_moves', '
struct Resource implements Owned {
	id int
}

fn (r Resource) consume() {
	_ = r
}

fn main() {
	cond := true
	r := Resource{id: 1}
	(if cond { r } else { Resource{id: 2} }).consume()
	println(r)
}
')
	assert fail_conditional_value_receiver.exit_code != 0
	assert fail_conditional_value_receiver.output.contains('use of moved value: `r`'), fail_conditional_value_receiver.output
}

fn test_ownership_duplicate_moves_in_one_expression() {
	v3_bin := ownership_build_v3()
	fail_call := run_ownership_check(v3_bin, 'duplicate_call_move', "
fn take(a string, b string) {
	println(a)
	println(b)
}

fn main() {
	s := 'hello'.to_owned()
	take(s, s)
}
")
	assert fail_call.exit_code != 0
	assert fail_call.output.contains('use of moved value: `s`'), fail_call.output

	fail_array := run_ownership_check(v3_bin, 'duplicate_array_move', "
fn main() {
	s := 'hello'.to_owned()
	arr := [s, s]
	println(arr.len)
}
")
	assert fail_array.exit_code != 0
	assert fail_array.output.contains('use of moved value: `s`'), fail_array.output
}

fn test_ownership_returning_owned_value_and_param() {
	v3_bin := ownership_build_v3()
	fail_owned := run_ownership_check(v3_bin, 'return_owned', "
fn make() string {
	return 'hello'.to_owned()
}

fn main() {
	s1 := make()
	s2 := s1
	println(s1)
	_ = s2
}
")
	assert fail_owned.exit_code != 0
	assert fail_owned.output.contains('use of moved value: `s1`'), fail_owned.output

	ok_local_return := run_ownership_check(v3_bin, 'return_local_owned', "
fn make() string {
	s := 'hello'.to_owned()
	return s
}

fn main() {
	s := make()
	println(s)
}
")
	assert ok_local_return.exit_code == 0, ok_local_return.output

	fail_param := run_ownership_check(v3_bin, 'return_param', "
fn pass(s string) string {
	return s
}

fn main() {
	s1 := 'hello'.to_owned()
	s2 := pass(s1)
	s3 := s2
	println(s2)
	_ = s3
}
")
	assert fail_param.exit_code != 0
	assert fail_param.output.contains('use of moved value: `s2`'), fail_param.output

	fail_param_expr_arg := run_ownership_check(v3_bin, 'return_param_expr_arg', "
fn main() {
	x := pass('hello'.to_owned())
	y := x
	println(x)
	_ = y
}

fn pass(s string) string {
	return s
}
")
	assert fail_param_expr_arg.exit_code != 0
	assert fail_param_expr_arg.output.contains('use of moved value: `x`'), fail_param_expr_arg.output

	ok_param_literal_after_owned_call := run_ownership_check(v3_bin,
		'return_param_literal_after_owned_call', "
fn main() {
	owned := 'hello'.to_owned()
	x := pass(owned)
	println(x)
	y := pass('literal')
	z := y
	println(y)
	_ = z
}

fn pass(s string) string {
	return s
}
	")
	assert ok_param_literal_after_owned_call.exit_code == 0, ok_param_literal_after_owned_call.output

	ok_nested_fn_literal_return := run_ownership_check(v3_bin, 'nested_fn_literal_return_ignored', "
fn run(f fn () string) {
	_ = f
}

fn outer() string {
	run(fn () string {
		return 'hello'.to_owned()
	})
	return ''
}

fn main() {
	s := outer()
	t := s
	println(s)
	_ = t
}
")
	assert ok_nested_fn_literal_return.exit_code == 0, ok_nested_fn_literal_return.output

	fail_method_param := run_ownership_check(v3_bin, 'return_method_param', "
struct Helper {}

fn (h Helper) pass(s string) string {
	_ = h
	return s
}

fn main() {
	h := Helper{}
	s1 := 'hello'.to_owned()
	s2 := h.pass(s1)
	s3 := s2
	println(s2)
	_ = s3
}
")
	assert fail_method_param.exit_code != 0
	assert fail_method_param.output.contains('use of moved value: `s2`'), fail_method_param.output

	fail_choose_param := run_ownership_check(v3_bin, 'return_any_param', "
fn choose(cond bool, a string, b string) string {
	if cond {
		return a
	}
	return b
}

fn main() {
	cond := true
	owned := 'hello'.to_owned()
	s1 := choose(cond, owned, '')
	s2 := s1
	println(s1)
	_ = s2
}
")
	assert fail_choose_param.exit_code != 0
	assert fail_choose_param.output.contains('use of moved value: `s1`'), fail_choose_param.output

	fail_wrapper_param := run_ownership_check(v3_bin, 'return_param_wrapper', "
fn wrap(s string) string {
	return pass(s)
}

fn main() {
	owned := 'hello'.to_owned()
	x := wrap(owned)
	y := x
	println(x)
	_ = y
}

fn pass(s string) string {
	return s
}
")
	assert fail_wrapper_param.exit_code != 0
	assert fail_wrapper_param.output.contains('use of moved value: `x`'), fail_wrapper_param.output

	fail_return_field_literal_arg := run_ownership_check(v3_bin, 'return_field_literal_arg', "
struct Holder {
	value string
}

fn get(h Holder) string {
	return h.value
}

fn main() {
	s := 'field'.to_owned()
	x := get(Holder{value: s})
	y := x
	println(x)
	_ = y
}
")
	assert fail_return_field_literal_arg.exit_code != 0
	assert fail_return_field_literal_arg.output.contains('use of moved value: `x`'), fail_return_field_literal_arg.output

	fail_return_field_call_arg := run_ownership_check(v3_bin, 'return_field_call_arg', "
struct Holder {
	value string
}

fn make(s string) Holder {
	return Holder{value: s}
}

fn get(h Holder) string {
	return h.value
}

fn main() {
	s := 'field'.to_owned()
	x := get(make(s))
	y := x
	println(x)
	_ = y
}
")
	assert fail_return_field_call_arg.exit_code != 0
	assert fail_return_field_call_arg.output.contains('use of moved value: `x`'), fail_return_field_call_arg.output

	fail_return_field_value_receiver := run_ownership_check(v3_bin, 'return_field_value_receiver', "
struct Holder {
	value string
}

fn (h Holder) get() string {
	return h.value
}

fn main() {
	s := 'field'.to_owned()
	x := Holder{value: s}.get()
	y := x
	println(x)
	_ = y
}
")
	assert fail_return_field_value_receiver.exit_code != 0
	assert fail_return_field_value_receiver.output.contains('use of moved value: `x`'), fail_return_field_value_receiver.output

	fail_variadic_return_elem := run_ownership_check(v3_bin, 'return_variadic_elem_arg', "
fn main() {
	owned := 'field'.to_owned()
	x := pick('', owned)
	y := x
	println(x)
	_ = y
}

fn pick(xs ...string) string {
	return xs[1]
}
")
	assert fail_variadic_return_elem.exit_code != 0
	assert fail_variadic_return_elem.output.contains('use of moved value: `x`'), fail_variadic_return_elem.output

	fail_params_field_return := run_ownership_check(v3_bin, 'return_params_field_arg', "
@[params]
struct Cfg {
	s string
}

fn main() {
	owned := 'field'.to_owned()
	x := get(s: owned)
	y := x
	println(x)
	_ = y
}

fn get(cfg Cfg) string {
	return cfg.s
}
")
	assert fail_params_field_return.exit_code != 0
	assert fail_params_field_return.output.contains('use of moved value: `x`'), fail_params_field_return.output

	fail_multi_return := run_ownership_check(v3_bin, 'multi_return_owned_slot', "
fn make() (string, int) {
	return 'hello'.to_owned(), 0
}

fn main() {
	s, _ := make()
	t := s
	println(s)
	_ = t
}
")
	assert fail_multi_return.exit_code != 0
	assert fail_multi_return.output.contains('use of moved value: `s`'), fail_multi_return.output

	fail_multi_return_param := run_ownership_check(v3_bin, 'multi_return_param_slot', "
fn main() {
	owned := 'hello'.to_owned()
	s1, _ := pass(owned)
	s2 := s1
	println(s1)
	_ = s2
}

fn pass(s string) (string, int) {
	return s, 0
}
")
	assert fail_multi_return_param.exit_code != 0
	assert fail_multi_return_param.output.contains('use of moved value: `s1`'), fail_multi_return_param.output

	fail_multi_return_wrapper := run_ownership_check(v3_bin, 'multi_return_wrapper_slot', "
fn main() {
	_, s := wrap()
	t := s
	println(s)
	_ = t
}

fn wrap() (int, string) {
	return make()
}

fn make() (int, string) {
	return 0, 'hello'.to_owned()
}
")
	assert fail_multi_return_wrapper.exit_code != 0
	assert fail_multi_return_wrapper.output.contains('use of moved value: `s`'), fail_multi_return_wrapper.output

	fail_multi_return_reassign := run_ownership_check(v3_bin, 'multi_return_reassign_owned_slot', "
fn make() (string, int) {
	return 'hello'.to_owned(), 0
}

fn main() {
	mut s := ''
	s, _ = make()
	t := s
	println(s)
	_ = t
}
")
	assert fail_multi_return_reassign.exit_code != 0
	assert fail_multi_return_reassign.output.contains('use of moved value: `s`'), fail_multi_return_reassign.output

	ok_multi_return_reassign_plain := run_ownership_check(v3_bin,
		'multi_return_reassign_plain_slot', "
fn make() (string, int) {
	return 'literal', 0
}

fn main() {
	mut s := 'hello'.to_owned()
	s, _ = make()
	t := s
	println(s)
	_ = t
}
")
	assert ok_multi_return_reassign_plain.exit_code == 0, ok_multi_return_reassign_plain.output

	ok_owned_swap := run_ownership_check(v3_bin, 'owned_swap_multi_assign', "
fn main() {
	mut a := 'left'.to_owned()
	mut b := 'right'.to_owned()
	a, b = b, a
	x := a
	y := b
	_ = x
	_ = y
}
")
	assert ok_owned_swap.exit_code == 0, ok_owned_swap.output

	fail_clone_return_order := run_ownership_check(v3_bin, 'clone_return_before_decl', "
fn main() {
	s1 := make()
	s2 := s1
	println(s1)
	_ = s2
}

fn make() string {
	s := 'hello'.to_owned()
	return s.clone()
}
")
	assert fail_clone_return_order.exit_code != 0
	assert fail_clone_return_order.output.contains('use of moved value: `s1`'), fail_clone_return_order.output

	fail_conditional_return := run_ownership_check(v3_bin, 'conditional_return_owned', "
fn main() {
	s1 := make(true)
	s2 := s1
	println(s1)
	_ = s2
}

fn make(cond bool) string {
	return if cond {
		'a'.to_owned()
	} else {
		'b'.to_owned()
	}
}
")
	assert fail_conditional_return.exit_code != 0
	assert fail_conditional_return.output.contains('use of moved value: `s1`'), fail_conditional_return.output

	ok_return_conditional_move_does_not_leak := run_ownership_check(v3_bin,
		'return_conditional_move_does_not_leak', "
fn f(cond bool) string {
	s := 'x'.to_owned()
	return if cond {
		t := s
		_ = t
		''
	} else {
		''
	}
}

fn g() {
	s := 'y'.to_owned()
	println(s)
}

fn main() {
	_ = f(false)
	g()
}
")
	assert ok_return_conditional_move_does_not_leak.exit_code == 0, ok_return_conditional_move_does_not_leak.output

	fail_or_fallback_return := run_ownership_check(v3_bin, 'or_fallback_return_owned', "
fn main() {
	s1 := make()
	s2 := s1
	println(s1)
	_ = s2
}

fn maybe() ?string {
	return none
}

fn make() string {
	return maybe() or { 'hello'.to_owned() }
}
")
	assert fail_or_fallback_return.exit_code != 0
	assert fail_or_fallback_return.output.contains('use of moved value: `s1`'), fail_or_fallback_return.output

	fail_select_prescan_return := run_ownership_check(v3_bin, 'select_prescan_return_owned', "
fn main() {
	s1 := make()
	s2 := s1
	println(s1)
	_ = s2
}

fn make() string {
	ch := chan bool{cap: 1}
	mut s := 'hello'.to_owned()
	select {
		ch <- true {
			s = ''
		}
		else {}
	}
	return s
}
")
	assert fail_select_prescan_return.exit_code != 0
	assert fail_select_prescan_return.output.contains('use of moved value: `s1`'), fail_select_prescan_return.output

	fail_value_if_side_effect_return := run_ownership_check(v3_bin,
		'value_if_side_effect_prescan_return_owned', "
fn main() {
	s1 := make(true)
	s2 := s1
	println(s1)
	_ = s2
}

fn make(cond bool) string {
	mut s := ''
	_ := if cond {
		s = 'hello'.to_owned()
		0
	} else {
		0
	}
	return s
}
")
	assert fail_value_if_side_effect_return.exit_code != 0
	assert fail_value_if_side_effect_return.output.contains('use of moved value: `s1`'), fail_value_if_side_effect_return.output

	fail_defer_prescan_return := run_ownership_check(v3_bin, 'defer_prescan_return_owned', "
fn main() {
	s1 := make()
	s2 := s1
	println(s1)
	_ = s2
}

fn make() string {
	mut s := 'hello'.to_owned()
	defer {
		s = ''
	}
	return s
}
")
	assert fail_defer_prescan_return.exit_code != 0
	assert fail_defer_prescan_return.output.contains('use of moved value: `s1`'), fail_defer_prescan_return.output

	fail_loop_break_prescan_return := run_ownership_check(v3_bin,
		'loop_break_prescan_return_owned', "
fn main() {
	s1 := make()
	s2 := s1
	println(s1)
	_ = s2
}

fn make() string {
	mut s := 'hello'.to_owned()
	for {
		break
		s = ''
	}
	return s
}
")
	assert fail_loop_break_prescan_return.exit_code != 0
	assert fail_loop_break_prescan_return.output.contains('use of moved value: `s1`'), fail_loop_break_prescan_return.output

	fail_return_default_field_local := run_ownership_check(v3_bin,
		'return_default_field_local_prescan', "
struct Holder {
	value string = 'field'.to_owned()
}

fn main() {
	h := make()
	x := h.value
	println(h.value)
	_ = x
}

fn make() Holder {
	h := Holder{}
	return h
}
")
	assert fail_return_default_field_local.exit_code != 0
	assert fail_return_default_field_local.output.contains('use of moved value: `h.value`'), fail_return_default_field_local.output

	fail_return_array_init_local := run_ownership_check(v3_bin, 'return_array_init_local_prescan', "
fn main() {
	arr := make()
	x := arr[0]
	println(arr[0])
	_ = x
}

fn make() []string {
	a := []string{len: 1, init: 'item'.to_owned()}
	return a
}
")
	assert fail_return_array_init_local.exit_code != 0
	assert fail_return_array_init_local.output.contains('use of moved value'), fail_return_array_init_local.output

	fail_return_assoc_local := run_ownership_check(v3_bin, 'return_assoc_local_prescan', "
struct Holder {
	value string
}

fn main() {
	h := make()
	x := h.value
	println(h.value)
	_ = x
}

fn make() Holder {
	base := Holder{value: 'field'.to_owned()}
	h := Holder{...base}
	return h
}
")
	assert fail_return_assoc_local.exit_code != 0
	assert fail_return_assoc_local.output.contains('use of moved value: `h.value`'), fail_return_assoc_local.output

	fail_conditional_param_return := run_ownership_check(v3_bin, 'conditional_return_param', "
fn main() {
	cond := true
	owned := 'hello'.to_owned()
	x := choose(cond, owned, '')
	y := x
	println(x)
	_ = y
}

fn choose(cond bool, a string, b string) string {
	return if cond { a } else { b }
}
")
	assert fail_conditional_param_return.exit_code != 0
	assert fail_conditional_param_return.output.contains('use of moved value: `x`'), fail_conditional_param_return.output

	fail_match_param_return := run_ownership_check(v3_bin, 'match_return_param', "
enum Pick {
	a
	b
}

fn main() {
	owned := 'hello'.to_owned()
	x := choose(Pick.a, owned, '')
	y := x
	println(x)
	_ = y
}

fn choose(p Pick, a string, b string) string {
	return match p {
		.a { a }
		.b { b }
	}
}
")
	assert fail_match_param_return.exit_code != 0
	assert fail_match_param_return.output.contains('use of moved value: `x`'), fail_match_param_return.output

	fail_or_fallback_param_return := run_ownership_check(v3_bin, 'or_fallback_return_param', "
fn main() {
	owned := 'hello'.to_owned()
	x := pass(owned)
	y := x
	println(x)
	_ = y
}

fn maybe() ?string {
	return none
}

fn pass(a string) string {
	return maybe() or { a }
}
")
	assert fail_or_fallback_param_return.exit_code != 0
	assert fail_or_fallback_param_return.output.contains('use of moved value: `x`'), fail_or_fallback_param_return.output

	fail_module_return := run_ownership_check_with_module(v3_bin, 'module_return_owned', '
import foo

fn main() {
	s1 := foo.make()
	s2 := s1
	println(s1)
	_ = s2
}
',
		'foo', "
module foo

pub fn make() string {
	return 'hello'.to_owned()
}
")
	assert fail_module_return.exit_code != 0
	assert fail_module_return.output.contains('use of moved value: `s1`'), fail_module_return.output

	fail_module_param := run_ownership_check_with_module(v3_bin, 'module_return_param', "
import foo

fn main() {
	owned := 'hello'.to_owned()
	s1 := foo.pass(owned)
	s2 := s1
	println(s1)
	_ = s2
}
",
		'foo', '
module foo

pub fn pass(s string) string {
	return s
}
')
	assert fail_module_param.exit_code != 0
	assert fail_module_param.output.contains('use of moved value: `s1`'), fail_module_param.output

	fail_fn_value_return := run_ownership_check(v3_bin, 'fn_value_return_owned', "
fn make() string {
	return 'hello'.to_owned()
}

fn main() {
	f := make
	s := f()
	t := s
	println(s)
	_ = t
}
")
	assert fail_fn_value_return.exit_code != 0
	assert fail_fn_value_return.output.contains('use of moved value: `s`'), fail_fn_value_return.output

	fail_fn_literal_return := run_ownership_check(v3_bin, 'fn_literal_return_owned', "
fn main() {
	f := fn () string {
		return 'hello'.to_owned()
	}
	s := f()
	t := s
	println(s)
	_ = t
}
")
	assert fail_fn_literal_return.exit_code != 0
	assert fail_fn_literal_return.output.contains('use of moved value: `s`'), fail_fn_literal_return.output
}

fn test_ownership_assignment_to_storage_consumes_rhs() {
	v3_bin := ownership_build_v3()
	fail_field := run_ownership_check(v3_bin, 'assign_field', '
struct Holder {
mut:
	value string
}

fn main() {
	mut h := Holder{value: ""}
	r := "field".to_owned()
	h.value = r
	println(r)
}
')
	assert fail_field.exit_code != 0
	assert fail_field.output.contains('use of moved value: `r`'), fail_field.output

	fail_field_read := run_ownership_check(v3_bin, 'assign_field_then_duplicate_read', '
struct Holder {
mut:
	value string
}

fn main() {
	mut h := Holder{value: ""}
	r := "field".to_owned()
	h.value = r
	t := h.value
	println(h.value)
	_ = t
}
')
	assert fail_field_read.exit_code != 0
	assert fail_field_read.output.contains('use of moved value: `h.value`'), fail_field_read.output

	ok_whole_overwrite_clears_field := run_ownership_check(v3_bin,
		'whole_overwrite_clears_owned_field', '
struct Holder {
mut:
	value string
}

fn main() {
	mut h := Holder{value: "field".to_owned()}
	old := h.value
	h = Holder{value: ""}
	t := h.value
	println(h.value)
	_ = old
	_ = t
}
')
	assert ok_whole_overwrite_clears_field.exit_code == 0, ok_whole_overwrite_clears_field.output

	fail_field_expr_read := run_ownership_check(v3_bin, 'assign_field_then_expression_read', '
struct Holder {
mut:
	value string
}

fn main() {
	mut h := Holder{value: ""}
	r := "field".to_owned()
	h.value = r
	t := h.value
	println(h.value + "")
	_ = t
}
')
	assert fail_field_expr_read.exit_code != 0
	assert fail_field_expr_read.output.contains('use of moved value: `h.value`'), fail_field_expr_read.output

	fail_struct_literal_field_read := run_ownership_check(v3_bin,
		'struct_literal_field_then_duplicate_read', '
struct Holder {
	value string
}

fn main() {
	r := "field".to_owned()
	h := Holder{value: r}
	t := h.value
	println(h.value)
	_ = t
}
')
	assert fail_struct_literal_field_read.exit_code != 0
	assert fail_struct_literal_field_read.output.contains('use of moved value: `h.value`'), fail_struct_literal_field_read.output

	fail_aggregate_copy := run_ownership_check(v3_bin, 'aggregate_copy_moves_owned_field', '
struct Holder {
	value string
}

fn main() {
	s := "field".to_owned()
	h := Holder{value: s}
	h2 := h
	t := h2.value
	println(h2.value)
	println(h.value)
	_ = t
}
')
	assert fail_aggregate_copy.exit_code != 0
	assert fail_aggregate_copy.output.contains('use of moved value: `h2.value`'), fail_aggregate_copy.output
	assert fail_aggregate_copy.output.contains('use of moved value: `h.value`'), fail_aggregate_copy.output

	fail_default_owned_field_read := run_ownership_check(v3_bin,
		'default_owned_field_then_duplicate_read', '
struct Resource implements Owned {
	id int
}

struct Holder {
	r Resource
}

fn main() {
	h := Holder{}
	x := h.r
	y := h.r
	_ = x
	_ = y
}
')
	assert fail_default_owned_field_read.exit_code != 0
	assert fail_default_owned_field_read.output.contains('use of moved value: `h.r`'), fail_default_owned_field_read.output

	fail_default_to_owned_field_read := run_ownership_check(v3_bin,
		'default_to_owned_field_then_duplicate_read', '
struct Holder {
	value string = "field".to_owned()
}

fn main() {
	h := Holder{}
	x := h.value
	y := h.value
	_ = x
	_ = y
}
')
	assert fail_default_to_owned_field_read.exit_code != 0
	assert fail_default_to_owned_field_read.output.contains('use of moved value: `h.value`'), fail_default_to_owned_field_read.output

	fail_index := run_ownership_check(v3_bin, 'assign_index', '
fn main() {
	mut arr := [""]
	r := "item".to_owned()
	arr[0] = r
	println(r)
}
')
	assert fail_index.exit_code != 0
	assert fail_index.output.contains('use of moved value: `r`'), fail_index.output

	fail_index_read := run_ownership_check(v3_bin, 'assign_index_then_duplicate_read', '
fn main() {
	mut arr := [""]
	r := "item".to_owned()
	arr[0] = r
	t := arr[0]
	println(arr[0])
	_ = t
}
')
	assert fail_index_read.exit_code != 0
	assert fail_index_read.output.contains('use of moved value: `arr[0]`'), fail_index_read.output

	fail_array_literal_read := run_ownership_check(v3_bin, 'array_literal_then_duplicate_read', '
fn main() {
	s := "item".to_owned()
	arr := [s]
	t := arr[0]
	println(arr[0])
	_ = t
}
')
	assert fail_array_literal_read.exit_code != 0
	assert fail_array_literal_read.output.contains('use of moved value: `arr[0]`'), fail_array_literal_read.output

	fail_for_in_owned_element := run_ownership_check(v3_bin, 'for_in_owned_element', '
fn main() {
	s := "item".to_owned()
	arr := [s]
	for x in arr {
		y := x
		println(x)
		_ = y
	}
}
')
	assert fail_for_in_owned_element.exit_code != 0
	assert fail_for_in_owned_element.output.contains('use of moved value: `x`'), fail_for_in_owned_element.output

	fail_conditional_array_literal_read := run_ownership_check(v3_bin,
		'conditional_array_literal_then_duplicate_read', '
fn main() {
	cond := true
	s := "item".to_owned()
	arr := if cond { [s] } else { []string{} }
	t := arr[0]
	println(arr[0])
	_ = t
}
')
	assert fail_conditional_array_literal_read.exit_code != 0
	assert fail_conditional_array_literal_read.output.contains('use of moved value: `arr[0]`'), fail_conditional_array_literal_read.output

	fail_array_literal_expr_read := run_ownership_check(v3_bin,
		'array_literal_then_expression_read', '
fn main() {
	s := "item".to_owned()
	arr := [s]
	t := arr[0]
	println(arr[0] + "")
	_ = t
}
')
	assert fail_array_literal_expr_read.exit_code != 0
	assert fail_array_literal_expr_read.output.contains('use of moved value: `arr[0]`'), fail_array_literal_expr_read.output

	fail_returned_default_field_read := run_ownership_check(v3_bin,
		'returned_default_field_then_duplicate_read', '
struct Holder {
	value string = "item".to_owned()
}

fn main() {
	h := make()
	t := h.value
	println(h.value)
	_ = t
}

fn make() Holder {
	return Holder{}
}
')
	assert fail_returned_default_field_read.exit_code != 0
	assert fail_returned_default_field_read.output.contains('use of moved value: `h.value`'), fail_returned_default_field_read.output

	fail_returned_array_literal_read := run_ownership_check(v3_bin,
		'returned_array_literal_then_duplicate_read', '
fn main() {
	arr := make()
	t := arr[0]
	println(arr[0])
	_ = t
}

fn make() []string {
	s := "item".to_owned()
	return [s]
}
')
	assert fail_returned_array_literal_read.exit_code != 0
	assert fail_returned_array_literal_read.output.contains('use of moved value: `arr[0]`'), fail_returned_array_literal_read.output

	fail_projected_return_field := run_ownership_check(v3_bin, 'projected_return_field', '
struct Holder {
	value string
}

fn main() {
	s := make().value
	t := s
	println(s)
	_ = t
}

fn make() Holder {
	s := "field".to_owned()
	return Holder{value: s}
}
')
	assert fail_projected_return_field.exit_code != 0
	assert fail_projected_return_field.output.contains('use of moved value: `s`'), fail_projected_return_field.output

	fail_array_dynamic_index_read := run_ownership_check(v3_bin,
		'array_dynamic_index_then_literal_read', '
fn main() {
	s := "item".to_owned()
	arr := [s]
	i := 0
	t := arr[i]
	u := arr[0]
	_ = t
	_ = u
}
')
	assert fail_array_dynamic_index_read.exit_code != 0
	assert fail_array_dynamic_index_read.output.contains('use of moved value: `arr[0]`'), fail_array_dynamic_index_read.output

	fail_array_append_read := run_ownership_check(v3_bin, 'array_append_then_duplicate_read', '
fn main() {
	mut arr := []string{}
	s := "item".to_owned()
	arr << s
	t := arr[0]
	println(arr[0])
	_ = t
}
')
	assert fail_array_append_read.exit_code != 0
	assert fail_array_append_read.output.contains('use of moved value: `arr[0]`'), fail_array_append_read.output

	fail_array_append_array_read := run_ownership_check(v3_bin,
		'array_append_array_then_duplicate_read', '
fn main() {
	mut dst := []string{}
	s := "item".to_owned()
	src := [s]
	dst << src
	t := dst[0]
	println(dst[0])
	_ = t
}
')
	assert fail_array_append_array_read.exit_code != 0
	assert fail_array_append_array_read.output.contains('use of moved value: `dst[0]`'), fail_array_append_array_read.output

	fail_array_append_len_init_read := run_ownership_check(v3_bin,
		'array_append_len_init_then_duplicate_read', '
fn main() {
	mut arr := []string{len: 1}
	s := "item".to_owned()
	arr << s
	t := arr[1]
	println(arr[1])
	_ = t
}
')
	assert fail_array_append_len_init_read.exit_code != 0
	assert fail_array_append_len_init_read.output.contains('use of moved value: `arr[1]`'), fail_array_append_len_init_read.output

	fail_array_append_unknown_len_read := run_ownership_check(v3_bin,
		'array_append_unknown_len_then_duplicate_read', '
fn main() {
	n := 1
	mut arr := []string{len: n}
	s := "item".to_owned()
	arr << s
	t := arr[1]
	println(arr[1])
	_ = t
}
')
	assert fail_array_append_unknown_len_read.exit_code != 0
	assert fail_array_append_unknown_len_read.output.contains('use of moved value: `arr[*]`'), fail_array_append_unknown_len_read.output

	ok_array_loop_dynamic_append_return := run_ownership_check(v3_bin,
		'array_loop_dynamic_append_return', '
fn split_parts(glob string) []string {
	mut parts := []string{}
	for i := 0; i < glob.len; i++ {
		if glob[i] == `,` {
			parts << glob[0..i].to_owned()
		}
	}
	parts << glob[0..].to_owned()
	return parts
}

fn main() {
	parts := split_parts("a,b")
	println(int_str(parts.len))
}
')
	assert ok_array_loop_dynamic_append_return.exit_code == 0, ok_array_loop_dynamic_append_return.output

	fail_array_pop_return := run_ownership_check(v3_bin, 'array_pop_returns_owned', '
fn main() {
	mut arr := ["item".to_owned()]
	x := arr.pop()
	y := x
	println(x)
	_ = y
}
')
	assert fail_array_pop_return.exit_code != 0
	assert fail_array_pop_return.output.contains('use of moved value: `x`'), fail_array_pop_return.output

	fail_array_pop_return_from_fn := run_ownership_check(v3_bin, 'array_pop_return_from_later_fn', '
fn main() {
	x := make()
	y := x
	println(x)
	_ = y
}

fn make() string {
	mut arr := ["item".to_owned()]
	return arr.pop()
}
')
	assert fail_array_pop_return_from_fn.exit_code != 0
	assert fail_array_pop_return_from_fn.output.contains('use of moved value: `x`'), fail_array_pop_return_from_fn.output

	fail_array_first_call_arg := run_ownership_check(v3_bin, 'array_first_call_arg_moves_element', '
fn sink(s string) {
	_ = s
}

fn main() {
	mut arr := ["item".to_owned()]
	sink(arr.first())
	x := arr[0]
	_ = x
}
')
	assert fail_array_first_call_arg.exit_code != 0
	assert fail_array_first_call_arg.output.contains('use of moved value: `arr[0]`'), fail_array_first_call_arg.output

	fail_array_first_discard := run_ownership_check(v3_bin, 'array_first_discard_moves_element', '
fn main() {
	mut arr := ["item".to_owned()]
	arr.first()
	x := arr[0]
	_ = x
}
')
	assert fail_array_first_discard.exit_code != 0
	assert fail_array_first_discard.output.contains('use of moved value: `arr[0]`'), fail_array_first_discard.output

	fail_array_first_blank := run_ownership_check(v3_bin, 'array_first_blank_moves_element', '
fn main() {
	mut arr := ["item".to_owned()]
	_ = arr.first()
	x := arr[0]
	_ = x
}
')
	assert fail_array_first_blank.exit_code != 0
	assert fail_array_first_blank.output.contains('use of moved value: `arr[0]`'), fail_array_first_blank.output

	ok_array_pop_discard_append := run_ownership_check(v3_bin, 'array_pop_discard_updates_length', '
fn main() {
	mut arr := ["item".to_owned()]
	arr.pop()
	arr << "literal"
	x := arr[0]
	y := x
	println(x)
	_ = y
}
')
	assert ok_array_pop_discard_append.exit_code == 0, ok_array_pop_discard_append.output

	ok_array_pop_call_arg_append := run_ownership_check(v3_bin,
		'array_pop_call_arg_updates_length', '
fn sink(s string) {
	_ = s
}

fn main() {
	mut arr := ["item".to_owned()]
	sink(arr.pop())
	arr << "literal"
	x := arr[0]
	y := x
	println(x)
	_ = y
}
')
	assert ok_array_pop_call_arg_append.exit_code == 0, ok_array_pop_call_arg_append.output

	fail_array_pop_left_shifted_read := run_ownership_check(v3_bin,
		'array_pop_left_shifted_owned_read', '
fn main() {
	a := "old".to_owned()
	b := "new".to_owned()
	mut arr := [a, b]
	arr.pop_left()
	x := arr[0]
	y := x
	println(x)
	_ = y
}
')
	assert fail_array_pop_left_shifted_read.exit_code != 0
	assert fail_array_pop_left_shifted_read.output.contains('use of moved value: `x`'), fail_array_pop_left_shifted_read.output

	fail_array_insert_read := run_ownership_check(v3_bin, 'array_insert_then_duplicate_read', '
fn main() {
	mut arr := []string{}
	s := "item".to_owned()
	arr.insert(0, s)
	x := arr[0]
	y := x
	println(x)
	_ = y
}
')
	assert fail_array_insert_read.exit_code != 0
	assert fail_array_insert_read.output.contains('use of moved value: `x`'), fail_array_insert_read.output

	fail_array_insert_shifted_read := run_ownership_check(v3_bin,
		'array_insert_shifted_owned_read', '
fn main() {
	a := "old".to_owned()
	mut arr := [a]
	b := "new".to_owned()
	arr.insert(0, b)
	x := arr[1]
	y := x
	println(x)
	_ = y
}
')
	assert fail_array_insert_shifted_read.exit_code != 0
	assert fail_array_insert_shifted_read.output.contains('use of moved value: `x`'), fail_array_insert_shifted_read.output

	fail_array_prepend_read := run_ownership_check(v3_bin, 'array_prepend_then_duplicate_read', '
fn main() {
	mut arr := []string{}
	s := "item".to_owned()
	arr.prepend(s)
	x := arr[0]
	y := x
	println(x)
	_ = y
}
')
	assert fail_array_prepend_read.exit_code != 0
	assert fail_array_prepend_read.output.contains('use of moved value: `x`'), fail_array_prepend_read.output

	fail_array_prepend_shifted_read := run_ownership_check(v3_bin,
		'array_prepend_shifted_owned_read', '
fn main() {
	a := "old".to_owned()
	mut arr := [a]
	b := "new".to_owned()
	arr.prepend(b)
	x := arr[1]
	y := x
	println(x)
	_ = y
}
')
	assert fail_array_prepend_shifted_read.exit_code != 0
	assert fail_array_prepend_shifted_read.output.contains('use of moved value: `x`'), fail_array_prepend_shifted_read.output

	fail_array_prepend_self_read := run_ownership_check(v3_bin, 'array_prepend_self_owned_read', '
fn main() {
	a := "old".to_owned()
	mut arr := [a]
	arr.prepend(arr[0])
	x := arr[0]
	y := x
	println(x)
	_ = y
}
')
	assert fail_array_prepend_self_read.exit_code != 0
	assert fail_array_prepend_self_read.output.contains('use of moved value: `x`'), fail_array_prepend_self_read.output

	fail_fixed_array_init := run_ownership_check(v3_bin, 'fixed_array_init_owned_elements', '
fn main() {
	s := "item".to_owned()
	arr := [2]string{s, s}
	_ = arr
}
')
	assert fail_fixed_array_init.exit_code != 0
	assert fail_fixed_array_init.output.contains('use of moved value: `s`'), fail_fixed_array_init.output

	fail_array_len_init := run_ownership_check(v3_bin, 'array_len_init_owned_element', '
fn main() {
	s := "item".to_owned()
	arr := []string{len: 2, init: s}
	println(s)
	t := arr[0]
	println(arr[0])
	_ = t
}
')
	assert fail_array_len_init.exit_code != 0
	assert fail_array_len_init.output.contains('use of moved value: `s`'), fail_array_len_init.output
	assert fail_array_len_init.output.contains('use of moved value: `arr[*]`'), fail_array_len_init.output

	fail_map_literal_read := run_ownership_check(v3_bin, 'map_literal_then_duplicate_read', '
fn main() {
	s := "item".to_owned()
	m := {"k": s}
	t := m["k"]
	println(m["k"])
	_ = t
}
')
	assert fail_map_literal_read.exit_code != 0
	assert fail_map_literal_read.output.contains('use of moved value: `m[k]`'), fail_map_literal_read.output

	fail_map_dynamic_index_read := run_ownership_check(v3_bin,
		'map_dynamic_index_then_literal_read', '
fn main() {
	s := "item".to_owned()
	m := {"k": s}
	key := "k"
	t := m[key]
	u := m["k"]
	_ = t
	_ = u
}
')
	assert fail_map_dynamic_index_read.exit_code != 0
	assert fail_map_dynamic_index_read.output.contains('use of moved value: `m[k]`'), fail_map_dynamic_index_read.output
}

fn test_ownership_aggregate_borrows_block_overlapping_reassignment() {
	v3_bin := ownership_build_v3()
	fail_field_borrow := run_ownership_check(v3_bin, 'field_borrow_blocks_whole_assign', '
struct Holder {
mut:
	value string
}

fn main() {
	mut h := Holder{value: "old".to_owned()}
	r := &h.value
	h = Holder{value: "new".to_owned()}
	_ = r
}
')
	assert fail_field_borrow.exit_code != 0
	assert fail_field_borrow.output.contains('cannot assign to `h` because `h.value` is borrowed by `r`'), fail_field_borrow.output

	fail_whole_borrow := run_ownership_check(v3_bin, 'whole_borrow_blocks_field_assign', '
struct Holder {
mut:
	value string
}

fn main() {
	mut h := Holder{value: "old".to_owned()}
	r := &h
	h.value = "new".to_owned()
	_ = r
}
')
	assert fail_whole_borrow.exit_code != 0
	assert fail_whole_borrow.output.contains('cannot assign to `h.value` because `h` is borrowed by `r`'), fail_whole_borrow.output

	ok_scoped_owned_field := run_ownership_check(v3_bin, 'scoped_owned_field_removed', '
struct Holder {
	value string
}

fn main() {
	{
		s := "old".to_owned()
		h := Holder{value: s}
		t := h.value
		_ = t
	}
	h := Holder{value: ""}
	t := h.value
	println(h.value)
	_ = t
}
')
	assert ok_scoped_owned_field.exit_code == 0, ok_scoped_owned_field.output
}

fn test_ownership_callee_params_are_order_independent() {
	v3_bin := ownership_build_v3()
	fail := run_ownership_check(v3_bin, 'callee_before_owned_call', "
fn sink(s string) {
	t := s
	println(s)
	_ = t
}

fn main() {
	r := 'hello'.to_owned()
	sink(r)
}
")
	assert fail.exit_code != 0
	assert fail.output.contains('use of moved value: `s`'), fail.output

	fail_method := run_ownership_check(v3_bin, 'receiver_method_before_owned_call', "
struct Holder {}

fn (h Holder) sink(s string) {
	_ = h
	t := s
	println(s)
	_ = t
}

fn main() {
	h := Holder{}
	r := 'hello'.to_owned()
	h.sink(r)
}
")
	assert fail_method.exit_code != 0
	assert fail_method.output.contains('use of moved value: `s`'), fail_method.output

	fail_aggregate_param := run_ownership_check(v3_bin, 'aggregate_param_owned_field', '
struct Holder {
	value string
}

fn dup(h Holder) {
	t := h.value
	println(h.value)
	_ = t
}

fn main() {
	s := "field".to_owned()
	h := Holder{value: s}
	dup(h)
}
	')
	assert fail_aggregate_param.exit_code != 0
	assert fail_aggregate_param.output.contains('use of moved value: `h.value`'), fail_aggregate_param.output

	fail_aggregate_literal_param := run_ownership_check(v3_bin,
		'aggregate_literal_param_owned_element', '
fn main() {
	s := "field".to_owned()
	dup([s])
}

fn dup(arr []string) {
	t := arr[0]
	println(arr[0])
	_ = t
}
	')
	assert fail_aggregate_literal_param.exit_code != 0
	assert fail_aggregate_literal_param.output.contains('use of moved value: `arr[0]`'), fail_aggregate_literal_param.output

	fail_array_init_param := run_ownership_check(v3_bin, 'array_init_param_owned_element', '
fn main() {
	s := "field".to_owned()
	dup([]string{len: 1, init: s})
}

fn dup(arr []string) {
	t := arr[0]
	println(arr[0])
	_ = t
}
	')
	assert fail_array_init_param.exit_code != 0
	assert fail_array_init_param.output.contains('use of moved value'), fail_array_init_param.output

	fail_fixed_array_init_param := run_ownership_check(v3_bin,
		'fixed_array_init_param_owned_element', '
fn main() {
	s := "field".to_owned()
	dup([1]string{s})
}

fn dup(arr [1]string) {
	t := arr[0]
	println(arr[0])
	_ = t
}
	')
	assert fail_fixed_array_init_param.exit_code != 0
	assert fail_fixed_array_init_param.output.contains('use of moved value'), fail_fixed_array_init_param.output

	fail_variadic_param := run_ownership_check(v3_bin, 'variadic_owned_arg_element', '
fn dup(args ...string) {
	t := args[0]
	println(args[0])
	_ = t
}

fn main() {
	s := "field".to_owned()
	dup(s)
}
')
	assert fail_variadic_param.exit_code != 0
	assert fail_variadic_param.output.contains('use of moved value: `args[0]`'), fail_variadic_param.output

	fail_params_field := run_ownership_check(v3_bin, 'params_field_owned_arg', '
@[params]
struct Cfg {
	s string
}

fn dup(cfg Cfg) {
	t := cfg.s
	println(cfg.s)
	_ = t
}

fn main() {
	owned := "field".to_owned()
	dup(s: owned)
}
')
	assert fail_params_field.exit_code != 0
	assert fail_params_field.output.contains('use of moved value: `cfg.s`'), fail_params_field.output

	fail_params_field_borrow_move := run_ownership_check(v3_bin, 'params_field_borrow_then_move', '
@[params]
struct Cfg {
	r &string
	v string
}

fn foo(cfg Cfg) {
	_ = cfg
}

fn main() {
	s := "field".to_owned()
	foo(r: &s, v: s)
}
')
	assert fail_params_field_borrow_move.exit_code != 0
	assert fail_params_field_borrow_move.output.contains('cannot move `s` because it is borrowed'), fail_params_field_borrow_move.output
}

fn test_ownership_branch_moves_are_isolated_between_siblings() {
	v3_bin := ownership_build_v3()
	ok_sibling := run_ownership_check(v3_bin, 'branch_sibling_isolation', "
fn main() {
	cond := true
	s := 'hello'.to_owned()
	if cond {
		t := s
		println(t)
	} else {
		println(s)
	}
}
")
	assert ok_sibling.exit_code == 0, ok_sibling.output

	ok_branch_local_borrow := run_ownership_check(v3_bin, 'branch_local_borrow_released', "
fn main() {
	cond := true
	s := 'hello'.to_owned()
	if cond {
		r := &s
		_ = r
	}
	u := s
	println(u)
}
")
	assert ok_branch_local_borrow.exit_code == 0, ok_branch_local_borrow.output

	fail_after := run_ownership_check(v3_bin, 'branch_merge_after_if', "
fn main() {
	cond := true
	s := 'hello'.to_owned()
	if cond {
		t := s
		println(t)
	}
	println(s)
}
	")
	assert fail_after.exit_code != 0
	assert fail_after.output.contains('use of moved value: `s`'), fail_after.output

	fail_select_branch_merge := run_ownership_check(v3_bin, 'select_branch_merge', "
fn main() {
	ch := chan bool{cap: 1}
	mut s := 'hello'.to_owned()
	select {
		ch <- true {
			s = 'literal'
		}
		else {}
	}
	t := s
	println(s)
	_ = t
}
")
	assert fail_select_branch_merge.exit_code != 0
	assert fail_select_branch_merge.output.contains('use of moved value: `s`'), fail_select_branch_merge.output

	ok_match_return := run_ownership_check(v3_bin, 'match_return_branch_isolation', "
fn main() {
	x := 0
	s := 'hello'.to_owned()
	match x {
		0 {
			t := s
			println(t)
			return
		}
		else {}
	}
	println(s)
}
")
	assert ok_match_return.exit_code == 0, ok_match_return.output

	fail_loop_post_body_local := run_ownership_check(v3_bin, 'loop_post_body_local_scope', '
fn main() {
	mut cond := true
	for ; cond; j++ {
		mut j := 0
		cond = false
		_ = j
	}
}
')
	assert fail_loop_post_body_local.exit_code != 0
	assert fail_loop_post_body_local.output.contains('unknown identifier `j`'), fail_loop_post_body_local.output

	ok_labeled_break := run_ownership_check(v3_bin, 'labeled_break_targets_outer_loop', "
fn main() {
	mut s := 'hello'.to_owned()
	outer:
	for {
		for {
			break outer
		}
		t := s
		_ = t
	}
	u := s
	_ = u
}
")
	assert ok_labeled_break.exit_code == 0, ok_labeled_break.output

	ok_labeled_continue := run_ownership_check(v3_bin, 'labeled_continue_targets_outer_loop', "
fn main() {
	mut i := 0
	mut s := 'hello'.to_owned()
	outer:
	for i < 1 {
		i++
		for {
			continue outer
		}
		t := s
		_ = t
	}
	u := s
	_ = u
}
")
	assert ok_labeled_continue.exit_code == 0, ok_labeled_continue.output

	fail_branch_borrow := run_ownership_check(v3_bin, 'branch_borrow_merge', "
fn main() {
	cond := true
	other := 'other'.to_owned()
	s := 'hello'.to_owned()
	mut r := &other
	if cond {
		r = &s
	}
	u := s
	println(u)
	_ = r
}
")
	assert fail_branch_borrow.exit_code != 0
	assert fail_branch_borrow.output.contains('cannot move `s` because it is borrowed by `r`'), fail_branch_borrow.output

	fail_branch_owned_assign := run_ownership_check(v3_bin, 'branch_merge_owned_assignment', "
fn main() {
	cond := true
	mut s := ''
	if cond {
		s = 'a'.to_owned()
	} else {
		s = 'b'.to_owned()
	}
	t := s
	println(s)
	_ = t
}
")
	assert fail_branch_owned_assign.exit_code != 0
	assert fail_branch_owned_assign.output.contains('use of moved value: `s`'), fail_branch_owned_assign.output

	ok_branch_plain_assign := run_ownership_check(v3_bin, 'branch_plain_assignment_clears_owned', "
fn main() {
	cond := true
	mut s := 'hello'.to_owned()
	if cond {
		s = ''
	} else {
		s = ''
	}
	t := s
	println(s)
	_ = t
}
")
	assert ok_branch_plain_assign.exit_code == 0, ok_branch_plain_assign.output

	ok_exhaustive_match_plain_assign := run_ownership_check(v3_bin,
		'exhaustive_match_plain_assignment_clears_owned', "
enum Pick {
	a
	b
}

fn main() {
	p := Pick.a
	mut s := 'hello'.to_owned()
	match p {
		.a {
			s = ''
		}
		.b {
			s = ''
		}
	}
	t := s
	println(s)
	_ = t
}
")
	assert ok_exhaustive_match_plain_assign.exit_code == 0, ok_exhaustive_match_plain_assign.output

	ok_exhaustive_sum_match_plain_assign := run_ownership_check(v3_bin,
		'exhaustive_sum_match_plain_assignment_clears_owned', "
struct A {}
struct B {}
type Choice = A | B

fn main() {
	c := Choice(A{})
	mut s := 'hello'.to_owned()
	match c {
		A {
			s = ''
		}
		B {
			s = ''
		}
	}
	t := s
	println(s)
	_ = t
}
")
	assert ok_exhaustive_sum_match_plain_assign.exit_code == 0, ok_exhaustive_sum_match_plain_assign.output

	fail_branch_plain_assign_without_else := run_ownership_check(v3_bin,
		'branch_plain_assignment_without_else_keeps_owned', "
fn main() {
	cond := true
	mut s := 'hello'.to_owned()
	if cond {
		s = ''
	}
	t := s
	println(s)
	_ = t
}
")
	assert fail_branch_plain_assign_without_else.exit_code != 0
	assert fail_branch_plain_assign_without_else.output.contains('use of moved value: `s`'), fail_branch_plain_assign_without_else.output
}

fn test_ownership_veb_implicit_context_arg_moves() {
	v3_bin := ownership_build_v3()
	fail := run_ownership_check(v3_bin, 'veb_implicit_ctx_arg_move', '
import veb

pub struct Context {
	veb.Context
}

pub struct App {}

pub fn (app &App) index() veb.Result {
	s := "hello".to_owned()
	app.show(s)
	println(s)
	return veb.Result{}
}

pub fn (app &App) show(s string) veb.Result {
	_ = s
	return veb.Result{}
}

fn main() {
	app := &App{}
	_ = app
}
')
	assert fail.exit_code != 0
	assert fail.output.contains('use of moved value: `s`'), fail.output
	assert fail.output.contains('value moved into function `App.show`'), fail.output
}

fn test_ownership_struct_marker_and_copy_marker() {
	v3_bin := ownership_build_v3()
	fail := run_ownership_check(v3_bin, 'owned_struct', '
struct Resource implements Owned {
	id int
}

fn main() {
	r := Resource{id: 1}
	r2 := r
	println(r.id)
	_ = r2
}
')
	assert fail.exit_code != 0
	assert fail.output.contains('use of moved value: `r`'), fail.output

	fail_generic := run_ownership_check(v3_bin, 'generic_owned_struct', '
struct Box[T] implements Owned {
	value T
}

fn main() {
	b := Box[string]{value: "x"}
	b2 := b
	println(b.value)
	_ = b2
}
')
	assert fail_generic.exit_code != 0
	assert fail_generic.output.contains('use of moved value: `b`'), fail_generic.output

	fail_generic_interface_marker := run_ownership_check(v3_bin, 'generic_interface_owned_marker', '
interface Marker[T, U] {}

struct Resource implements Marker[int, string], Owned {
	id int
}

fn main() {
	r := Resource{id: 1}
	r2 := r
	println(r.id)
	_ = r2
}
')
	assert fail_generic_interface_marker.exit_code != 0
	assert fail_generic_interface_marker.output.contains('use of moved value: `r`'), fail_generic_interface_marker.output

	fail_qualified_marker := run_ownership_check_with_module(v3_bin,
		'qualified_owned_marker_ignores_main_copy', '
import foo

struct Resource implements Copy {
	id int
}

fn main() {
	r := foo.Resource{id: 1}
	r2 := r
	println(r.id)
	_ = r2
}
',
		'foo', '
module foo

pub struct Resource implements Owned {
pub:
	id int
}
')
	assert fail_qualified_marker.exit_code != 0
	assert fail_qualified_marker.output.contains('use of moved value: `r`'), fail_qualified_marker.output

	fail_option := run_ownership_check(v3_bin, 'option_owned_payload', '
struct Resource implements Owned {
	id int
}

fn dup(o ?Resource) {
	o2 := o
	o3 := o
	_ = o2
	_ = o3
}

fn main() {}
')
	assert fail_option.exit_code != 0
	assert fail_option.output.contains('use of moved value: `o`'), fail_option.output

	fail_result := run_ownership_check(v3_bin, 'result_owned_payload', '
struct Resource implements Owned {
	id int
}

fn dup(r !Resource) {
	r2 := r
	r3 := r
	_ = r2
	_ = r3
}

fn main() {}
')
	assert fail_result.exit_code != 0
	assert fail_result.output.contains('use of moved value: `r`'), fail_result.output

	ok := run_ownership_check(v3_bin, 'copy_struct', '
struct Small implements Copy {
	id int
}

fn main() {
	s1 := Small{id: 1}
	s2 := s1
	println(s1.id)
	println(s2.id)
}
')
	assert ok.exit_code == 0, ok.output
}
