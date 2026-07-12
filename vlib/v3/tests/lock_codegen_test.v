import os
import v3.gen.c as cgen
import v3.markused
import v3.parser
import v3.pref
import v3.transform
import v3.types

fn lock_codegen_gen_c(name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[src] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	mut used_fns := markused.mark_used(a, tc)
	used_fns = transform.monomorphize_with_used(mut a, &tc, used_fns)
	mut g := cgen.FlatGen.new()
	return g.gen_with_used_options(a, used_fns, &tc, true)
}

fn lock_codegen_gen_c_sources(name string, files map[string]string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	mut paths := []string{}
	mut rels := files.keys()
	rels.sort()
	for rel in rels {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, files[rel]) or { panic(err) }
		paths << path
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files(paths)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	for path in paths {
		tc.diagnostic_files[path] = true
	}
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	mut used_fns := markused.mark_used(a, tc)
	used_fns = transform.monomorphize_with_used(mut a, &tc, used_fns)
	mut g := cgen.FlatGen.new()
	return g.gen_with_used_options(a, used_fns, &tc, true)
}

fn lock_codegen_fn_fragment(c_code string, name string) string {
	return lock_codegen_counter_fn_fragment(c_code, name, 'void')
}

fn lock_codegen_counter_fn_fragment(c_code string, name string, return_type string) string {
	start := c_code.index('\n${return_type} ${name}(Counter* c) {') or { return '' }
	rest := c_code[start + 1..]
	body_start := rest.index(') {') or { return rest }
	after_body_start := rest[body_start + 3..]
	mut next_fn := after_body_start.len
	for marker in ['\nvoid ', '\nint ', '\nstring ', '\nArray_', '\nOptional_'] {
		if idx := after_body_start.index(marker) {
			if idx < next_fn {
				next_fn = idx
			}
		}
	}
	return rest[..body_start + 3 + next_fn]
}

fn assert_lock_cleanup_before_branch(c_code string, name string, branch string) {
	fragment := lock_codegen_fn_fragment(c_code, name)
	assert fragment.len > 0, c_code
	branch_idx := fragment.last_index(branch) or { -1 }
	assert branch_idx >= 0, fragment
	before_branch := fragment[..branch_idx]
	unlock_idx := before_branch.last_index('sync__RwMutex__unlock((sync__RwMutex*)') or { -1 }
	assert unlock_idx >= 0, fragment
	lock_idx := before_branch.last_index('sync__RwMutex__lock((sync__RwMutex*)') or { -1 }
	assert lock_idx >= 0, fragment
	assert lock_idx < unlock_idx, fragment
}

fn assert_lock_defer_runs_before_branch(c_code string, name string, branch string) {
	fragment := lock_codegen_fn_fragment(c_code, name)
	assert fragment.len > 0, c_code
	branch_idx := fragment.last_index(branch) or { -1 }
	assert branch_idx >= 0, fragment
	before_branch := fragment[..branch_idx]
	defer_idx := before_branch.last_index('->val = 2;') or { -1 }
	assert defer_idx >= 0, fragment
	unlock_idx := before_branch.last_index('sync__RwMutex__unlock((sync__RwMutex*)') or { -1 }
	assert unlock_idx >= 0, fragment
	assert defer_idx < unlock_idx, fragment
	assert unlock_idx < branch_idx, fragment
}

fn assert_no_lock_cleanup_before_branch(c_code string, name string, branch string) {
	fragment := lock_codegen_fn_fragment(c_code, name)
	assert fragment.len > 0, c_code
	branch_idx := fragment.last_index(branch) or { -1 }
	assert branch_idx >= 0, fragment
	before_branch := fragment[..branch_idx]
	unlock_idx := before_branch.last_index('sync__RwMutex__unlock((sync__RwMutex*)') or { -1 }
	assert unlock_idx < 0, fragment
}

fn assert_lock_scope_goto_rejected(c_code string, name string, branch string) {
	fragment := lock_codegen_fn_fragment(c_code, name)
	assert fragment.len > 0, c_code
	error_idx := fragment.index('#error goto into a different lock scope is not supported') or {
		-1
	}
	assert error_idx >= 0, fragment
	branch_idx := fragment.index(branch) or { -1 }
	assert branch_idx < 0, fragment
}

fn assert_return_read_before_lock_cleanup(c_code string, name string) {
	fragment := lock_codegen_counter_fn_fragment(c_code, name, 'int')
	assert fragment.len > 0, c_code
	read_idx := fragment.index('->val') or { -1 }
	assert read_idx >= 0, fragment
	unlock_idx := fragment.index('sync__RwMutex__unlock((sync__RwMutex*)') or { -1 }
	assert unlock_idx >= 0, fragment
	return_idx := fragment.last_index('return ') or { -1 }
	assert return_idx >= 0, fragment
	assert read_idx < unlock_idx, fragment
	assert unlock_idx < return_idx, fragment
}

fn assert_outer_defer_runs_after_lock_cleanup(c_code string, name string) {
	fragment := lock_codegen_counter_fn_fragment(c_code, name, 'int')
	assert fragment.len > 0, c_code
	first_lock_idx := fragment.index('sync__RwMutex__lock((sync__RwMutex*)') or { -1 }
	assert first_lock_idx >= 0, fragment
	read_idx := fragment.index('->val') or { -1 }
	assert read_idx >= 0, fragment
	after_read := fragment[read_idx..]
	unlock_rel := after_read.index('sync__RwMutex__unlock((sync__RwMutex*)') or { -1 }
	assert unlock_rel >= 0, fragment
	unlock_idx := read_idx + unlock_rel
	after_unlock := fragment[unlock_idx + 1..]
	defer_lock_rel := after_unlock.index('sync__RwMutex__lock((sync__RwMutex*)') or { -1 }
	assert defer_lock_rel >= 0, fragment
	defer_lock_idx := unlock_idx + 1 + defer_lock_rel
	assert first_lock_idx < read_idx, fragment
	assert read_idx < unlock_idx, fragment
	assert unlock_idx < defer_lock_idx, fragment
}

fn assert_lock_expr_defer_runs_before_unlock(c_code string, name string) {
	fragment := lock_codegen_counter_fn_fragment(c_code, name, 'int')
	assert fragment.len > 0, c_code
	result_idx := fragment.index(' = 1;') or { -1 }
	assert result_idx >= 0, fragment
	defer_idx := fragment.index('->val = 2;') or { -1 }
	assert defer_idx >= 0, fragment
	unlock_idx := fragment.index('sync__RwMutex__unlock((sync__RwMutex*)') or { -1 }
	assert unlock_idx >= 0, fragment
	return_idx := fragment.last_index('return ') or { -1 }
	assert return_idx >= 0, fragment
	assert result_idx < defer_idx, fragment
	assert defer_idx < unlock_idx, fragment
	assert unlock_idx < return_idx, fragment
}

fn assert_lock_question_defer_runs_before_error_return(c_code string, name string) {
	fragment := lock_codegen_counter_fn_fragment(c_code, name, 'Optional')
	assert fragment.len > 0, c_code
	defer_idx := fragment.index('->val = 2;') or { -1 }
	assert defer_idx >= 0, fragment
	unlock_idx := fragment.index('sync__RwMutex__unlock((sync__RwMutex*)') or { -1 }
	assert unlock_idx >= 0, fragment
	return_idx := fragment.index('return _t') or { -1 }
	assert return_idx >= 0, fragment
	assert defer_idx < unlock_idx, fragment
	assert unlock_idx < return_idx, fragment
}

fn test_lock_codegen_sorts_deduplicates_and_cleans_branch_exits() {
	c_code := lock_codegen_gen_c('lock_codegen_regression', 'struct Counter {
mut:
	a shared int
	b shared int
}

fn multi_lock(mut c Counter) {
	lock c.b, c.a, c.a {
		_ := 1
	}
}

fn branch_continue(mut c Counter) {
	for {
		lock c.a {
			defer {
				c.a = 2
			}
			continue
		}
	}
}

fn branch_break(mut c Counter) {
	for {
		lock c.a {
			defer {
				c.a = 2
			}
			break
		}
	}
}

fn branch_labeled_break(mut c Counter) {
outer:
	for {
		lock c.a {
			for {
				break outer
			}
		}
	}
}

fn branch_labeled_continue(mut c Counter) {
outer:
	for {
		lock c.a {
			for {
				continue outer
			}
		}
	}
}

fn lock_if_tail(mut c Counter, cond bool) int {
	return lock c.a {
		if cond {
			1
		} else {
			2
		}
	}
}

fn return_shared(mut c Counter) int {
	lock c.a {
		return c.a
	}
	return 0
}

fn defer_after_lock(mut c Counter) int {
	defer {
		lock c.a {
			_ := c.a
		}
	}
	lock c.a {
		return c.a
	}
	return 0
}

fn goto_out_of_lock(mut c Counter) {
	lock c.a {
		defer {
			c.a = 2
		}
		goto done
	}
done:
	lock c.a {
		_ := c.a
	}
}

fn goto_inside_lock(mut c Counter) {
	lock c.a {
		goto inside
	inside:
		_ := c.a
	}
}

fn goto_into_different_lock(mut c Counter) {
	lock c.a {
		goto other
	}
	lock c.b {
	other:
		_ := c.b
	}
}

fn lock_expr_defer_tail(mut c Counter) int {
	value := lock c.a {
		defer {
			c.a = 2
		}
		1
	}
	return value
}

fn may_fail() !int {
	return error("fail")
}

fn lock_question_defer(mut c Counter) !int {
	lock c.a {
		defer {
			c.a = 2
		}
		may_fail()?
	}
	return 0
}

fn main() {
	mut c := Counter{}
	multi_lock(mut c)
	branch_continue(mut c)
	branch_break(mut c)
	branch_labeled_break(mut c)
	branch_labeled_continue(mut c)
	_ = lock_if_tail(mut c, true)
	_ = return_shared(mut c)
	_ = defer_after_lock(mut c)
	goto_out_of_lock(mut c)
	goto_inside_lock(mut c)
	goto_into_different_lock(mut c)
	_ = lock_expr_defer_tail(mut c)
	_ = lock_question_defer(mut c) or { 0 }
}
')
	assert c_code.contains('uintptr_t _t'), c_code
	assert c_code.contains('if (_t'), c_code
	assert c_code.contains(' > _t'), c_code
	assert c_code.contains('== _t'), c_code
	assert c_code.contains(']) continue;'), c_code
	assert c_code.contains('sync__RwMutex__lock((sync__RwMutex*)'), c_code
	assert !c_code.contains('sync__RwMutex__lock(&'), c_code
	assert_lock_cleanup_before_branch(c_code, 'branch_continue', 'continue;')
	assert_lock_cleanup_before_branch(c_code, 'branch_break', 'break;')
	assert_lock_cleanup_before_branch(c_code, 'branch_labeled_break', 'goto outer_break;')
	assert_lock_cleanup_before_branch(c_code, 'branch_labeled_continue', 'goto outer_continue;')
	assert_lock_cleanup_before_branch(c_code, 'goto_out_of_lock', 'goto done;')
	assert_lock_defer_runs_before_branch(c_code, 'branch_continue', 'continue;')
	assert_lock_defer_runs_before_branch(c_code, 'branch_break', 'break;')
	assert_lock_defer_runs_before_branch(c_code, 'goto_out_of_lock', 'goto done;')
	assert_no_lock_cleanup_before_branch(c_code, 'goto_inside_lock', 'goto inside;')
	assert_lock_scope_goto_rejected(c_code, 'goto_into_different_lock', 'goto other;')
	assert c_code.contains(' = (cond ? 1 : 2);'), c_code
	assert_return_read_before_lock_cleanup(c_code, 'return_shared')
	assert_outer_defer_runs_after_lock_cleanup(c_code, 'defer_after_lock')
	assert_lock_expr_defer_runs_before_unlock(c_code, 'lock_expr_defer_tail')
	assert_lock_question_defer_runs_before_error_return(c_code, 'lock_question_defer')
}

fn test_shared_wrapper_value_type_uses_declaring_module() {
	c_code := lock_codegen_gen_c_sources('shared_wrapper_decl_module', {
		'main.v': 'module main

import m

struct Foo {
	wrong int
}

fn main() {
	_ := m.Box{}
}
'
		'm/m.v':  'module m

pub struct Foo {
pub:
	value int
}

pub struct Box {
pub mut:
	item shared Foo
}
'
	})
	assert c_code.contains('struct __shared__m__Foo {'), c_code
	assert c_code.contains('\tm__Foo val;'), c_code
	assert !c_code.contains('\tFoo val;'), c_code
}

fn test_generic_shared_field_uses_concrete_wrapper() {
	c_code := lock_codegen_gen_c('generic_shared_field_wrapper', 'struct Box[T] {
mut:
	item shared T
}

fn read_box(mut b Box[int]) int {
	lock b.item {
		return b.item
	}
	return 0
}

fn make_box() Box[int] {
	return Box{
		item: 1
	}
}

fn main() {
	mut b := make_box()
	_ := read_box(mut b)
}
')
	assert c_code.contains('struct __shared__int {'), c_code
	assert c_code.contains('\tint val;'), c_code
	assert c_code.contains('struct Box_int {'), c_code
	assert c_code.contains('\t__shared__int* item;'), c_code
	assert !c_code.contains('\tint item;'), c_code
	assert !c_code.contains('__shared__T'), c_code
	assert c_code.contains('b->item->val'), c_code
}

fn test_shared_option_payload_wrappers_are_unique() {
	c_code := lock_codegen_gen_c('shared_option_payload_wrapper', 'struct Holder {
mut:
	a shared ?int
	b shared ?string
	c shared !string
}

fn main() {
	_ := Holder{}
}
')
	assert c_code.contains('struct __shared__Optional {'), c_code
	assert c_code.contains('\tOptional val;'), c_code
	assert c_code.contains('struct __shared__Optional_string {'), c_code
	assert c_code.contains('\tOptional_string val;'), c_code
	assert c_code.contains('\t__shared__Optional* a;'), c_code
	assert c_code.contains('\t__shared__Optional_string* b;'), c_code
	assert c_code.contains('\t__shared__Optional_string* c;'), c_code
	assert !c_code.contains('struct __shared__Optional {\n\tsync__RwMutex mtx;\n\tOptional_string val;'), c_code
}
