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
	used_fns := markused.mark_used(a, tc)
	mut g := cgen.FlatGen.new()
	return g.gen_with_used_options(a, used_fns, &tc, true)
}

fn lock_codegen_fn_fragment(c_code string, name string) string {
	start := c_code.index('\nvoid ${name}(Counter* c) {') or { return '' }
	rest := c_code[start + 1..]
	body_start := rest.index(') {') or { return rest }
	after_body_start := rest[body_start + 3..]
	next_fn := after_body_start.index('\nvoid ') or { return rest }
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
			continue
		}
	}
}

fn branch_break(mut c Counter) {
	for {
		lock c.a {
			break
		}
	}
}

fn main() {
	mut c := Counter{}
	multi_lock(mut c)
	branch_continue(mut c)
	branch_break(mut c)
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
}
