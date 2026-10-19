module transform

import v.flat
import v.types

// A local of a nested block moves to the heap when an outer variable keeps its address,
// but not for `_ = Holder{next: &x}`, which keeps nothing: the move would allocate on
// every execution of the block.
fn test_outer_scope_address_store_skips_a_discarded_destination() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.escape_decl_depths['saved'] = 0
	t.escape_decl_depths['x'] = 1
	for target, escapes in {
		'_':     false
		'saved': true
	} {
		t.escaping_amp_sources.clear()
		rhs := t.make_prefix(.amp, t.make_ident('x'))
		t.mark_outer_scope_address_store(t.make_ident(target), rhs, map[string][]string{},
			map[string]string{})
		assert ('x' in t.escaping_amp_sources) == escapes, target
	}
}
