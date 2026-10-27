module types

import v.flat

fn test_comptime_methods_loop_exists_rescans_appended_nodes() {
	mut a := flat.FlatAst.new()
	a.add_val(.ident, 'm')
	a.add_val(.comptime_for, 'f|fields')
	mut tc := TypeChecker.new(&a)
	assert !tc.comptime_methods_loop_exists()
	// Nodes appended after a negative answer must still be seen.
	tc.a.add_val(.comptime_for, 'm|methods')
	assert tc.comptime_methods_loop_exists()
	assert tc.comptime_methods_loop_exists()
}

fn test_comptime_methods_loop_exists_resets_for_another_ast() {
	mut first := flat.FlatAst.new()
	first.add_val(.comptime_for, 'm|methods')
	mut tc := TypeChecker.new(&first)
	assert tc.comptime_methods_loop_exists()
	mut second := flat.FlatAst.new()
	second.add_val(.comptime_for, 'v|values')
	tc.a = &second
	assert !tc.comptime_methods_loop_exists()
}
