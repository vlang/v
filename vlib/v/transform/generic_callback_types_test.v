module transform

import v.flat
import v.types

fn test_specialized_receiver_callback_uses_function_type_compatibility() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'callback')
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['SharedCb'] = 'fn (shared int)'
	tc.type_aliases['PlainCb'] = 'fn (int)'
	tc.type_aliases['SharedHandlers'] = 'map[string]fn (shared int)'
	tc.type_aliases['PlainHandlers'] = 'map[string]fn (int)'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.resolved_receiver_arg_compatible(id, 'fn ([]int, []int) int', 'fn([]int, []int) int')
	assert t.resolved_receiver_arg_compatible(id, 'fn (values []f64, indices []int) f64', 'fn([]f64, []int) f64')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([]int) int', 'fn([]int, []int) int')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([]int, []int) string', 'fn([]int, []int) int')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (mut []int) int', 'fn([]int) int')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (shared int)', 'fn (int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (int)', 'fn (shared int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (atomic int)', 'fn (int)')
	assert t.resolved_receiver_arg_compatible(id, 'fn (shared int)', 'fn (shared int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (fn (shared int))',
		'fn (fn (int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (fn (int))', 'fn (fn (atomic int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn () fn (shared int)',
		'fn () fn (int)')
	assert t.resolved_receiver_arg_compatible(id, 'fn (fn (shared int))',
		'fn (fn (shared int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([]fn (shared int))',
		'fn ([]fn (int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([2]fn (atomic int))',
		'fn ([2]fn (int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (map[string]fn (shared int))',
		'fn (map[string]fn (int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (SharedCb)', 'fn (PlainCb)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn () SharedCb', 'fn () PlainCb')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (SharedHandlers)',
		'fn (PlainHandlers)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (chan fn (shared int))',
		'fn (chan fn (int))')
}
