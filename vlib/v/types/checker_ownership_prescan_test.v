module types

import v.flat

fn test_ownership_prescan_reports_exhaustion_without_a_source_position() {
	$if ownership ? {
		for returns in [false, true] {
			mut a := flat.FlatAst.new()
			id := a.add_val(.fn_decl, 'pending')
			mut tc := TypeChecker.new(&a)
			tc.cur_file = 'caller.v'
			tc.cur_module = 'caller'
			items := [OwnershipFnScanItem{
				idx:    int(id)
				file:   'pending.v'
				module: 'pending_module'
				name:   'pending'
			}]
			// An exhausted budget must fail before checking the pending function,
			// even when it is synthetic and has no diagnostic source position.
			if returns {
				tc.ownership_prescan_fn_returns(items, 0)
			} else {
				tc.ownership_prescan_owned_call_params(items, 0)
			}
			assert tc.errors.len == 1
			what := if returns {
				'ownership return-alias inference'
			} else {
				'ownership parameter inference'
			}
			assert tc.errors[0].msg == '${what} did not converge for `pending` after 0 rounds; this is a compiler bug, please report it'
			assert tc.errors[0].file == 'pending.v'
			assert tc.errors[0].node == id
			assert tc.cur_file == 'caller.v'
			assert tc.cur_module == 'caller'
			assert !tc.ownership_return_record_calls
			assert tc.ownership_return_current_item == -1
			assert tc.ownership_return_item_by_name.len == 0
			assert tc.ownership_return_edges.len == 0
			assert !tc.ownership_param_track_changes
			assert tc.ownership_param_current_item == -1
			assert tc.ownership_param_item_by_name.len == 0
			assert tc.ownership_param_changed_items.len == 0
		}
	}
}

fn test_ownership_prescan_detached_mut_array_sum_return_summaries() {
	$if ownership ? {
		for index, target in ['Choice', 'Layer', 'Generic[int]', 'Choice', 'Choice', 'RefChoice',
			'MixedChoice', 'DirectValue', 'DirectRef', 'BothNested', 'WrappedDirectValue', 'NestedValue',
			'Generic[string]', '?Choice', '!Choice', '?RefChoice', '!RefChoice', '?Choice', '!Choice',
			'?Choice', 'Choice', 'CyclicValue', 'CyclicRef'] {
			mut a := flat.FlatAst.new()
			name := 'keep_${index}'
			param_type := if index == 3 {
				'[]int'
			} else if index == 20 {
				'&PtrArrayAlias'
			} else {
				'&[]int'
			}
			value_param := a.add_node(flat.Node{
				kind:   .param
				value:  'values'
				typ:    flat.source_type_id(param_type)
				is_mut: index != 3
				op:     if index == 4 { .amp } else { .none }
			})
			borrowed_param := a.add_node(flat.Node{
				kind:  .param
				value: 'borrowed'
				typ:   flat.source_type_id('&Cell')
			})
			mut value := a.add_val(.ident, 'values')
			if index in [17, 18] {
				payload_children := a.begin_children()
				a.add_child(value)
				value = a.add_node(flat.Node{
					kind:           .cast_expr
					value:          'Choice'
					children_start: payload_children
					children_count: 1
				})
				wrapper_children := a.begin_children()
				a.add_child(value)
				value = a.add_node(flat.Node{
					kind:           .cast_expr
					value:          target
					children_start: wrapper_children
					children_count: 1
				})
			} else if index == 19 {
				value = a.add(.none_expr)
			}
			children := a.begin_children()
			a.add_child(value)
			returned := a.add_node(flat.Node{
				kind:           .return_stmt
				children_start: children
				children_count: 1
			})
			fn_children := a.begin_children()
			a.add_child(value_param)
			a.add_child(borrowed_param)
			a.add_child(returned)
			fn_id := a.add_node(flat.Node{
				kind:           .fn_decl
				value:          name
				typ:            flat.source_type_id(target)
				children_start: fn_children
				children_count: 3
			})
			mut tc := TypeChecker.new(&a)
			tc.top_level_idx = [i32(fn_id)]
			tc.sum_types.set('Choice', ['[]int', '&Cell'])
			tc.sum_types.set('Layer', ['Choice', 'bool'])
			tc.sum_types.set('Generic', ['[]T', 'int'])
			tc.sum_generic_params['Generic'] = ['T']
			tc.sum_types.set('RefChoice', ['&[]int', 'int'])
			tc.sum_types.set('MixedChoice', ['[]int', '&[]int'])
			tc.sum_types.set('ValueInner', ['[]int', 'bool'])
			tc.sum_types.set('RefInner', ['&[]int', 'bool'])
			tc.sum_types.set('DirectValue', ['[]int', 'RefInner'])
			tc.sum_types.set('DirectRef', ['&[]int', 'ValueInner'])
			tc.sum_types.set('BothNested', ['ValueInner', 'RefInner'])
			tc.sum_types.set('WrappedDirectValue', ['DirectValue', 'bool'])
			tc.sum_types.set('NestedValue', ['ValueInner', 'bool'])
			tc.sum_types.set('CyclicValue', ['CyclicRef', '[]int'])
			tc.sum_types.set('CyclicRef', ['CyclicValue', '&[]int'])
			tc.type_aliases.set('PtrArrayAlias', '&[]int')
			tc.structs['Cell'] = []StructField{}
			tc.fn_ret_types[name] = *tc.parse_type(flat.source_type_id(target))
			tc.ownership_register_fn_param_mut(name, [index != 3, false])
			mut local_types := {
				'values':   *tc.parse_type(flat.source_type_id(param_type))
				'borrowed': *tc.parse_type(flat.source_type_id('&Cell'))
			}
			mut owned_locals := map[string]bool{}
			tc.ownership_prescan_returns_in(name, returned, ['values', 'borrowed'], mut owned_locals, mut local_types)
			fresh := index in [0, 1, 2, 7, 10, 11, 13, 14, 17, 18, 21]
			return_slots := tc.ownership_state().ownership_fn_return_slots[name] or { []int{} }
			return_sources := tc.ownership_state().ownership_fn_returns_param[name] or { []int{} }
			return_params := tc.ownership_state().ownership_fn_return_params[name] or { []OwnershipReturnParamSlot{} }
			assert (name in tc.ownership_state().ownership_fns) == fresh
			assert (0 in return_slots) == fresh
			assert (0 in return_sources) == (!fresh && index != 19)
			assert return_params.len == if fresh || index == 19 { 0 } else { 1 }
			if index == 14 {
				error_fn := a.add_val(.ident, 'error')
				message := a.add_val(.string_literal, 'missing')
				error_children := a.begin_children()
				a.add_child(error_fn)
				a.add_child(message)
				error_call := a.add_node(flat.Node{
					kind:           .call
					children_start: error_children
					children_count: 2
				})
				assert !tc.ownership_prescan_return_copies_mut_array(name, error_call, [
					'values',
					'borrowed',
				], local_types)
			}
			if index == 0 {
				// A sibling pointer return still aliases its own argument; the detached
				// array branch must not remove that independent source mapping.
				borrowed := a.add_val(.ident, 'borrowed')
				borrow_children := a.begin_children()
				a.add_child(borrowed)
				borrow_return := a.add_node(flat.Node{
					kind:           .return_stmt
					children_start: borrow_children
					children_count: 1
				})
				tc.ownership_prescan_returns_in(name, borrow_return, ['values', 'borrowed'], mut owned_locals, mut local_types)
				sibling_sources := tc.ownership_state().ownership_fn_returns_param[name] or { []int{} }
				sibling_params := tc.ownership_state().ownership_fn_return_params[name] or { []OwnershipReturnParamSlot{} }
				assert name in tc.ownership_state().ownership_fns
				assert sibling_sources == [1]
				assert sibling_params.len == 1
				assert sibling_params[0].param_idx == 1
			}
		}
	}
}

fn test_ownership_prescan_wrapped_mut_sum_returns_acquire_owned_storage() {
	$if ownership ? {
		for index, source in ['?Choice', '!Choice', '?Layer', '?Generic[int]', '?MixedChoice',
			'?NestedPointerChoice', '?AliasPointerChoice', '?Choice', '?Choice', '?Choice', '?Choice',
			'?Cycle', '?ScalarChoice', '?Choice', '!Choice', '?[]int', '?AliasChoice', 'Choice',
			'WrappedStorage', 'MixedChoice', 'WrappedLayer', '?OptionalPointerChoice'] {
			mut a := flat.FlatAst.new()
			name := 'wrapped_${index}'
			target := if index == 9 {
				'!Choice'
			} else if index == 10 {
				'?Layer'
			} else {
				source
			}
			param_type := if index == 8 { source } else { '&${source}' }
			param := a.add_node(flat.Node{
				kind:   .param
				value:  'values'
				typ:    flat.source_type_id(param_type)
				is_mut: index != 8
				op:     if index == 7 { .amp } else { .none }
			})
			mut value := a.add_val(.ident, 'values')
			if index == 13 {
				value = a.add(.none_expr)
			} else if index == 14 {
				error_fn := a.add_val(.ident, 'error')
				message := a.add_val(.string_literal, 'missing')
				error_children := a.begin_children()
				a.add_child(error_fn)
				a.add_child(message)
				value = a.add_node(flat.Node{
					kind:           .call
					children_start: error_children
					children_count: 2
				})
			}
			children := a.begin_children()
			a.add_child(value)
			returned := a.add_node(flat.Node{
				kind:           .return_stmt
				children_start: children
				children_count: 1
			})
			fn_children := a.begin_children()
			a.add_child(param)
			a.add_child(returned)
			fn_id := a.add_node(flat.Node{
				kind:           .fn_decl
				value:          name
				typ:            flat.source_type_id(target)
				children_start: fn_children
				children_count: 2
			})
			mut tc := TypeChecker.new(&a)
			tc.top_level_idx = [i32(fn_id)]
			tc.sum_types.set('Choice', ['[]int', 'int'])
			tc.sum_types.set('Layer', ['Choice', 'bool'])
			tc.sum_types.set('Generic', ['[]T', 'int'])
			tc.sum_generic_params['Generic'] = ['T']
			tc.sum_types.set('MixedChoice', ['[]int', '&int'])
			tc.sum_types.set('NestedPointerChoice', ['Choice', 'MixedChoice'])
			tc.sum_types.set('AliasPointerChoice', ['[]int', 'IntPointer'])
			tc.sum_types.set('Cycle', ['Cycle', '[]int'])
			tc.sum_types.set('ScalarChoice', ['int', 'bool'])
			tc.sum_types.set('WrappedStorage', ['?[]int', 'int'])
			tc.sum_types.set('WrappedLayer', ['WrappedStorage', 'bool'])
			tc.sum_types.set('OptionalPointerChoice', ['[]int', '?&int'])
			tc.type_aliases.set('IntPointer', '&int')
			tc.type_aliases.set('AliasChoice', 'Choice')
			tc.fn_ret_types[name] = *tc.parse_type(flat.source_type_id(target))
			tc.ownership_register_fn_param_mut(name, [index != 8])
			mut local_types := {
				'values': *tc.parse_type(flat.source_type_id(param_type))
			}
			if index == 14 {
				assert !tc.ownership_prescan_return_copies_mut_array(name, value, ['values'], local_types)
				continue
			}
			mut owned_locals := map[string]bool{}
			tc.ownership_prescan_returns_in(name, returned, ['values'], mut owned_locals, mut local_types)
			fresh := index in [0, 1, 2, 3, 16, 17, 18, 20]
			return_slots := tc.ownership_state().ownership_fn_return_slots[name] or { []int{} }
			return_sources := tc.ownership_state().ownership_fn_returns_param[name] or { []int{} }
			return_params := tc.ownership_state().ownership_fn_return_params[name] or { []OwnershipReturnParamSlot{} }
			assert (name in tc.ownership_state().ownership_fns) == fresh
			assert (0 in return_slots) == fresh
			assert (0 in return_sources) == (!fresh && index != 13)
			assert return_params.len == if fresh || index == 13 { 0 } else { 1 }
		}
	}
}
