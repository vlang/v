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
