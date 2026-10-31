module transform

import v.flat
import v.types

fn sql_guard_node(mut a flat.FlatAst, template flat.Node, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		...template
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

fn test_sql_bookkeeping_is_conservative_before_program_scan() {
	mut a := flat.FlatAst.new()
	field := a.add_val(.field_init, 'name')
	init := sql_guard_node(mut a, flat.Node{ kind: .struct_init }, [field])
	data := a.add_val(.sql_expr, 'querydata select from records')
	name := a.add_val(.ident, 'record')
	other := a.add_val(.int_literal, '1')
	assignment := sql_guard_node(mut a, flat.Node{ kind: .assign, op: .assign }, [
		name,
		other,
	])
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert !t.sql_tracking_disabled
	t.record_orm_initialized_fields('record', init)
	assert t.sql_initialized_fields('record') == ['name']
	t.record_sql_query_data_alias('record', data)
	assert t.sql_query_data_aliases['record'] == ['select', 'from', 'records']
	t.update_orm_initialized_fields_for_assignment(a.nodes[int(assignment)])
	t.update_sql_query_data_aliases_for_assignment(a.nodes[int(assignment)])
	assert 'record' !in t.orm_initialized_fields
	assert 'record' !in t.sql_query_data_aliases
	assert t.sql_initialized_fields('record') == ['*']
}

fn test_sql_free_program_disables_bookkeeping_and_workers_keep_the_proof() {
	mut a := flat.FlatAst.new()
	field := a.add_val(.field_init, 'name')
	init := sql_guard_node(mut a, flat.Node{ kind: .struct_init }, [field])
	name := a.add_val(.ident, 'record')
	assignment := sql_guard_node(mut a, flat.Node{ kind: .assign, op: .assign }, [
		name,
		init,
	])
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.collect_exclusive_closure_return_fns()
	assert t.sql_tracking_disabled
	t.record_orm_initialized_fields('record', init)
	t.update_orm_initialized_fields_for_assignment(a.nodes[int(assignment)])
	t.record_sql_query_data_alias('record', init)
	t.update_sql_query_data_aliases_for_assignment(a.nodes[int(assignment)])
	assert t.orm_initialized_fields.len == 0
	assert t.sql_query_data_aliases.len == 0
	mut worker := t.fork_program_view(&a, &tc, map[string]bool{}, false)
	assert worker.sql_tracking_disabled
	worker.record_orm_initialized_fields('record', init)
	assert worker.orm_initialized_fields.len == 0
	t.reset_var_types()
	assert t.sql_tracking_disabled
}

fn test_sql_in_generic_or_unreachable_body_preserves_program_bookkeeping() {
	mut a := flat.FlatAst.new()
	field := a.add_val(.field_init, 'name')
	init := sql_guard_node(mut a, flat.Node{ kind: .struct_init }, [field])
	sql := a.add_val(.sql_expr, 'querydata select from records')
	body := sql_guard_node(mut a, flat.Node{ kind: .comptime_if }, [sql])
	generic_fn := sql_guard_node(mut a, flat.Node{ kind: .fn_decl, value: 'unreachable' }, [body])
	a.nodes[int(generic_fn)].set_generic_params(['T'])
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.collect_exclusive_closure_return_fns()
	assert !t.sql_tracking_disabled
	t.record_orm_initialized_fields('record', init)
	assert t.sql_initialized_fields('record') == ['name']
	mut worker := t.fork_program_view(&a, &tc, map[string]bool{}, false)
	assert !worker.sql_tracking_disabled
	worker.record_orm_initialized_fields('record', init)
	worker.record_sql_query_data_alias('query', sql)
	assert worker.sql_initialized_fields('record') == ['name']
	assert worker.sql_query_data_aliases['query'] == ['select', 'from', 'records']
	// A second use of the completed prescan retains its immutable result.
	t.collect_exclusive_closure_return_fns()
	assert !t.sql_tracking_disabled
}
