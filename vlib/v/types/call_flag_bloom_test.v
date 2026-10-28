module types

fn set_call_flag(mut tc TypeChecker, table int, name string) {
	match table {
		0 { tc.fn_shared_params[name] = [true] }
		1 { tc.fn_variadic[name] = true }
		2 { tc.c_variadic_fns[name] = true }
		3 { tc.fn_implicit_veb_ctx[name] = true }
		else {}
	}
}

fn assert_call_flag(info CallInfo, table int) {
	match table {
		0 {
			assert info.shared_params == [true]
		}
		1 {
			assert info.is_variadic
		}
		2 {
			assert info.is_c_variadic
		}
		3 {
			assert info.has_implicit_veb_ctx
		}
		else {}
	}
}

fn test_call_flag_bloom_covers_only_set_flags() {
	mut tc := TypeChecker{}
	tc.fn_variadic['plain'] = false
	tc.c_variadic_fns['plain'] = false
	tc.fn_implicit_veb_ctx['plain'] = false
	tc.fn_shared_params['plain'] = []bool{}
	for table in 0 .. 4 {
		set_call_flag(mut tc, table, 'flagged_${table}')
	}
	tc.prepare_check_name_filters()
	count := tc.call_flag_tables_count()
	assert count == call_flag_bloom_built
	assert tc.call_flag_bloom.excludes('plain', count)
	assert tc.call_flag_bloom.excludes('absent', count)
	plain := tc.call_info('plain', false)
	assert plain.shared_params.len == 0
	assert !plain.is_variadic && !plain.is_c_variadic && !plain.has_implicit_veb_ctx
	for table in 0 .. 4 {
		name := 'flagged_${table}'
		assert !tc.call_flag_bloom.excludes(name, count)
		assert_call_flag(tc.call_info(name, false), table)
	}
}

fn test_call_flag_bloom_invalidates_when_any_sparse_table_grows() {
	for table in 0 .. 4 {
		mut tc := TypeChecker{}
		tc.prepare_check_name_filters()
		name := 'late_function'
		assert tc.call_flag_bloom.excludes(name, tc.call_flag_tables_count())
		set_call_flag(mut tc, table, name)
		after := tc.call_flag_tables_count()
		assert after == name_bloom_invalid_count, 'table ${table} growth must invalidate the filter'
		assert !tc.call_flag_bloom.excludes(name, after)
		assert_call_flag(tc.call_info(name, false), table)
	}
}

fn test_call_flag_bloom_is_disabled_by_registration() {
	mut tc := TypeChecker{}
	tc.fn_variadic['late_variadic'] = false
	tc.prepare_check_name_filters()
	assert tc.call_flag_bloom.excludes('late_variadic', tc.call_flag_tables_count())
	// Overwriting an entry keeps the table sizes, so the registration itself
	// must turn the filter off.
	tc.register_fn_ancillary_owned('late_variadic', []bool{}, true, false)
	assert tc.call_info('late_variadic', false).is_variadic
	tc.prepare_check_name_filters()
	assert tc.call_flag_bloom.excludes('late_c', tc.call_flag_tables_count())
	tc.register_c_variadic_fn_with_lowered('late_c', 'late_c')
	assert tc.call_info('late_c', false).is_c_variadic
}
