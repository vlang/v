module types

fn test_call_flag_bloom_invalidates_when_any_sparse_table_grows() {
	for table in 0 .. 4 {
		mut tc := TypeChecker{}
		tc.prepare_check_name_filters()
		before := tc.call_flag_tables_count()
		name := 'late_function'
		assert tc.call_flag_bloom.excludes(name, before)
		match table {
			0 { tc.fn_shared_params[name] = [true] }
			1 { tc.fn_variadic[name] = true }
			2 { tc.c_variadic_fns[name] = true }
			3 { tc.fn_implicit_veb_ctx[name] = true }
			else {}
		}
		after := tc.call_flag_tables_count()
		assert after == u64(1) << u32(48 - table * 16)
		assert after != before, 'table ${table} growth must invalidate the filter'
		assert !tc.call_flag_bloom.excludes(name, after)
		info := tc.call_info(name, false)
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
}

fn test_call_flag_bloom_disables_filter_for_an_unrepresentable_table_count() {
	mut tc := TypeChecker{}
	for i in 0 .. 65535 {
		tc.fn_variadic['function_${i}'] = true
	}
	tc.prepare_check_name_filters()
	// This length exceeds its packed slot. Matching invalid fingerprints cannot
	// prove that a function added after the snapshot has no call flags.
	tc.fn_shared_params['late_shared'] = [true]
	assert tc.call_info('late_shared', false).shared_params == [true]
}
