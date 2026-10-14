// Coverage for the *global* tenant filter state machine: the enable/disable
// flag, field-name normalisation, the current-tenant slot, and the
// snapshot/restore that `with_tenant`/`without_tenant_filter` rely on.
//
// Every entry point here mutates one process-global, so each test resets it in
// a `defer` the way `orm_fn_test.v` does. The reset cannot be shared with that
// file: a directory-level `v test vlib/orm/` compiles the test files together.
//
// Note the whole surface is `@[deprecated]`. That is asserted nowhere here; the
// deprecation is a property of the annotation, not of the behaviour.
import orm

// tfs_default leaves the global state as the rest of the module expects it:
// enabled, the default field name, and no current tenant.
fn tfs_default() {
	orm.set_tenant_filter_enabled(true)
	orm.configure_tenant_filter(field_name: 'tenant_id')
	orm.clear_current_tenant_id()
}

fn tfs_table() orm.Table {
	return orm.new_table('test_table', [])
}

fn tfs_apply(where orm.QueryData) orm.QueryData {
	return orm.apply_tenant_filter(tfs_table(), where)
}

fn test_set_tenant_filter_enabled_false_stops_the_filter_being_applied() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id(21)

	orm.set_tenant_filter_enabled(false)
	// Measured: the input is returned untouched, so the caller's own WHERE is
	// still intact and no tenant condition is added.
	filtered := tfs_apply(orm.QueryData{
		fields: ['id']
		data:   [orm.Primitive(1)]
		kinds:  [.eq]
	})
	assert filtered.fields == ['id']
	assert filtered.data == [orm.Primitive(1)]
}

fn test_configure_tenant_filter_can_disable_the_filter_outright() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id(21)

	orm.configure_tenant_filter(enabled: false, field_name: 'tenant_id')
	assert tfs_apply(orm.QueryData{}).fields.len == 0
}

fn test_set_tenant_filter_enabled_true_re_enables_it() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id(21)
	orm.set_tenant_filter_enabled(false)
	orm.set_tenant_filter_enabled(true)

	assert tfs_apply(orm.QueryData{}).fields == ['tenant_id']
}

fn test_an_empty_field_name_falls_back_to_the_default() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id(21)

	orm.configure_tenant_filter(field_name: '')
	// Measured: an empty name is not an error, it becomes `tenant_id`.
	assert tfs_apply(orm.QueryData{}).fields == ['tenant_id']
}

fn test_a_field_name_is_trimmed_of_whitespace_and_quotes() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id(21)

	// Measured: `trim_attr_arg` strips the surrounding whitespace first and then
	// one matching pair of quotes, so the SQL column name is bare.
	orm.configure_tenant_filter(field_name: "  'acme_id' ")
	assert tfs_apply(orm.QueryData{}).fields == ['acme_id']

	// Measured: whitespace *inside* the quotes never reaches the SQL. The name
	// is normalised twice — once when `configure_tenant_filter` stores it and
	// again in `apply_tenant_filter` — and the second `trim_space` removes it
	// either way, so the emitted column is bare.
	orm.configure_tenant_filter(field_name: "'  acme_id  '")
	assert tfs_apply(orm.QueryData{}).fields == ['acme_id']

	orm.configure_tenant_filter(field_name: '   ')
	assert tfs_apply(orm.QueryData{}).fields == ['tenant_id']
}

fn test_setting_the_tenant_to_null_clears_it() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id(21)
	assert tfs_apply(orm.QueryData{}).fields == ['tenant_id']

	orm.set_current_tenant_id(orm.Null{})
	// Measured: a Null tenant is treated as "no tenant", not as a NULL
	// comparison, so no condition is appended at all.
	assert tfs_apply(orm.QueryData{}).fields.len == 0
}

fn test_a_string_tenant_records_a_string_type_index() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id('acme')

	filtered := tfs_apply(orm.QueryData{})
	assert filtered.data == [orm.Primitive('acme')]
	// Measured: the reported type index comes from the same `type_string`
	// constant the DDL generator uses for a string column.
	assert filtered.types == [orm.type_string]
}

fn test_with_tenant_restores_a_disabled_previous_state() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id(9)
	// Deliberately start *disabled*: the snapshot has to restore the flag, not
	// just force it back on.
	orm.set_tenant_filter_enabled(false)

	inside := orm.with_tenant_value[orm.QueryData](33, fn () orm.QueryData {
		return tfs_apply(orm.QueryData{})
	})
	assert inside.fields == ['tenant_id']
	assert inside.data == [orm.Primitive(33)]

	after := tfs_apply(orm.QueryData{})
	assert after.fields.len == 0
}

fn test_without_tenant_filter_value_restores_the_previous_tenant() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id(9)

	off := orm.without_tenant_filter_value[orm.QueryData](fn () orm.QueryData {
		return tfs_apply(orm.QueryData{})
	})
	assert off.fields.len == 0

	on := tfs_apply(orm.QueryData{})
	assert on.fields == ['tenant_id']
	assert on.data == [orm.Primitive(9)]
}

fn test_with_tenant_value_restores_after_a_failing_callback() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id(9)

	orm.with_tenant[orm.QueryData](33, fn () !orm.QueryData {
		return error('boom')
	}) or {}

	after := tfs_apply(orm.QueryData{})
	assert after.fields == ['tenant_id']
	assert after.data == [orm.Primitive(9)]
}

fn test_a_tenant_field_attribute_overrides_the_global_name() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id(5)

	// Measured: the attribute argument goes through the same trimming path, so
	// quotes are accepted here too.
	with_override := orm.apply_tenant_filter(orm.new_table('test_table', [
		VAttribute{
			name:    'tenant_field'
			has_arg: true
			arg:     " 'company_id' "
			kind:    .string
		},
	]), orm.QueryData{})
	assert with_override.fields == ['company_id']

	// An empty override argument is ignored rather than clearing the name.
	with_empty_override := orm.apply_tenant_filter(orm.new_table('test_table', [
		VAttribute{
			name:    'tenant_field'
			has_arg: true
			arg:     '   '
			kind:    .string
		},
	]), orm.QueryData{})
	assert with_empty_override.fields == ['tenant_id']
}

fn test_a_filtered_table_is_returned_as_is() {
	defer {
		tfs_default()
	}
	tfs_default()
	orm.set_current_tenant_id(5)

	ignored := orm.apply_tenant_filter(orm.new_table('test_table', [
		VAttribute{
			name: 'ignore_tenant_filter'
		},
	]), orm.QueryData{})
	assert ignored.fields.len == 0
	assert ignored.data.len == 0
}
