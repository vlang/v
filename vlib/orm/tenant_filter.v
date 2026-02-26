@[has_globals]
module orm

import time

// TenantFilterConfig configures automatic tenant filtering in ORM queries.
pub struct TenantFilterConfig {
pub:
	enabled    bool   = true
	field_name string = 'tenant_id'
}

struct TenantFilterState {
mut:
	config             TenantFilterConfig = TenantFilterConfig{}
	has_current_tenant bool
	current_tenant_id  Primitive = Primitive(Null{})
	disabled_scopes    int
}

struct TenantFilterSpec {
	field_name string
	tenant_id  Primitive
}

__global tenant_filter_state = TenantFilterState{}

const disabled_tenant_filter_attr_values = ['0', 'disable', 'disabled', 'false', 'ignore', 'no',
	'none', 'off']

// configure_tenant_filter configures global tenant filter behavior.
pub fn configure_tenant_filter(config TenantFilterConfig) {
	field_name := if config.field_name.trim_space().len == 0 {
		'tenant_id'
	} else {
		config.field_name.trim_space()
	}
	tenant_filter_state.config = TenantFilterConfig{
		enabled:    config.enabled
		field_name: field_name
	}
}

// set_current_tenant_id sets the tenant id used for automatic filtering.
pub fn set_current_tenant_id(tenant_id Primitive) {
	tenant_filter_state.current_tenant_id = tenant_id
	tenant_filter_state.has_current_tenant = true
}

// clear_current_tenant_id clears the currently configured tenant id.
pub fn clear_current_tenant_id() {
	tenant_filter_state.current_tenant_id = Primitive(Null{})
	tenant_filter_state.has_current_tenant = false
}

// enable_tenant_filter enables the tenant filter globally.
pub fn enable_tenant_filter() {
	tenant_filter_state.config = TenantFilterConfig{
		enabled:    true
		field_name: tenant_filter_state.config.field_name
	}
}

// disable_tenant_filter disables the tenant filter globally.
pub fn disable_tenant_filter() {
	tenant_filter_state.config = TenantFilterConfig{
		enabled:    false
		field_name: tenant_filter_state.config.field_name
	}
}

// without_tenant_filter runs `f` with tenant filtering temporarily disabled.
pub fn without_tenant_filter[T](f fn () !T) !T {
	tenant_filter_state.disabled_scopes++
	defer {
		tenant_filter_state.disabled_scopes--
	}
	return f()
}

// with_tenant runs `f` with a temporary tenant id.
pub fn with_tenant[T](tenant_id Primitive, f fn () !T) !T {
	old_has_current_tenant := tenant_filter_state.has_current_tenant
	old_tenant_id := tenant_filter_state.current_tenant_id
	tenant_filter_state.current_tenant_id = tenant_id
	tenant_filter_state.has_current_tenant = true
	defer {
		tenant_filter_state.current_tenant_id = old_tenant_id
		tenant_filter_state.has_current_tenant = old_has_current_tenant
	}
	return f()
}

// apply_tenant_filter appends the current tenant filter condition to a where clause.
pub fn apply_tenant_filter(table Table, where QueryData) QueryData {
	spec := tenant_filter_spec_for_table(table) or { return where }
	mut merged_where := clone_query_data(where)
	if merged_where.fields.len > 0 {
		merged_where.is_and << true
	}
	merged_where.fields << spec.field_name
	merged_where.data << spec.tenant_id
	merged_where.types << primitive_to_type_idx(spec.tenant_id)
	merged_where.kinds << .eq
	return merged_where
}

// apply_tenant_filter_to_select_config appends tenant filtering and updates `has_where`.
pub fn apply_tenant_filter_to_select_config(config SelectConfig, where QueryData) (SelectConfig, QueryData) {
	mut effective_config := config
	effective_where := apply_tenant_filter(config.table, where)
	effective_config.has_where = effective_where.fields.len > 0
	return effective_config, effective_where
}

fn clone_query_data(data QueryData) QueryData {
	return QueryData{
		fields:      data.fields.clone()
		data:        data.data.clone()
		types:       data.types.clone()
		parentheses: data.parentheses.clone()
		kinds:       data.kinds.clone()
		auto_fields: data.auto_fields.clone()
		is_and:      data.is_and.clone()
	}
}

fn primitive_to_type_idx(value Primitive) int {
	return match value {
		i8 { type_idx['i8'] }
		i16 { type_idx['i16'] }
		int { type_idx['int'] }
		i64 { type_idx['i64'] }
		u8 { type_idx['u8'] }
		u16 { type_idx['u16'] }
		u32 { type_idx['u32'] }
		u64 { type_idx['u64'] }
		f32 { type_idx['f32'] }
		f64 { type_idx['f64'] }
		bool { type_idx['bool'] }
		string { type_string }
		time.Time { time_ }
		Null { type_string }
		InfixType { type_string }
		[]Primitive { type_string }
	}
}

fn tenant_filter_spec_for_table(table Table) ?TenantFilterSpec {
	if !tenant_filter_state.config.enabled {
		return none
	}
	if tenant_filter_state.disabled_scopes > 0 {
		return none
	}
	if !tenant_filter_state.has_current_tenant {
		return none
	}
	mut field_name := tenant_filter_state.config.field_name
	for attr in table.attrs {
		if attr.name !in ['tenant', 'tenant_filter'] {
			continue
		}
		if !attr.has_arg {
			continue
		}
		attr_arg := attr.arg.trim_space()
		if attr_arg.len == 0 {
			continue
		}
		if attr_arg.to_lower() in disabled_tenant_filter_attr_values {
			return none
		}
		field_name = attr_arg
	}
	if field_name.len == 0 {
		return none
	}
	return TenantFilterSpec{
		field_name: field_name
		tenant_id:  tenant_filter_state.current_tenant_id
	}
}
