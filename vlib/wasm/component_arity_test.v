module wasm

// Canonical ABI MAX_FLAT_PARAMS = 16: functions flattening to more than 16
// slots must travel indirectly via memory. The wrapper has no indirect
// adapter yet, so it refuses them instead of emitting an invalid lift.

fn arity_section(data []u8, id u8) []u8 {
	mut pos := 8
	for pos + 2 <= data.len {
		sid := data[pos]
		pos++
		mut size := 0
		mut shift := 0
		for {
			b := data[pos]
			pos++
			size |= (int(b) & 0x7f) << shift
			shift += 7
			if b & 0x80 == 0 {
				break
			}
		}
		if sid == id {
			return data[pos..pos + size]
		}
		pos += size
	}
	return []u8{}
}

fn test_17_flat_params_fall_back_without_lift() {
	mut m := Module{}
	params := []ValType{len: 17, init: .i32_t}
	mut func := m.new_function('many', params, [.i32_t])
	{
		func.i32_const(0)
	}
	m.commit(func, true)
	core := m.compile()
	out := wrap_core_module(core)
	assert is_component(out)
	// Fallback is a plain module wrapper with no lifted exports.
	assert arity_section(out, 11).len == 0, '17 flat params must not be lifted'
}

fn test_16_flat_params_lift() {
	mut m := Module{}
	params := []ValType{len: 16, init: .i32_t}
	mut func := m.new_function('many16', params, [.i32_t])
	{
		func.i32_const(0)
	}
	m.commit(func, true)
	core := m.compile()
	out := wrap_core_module(core)
	assert is_component(out)
	assert arity_section(out, 11).len != 0, '16 flat params must lift'
}

fn test_multi_string_params_count_two_slots_each() {
	// 9 strings flatten to 18 slots (> 16) and must not lift.
	mut m := Module{}
	wide_params := []ValType{len: 18, init: .i32_t}
	mut wide := m.new_function('many_strings', wide_params, [.i32_t])
	{
		wide.i32_const(0)
	}
	m.commit(wide, true)
	core := m.compile()
	mut wide_labels := []string{}
	for i in 0 .. 9 {
		wide_labels << 's${i}'
	}
	wide_spec := CabiStringExport{
		wit_name:     'many-strings'
		comp_params:  []u8{len: 9, init: 0x73}
		comp_labels:  wide_labels
		comp_results: [u8(0x7a)]
		core_params:  []u8{len: 18, init: 0x7f}
		core_results: [u8(0x7f)]
	}
	wide_out := wrap_core_module_strings(core, {
		'many_strings': wide_spec
	})
	assert is_component(wide_out)
	assert arity_section(wide_out, 11).len == 0, '9 strings (18 flats) must not lift'

	// 8 strings flatten to exactly 16 slots and must lift.
	mut m2 := Module{}
	ok_params := []ValType{len: 16, init: .i32_t}
	mut ok := m2.new_function('ok_strings', ok_params, [.i32_t])
	{
		ok.i32_const(0)
	}
	m2.commit(ok, true)
	core2 := m2.compile()
	mut ok_labels := []string{}
	for i in 0 .. 8 {
		ok_labels << 's${i}'
	}
	ok_spec := CabiStringExport{
		wit_name:     'ok-strings'
		comp_params:  []u8{len: 8, init: 0x73}
		comp_labels:  ok_labels
		comp_results: [u8(0x7a)]
		core_params:  []u8{len: 16, init: 0x7f}
		core_results: [u8(0x7f)]
	}
	ok_out := wrap_core_module_strings(core2, {
		'ok_strings': ok_spec
	})
	assert is_component(ok_out)
	assert arity_section(ok_out, 11).len != 0, '8 strings (16 flats) must lift'
}

fn test_over_arity_import_falls_back() {
	mut m := Module{}
	imp_params := []ValType{len: 17, init: .i32_t}
	m.new_function_import('env', 'host', imp_params, [.i32_t])
	mut func := m.new_function('add', [.i32_t, .i32_t], [.i32_t])
	{
		func.local_get(0)
		func.local_get(1)
		func.add(.i32_t)
	}
	m.commit(func, true)
	core := m.compile()
	out := wrap_core_module(core)
	assert is_component(out)
	// Over-arity imports fall back to plain wrapping: no component-level
	// imports and no lifted exports.
	assert arity_section(out, 10).len == 0, '17-param import must not be forwarded'
	assert arity_section(out, 11).len == 0, 'over-arity import must not produce lifted exports'
}

fn test_flat_param_count_helpers() {
	assert core_flat_param_count(CoreFuncType{ params: []u8{len: 16, init: 0x7f} }) == 16
	assert core_flat_param_count(CoreFuncType{ params: []u8{len: 17, init: 0x7f} }) == 17
	assert core_flat_param_count(CoreFuncType{ params: [u8(0x7b)] }) == -1
	assert component_flat_param_count([u8(0x7a)]) == 1
	assert component_flat_param_count([u8(0x73)]) == 2
	assert component_flat_param_count([]u8{len: 8, init: 0x73}) == 16
	assert component_flat_param_count([]u8{len: 9, init: 0x73}) == 18
}
