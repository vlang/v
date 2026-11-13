module arm64

import v.ssa

// big_struct_type is the probed layout of `struct big { long a, b, c; }` bound
// in V as `struct C.big { c i64 }`: 24 bytes, `c` at offset 16.
fn big_struct_type(mut m ssa.Module) ssa.TypeID {
	i64_type := m.type_store.get_int(64)
	typ := m.type_store.register(ssa.Type{
		kind:          .struct_t
		fields:        [i64_type]
		field_names:   ['c']
		is_c_struct:   true
		alignment:     8
		size:          24
		field_offsets: [16]
	})
	m.c_struct_names[int(typ)] = 'struct big'
	return typ
}

fn c_extern_fn(mut m ssa.Module, name string, ret ssa.TypeID) int {
	id := m.new_function('C.${name}', ret)
	mut f := m.funcs[id]
	f.is_c_extern = true
	m.funcs[id] = f
	return id
}

fn test_c_wrapper_keeps_probed_layout_of_struct_result() {
	mut m := ssa.Module.new()
	big := big_struct_type(mut m)
	make_big := c_extern_fn(mut m, 'make_big', big)
	caller := m.new_function('caller', ssa.TypeID(0))
	entry := m.add_block(caller, 'entry')
	callee := m.add_value(.func_ref, ssa.TypeID(0), 'make_big', make_big)
	m.add_instr(.call, entry, big, [callee])
	m.add_instr(.ret, entry, 0, [])
	mut g := Gen.new(m)
	source := g.c_function_wrappers({
		'make_big': CFunctionKind.internal
	})
	// The twin is 24 bytes with the field at 16, so C returns it through x8
	// as the native caller expects, and the copy keeps `c`.
	assert source.contains('struct __v_cfn_t${int(big)} { unsigned char f_pad0[16]; int64_t f0; }')
	assert source.contains('__typeof__(make_big()) v = make_big();')
	assert g.m.c_extern_symbols['make_big'] == '__v_cfn_make_big'
}

fn test_c_wrapper_passes_struct_argument_as_its_c_type() {
	mut m := ssa.Module.new()
	big := big_struct_type(mut m)
	i64_type := m.type_store.get_int(64)
	take_big := c_extern_fn(mut m, 'take_big', i64_type)
	caller := m.new_function('caller', ssa.TypeID(0))
	entry := m.add_block(caller, 'entry')
	arg := m.add_instr(.struct_init, entry, big, [])
	callee := m.add_value(.func_ref, ssa.TypeID(0), 'take_big', take_big)
	m.add_instr(.call, entry, i64_type, [callee, arg])
	m.add_instr(.ret, entry, 0, [])
	mut g := Gen.new(m)
	source := g.c_function_wrappers({
		'take_big': CFunctionKind.internal
	})
	assert source.contains('struct big p0; __builtin_memcpy(&p0, &a0, sizeof(p0));')
	assert source.contains('return (int64_t)take_big(p0);')
	// No call through a cast function pointer: C passes `struct big` by its
	// own ABI.
	assert !source.contains('(*)(')
}

fn test_c_wrapper_keeps_float_aggregate_twin_unpadded() {
	mut m := ssa.Module.new()
	f64_type := m.type_store.get_float(64)
	pair := m.type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      [f64_type, f64_type]
		field_names: ['x', 'y']
		is_c_struct: true
	})
	make_pair := c_extern_fn(mut m, 'make_pair', pair)
	caller := m.new_function('caller', ssa.TypeID(0))
	entry := m.add_block(caller, 'entry')
	callee := m.add_value(.func_ref, ssa.TypeID(0), 'make_pair', make_pair)
	m.add_instr(.call, entry, pair, [callee])
	m.add_instr(.ret, entry, 0, [])
	mut g := Gen.new(m)
	source := g.c_function_wrappers({
		'make_pair': CFunctionKind.macro
	})
	// Padding would stop C from treating the twin as an HFA.
	assert source.contains('struct __v_cfn_t${int(pair)} { double f0; double f1; }')
}

// pair_function defines `fn pair_fn(p Pair) Pair` or `fn pair_fn(p Pair) f64`,
// exported to C as `pair_c`.
fn pair_function(mut m ssa.Module, returns_pair bool) (ssa.TypeID, int) {
	f64_type := m.type_store.get_float(64)
	pair := m.type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      [f64_type, f64_type]
		field_names: ['x', 'y']
	})
	ret_type := if returns_pair { pair } else { f64_type }
	f := m.new_function('main.pair_fn', ret_type)
	param := m.add_value(.argument, pair, 'p', 0)
	m.func_add_param(f, param)
	entry := m.add_block(f, 'entry')
	if returns_pair {
		m.add_instr(.ret, entry, 0, [param])
	} else {
		m.add_instr(.ret, entry, 0, [m.get_or_add_const(f64_type, '1.5')])
	}
	m.export_aliases['main.pair_fn'] = 'pair_c'
	return pair, f
}

fn text_words(g &Gen) []u32 {
	mut words := []u32{}
	for i := 0; i < g.macho.text_data.len; i += 4 {
		words << read_u32_le(g.macho.text_data, i)
	}
	return words
}

fn words_contain_in_order(words []u32, wanted []u32) bool {
	mut next := 0
	for word in words {
		if next < wanted.len && word == wanted[next] {
			next++
		}
	}
	return next == wanted.len
}

fn test_export_with_float_aggregate_argument_gets_c_entry() {
	mut m := ssa.Module.new()
	_, _ = pair_function(mut m, false)
	mut g := Gen.new(m)
	g.gen()
	body := g.macho.symbols[g.macho.sym_by_name['_main.pair_fn']]
	entry := g.macho.symbols[g.macho.sym_by_name['_pair_c']]
	assert entry.value != body.value
	assert 'pair_c' in g.export_entries
	words := text_words(g)[int(entry.value) / 4..]
	// The pair arrives in d0/d1: saved, rebuilt in the frame, then loaded into
	// x0/x1 for the native body.
	assert words_contain_in_order(words, [
		asm_stp_d_offset(0, 1, sp, arm64_export_saved_d / 8),
		asm_ldr_imm(Reg(9), sp, arm64_export_saved_d / 8),
		asm_str_imm(Reg(9), sp, arm64_export_buffers / 8),
		asm_ldr_imm(Reg(9), sp, arm64_export_saved_d / 8 + 1),
		asm_str_imm(Reg(9), sp, arm64_export_buffers / 8 + 1),
		asm_ldr_imm(Reg(0), sp, arm64_export_buffers / 8),
		asm_ldr_imm(Reg(1), sp, arm64_export_buffers / 8 + 1),
		asm_ret(),
	])
}

fn test_export_with_float_aggregate_result_returns_in_d_registers() {
	mut m := ssa.Module.new()
	_, _ = pair_function(mut m, true)
	mut g := Gen.new(m)
	g.gen()
	entry := g.macho.symbols[g.macho.sym_by_name['_pair_c']]
	words := text_words(g)[int(entry.value) / 4..]
	buffer := arm64_export_buffers + 16
	assert words_contain_in_order(words, [
		asm_stp_offset(Reg(0), Reg(1), sp, i32(buffer / 8)),
		asm_ldr_d_imm(0, sp, u32(buffer / 8)),
		asm_ldr_d_imm(1, sp, u32(buffer / 8 + 1)),
		asm_ret(),
	])
}

fn test_export_without_float_aggregates_keeps_the_alias() {
	mut m := ssa.Module.new()
	i64_type := m.type_store.get_int(64)
	f := m.new_function('main.plain', i64_type)
	param := m.add_value(.argument, i64_type, 'x', 0)
	m.func_add_param(f, param)
	entry := m.add_block(f, 'entry')
	m.add_instr(.ret, entry, 0, [param])
	m.export_aliases['main.plain'] = 'plain_c'
	mut g := Gen.new(m)
	g.gen()
	body := g.macho.symbols[g.macho.sym_by_name['_main.plain']]
	alias := g.macho.symbols[g.macho.sym_by_name['_plain_c']]
	assert alias.value == body.value
}

fn test_native_tls_slots_bind_to_uncollectable_blocks_with_the_collector() {
	m := ssa.Module.new()
	mut g := Gen.new(m)
	assert g.c_symbol_name('v_native_tls_calloc', true) == 'calloc'
	assert g.c_symbol_name('v_native_tls_free', true) == 'free'
	g.gc_runtime = true
	assert g.c_symbol_name('v_native_tls_calloc', true) == 'v_native_gc_tls_calloc'
	assert g.c_symbol_name('v_native_tls_free', true) == 'v_native_gc_tls_free'
	// Everything else V allocates stays collectable.
	assert g.c_symbol_name('calloc', true) == 'v_native_gc_calloc'
}
