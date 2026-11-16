module ssa

// Per-thread globals, for the same names the C backend declares thread-local
// (global_is_thread_local and the `g_memory_block` case in gen/c/cleanc.v). A native
// binary has no thread-local section, so each global is a heap slot per thread,
// found through a pthread key, as the C backend does for tcc
// (emit_tinyc_pthread_pointer_slot).
//
// The slots come from `v_native_tls_calloc` and go back through
// `v_native_tls_free`, the key's destructor, when the thread exits. The arm64
// backend binds those names to libc's calloc/free, or, with the collector
// linked in, to uncollectable, scanned Boehm blocks (vlib/builtin/gc_boehm_helpers.c):
// Boehm cannot see thread-specific data, so a collectable slot reachable only
// through its pthread key could be reclaimed while the thread still uses it.

// native_tls_calloc and native_tls_free allocate and release the per-thread
// slots; see gen/arm64 arm64_native_tls_symbols.
const native_tls_calloc = 'v_native_tls_calloc'
const native_tls_free = 'v_native_tls_free'

const native_thread_local_globals = ['g_memory_block', 'g_arena_top', 'g_autostr_addr_state',
	'g_panic_state']

fn native_global_is_thread_local(name string) bool {
	return name in native_thread_local_globals || name.contains('__anon_fn_')
}

// lower_native_thread_locals points every use of a per-thread global at its slot.
// A function asks for the slot once, at its entry: it cannot change threads.
fn (mut b Builder) lower_native_thread_locals() {
	main_id := b.fn_ids['main'] or { return }
	// Hot-function and signature-only builds leave main as a prototype.
	if b.m.funcs[main_id].blocks.len == 0 {
		return
	}
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut slot_functions := map[int]int{}
	mut value_types := map[int]TypeID{}
	mut is_thread_local := []bool{len: b.m.values.len}
	mut key_slots := []ValueID{}
	mut function_count := b.m.funcs.len
	for value_id in 1 .. b.m.values.len {
		value := b.m.values[value_id]
		if value.kind != .global || value.index < 0 || value.index >= b.m.globals.len {
			continue
		}
		global := b.m.globals[value.index]
		if global.linkage == .external || !native_global_is_thread_local(global.name) {
			continue
		}
		is_thread_local[value_id] = true
		value_types[value_id] = value.typ
		if value.index !in slot_functions {
			if native_tls_calloc !in b.fn_ids {
				b.register_extern(native_tls_calloc, ptr_i8, [b.i64_type, b.i64_type])
				b.register_extern(native_tls_free, b.void_type, [ptr_i8])
			}
			key_slot := b.m.add_global('__ssa_tls_key_${global.name}', b.u64_type)
			key_slots << key_slot
			slot_functions[value.index] = b.generate_thread_local_slot(global.name, key_slot,
				b.m.type_size(global.typ), value.typ)
		}
	}
	if key_slots.len == 0 {
		return
	}
	mut slots := map[int]ValueID{}
	for function_id in 0 .. function_count {
		if b.m.funcs[function_id].blocks.len == 0 {
			continue
		}
		slots.clear()
		entry := b.m.funcs[function_id].blocks[0]
		for block in b.m.funcs[function_id].blocks {
			for value_id in b.m.blocks[block].instrs {
				index := b.m.values[value_id].index
				for operand_index in 0 .. b.m.instrs[index].operands.len {
					operand := int(b.m.instrs[index].operands[operand_index])
					if operand <= 0 || operand >= is_thread_local.len || !is_thread_local[operand]
						|| !b.m.instrs[index].is_value_operand(operand_index) {
						continue
					}
					if operand !in slots {
						global_index := b.m.values[operand].index
						slot_function := slot_functions[global_index]
						name := b.m.funcs[slot_function].name
						callee := b.m.add_value(.func_ref, b.void_type, name, slot_function)
						slots[operand] = b.m.add_instr_front(.call, entry, value_types[operand], [
							callee,
						])
					}
					b.m.instrs[index].operands[operand_index] = slots[operand]
				}
			}
		}
	}
	release_id := b.generate_thread_local_release()
	init_id := b.register_synthetic_function('__ssa_tls_init', b.void_type, []TypeID{})
	init_entry := b.m.add_block(init_id, 'entry')
	create := b.m.add_value(.func_ref, b.void_type, 'pthread_key_create', b.fn_ids['pthread_key_create'])
	release := b.m.add_value(.func_ref, ptr_i8, '__ssa_tls_release', release_id)
	for key_slot in key_slots {
		key_pointer := b.block_instr1(.bitcast, init_entry, ptr_i8, key_slot)
		b.block_instr3(.call, init_entry, b.i32_type, create, key_pointer, release)
	}
	b.block_instr0(.ret, init_entry, b.void_type)
	main_entry := b.m.funcs[main_id].blocks[0]
	init_ref := b.m.add_value(.func_ref, b.void_type, '__ssa_tls_init', init_id)
	b.m.add_instr_front(.call, main_entry, b.void_type, [init_ref])
}

// generate_thread_local_slot defines the function that returns the calling
// thread's copy of a global, zeroed the first time the thread asks.
fn (mut b Builder) generate_thread_local_slot(name string, key_slot ValueID, size int, slot_type TypeID) int {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	func_id := b.register_synthetic_function('__ssa_tls_slot_${name}', slot_type, []TypeID{})
	entry := b.m.add_block(func_id, 'entry')
	ready := b.m.add_block(func_id, 'ready')
	create := b.m.add_block(func_id, 'create')
	key := b.block_instr1(.load, entry, b.u64_type, key_slot)
	get_ref := b.m.add_value(.func_ref, b.void_type, 'pthread_getspecific', b.fn_ids['pthread_getspecific'])
	existing := b.block_instr2(.call, entry, ptr_i8, get_ref, key)
	found := b.block_instr2(.ne, entry, b.i1_type, existing, b.m.get_or_add_const(ptr_i8, '0'))
	b.block_instr3(.br, entry, b.void_type, found, ValueID(ready), ValueID(create))
	b.block_instr1(.ret, ready, b.void_type, b.block_instr1(.bitcast, ready, slot_type, existing))
	calloc_ref := b.m.add_value(.func_ref, ptr_i8, native_tls_calloc, b.fn_ids[native_tls_calloc])
	one := b.m.get_or_add_const(b.i64_type, '1')
	bytes := b.m.get_or_add_const(b.i64_type, (if size > 0 { size } else { 8 }).str())
	storage := b.block_instr3(.call, create, ptr_i8, calloc_ref, one, bytes)
	set_ref := b.m.add_value(.func_ref, b.void_type, 'pthread_setspecific', b.fn_ids['pthread_setspecific'])
	b.block_instr3(.call, create, b.i32_type, set_ref, key, storage)
	b.block_instr1(.ret, create, b.void_type, b.block_instr1(.bitcast, create, slot_type, storage))
	return func_id
}

// generate_thread_local_release defines the pthread key destructor that frees a
// thread's slot when the thread exits.
fn (mut b Builder) generate_thread_local_release() int {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	func_id := b.register_synthetic_function('__ssa_tls_release', b.void_type, [ptr_i8])
	entry := b.m.add_block(func_id, 'entry')
	storage := b.func_add_argument(func_id, ptr_i8, 'storage')
	free_ref := b.m.add_value(.func_ref, b.void_type, native_tls_free, b.fn_ids[native_tls_free])
	b.block_instr2(.call, entry, b.void_type, free_ref, storage)
	b.block_instr0(.ret, entry, b.void_type)
	return func_id
}
