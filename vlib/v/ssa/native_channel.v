module ssa

import v.flat

// Channels and `select` lower to the runtime in vlib/sync, as the C backend does
// them (gen_channel_init, gen_select and the arrow operators in gen/c).

// channel_function_ref returns a reference to a function of `sync`, which markused
// keeps for a program that uses channels.
fn (mut b Builder) channel_function_ref(name string) ?ValueID {
	id := b.fn_ids[name] or { return none }
	return b.m.add_value(.func_ref, b.void_type, name, id)
}

// channel_operand evaluates a channel expression, through any pointers around it,
// and returns its value with the element's storage type and source type.
fn (mut b Builder) channel_operand(id flat.NodeId) ?(ValueID, TypeID, u32) {
	mut type_id := b.checked_expr_type_name(id)
	if type_id == 0 || type_id == flat.source_type_unknown {
		return none
	}
	mut depth := 0
	for flat.source_type_shape(type_id).has(.pointer) {
		type_id = flat.source_type_syntax(type_id).raw_base()
		depth++
	}
	if !flat.source_type_shape(type_id).has(.channel_type) {
		return none
	}
	channel_type := b.resolve_type(type_id)
	mut value := b.build_expr(id)
	for _ in 0 .. depth {
		value = b.emit1(.load, channel_type, value)
	}
	element_name := flat.source_type_syntax(type_id).raw_base()
	return value, b.resolve_struct_storage_type(element_name, b.cur_module), element_name
}

// build_channel_property lowers `channel.closed`, `.len` and `.cap`.
fn (mut b Builder) build_channel_property(node flat.Node) ?ValueID {
	base_id := b.a.child(&node, 0)
	mut type_id := b.checked_expr_type_name(base_id)
	for flat.source_type_shape(type_id).has(.pointer) {
		type_id = flat.source_type_syntax(type_id).raw_base()
	}
	if type_id == 0 || !flat.source_type_shape(type_id).has(.channel_type) {
		return none
	}
	channel, _, _ := b.channel_operand(base_id)?
	field := node.value_text()
	if field == 'cap' {
		typed := b.emit1(.bitcast, b.resolve_type(flat.source_type_id('&sync.Channel')), channel)
		return b.emit1(.load, b.u32_type, b.get_field_ptr(typed, 'cap'))
	}
	getter := b.channel_function_ref('sync.Channel.${field}')?
	result := if field == 'closed' { b.i1_type } else { b.resolve_type(flat.source_type_int) }
	return b.emit2(.call, result, getter, channel)
}

// channel_slot reserves storage for one element, holding `value`.
fn (mut b Builder) channel_slot(element TypeID, value ValueID) ValueID {
	slot := b.emit0(.alloca, b.m.type_store.get_ptr(element))
	b.emit2(.store, b.void_type, value, slot)
	return slot
}

fn (mut b Builder) as_byte_pointer(value ValueID) ValueID {
	return b.emit1(.bitcast, b.m.type_store.get_ptr(b.i8_type), value)
}

// build_channel_payload converts a send expression to its inline element value.
fn (mut b Builder) build_channel_payload(id flat.NodeId, element TypeID, element_name u32) ValueID {
	value := b.build_expr(id)
	value_type := b.value_type(value)
	if b.is_pointer_type(element) && b.is_pointer_type(value_type)
		&& (element_name == flat.source_type_voidptr
			|| b.type_alias_target(element_name) or { u32(0) } == flat.source_type_voidptr) {
		// A voidptr channel sends the address itself, even for a pointer to a pointer.
		return if value_type == element { value } else { b.emit1(.bitcast, element, value) }
	}
	// Fixed arrays can be represented by their storage pointer, including when
	// a nested array's pointee type differs from the inline element type.
	return b.coerce_store_value(b.fixed_array_value(value, element), element)
}

// build_channel_init lowers `chan T{cap: n}`.
fn (mut b Builder) build_channel_init(node flat.Node) ?ValueID {
	create := b.channel_function_ref('sync.new_channel_st')?
	element := b.resolve_struct_storage_type(flat.source_type_syntax(node.source_value_id()).raw_base(), b.cur_module)
	mut capacity := b.m.get_or_add_const(b.u32_type, '0')
	for i in 0 .. node.children_count {
		field := b.a.child_node(&node, i)
		if field.kind == .field_init && field.value == 'cap' && field.children_count > 0 {
			capacity = b.coerce_value_for_type(b.build_expr(b.a.child(field, 0)), b.u32_type)
		}
	}
	size := if b.m.type_size(element) > 0 { b.m.type_size(element) } else { 1 }
	return b.emit3(.call, b.resolve_type(flat.source_type_id('&sync.Channel')), create, capacity,
		b.m.get_or_add_const(b.u32_type, size.str()))
}

// build_channel_receive lowers `<-channel`. A closed channel leaves the zero value,
// as it does in C.
fn (mut b Builder) build_channel_receive(channel_id flat.NodeId) ?ValueID {
	pop := b.channel_function_ref('sync.Channel.pop')?
	channel, element, element_name := b.channel_operand(channel_id)?
	slot := b.channel_slot(element, b.default_value_for_type(element))
	b.emit3(.call, b.i1_type, pop, channel, b.as_byte_pointer(slot))
	if b.m.type_store.types[element].kind == .array_t {
		// Fixed-array locals keep a pointer to the received array's storage.
		return b.emit1(.bitcast, b.resolve_type(element_name), slot)
	}
	return b.emit1(.load, element, slot)
}

// build_channel_send lowers `channel <- value`.
fn (mut b Builder) build_channel_send(channel_id flat.NodeId, value_id flat.NodeId) bool {
	push := b.channel_function_ref('sync.Channel.push') or { return false }
	channel, element, element_name := b.channel_operand(channel_id) or { return false }
	value := b.build_channel_payload(value_id, element, element_name)
	b.emit3(.call, b.void_type, push, channel, b.as_byte_pointer(b.channel_slot(element, value)))
	return true
}

// build_channel_try_call lowers nonblocking channel methods to the sync runtime,
// passing element storage rather than a value through its opaque pointer argument.
fn (mut b Builder) build_channel_try_call(node flat.Node, callee flat.Node) ?ValueID {
	if callee.children_count == 0 || node.children_count != 2 {
		return none
	}
	channel, element, element_name := b.channel_operand(b.a.child(&callee, 0))?
	method := b.channel_function_ref('sync.Channel.${callee.value_text()}') or {
		panic('ssa: missing channel runtime method `${callee.value_text()}`')
	}
	mut arg_id := b.a.child(&node, 1)
	arg := b.a.nodes[int(arg_id)]
	if arg.kind == .cast_expr && arg.children_count > 0 && !arg.pos.is_valid()
		&& arg.source_value_id() == flat.source_type_voidptr {
		address := b.a.child_node(&arg, 0)
		if address.kind == .prefix && address.op == .amp && address.children_count > 0 {
			// Transform may have adapted a value to the runtime's voidptr parameter.
			// Recover that value before selecting its channel element storage.
			arg_id = b.a.child(address, 0)
		}
	}
	storage := if callee.value_text() == 'try_push' {
		value := b.build_channel_payload(arg_id, element, element_name)
		b.channel_slot(element, value)
	} else if flat.source_type_shape(b.checked_expr_type_name(arg_id)).has(.pointer) {
		b.build_expr(arg_id)
	} else {
		b.build_lvalue_addr(arg_id)
	}
	return b.emit3(.call, b.i32_type, method, channel, b.as_byte_pointer(storage))
}

struct NativeSelectCase {
	branch     flat.Node
	channel_id flat.NodeId
	value_id   flat.NodeId
	lhs_id     flat.NodeId = flat.NodeId(-1)
	body_start int
	is_push    bool
	is_decl    bool
}

// native_select_case reads one branch, or none for a timeout.
fn (b &Builder) native_select_case(branch flat.Node) ?NativeSelectCase {
	first_id := b.a.child(&branch, 0)
	first := b.a.child_node(&branch, 0)
	if first.kind == .infix && first.op == .arrow && first.children_count >= 2 {
		return NativeSelectCase{
			branch:     branch
			channel_id: b.a.child(first, 0)
			value_id:   b.a.child(first, 1)
			body_start: 1
			is_push:    true
		}
	}
	if branch.value in ['recv', 'recv_assign'] && branch.children_count >= 2 {
		second_id := b.a.child(&branch, 1)
		second := b.a.child_node(&branch, 1)
		if second.kind == .prefix && second.op == .arrow && second.children_count > 0 {
			return NativeSelectCase{
				branch:     branch
				channel_id: b.a.child(second, 0)
				value_id:   second_id
				lhs_id:     first_id
				body_start: 2
				is_decl:    branch.value == 'recv'
			}
		}
	}
	if first.kind == .prefix && first.op == .arrow && first.children_count > 0 {
		return NativeSelectCase{
			branch:     branch
			channel_id: b.a.child(first, 0)
			value_id:   first_id
			body_start: 1
		}
	}
	return none
}

// new_channel_select_array makes the array that sync.channel_select reads, and
// returns the slot that holds it.
fn (mut b Builder) new_channel_select_array(element_size int, values []ValueID, element TypeID) ValueID {
	zero := b.m.get_or_add_const(b.i64_type, '0')
	create := b.m.add_value(.func_ref, b.array_type, 'array_new', b.runtime_fn_id('array_new'))
	created := b.emit4(.call, b.array_type, create, b.m.get_or_add_const(b.i64_type, element_size.str()),
		zero, b.m.get_or_add_const(b.i64_type, values.len.str()))
	slot := b.emit0(.alloca, b.m.type_store.get_ptr(b.array_type))
	b.emit2(.store, b.void_type, created, slot)
	push := b.m.add_value(.func_ref, b.void_type, 'array_push', b.runtime_fn_id('array_push'))
	for value in values {
		b.emit3(.call, b.void_type, push, slot, b.as_byte_pointer(b.channel_slot(element, value)))
	}
	return slot
}

// build_channel_select lowers a `select`. As an expression it says whether a case
// or the timeout ran, and is false once every channel is closed.
fn (mut b Builder) build_channel_select(node flat.Node, is_expr bool) ?ValueID {
	select_name := if is_expr { 'sync.channel_select' } else { 'sync.channel_select_lang' }
	select_fn := b.channel_function_ref(select_name)?
	mut cases := []NativeSelectCase{cap: int(node.children_count)}
	mut fallback := flat.Node{}
	mut has_fallback := false
	mut timeout_id := flat.NodeId(-1)
	for i in 0 .. node.children_count {
		branch := *b.a.child_node(&node, i)
		if branch.kind != .select_branch {
			continue
		}
		if branch.value != 'else' {
			if select_case := b.native_select_case(branch) {
				cases << select_case
				continue
			}
			timeout_id = b.a.child(&branch, 0)
		}
		fallback = branch
		has_fallback = true
	}
	int_type := b.resolve_type(flat.source_type_int)
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	mut slots := []ValueID{cap: cases.len}
	mut elements := []TypeID{cap: cases.len}
	mut channels := []ValueID{cap: cases.len}
	mut directions := []ValueID{cap: cases.len}
	mut objects := []ValueID{cap: cases.len}
	for select_case in cases {
		channel, element, element_name := b.channel_operand(select_case.channel_id)?
		value := if select_case.is_push {
			b.build_channel_payload(select_case.value_id, element, element_name)
		} else {
			b.default_value_for_type(element)
		}
		slot := b.channel_slot(element, value)
		slots << slot
		elements << element
		channels << channel
		directions << b.m.get_or_add_const(int_type, if select_case.is_push { '1' } else { '0' })
		objects << b.as_byte_pointer(slot)
	}
	timeout := if int(timeout_id) >= 0 {
		b.build_expr(timeout_id)
	} else if has_fallback {
		b.m.get_or_add_const(b.i64_type, '-1')
	} else {
		b.m.get_or_add_const(b.i64_type, max_i64.str())
	}
	mut result := ValueID(0)
	sleep_fn := b.channel_function_ref('time.sleep') or { ValueID(0) }
	if cases.len == 0 && int(timeout_id) >= 0 && sleep_fn != 0 {
		// Only a timeout: wait it out, as the C backend does. The select
		// runtime would report "all channels closed" (-2) at once instead.
		b.emit2(.call, b.void_type, sleep_fn, timeout)
		result = b.m.get_or_add_const(int_type, '-1')
	} else {
		channels_slot := b.new_channel_select_array(8, channels, ptr_i8)
		directions_slot := b.new_channel_select_array(b.m.type_size(int_type), directions, int_type)
		objects_slot := b.new_channel_select_array(8, objects, ptr_i8)
		result = b.m.add_instr(.call, b.cur_block, int_type, [
			select_fn,
			channels_slot,
			b.emit1(.load, b.array_type, directions_slot),
			objects_slot,
			timeout,
		])
		// The arrays only carry the call's arguments; free them like the C
		// backend, so a `select` in a loop does not leak without the collector.
		free_fn := b.channel_function_ref('array__free') or {
			b.channel_function_ref('array.free') or { ValueID(0) }
		}
		if free_fn != 0 {
			for slot in [channels_slot, directions_slot, objects_slot] {
				b.emit2(.call, b.void_type, free_fn, slot)
			}
		}
	}
	merge := b.m.add_block(b.cur_func, 'select_merge')
	for i, select_case in cases {
		body := b.m.add_block(b.cur_func, 'select_case')
		next := b.m.add_block(b.cur_func, 'select_next')
		matched := b.emit2(.eq, b.i1_type, result, b.m.get_or_add_const(int_type, i.str()))
		b.emit3(.br, b.void_type, matched, ValueID(body), ValueID(next))
		b.cur_block = body
		scope := b.save_scope()
		if !select_case.is_push && int(select_case.lhs_id) >= 0 {
			b.bind_channel_select_value(select_case, slots[i], elements[i])
		}
		b.build_channel_select_body(select_case.branch, select_case.body_start, merge)
		b.restore_scope(scope)
		b.cur_block = next
	}
	if has_fallback {
		body := b.m.add_block(b.cur_func, 'select_fallback')
		next := b.m.add_block(b.cur_func, 'select_next')
		timed_out := b.emit2(.eq, b.i1_type, result, b.m.get_or_add_const(int_type, '-1'))
		// A statement also runs `else` when every channel is closed; an expression
		// reports that as false instead.
		ran := if int(timeout_id) < 0 && !is_expr {
			closed := b.emit2(.eq, b.i1_type, result, b.m.get_or_add_const(int_type, '-2'))
			b.emit2(.or_, b.i1_type, timed_out, closed)
		} else {
			timed_out
		}
		b.emit3(.br, b.void_type, ran, ValueID(body), ValueID(next))
		b.cur_block = body
		scope := b.save_scope()
		b.build_channel_select_body(fallback, if fallback.value == 'else' { 0 } else { 1 }, merge)
		b.restore_scope(scope)
		b.cur_block = next
	}
	b.emit1(.jmp, b.void_type, ValueID(merge))
	b.cur_block = merge
	return b.emit2(.ne, b.i1_type, result, b.m.get_or_add_const(int_type, '-2'))
}

fn (mut b Builder) bind_channel_select_value(select_case NativeSelectCase, slot ValueID, element TypeID) {
	lhs := b.a.node(select_case.lhs_id)
	if lhs.kind == .ident && lhs.value_text() == '_' {
		return
	}
	value := b.emit1(.load, element, slot)
	if select_case.is_decl && lhs.kind == .ident {
		b.vars.set(lhs.value_text(), b.channel_slot(element, value))
		return
	}
	address := b.build_lvalue_addr(select_case.lhs_id)
	b.emit2(.store, b.void_type, b.coerce_value_for_type(value, b.deref_type(address)), address)
}

fn (mut b Builder) build_channel_select_body(branch flat.Node, start int, merge BlockID) {
	for i in start .. branch.children_count {
		b.build_stmt(b.a.child(&branch, i))
	}
	if !b.current_block_terminated() {
		b.emit1(.jmp, b.void_type, ValueID(merge))
	}
}
