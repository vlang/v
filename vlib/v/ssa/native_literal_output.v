module ssa

import v.flat

struct NativeLiteralOutput {
	fd   int
	text string
	line bool
}

// build_literal_output lowers the driver's checked literal-only program without
// registering the allocator, globals, formatting, or other V runtime helpers.
fn build_literal_output(a &flat.FlatAst, files map[string]bool, target TargetData, track_uses bool) &Module {
	mut calls := []NativeLiteralOutput{}
	for id, node in a.nodes {
		if node.kind == .file && node.value in files {
			collect_native_literal_output(a, flat.NodeId(id), mut calls)
		}
	}
	calls = calls.filter(it.text.len > 0 || it.line)
	mut m := Module.new()
	m.target = target
	m.track_uses = track_uses
	i32_type := m.type_store.get_int(32)
	i64_type := m.type_store.get_int(64)
	ptr := m.type_store.get_ptr(m.type_store.get_int(8))
	mut b := Builder{
		m:        m
		i32_type: i32_type
		i64_type: i64_type
		i1_type:  m.type_store.get_int(1)
	}
	main := m.new_function('main', i32_type)
	entry := m.add_block(main, 'entry')
	if calls.len == 1 && calls[0].fd == 1 && calls[0].line && !calls[0].text.contains('\x00') {
		b.register_extern('puts', i32_type, [ptr])
		callee := m.add_value(.func_ref, i32_type, 'puts', b.fn_ids['puts'])
		text := m.add_value(.string_literal, ptr, calls[0].text, 0)
		m.add_instr(.call, entry, i32_type, [callee, text])
	} else if calls.len > 0 {
		write := b.literal_write_all(ptr)
		for call in calls {
			text := if call.line { call.text + '\n' } else { call.text }
			if text.len == 0 {
				continue
			}
			fd := m.get_or_add_const(i32_type, call.fd.str())
			data := m.add_value(.string_literal, ptr, text, 0)
			len := m.get_or_add_const(i64_type, text.len.str())
			m.add_instr(.call, entry, TypeID(0), [write, fd, data, len])
		}
	}
	zero := m.get_or_add_const(i32_type, '0')
	m.add_instr(.ret, entry, TypeID(0), [zero])
	return m
}

fn collect_native_literal_output(a &flat.FlatAst, id flat.NodeId, mut calls []NativeLiteralOutput) {
	node := a.node(id)
	if node.kind == .call {
		callee := a.child_node(node, 0)
		arg := a.child_node(node, 1)
		calls << NativeLiteralOutput{
			fd:   if callee.value.starts_with('e') { 2 } else { 1 }
			text: arg.value
			line: callee.value.ends_with('ln')
		}
		return
	}
	for i in 0 .. node.children_count {
		collect_native_literal_output(a, a.child(node, i), mut calls)
	}
}

// literal_write_all preserves embedded NULs and retries short writes, as the C
// literal-output path does. An I/O error ends this print call without looping.
fn (mut b Builder) literal_write_all(ptr TypeID) ValueID {
	b.register_extern('write', b.i64_type, [b.i32_type, ptr, b.i64_type])
	func := b.register_synthetic_function('__literal_write_all', b.void_type,
		[b.i32_type, ptr, b.i64_type])
	entry := b.m.add_block(func, 'entry')
	fd := b.func_add_argument(func, b.i32_type, 'fd')
	data := b.func_add_argument(func, ptr, 'data')
	len := b.func_add_argument(func, b.i64_type, 'len')
	p := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(ptr))
	n := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.i64_type))
	b.block_instr2(.store, entry, b.void_type, data, p)
	b.block_instr2(.store, entry, b.void_type, len, n)
	header := b.m.add_block(func, 'header')
	body := b.m.add_block(func, 'body')
	advance := b.m.add_block(func, 'advance')
	exit := b.m.add_block(func, 'exit')
	b.block_instr1(.jmp, entry, b.void_type, ValueID(header))
	remaining := b.block_instr1(.load, header, b.i64_type, n)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	more := b.block_instr2(.ne, header, b.i1_type, remaining, zero)
	b.block_instr3(.br, header, b.void_type, more, ValueID(body), ValueID(exit))
	current := b.block_instr1(.load, body, ptr, p)
	write := b.m.add_value(.func_ref, b.i64_type, 'write', b.fn_ids['write'])
	written := b.block_instr4(.call, body, b.i64_type, write, fd, current, remaining)
	progress := b.block_instr2(.gt, body, b.i1_type, written, zero)
	b.block_instr3(.br, body, b.void_type, progress, ValueID(advance), ValueID(exit))
	next := b.block_instr2(.get_element_ptr, advance, ptr, current, written)
	left := b.block_instr2(.sub, advance, b.i64_type, remaining, written)
	b.block_instr2(.store, advance, b.void_type, next, p)
	b.block_instr2(.store, advance, b.void_type, left, n)
	b.block_instr1(.jmp, advance, b.void_type, ValueID(header))
	b.block_instr0(.ret, exit, b.void_type)
	return b.m.add_value(.func_ref, b.void_type, '__literal_write_all', func)
}
