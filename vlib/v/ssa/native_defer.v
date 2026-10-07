module ssa

import v.flat

fn (mut b Builder) initialize_deferred_stmts(body_ids []flat.NodeId) {
	mut pending := []flat.NodeId{}
	for i := body_ids.len; i > 0; i-- {
		pending << body_ids[i - 1]
	}
	zero := b.m.get_or_add_const(b.i1_type, '0')
	for pending.len > 0 {
		id := pending.pop()
		if !b.valid_node_id(id) {
			continue
		}
		node := b.a.nodes[int(id)]
		if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			continue
		}
		if node.kind == .defer_stmt {
			if node.children_count > 0 {
				body_id := b.a.child(&node, 0)
				if body_id !in b.defer_active_slots {
					active := b.emit0(.alloca, b.m.type_store.get_ptr(b.i1_type))
					b.emit2(.store, b.void_type, zero, active)
					b.defer_body_ids << body_id
					b.defer_active_slots[body_id] = active
				}
			}
			continue
		}
		for i := node.children_count; i > 0; i-- {
			pending << b.a.child(&node, i - 1)
		}
	}
}

fn (mut b Builder) activate_deferred_stmt(body_id flat.NodeId) {
	if active := b.defer_active_slots[body_id] {
		one := b.m.get_or_add_const(b.i1_type, '1')
		b.emit2(.store, b.void_type, one, active)
	}
}

fn (mut b Builder) emit_deferred_stmts() {
	for i := b.defer_body_ids.len; i > 0; i-- {
		body_id := b.defer_body_ids[i - 1]
		active_slot := b.defer_active_slots[body_id] or { continue }
		active := b.emit1(.load, b.i1_type, active_slot)
		run_block := b.m.add_block(b.cur_func, 'defer_run')
		next_block := b.m.add_block(b.cur_func, 'defer_next')
		b.emit3(.br, b.void_type, active, ValueID(run_block), ValueID(next_block))
		b.cur_block = run_block
		zero := b.m.get_or_add_const(b.i1_type, '0')
		b.emit2(.store, b.void_type, zero, active_slot)
		body := b.a.nodes[int(body_id)]
		if body.kind == .block {
			for j in 0 .. body.children_count {
				b.build_stmt(b.a.child(&body, j))
				if b.current_block_terminated() {
					break
				}
			}
		} else {
			b.build_stmt(body_id)
		}
		if !b.current_block_terminated() {
			b.emit1(.jmp, b.void_type, ValueID(next_block))
		}
		b.cur_block = next_block
	}
}
