module optimize

import v.ssa

// build_pass_sample builds a deterministic module exercising allocation
// promotion, constant folding, branches, phis, and dead code.
fn build_pass_sample() &ssa.Module {
	mut m := ssa.Module.new()
	i32_t := m.type_store.get_int(32)
	ptr_t := m.type_store.get_ptr(i32_t)
	fid := m.new_function('f', i32_t)
	arg := m.add_value(.argument, i32_t, 'x', 0)
	m.func_add_param(fid, arg)
	entry := m.add_block(fid, 'entry')
	body := m.add_block(fid, 'body')
	exit_blk := m.add_block(fid, 'exit')
	slot := m.add_instr(.alloca, entry, ptr_t, [])
	zero := m.get_or_add_const(i32_t, '0')
	one := m.get_or_add_const(i32_t, '1')
	s := m.add_instr(.add, entry, i32_t, [one, one])
	_ = m.add_instr(.mul, entry, i32_t, [one, one])
	m.add_instr(.store, entry, 0, [s, slot])
	cmp := m.add_instr(.gt, entry, i32_t, [arg, zero])
	m.add_instr(.br, entry, 0, [cmp, ssa.ValueID(body), ssa.ValueID(exit_blk)])
	lv := m.add_instr(.load, body, i32_t, [slot])
	m.add_instr(.jmp, body, 0, [ssa.ValueID(exit_blk)])
	phi := m.add_instr(.phi, exit_blk, i32_t, [lv, ssa.ValueID(body), s, ssa.ValueID(entry)])
	m.add_instr(.ret, exit_blk, i32_t, [phi])
	return m
}

// assert_modules_equal asserts deep structural equality of two modules.
fn assert_modules_equal(a &ssa.Module, b &ssa.Module) {
	assert a.type_store.types.len == b.type_store.types.len
	for i, t in a.type_store.types {
		u := b.type_store.types[i]
		assert t.kind == u.kind
		assert t.width == u.width
		assert t.is_unsigned == u.is_unsigned
		assert t.elem_type == u.elem_type
		assert t.len == u.len
		assert t.fields == u.fields
		assert t.field_names == u.field_names
		assert t.params == u.params
		assert t.ret_type == u.ret_type
		assert t.is_c_struct == u.is_c_struct
		assert t.is_union == u.is_union
		assert t.is_packed == u.is_packed
		assert t.alignment == u.alignment
		assert t.size == u.size
		assert t.field_offsets == u.field_offsets
	}
	assert a.values.len == b.values.len
	for i, v in a.values {
		w := b.values[i]
		assert v.id == w.id
		assert v.kind == w.kind
		assert v.typ == w.typ
		assert v.name == w.name
		assert v.index == w.index
		assert v.uses == w.uses
	}
	assert a.instrs.len == b.instrs.len
	for i, ins in a.instrs {
		j := b.instrs[i]
		assert ins.op == j.op
		assert ins.operands == j.operands
		assert ins.block == j.block
		assert ins.typ == j.typ
		assert ins.atomic_ord == j.atomic_ord
		assert ins.inline == j.inline
		assert ins.asm_id == j.asm_id
	}
	assert a.blocks.len == b.blocks.len
	for i, blk in a.blocks {
		c := b.blocks[i]
		assert blk.id == c.id
		assert blk.name == c.name
		assert blk.parent == c.parent
		assert blk.instrs == c.instrs
		assert blk.preds == c.preds
		assert blk.succs == c.succs
		assert blk.idom == c.idom
		assert blk.dom_tree == c.dom_tree
	}
	assert a.funcs.len == b.funcs.len
	for i, f in a.funcs {
		g := b.funcs[i]
		assert f.id == g.id
		assert f.name == g.name
		assert f.typ == g.typ
		assert f.blocks == g.blocks
		assert f.params == g.params
		assert f.is_c_extern == g.is_c_extern
		assert f.is_variadic == g.is_variadic
		assert f.is_prototype == g.is_prototype
		assert f.linkage == g.linkage
		assert f.call_conv == g.call_conv
	}
	assert a.globals.len == b.globals.len
	assert a.c_struct_names.len == b.c_struct_names.len
	assert a.export_c_names.len == b.export_c_names.len
}

fn run_default_manager(mut m ssa.Module, opts OptimizeOptions) {
	m.track_uses = true
	mut pm := PassManager.new(default_pipeline(opts))
	pm.run(mut m, opts)
}

fn test_pass_manager_matches_handwritten_pipeline() {
	opts := OptimizeOptions{
		mem2reg:        true
		eliminate_phis: true
	}
	mut a := build_pass_sample()
	optimize_with_options(mut a, opts)
	mut b := build_pass_sample()
	run_default_manager(mut b, opts)
	assert_modules_equal(a, b)
	assert verify(b).len == 0

	plain := OptimizeOptions{}
	mut c := build_pass_sample()
	optimize_with_options(mut c, plain)
	mut d := build_pass_sample()
	run_default_manager(mut d, plain)
	assert_modules_equal(c, d)
	assert verify(d).len == 0
}

fn test_default_pipeline_lists_expected_passes() {
	full := default_pipeline(OptimizeOptions{
		mem2reg:        true
		eliminate_phis: true
	}).map(it.label())
	assert full == ['input', 'inline', '', '', '', 'compute_dominators', 'mem2reg', 'simplify_phi',
		'cse', 'memory', '', 'eliminate_phi', '', 'final']
	plain := default_pipeline(OptimizeOptions{}).map(it.label())
	assert plain == ['input', '', '', '', 'final']
}

fn test_phi_elimination_splits_only_edges_entering_phi_blocks() {
	for with_phi in [false, true] {
		mut m := ssa.Module.new()
		int_type := m.type_store.get_int(32)
		bool_type := m.type_store.get_int(1)
		function := m.new_function('select', int_type)
		condition := m.add_value(.argument, bool_type, 'condition', 0)
		m.func_add_param(function, condition)
		entry := m.add_block(function, 'entry')
		body := m.add_block(function, 'body')
		join := m.add_block(function, 'join')
		seven := m.get_or_add_const(int_type, '7')
		nine := m.get_or_add_const(int_type, '9')
		branch := m.add_instr(.br, entry, 0, [condition, ssa.ValueID(body), ssa.ValueID(join)])
		m.add_instr(.jmp, body, 0, [ssa.ValueID(join)])
		result := if with_phi {
			m.add_instr(.phi, join, int_type, [seven, ssa.ValueID(entry), nine, ssa.ValueID(body)])
		} else {
			seven
		}
		m.add_instr(.ret, join, 0, [result])

		optimize_with_options(mut m, OptimizeOptions{
			mem2reg:          true
			eliminate_phis:   true
			verify_each_pass: true
			strict_verify:    true
		})

		branch_targets := m.instrs[m.values[branch].index].operands
		if with_phi {
			assert m.funcs[function].blocks.len == 4
			split := m.blocks[branch_targets[2]]
			assert split.preds == [entry]
			assert split.succs == [join]
			assert split.instrs.len == 2
			copy := m.instrs[m.values[split.instrs[0]].index]
			assert copy.op == .assign
			assert copy.operands == [result, seven]
		} else {
			assert m.funcs[function].blocks == [entry, body, join]
			assert branch_targets[2] == join
		}
	}
}
