module optimize

import v.ssa

// Common subexpression elimination.
//
// Go's cse.go (cmd/compile/internal/ssa) partitions values into equivalence
// classes and rewrites a value to an equal one that dominates it. In SSA that is
// the same result as this scoped walk of the dominator tree, which keeps the
// expressions available on the path from the entry. Zig leaves this to LLVM.

@[inline]
fn cse_is_pure(op ssa.OpCode) bool {
	return op in [.add, .sub, .mul, .and_, .or_, .xor, .shl, .ashr, .lshr, .fadd, .fsub, .fmul,
		.fdiv, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge, .eq, .ne, .neg, .zext, .sext, .trunc,
		.bitcast, .fptoui, .fptosi, .uitofp, .sitofp, .get_element_ptr]
}

@[inline]
fn cse_is_commutative(op ssa.OpCode) bool {
	return op in [.add, .mul, .and_, .or_, .xor, .eq, .ne, .fadd, .fmul]
}

fn cse_key(val_typ ssa.TypeID, instr ssa.Instruction) u64 {
	mut hash := u64(1469598103934665603)
	hash = (hash ^ u64(int(instr.op))) * 1099511628211
	hash = (hash ^ u64(int(val_typ))) * 1099511628211
	if cse_is_commutative(instr.op) && instr.operands.len == 2 {
		low := if instr.operands[0] < instr.operands[1] {
			instr.operands[0]
		} else {
			instr.operands[1]
		}
		high := if instr.operands[0] < instr.operands[1] {
			instr.operands[1]
		} else {
			instr.operands[0]
		}
		hash = (hash ^ u64(int(low))) * 1099511628211
		return (hash ^ u64(int(high))) * 1099511628211
	}
	for operand in instr.operands {
		hash = (hash ^ u64(int(operand))) * 1099511628211
	}
	return hash
}

fn cse_same(m &ssa.Module, a ssa.ValueID, b ssa.ValueID) bool {
	left := m.instrs[m.values[a].index]
	right := m.instrs[m.values[b].index]
	if left.op != right.op {
		return false
	}
	if m.values[a].typ != m.values[b].typ {
		return false
	}
	if left.operands.len != right.operands.len {
		return false
	}
	if cse_is_commutative(left.op) && left.operands.len == 2 {
		return (left.operands[0] == right.operands[0] && left.operands[1] == right.operands[1])
			|| (left.operands[0] == right.operands[1] && left.operands[1] == right.operands[0])
	}
	return left.operands == right.operands
}

struct CseFrame {
	block int
mut:
	child    int
	undo_len int
}

// common_subexpression_elimination expects dominators computed for the current
// control flow.
fn common_subexpression_elimination(mut m ssa.Module) {
	mut available := map[u64]ssa.ValueID{}
	mut undo := []u64{}
	for fi in 0 .. m.funcs.len {
		if m.funcs[fi].blocks.len == 0 {
			continue
		}
		available.clear()
		undo.clear()
		mut stack := [CseFrame{
			block: m.funcs[fi].blocks[0]
		}]
		cse_block(mut m, stack[0].block, mut available, mut undo)
		stack[0].undo_len = 0
		for stack.len > 0 {
			top := stack.len - 1
			children := m.blocks[stack[top].block].dom_tree
			if stack[top].child < children.len {
				child := int(children[stack[top].child])
				stack[top].child++
				mark := undo.len
				cse_block(mut m, child, mut available, mut undo)
				stack << CseFrame{
					block:    child
					undo_len: mark
				}
				continue
			}
			for undo.len > stack[top].undo_len {
				available.delete(undo.pop())
			}
			stack.delete_last()
		}
	}
}

fn cse_block(mut m ssa.Module, blk_id int, mut available map[u64]ssa.ValueID, mut undo []u64) {
	for val_id in m.blocks[blk_id].instrs {
		val := m.values[val_id]
		if val.kind != .instruction {
			continue
		}
		instr := m.instrs[val.index]
		if !cse_is_pure(instr.op) {
			continue
		}
		key := cse_key(val.typ, instr)
		if existing := available[key] {
			if cse_same(m, existing, val_id) {
				m.replace_uses(val_id, existing)
				continue
			}
			// A hash collision: keep the first expression.
			continue
		}
		available[key] = val_id
		undo << key
	}
}
