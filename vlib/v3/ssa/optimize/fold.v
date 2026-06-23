module optimize

import v3.ssa

// constant_fold supports constant fold handling for optimize.
fn constant_fold(mut m ssa.Module) bool {
	mut changed := false
	for fi in 0 .. m.funcs.len {
		for blk_id in m.funcs[fi].blocks {
			for val_id in m.blocks[blk_id].instrs {
				if m.values[val_id].kind != .instruction {
					continue
				}
				instr := m.instrs[m.values[val_id].index]
				if instr.operands.len != 2 {
					continue
				}
				lhs := m.values[instr.operands[0]]
				rhs := m.values[instr.operands[1]]

				// Skip undef values - can't fold/simplify with undefined.
				if lhs.kind == .constant && lhs.name == 'undef' {
					continue
				}
				if rhs.kind == .constant && rhs.name == 'undef' {
					continue
				}

				// Algebraic simplifications first (work even with non-constant operands):
				// x+0, x*1, x*0, x-x, x^x, x&x, x|x, x<<0, x*2 -> x<<1, etc.
				repl, needs_zero := try_algebraic_simplify(m, val_id, instr, lhs, rhs)
				if repl >= 0 || repl == -2 {
					if repl == -2 {
						// x * 2 -> x << 1
						other_id := if lhs.kind == .constant {
							instr.operands[1]
						} else {
							instr.operands[0]
						}
						typ := m.values[val_id].typ
						one_val := m.get_or_add_const(typ, '1')
						mut shl_instr := m.instrs[m.values[val_id].index]
						shl_instr.op = .shl
						shl_instr.operands = [other_id, one_val]
						m.instrs[m.values[val_id].index] = shl_instr
						changed = true
						continue
					} else if needs_zero {
						// x * 0, x & 0, x - x, x ^ x -> 0
						typ := m.values[val_id].typ
						zero_val := m.get_or_add_const(typ, '0')
						m.replace_uses(val_id, zero_val)
					} else {
						m.replace_uses(val_id, repl)
					}
					changed = true
					continue
				}

				if lhs.kind != .constant || rhs.kind != .constant {
					continue
				}
				l_int := lhs.name.i64()
				r_int := rhs.name.i64()

				mut result := i64(0)
				mut folded := false

				match instr.op {
					.add {
						result = l_int + r_int
						folded = true
					}
					.sub {
						result = l_int - r_int
						folded = true
					}
					.mul {
						result = l_int * r_int
						folded = true
					}
					.sdiv {
						if r_int != 0 {
							result = l_int / r_int
							folded = true
						}
					}
					.srem {
						if r_int != 0 {
							result = l_int % r_int
							folded = true
						}
					}
					.udiv {
						if r_int != 0 {
							result = i64(u64(l_int) / u64(r_int))
							folded = true
						}
					}
					.urem {
						if r_int != 0 {
							result = i64(u64(l_int) % u64(r_int))
							folded = true
						}
					}
					.and_ {
						result = l_int & r_int
						folded = true
					}
					.or_ {
						result = l_int | r_int
						folded = true
					}
					.xor {
						result = l_int ^ r_int
						folded = true
					}
					.shl {
						if r_int >= 0 && r_int < 64 {
							result = i64(u64(l_int) << u64(r_int))
							folded = true
						}
					}
					.ashr {
						if r_int >= 0 && r_int < 64 {
							result = l_int >> u64(r_int)
							folded = true
						}
					}
					.lshr {
						if r_int >= 0 && r_int < 64 {
							result = i64(u64(l_int) >> u64(r_int))
							folded = true
						}
					}
					.eq {
						result = if l_int == r_int { 1 } else { 0 }
						folded = true
					}
					.ne {
						result = if l_int != r_int { 1 } else { 0 }
						folded = true
					}
					.lt {
						result = if l_int < r_int { 1 } else { 0 }
						folded = true
					}
					.gt {
						result = if l_int > r_int { 1 } else { 0 }
						folded = true
					}
					.le {
						result = if l_int <= r_int { 1 } else { 0 }
						folded = true
					}
					.ge {
						result = if l_int >= r_int { 1 } else { 0 }
						folded = true
					}
					.ult {
						result = if u64(l_int) < u64(r_int) { 1 } else { 0 }
						folded = true
					}
					.ugt {
						result = if u64(l_int) > u64(r_int) { 1 } else { 0 }
						folded = true
					}
					.ule {
						result = if u64(l_int) <= u64(r_int) { 1 } else { 0 }
						folded = true
					}
					.uge {
						result = if u64(l_int) >= u64(r_int) { 1 } else { 0 }
						folded = true
					}
					else {}
				}

				if folded {
					typ := m.values[val_id].typ
					const_val := m.get_or_add_const(typ, '${result}')
					m.replace_uses(val_id, const_val)
					changed = true
				}
			}
		}
	}
	return changed
}

// try_algebraic_simplify implements identity/strength-reduction simplifications:
// x+0=x, x*1=x, x*0=0, x-x=0, x^x=0, x&x=x, x|x=x, x<<0=x, x*2=x<<1, x/1=x.
// Returns (replacement_id, needs_zero):
//   repl >= 0          -> replace all uses of val_id with `repl`
//   repl == -2         -> rewrite as `x << 1` (caller builds the shift)
//   needs_zero == true -> replace all uses with a fresh zero constant
//   repl == -1         -> no simplification
fn try_algebraic_simplify(_m &ssa.Module, val_id int, instr ssa.Instruction, lhs ssa.Value, rhs ssa.Value) (int, bool) {
	lhs_id := instr.operands[0]
	rhs_id := instr.operands[1]

	// Same-operand simplifications (x op x). Integer ops only.
	if lhs_id == rhs_id {
		match instr.op {
			.sub, .xor {
				return val_id, true // x - x = 0, x ^ x = 0
			}
			.and_, .or_ {
				return lhs_id, false // x & x = x, x | x = x
			}
			else {}
		}
	}

	mut const_val := i64(0)
	mut const_is_rhs := false
	mut other_id := 0
	if lhs.kind == .constant && lhs.name != 'undef' {
		const_val = lhs.name.i64()
		const_is_rhs = false
		other_id = instr.operands[1]
	} else if rhs.kind == .constant && rhs.name != 'undef' {
		const_val = rhs.name.i64()
		const_is_rhs = true
		other_id = instr.operands[0]
	} else {
		return -1, false
	}

	match instr.op {
		.add {
			if const_val == 0 {
				return other_id, false // x + 0 = x
			}
		}
		.sub {
			if const_is_rhs && const_val == 0 {
				return other_id, false // x - 0 = x
			}
		}
		.mul {
			if const_val == 0 {
				return val_id, true // x * 0 = 0
			}
			if const_val == 1 {
				return other_id, false // x * 1 = x
			}
			if const_val == 2 {
				return -2, false // x * 2 = x << 1
			}
		}
		.sdiv, .udiv {
			if const_is_rhs && const_val == 1 {
				return other_id, false // x / 1 = x
			}
		}
		.and_ {
			if const_val == 0 {
				return val_id, true // x & 0 = 0
			}
		}
		.or_ {
			if const_val == 0 {
				return other_id, false // x | 0 = x
			}
		}
		.xor {
			if const_val == 0 {
				return other_id, false // x ^ 0 = x
			}
		}
		.shl, .ashr, .lshr {
			if const_is_rhs && const_val == 0 {
				return other_id, false // x << 0 = x, x >> 0 = x
			}
		}
		else {}
	}

	return -1, false
}

// branch_fold supports branch fold handling for optimize.
fn branch_fold(mut m ssa.Module) bool {
	mut changed := false
	for fi in 0 .. m.funcs.len {
		for blk_id in m.funcs[fi].blocks {
			if m.blocks[blk_id].instrs.len == 0 {
				continue
			}
			term_val_id := m.blocks[blk_id].instrs.last()
			term := m.instrs[m.values[term_val_id].index]
			if term.op == .br && term.operands.len >= 3 {
				cond_val := m.values[term.operands[0]]
				if cond_val.kind == .constant {
					cond_int := cond_val.name.i64()
					target := if cond_int != 0 { term.operands[1] } else { term.operands[2] }
					mut jmp_instr := m.instrs[m.values[term_val_id].index]
					jmp_instr.op = .jmp
					mut new_ops := []ssa.ValueID{}
					new_ops << target
					jmp_instr.operands = new_ops
					m.instrs[m.values[term_val_id].index] = jmp_instr
					changed = true
				}
			}
		}
	}
	return changed
}
