// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

fn constant_fold(mut m ssa.Module) bool {
	mut changed := false
	for func in m.funcs {
		for blk_id in func.blocks {
			// Iterate directly without cloning - we don't modify the array during iteration
			for val_id in m.blocks[blk_id].instrs {
				if m.values[val_id].kind != .instruction {
					continue
				}

				instr := m.instrs[m.values[val_id].index]

				if instr.operands.len == 2 {
					lhs := m.values[instr.operands[0]]
					rhs := m.values[instr.operands[1]]

					// Skip undef values - can't fold with undefined
					if lhs.kind == .constant && lhs.name == 'undef' {
						continue
					}
					if rhs.kind == .constant && rhs.name == 'undef' {
						continue
					}

					// Try algebraic simplifications first (even with non-constants)
					// vfmt off
					repl, needs_zero := try_algebraic_simplify(m, val_id, instr, lhs, rhs)
					// vfmt on
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
							m.instrs[m.values[val_id].index].op = .shl
							m.instrs[m.values[val_id].index].operands = [other_id, one_val]
							changed = true
							continue
						} else if needs_zero {
							// x * 0 or x & 0 or x - x or x ^ x - reuse zero constant
							typ := m.values[val_id].typ
							zero_val := m.get_or_add_const(typ, '0')
							m.replace_uses(val_id, zero_val)
						} else {
							m.replace_uses(val_id, repl)
						}
						changed = true
						continue
					}

					// Constant folding - both operands must be constants
					if lhs.kind == .constant && rhs.kind == .constant {
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
									// Arithmetic shift right preserves sign
									result = l_int >> u64(r_int)
									folded = true
								}
							}
							.lshr {
								if r_int >= 0 && r_int < 64 {
									// Logical shift right treats as unsigned
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
							const_val := m.get_or_add_const(typ, result.str())
							m.replace_uses(val_id, const_val)
							changed = true
						}
					}
				}
			}
		}
	}
	return changed
}

// Branch folding: simplify conditional branches with constant conditions
fn branch_fold(mut m ssa.Module) bool {
	mut changed := false
	for func in m.funcs {
		for blk_id in func.blocks {
			blk := m.blocks[blk_id]
			if blk.instrs.len == 0 {
				continue
			}

			term_val_id := blk.instrs.last()
			term := m.instrs[m.values[term_val_id].index]

			if term.op == .br {
				// br cond, true_blk, false_blk
				cond_val := m.values[term.operands[0]]
				if cond_val.kind == .constant && cond_val.name != 'undef' {
					cond_int := cond_val.name.i64()
					// Replace with unconditional jump to the taken branch
					target := if cond_int != 0 { term.operands[1] } else { term.operands[2] }
					m.instrs[m.values[term_val_id].index].op = .jmp
					m.instrs[m.values[term_val_id].index].operands = [target]
					changed = true
				}
			}
		}
	}
	return changed
}

// Algebraic simplifications: x+0=x, x*1=x, x*0=0, x-x=0, x^x=0, x&x=x, x|x=x, x*2=x<<1, etc.
// Returns (replacement_id, needs_zero) - if needs_zero is true, caller should create zero constant
fn try_algebraic_simplify(m ssa.Module, val_id int, instr ssa.Instruction, lhs ssa.Value, rhs ssa.Value) (int, bool) {
	lhs_id := instr.operands[0]
	rhs_id := instr.operands[1]

	// Check for same-operand simplifications (x op x)
	if lhs_id == rhs_id {
		match instr.op {
			.sub {
				// x - x = 0
				return val_id, true
			}
			.xor {
				// x ^ x = 0
				return val_id, true
			}
			.and_ {
				// x & x = x
				return lhs_id, false
			}
			.or_ {
				// x | x = x
				return lhs_id, false
			}
			else {}
		}
	}

	// Check if either operand is a constant
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
			// x + 0 = 0 + x = x
			if const_val == 0 {
				return other_id, false
			}
		}
		.sub {
			// x - 0 = x
			if const_is_rhs && const_val == 0 {
				return other_id, false
			}
		}
		.mul {
			// x * 0 = 0 * x = 0
			if const_val == 0 {
				return val_id, true // Signal caller to create zero
			}
			// x * 1 = 1 * x = x
			if const_val == 1 {
				return other_id, false
			}
			// x * 2 = x << 1, 2 * x = x << 1
			if const_val == 2 {
				return -2, false // Signal caller to create shift
			}
		}
		.sdiv {
			// x / 1 = x
			if const_is_rhs && const_val == 1 {
				return other_id, false
			}
		}
		.and_ {
			// x & 0 = 0 & x = 0
			if const_val == 0 {
				return val_id, true // Signal caller to create zero
			}
		}
		.or_ {
			// x | 0 = 0 | x = x
			if const_val == 0 {
				return other_id, false
			}
		}
		.xor {
			// x ^ 0 = 0 ^ x = x
			if const_val == 0 {
				return other_id, false
			}
		}
		.shl, .ashr, .lshr {
			// x << 0 = x >> 0 = x
			if const_is_rhs && const_val == 0 {
				return other_id, false
			}
		}
		else {}
	}

	return -1, false
}
