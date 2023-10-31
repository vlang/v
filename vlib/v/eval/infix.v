[generated]
module eval

import v.token
import v.ast

fn (e Eval) infix_expr(left Object, right Object, op token.Kind, expecting ast.Type) Object {
	match op {
		.gt {
			match left {
				Int {
					match right {
						Int { return left.val > right.val }
						Uint { return left.val > right.val }
						Float { return left.val > right.val }
						i64 { return left.val > right }
						f64 { return left.val > right }
						else { e.error('invalid operands to >: Int and ${right.type_name()}') }
					}
				}
				Uint {
					match right {
						Int { return left.val > right.val }
						Uint { return left.val > right.val }
						Float { return left.val > right.val }
						i64 { return left.val > right }
						f64 { return left.val > right }
						else { e.error('invalid operands to >: Uint and ${right.type_name()}') }
					}
				}
				Float {
					match right {
						Int { return left.val > right.val }
						Uint { return left.val > right.val }
						Float { return left.val > right.val }
						i64 { return left.val > right }
						f64 { return left.val > right }
						else { e.error('invalid operands to >: Float and ${right.type_name()}') }
					}
				}
				i64 {
					match right {
						Int { return left > right.val }
						Uint { return left > right.val }
						Float { return left > right.val }
						i64 { return left > right }
						f64 { return left > right }
						else { e.error('invalid operands to >: int literal and ${right.type_name()}') }
					}
				}
				f64 {
					match right {
						Int { return left > right.val }
						Uint { return left > right.val }
						Float { return left > right.val }
						i64 { return left > right }
						f64 { return left > right }
						else { e.error('invalid operands to >: float literal and ${right.type_name()}') }
					}
				}
				else {
					e.error('invalid operands to >: ${left.type_name()} and ${right.type_name()}')
				}
			}
		}
		.lt {
			match left {
				Int {
					match right {
						Int { return left.val < right.val }
						Uint { return left.val < right.val }
						Float { return left.val < right.val }
						i64 { return left.val < right }
						f64 { return left.val < right }
						else { e.error('invalid operands to <: Int and ${right.type_name()}') }
					}
				}
				Uint {
					match right {
						Int { return left.val < right.val }
						Uint { return left.val < right.val }
						Float { return left.val < right.val }
						i64 { return left.val < right }
						f64 { return left.val < right }
						else { e.error('invalid operands to <: Uint and ${right.type_name()}') }
					}
				}
				Float {
					match right {
						Int { return left.val < right.val }
						Uint { return left.val < right.val }
						Float { return left.val < right.val }
						i64 { return left.val < right }
						f64 { return left.val < right }
						else { e.error('invalid operands to <: Float and ${right.type_name()}') }
					}
				}
				i64 {
					match right {
						Int { return left < right.val }
						Uint { return left < right.val }
						Float { return left < right.val }
						i64 { return left < right }
						f64 { return left < right }
						else { e.error('invalid operands to <: int literal and ${right.type_name()}') }
					}
				}
				f64 {
					match right {
						Int { return left < right.val }
						Uint { return left < right.val }
						Float { return left < right.val }
						i64 { return left < right }
						f64 { return left < right }
						else { e.error('invalid operands to <: float literal and ${right.type_name()}') }
					}
				}
				else {
					e.error('invalid operands to <: ${left.type_name()} and ${right.type_name()}')
				}
			}
		}
		.eq {
			match left {
				Int {
					match right {
						Int { return left.val == right.val }
						Uint { return left.val == right.val }
						Float { return left.val == right.val }
						i64 { return left.val == right }
						f64 { return left.val == right }
						else { e.error('invalid operands to ==: Int and ${right.type_name()}') }
					}
				}
				Uint {
					match right {
						Int { return left.val == right.val }
						Uint { return left.val == right.val }
						Float { return left.val == right.val }
						i64 { return left.val == right }
						f64 { return left.val == right }
						else { e.error('invalid operands to ==: Uint and ${right.type_name()}') }
					}
				}
				Float {
					match right {
						Int { return left.val == right.val }
						Uint { return left.val == right.val }
						Float { return left.val == right.val }
						i64 { return left.val == right }
						f64 { return left.val == right }
						else { e.error('invalid operands to ==: Float and ${right.type_name()}') }
					}
				}
				i64 {
					match right {
						Int { return left == right.val }
						Uint { return left == right.val }
						Float { return left == right.val }
						i64 { return left == right }
						f64 { return left == right }
						else { e.error('invalid operands to ==: int literal and ${right.type_name()}') }
					}
				}
				f64 {
					match right {
						Int { return left == right.val }
						Uint { return left == right.val }
						Float { return left == right.val }
						i64 { return left == right }
						f64 { return left == right }
						else { e.error('invalid operands to ==: float literal and ${right.type_name()}') }
					}
				}
				string {
					match right {
						string { return left == right }
						else { e.error('invalid operands to ==: string and ${right.type_name()}') }
					}
				}
				else {
					e.error('invalid operands to ==: ${left.type_name()} and ${right.type_name()}')
				}
			}
		}
		.ne {
			match left {
				Int {
					match right {
						Int { return left.val != right.val }
						Uint { return left.val != right.val }
						Float { return left.val != right.val }
						i64 { return left.val != right }
						f64 { return left.val != right }
						else { e.error('invalid operands to !=: Int and ${right.type_name()}') }
					}
				}
				Uint {
					match right {
						Int { return left.val != right.val }
						Uint { return left.val != right.val }
						Float { return left.val != right.val }
						i64 { return left.val != right }
						f64 { return left.val != right }
						else { e.error('invalid operands to !=: Uint and ${right.type_name()}') }
					}
				}
				Float {
					match right {
						Int { return left.val != right.val }
						Uint { return left.val != right.val }
						Float { return left.val != right.val }
						i64 { return left.val != right }
						f64 { return left.val != right }
						else { e.error('invalid operands to !=: Float and ${right.type_name()}') }
					}
				}
				i64 {
					match right {
						Int { return left != right.val }
						Uint { return left != right.val }
						Float { return left != right.val }
						i64 { return left != right }
						f64 { return left != right }
						else { e.error('invalid operands to !=: int literal and ${right.type_name()}') }
					}
				}
				f64 {
					match right {
						Int { return left != right.val }
						Uint { return left != right.val }
						Float { return left != right.val }
						i64 { return left != right }
						f64 { return left != right }
						else { e.error('invalid operands to !=: float literal and ${right.type_name()}') }
					}
				}
				string {
					match right {
						string { return left != right }
						else { e.error('invalid operands to !=: string and ${right.type_name()}') }
					}
				}
				else {
					e.error('invalid operands to !=: ${left.type_name()} and ${right.type_name()}')
				}
			}
		}
		.plus {
			match left {
				Int {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to +: Int and ${right.type_name()}')
						}
					}
				}
				Uint {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to +: Uint and ${right.type_name()}')
						}
					}
				}
				Float {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) + i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) + i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) + f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) + f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to +: Float and ${right.type_name()}')
						}
					}
				}
				i64 {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) + i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) + u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) + i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) + f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) + f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) + i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) + u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) + i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) + f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) + f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to +: int literal and ${right.type_name()}')
						}
					}
				}
				f64 {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) + u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) + i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) + f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) + f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) + i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) + u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) + i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) + f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) + f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) + i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) + u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) + i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) + f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) + f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to +: float literal and ${right.type_name()}')
						}
					}
				}
				else {
					e.error('invalid operands to +: ${left.type_name()} and ${right.type_name()}')
				}
			}
		}
		.minus {
			match left {
				Int {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to -: Int and ${right.type_name()}')
						}
					}
				}
				Uint {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to -: Uint and ${right.type_name()}')
						}
					}
				}
				Float {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) - i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) - i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) - f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) - f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to -: Float and ${right.type_name()}')
						}
					}
				}
				i64 {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) - i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) - u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) - i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) - f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) - f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) - i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) - u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) - i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) - f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) - f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to -: int literal and ${right.type_name()}')
						}
					}
				}
				f64 {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) - u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) - i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) - f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) - f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) - i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) - u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) - i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) - f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) - f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) - i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) - u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) - i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) - f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) - f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to -: float literal and ${right.type_name()}')
						}
					}
				}
				else {
					e.error('invalid operands to -: ${left.type_name()} and ${right.type_name()}')
				}
			}
		}
		.mul {
			match left {
				Int {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to *: Int and ${right.type_name()}')
						}
					}
				}
				Uint {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to *: Uint and ${right.type_name()}')
						}
					}
				}
				Float {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) * i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) * i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) * f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) * f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to *: Float and ${right.type_name()}')
						}
					}
				}
				i64 {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) * i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) * u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) * i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) * f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) * f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) * i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) * u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) * i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) * f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) * f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to *: int literal and ${right.type_name()}')
						}
					}
				}
				f64 {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) * u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) * i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) * f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) * f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) * i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) * u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) * i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) * f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) * f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) * i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) * u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) * i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) * f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) * f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to *: float literal and ${right.type_name()}')
						}
					}
				}
				else {
					e.error('invalid operands to *: ${left.type_name()} and ${right.type_name()}')
				}
			}
		}
		.div {
			match left {
				Int {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to /: Int and ${right.type_name()}')
						}
					}
				}
				Uint {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to /: Uint and ${right.type_name()}')
						}
					}
				}
				Float {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) / i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left.val) / i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left.val) / f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left.val) / f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to /: Float and ${right.type_name()}')
						}
					}
				}
				i64 {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) / i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) / u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) / i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) / f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) / f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) / i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) / u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) / i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) / f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) / f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to /: int literal and ${right.type_name()}')
						}
					}
				}
				f64 {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Float {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) / u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) / i64(right.val))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) / f64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) / f64(right.val))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) / i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) / u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) / i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) / f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) / f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						f64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) / i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) / u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								return i64(i64(left) / i64(right))
							} else if expecting in ast.float_type_idxs {
								return Float{f64(left) / f64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.float_literal_type_idx {
								return f64(f64(left) / f64(right))
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to /: float literal and ${right.type_name()}')
						}
					}
				}
				else {
					e.error('invalid operands to /: ${left.type_name()} and ${right.type_name()}')
				}
			}
		}
		.right_shift {
			match left {
				Int {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) >> i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) >> u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) >> i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) >> i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) >> u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) >> i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) >> i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) >> u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) >> i64(right))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to >>: Int and ${right.type_name()}')
						}
					}
				}
				Uint {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) >> i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) >> u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) >> i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) >> i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) >> u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) >> i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) >> i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) >> u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) >> i64(right))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to >>: Uint and ${right.type_name()}')
						}
					}
				}
				i64 {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) >> i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) >> u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left) >> i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) >> i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) >> u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left) >> i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) >> i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) >> u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left) >> i64(right))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to >>: int literal and ${right.type_name()}')
						}
					}
				}
				else {
					e.error('invalid operands to >>: ${left.type_name()} and ${right.type_name()}')
				}
			}
		}
		.left_shift {
			match left {
				Int {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) << i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) << u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) << i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) << i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) << u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) << i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) << i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) << u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) << i64(right))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to <<: Int and ${right.type_name()}')
						}
					}
				}
				Uint {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) << i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) << u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) << i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) << i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) << u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) << i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left.val) << i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) << u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left.val) << i64(right))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to <<: Uint and ${right.type_name()}')
						}
					}
				}
				i64 {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) << i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) << u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left) << i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) << i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) << u64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left) << i64(right.val))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						i64 {
							if expecting in ast.signed_integer_type_idxs {
								return Int{i64(left) << i64(right), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left) << u64(right), i8(e.type_to_size(expecting))}
							} else if expecting == ast.int_literal_type_idx {
								unsafe {
									return i64(i64(left) << i64(right))
								}
							} else {
								e.error('unknown infix expectation: ${e.table.sym(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to <<: int literal and ${right.type_name()}')
						}
					}
				}
				else {
					e.error('invalid operands to <<: ${left.type_name()} and ${right.type_name()}')
				}
			}
		}
		else {
			e.error('unknown infix expression: ${op}')
		}
	}
	return empty // should e.error before this anyway
}
