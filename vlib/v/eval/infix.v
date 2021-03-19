module eval

import v.token
import v.ast

fn (e Eval) infix_expr(left Object, right Object, op token.Kind, expecting ast.Type) Object {
	match op {
		.gt {
			match left {
				Int {
					match right {
						Int {
							return left.val > right.val
						}
						Uint {
							return left.val > right.val
						}
						else {
							e.error('invalid operands to >')
						}
					}
				}
				Uint {
					match right {
						Int {
							return left.val > right.val
						}
						Uint {
							return left.val > right.val
						}
						else {
							e.error('invalid operands to >')
						}
					}
				}
				else {
					e.error('invalid operands to >')
				}
			}
		}
		.lt {
			match left {
				Int {
					match right {
						Int {
							return left.val < right.val
						}
						Uint {
							return left.val < right.val
						}
						else {
							e.error('invalid operands to <')
						}
					}
				}
				Uint {
					match right {
						Int {
							return left.val < right.val
						}
						Uint {
							return left.val < right.val
						}
						else {
							e.error('invalid operands to <')
						}
					}
				}
				else {
					e.error('invalid operands to <')
				}
			}
		}
		.eq {
			match left {
				Int {
					match right {
						Int {
							return left.val == right.val
						}
						Uint {
							return left.val == right.val
						}
						else {
							e.error('invalid operands to ==: Int and $right.type_name()')
						}
					}
				}
				Uint {
					match right {
						Int {
							return left.val == right.val
						}
						Uint {
							return left.val == right.val
						}
						else {
							e.error('invalid operands to ==: Uint and $right.type_name()')
						}
					}
				}
				Charptr {
					match right {
						Int {
							return &i8(left) == right.val
						}
						Uint {
							return &i8(left) == right.val
						}
						Charptr {
							return &i8(left) == &i8(right)
						}
						else {
							e.error('invalid operands to ==: charptr and $right.type_name()')
						}
					}
				}
				else {
					e.error('invalid operands to ==: $left.type_name() and $right.type_name()')
				}
			}
		}
		.ne {
			match left {
				Int {
					match right {
						Int {
							return left.val != right.val
						}
						Uint {
							return left.val != right.val
						}
						else {
							e.error('invalid operands to !=')
						}
					}
				}
				Uint {
					match right {
						Int {
							return left.val != right.val
						}
						Uint {
							return left.val != right.val
						}
						else {
							e.error('invalid operands to !=')
						}
					}
				}
				else {
					e.error('invalid operands to !=')
				}
			}
		}
		.plus {
			match left {
				Int {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{left.val + right.val, i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + u64(right.val), i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{left.val + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) + right.val, i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to +')
						}
					}
				}
				Uint {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{i64(left.val) + right.val, i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{left.val + u64(right.val), i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{i64(left.val) + i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{left.val + right.val, i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to +')
						}
					}
				}
				else {
					e.error('invalid operands to +')
				}
			}
		}
		.minus {
			match left {
				Int {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{left.val - right.val, i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - u64(right.val), i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{left.val - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) - right.val, i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to -: int and $right.type_name()')
						}
					}
				}
				Uint {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{i64(left.val) - right.val, i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{left.val - u64(right.val), i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{i64(left.val) - i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{left.val - right.val, i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to -: uint and $right.type_name()')
						}
					}
				}
				else {
					e.error('invalid operands to -: $left.type_name() and $right.type_name()')
				}
			}
		}
		.mul {
			match left {
				Int {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{left.val * right.val, i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * u64(right.val), i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{left.val * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) * right.val, i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to *')
						}
					}
				}
				Uint {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{i64(left.val) * right.val, i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{left.val * u64(right.val), i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{i64(left.val) * i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{left.val * right.val, i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to *')
						}
					}
				}
				else {
					e.error('invalid operands to *')
				}
			}
		}
		.div {
			match left {
				Int {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{left.val / right.val, i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / u64(right.val), i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{left.val / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) / right.val, i8(e.type_to_size(expecting))}
							}
						}
						else {
							e.error('invalid operands to /')
						}
					}
				}
				Uint {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{i64(left.val) / right.val, i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{left.val / u64(right.val), i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{i64(left.val) / i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{left.val / right.val, i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to /')
						}
					}
				}
				else {
					e.error('invalid operands to /')
				}
			}
		}
		.left_shift {
			// TODO: array
			match left {
				Int {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{left.val << right.val, i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) << u64(right.val), i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{left.val << i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{u64(left.val) << right.val, i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to <<')
						}
					}
				}
				Uint {
					match right {
						Int {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{i64(left.val) << right.val, i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{left.val << u64(right.val), i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						Uint {
							if expecting in ast.signed_integer_type_idxs
								|| expecting == ast.int_literal_type_idx {
								return Int{i64(left.val) << i64(right.val), i8(e.type_to_size(expecting))}
							} else if expecting in ast.unsigned_integer_type_idxs {
								return Uint{left.val << right.val, i8(e.type_to_size(expecting))}
							} else {
								e.error('unknown infix expectation: ${e.table.get_type_symbol(expecting).str()}')
							}
						}
						else {
							e.error('invalid operands to <<')
						}
					}
				}
				else {
					e.error('invalid operands to <<')
				}
			}
		}
		else {
			e.error('unknown infix expression: $op')
		}
	}
	return empty // should e.error before this anyway
}
