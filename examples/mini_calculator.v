// Q: What's this?
// A: This is a mini "home-made" calculator. You may also regard it as a very elementary version of "interpreter".
import os

const (
	numeric_char = [`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `.`, `e`, `E`]
)

// Convert expression to Reverse Polish Notation.
fn expr_to_rev_pol(expr string) ?[]string {
	if expr == '' {
		return error('err: empty expression')
	}
	mut stack := []string{}
	mut rev_pol := []string{}
	mut pos := 0
	for pos < expr.len {
		mut end_pos := pos
		for end_pos < expr.len && expr[end_pos] in numeric_char {
			end_pos++
		}
		if end_pos > pos {
			stack << expr[pos..end_pos]
			pos = end_pos
		} else if end_pos == pos {
			op := expr[pos].ascii_str()
			match op {
				'(' {
					stack << op
				}
				'*', '/' {
					for stack.len > 0 && stack.last() !in ['(', '+', '-'] {
						rev_pol << stack.last()
						stack.delete(stack.len - 1)
					}
					stack << op
				}
				'+', '-' {
					for stack.len > 0 && stack.last() != '(' {
						rev_pol << stack.last()
						stack.delete(stack.len - 1)
					}
					stack << op
				}
				')' {
					for stack.len > 0 && stack.last() != '(' {
						rev_pol << stack.last()
						stack.delete(stack.len - 1)
					}
					stack.delete(stack.len - 1)
				}
				else {
					return error('err: invalid character `$op`')
				}
			}
			pos++
		}
	}
	for stack.len > 0 {
		top := stack.last()
		rev_pol << top
		stack.delete(stack.len - 1)
	}
	return rev_pol
}

// Evaluate the result of Reverse Polish Notation.
fn eval_rev_pol(rev_pol []string) ?f64 {
	mut stack := []f64{}
	for item in rev_pol {
		if is_num_string(item) {
			stack << item.f64()
		} else {
			if stack.len >= 2 {
				oprand_r := stack.last()
				stack.delete(stack.len - 1)
				oprand_l := stack.last()
				stack.delete(stack.len - 1)
				match item {
					'+' {
						stack << oprand_l + oprand_r
					}
					'-' {
						stack << oprand_l - oprand_r
					}
					'*' {
						stack << oprand_l * oprand_r
					}
					'/' {
						if oprand_r == 0 {
							return error('err: divide by zero')
						}
						stack << oprand_l / oprand_r
					}
					else {}
				}
			} else {
				return error('err: invalid expression')
			}
		}
	}
	return stack[0]
}

fn is_num_string(str string) bool {
	for c in str {
		if c !in numeric_char {
			return false
		}
	}
	return true
}

fn main() {
	println('Please enter the expression you want to calculate, e.g. 1e2+(3-2.5)*6/1.5 .')
	println("Enter 'exit' or 'EXIT' to quit.")
	mut expr_count := 0
	for {
		expr_count++
		expr := os.input('[$expr_count] ').trim_space()
		if expr in ['exit', 'EXIT'] {
			break
		}
		rev_pol := expr_to_rev_pol(expr) or {
			eprintln(err)
			continue
		}
		res := eval_rev_pol(rev_pol) or {
			eprintln(err)
			continue
		}
		println(res)
	}
}
