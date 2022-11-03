// this file generates ../infix.v
module main

import strings
import os

const (
	header = '[generated]
module eval
import v.token
import v.ast
fn(e Eval)infix_expr(left Object,right Object,op token.Kind,expecting ast.Type)Object{ match op{'
	footer = "else{ e.error('unknown infix expression: \$op')}}return empty // should e.error before this anyway
}
"
	uk_expect_footer = "else{ e.error('unknown infix expectation: \${e.table.sym(expecting).str()}')}}"
	comparison       = {
		'gt': '>'
		'lt': '<'
		'eq': '=='
		'ne': '!='
	}
	math_ops = {
		'plus':        '+'
		'minus':       '-'
		'mul':         '*'
		'div':         '/'
		'right_shift': '>>'
		'left_shift':  '<<'
	}
	compound_types = ['Int', 'Uint', 'Float']
	literal_types  = ['i64', 'f64']
)

fn main() {
	mut b := strings.new_builder(124000)
	b.write_string(header)

	for enm, op in comparison {
		b.write_string('.$enm{ match left{')
		for ct in compound_types {
			b.write_string('$ct { match right{')
			for ct2 in compound_types {
				b.write_string('$ct2{ return left.val${op}right.val}')
			}
			for lt2 in literal_types {
				b.write_string('$lt2{ return left.val${op}right}')
			}
			b.write_string("else{ e.error('invalid operands to $op: $ct and \$right.type_name()')}}}")
		}
		for lt in literal_types {
			b.write_string('$lt { match right{')
			for ct2 in compound_types {
				b.write_string('$ct2{ return left${op}right.val}')
			}
			for lt2 in literal_types {
				b.write_string('$lt2{ return left${op}right}')
			}
			b.write_string("else { e.error('invalid operands to $op: ")
			b.write_string(if lt == 'i64' { 'int' } else { 'float' })
			b.write_string(" literal and \$right.type_name()')}}}")
		}
		if op in ['==', '!='] {
			b.write_string('string{ match right{ string{ return left${op}right}else{ e.error(\'invalid operands to $op: string and \$right.type_name()\')}}}')
		}
		b.write_string("else { e.error('invalid operands to $op: \$left.type_name() and \$right.type_name()')}}}")
	}
	for math, op in math_ops {
		b.write_string('.$math{ match left{')
		for ct in compound_types {
			if op in ['<<', '>>'] && ct == 'Float' {
				continue
			}
			b.write_string('$ct { match right{')
			for ct2 in compound_types {
				if op in ['<<', '>>'] && ct2 == 'Float' {
					continue
				}
				unsafe_start, unsafe_end := if op in ['<<', '>>'] { 'unsafe{', '}' } else { '', '' }
				b.write_string('$ct2{ if expecting in ast.signed_integer_type_idxs{ return Int{ i64(left.val)${op}i64(right.val),i8(e.type_to_size(expecting))}}else if expecting in ast.unsigned_integer_type_idxs{ return Uint{ u64(left.val)${op}u64(right.val),i8(e.type_to_size(expecting))}}else if expecting==ast.int_literal_type_idx{${unsafe_start}return i64(i64(left.val)${op}i64(right.val))$unsafe_end}')
				if op !in ['<<', '>>'] {
					b.write_string('else if expecting in ast.float_type_idxs{ return Float{ f64(left.val)${op}f64(right.val),i8(e.type_to_size(expecting))}}else if expecting==ast.float_literal_type_idx{ return f64(f64(left.val)${op}f64(right.val))}')
				}
				b.write_string(uk_expect_footer)
			}
			for lt2 in literal_types {
				if op in ['<<', '>>'] && lt2 == 'f64' {
					continue
				}
				unsafe_start, unsafe_end := if op in ['<<', '>>'] { 'unsafe{', '}' } else { '', '' }
				b.write_string('$lt2{ if expecting in ast.signed_integer_type_idxs{ return Int{ i64(left.val)${op}i64(right),i8(e.type_to_size(expecting))}}else if expecting in ast.unsigned_integer_type_idxs{ return Uint{ u64(left.val)${op}u64(right),i8(e.type_to_size(expecting))}}else if expecting==ast.int_literal_type_idx{${unsafe_start}return i64(i64(left.val)${op}i64(right))$unsafe_end}')
				if op !in ['<<', '>>'] {
					b.write_string('else if expecting in ast.float_type_idxs{ return Float{ f64(left.val)${op}f64(right), i8(e.type_to_size(expecting))}}else if expecting==ast.float_literal_type_idx{ return f64(f64(left.val)${op}f64(right))}')
				}
				b.write_string(uk_expect_footer)
			}
			b.write_string("else { e.error('invalid operands to $op: $ct and \$right.type_name()')}}}")
		}
		for lt in literal_types {
			if op in ['<<', '>>'] && lt == 'f64' {
				continue
			}
			b.write_string('$lt{ match right{')
			for ct2 in compound_types {
				if op in ['<<', '>>'] && ct2 == 'Float' {
					continue
				}
				unsafe_start, unsafe_end := if op in ['<<', '>>'] { 'unsafe{', '}' } else { '', '' }
				b.write_string('$ct2{ if expecting in ast.signed_integer_type_idxs{ return Int{ i64(left)${op}i64(right.val),i8(e.type_to_size(expecting))}}else if expecting in ast.unsigned_integer_type_idxs{ return Uint{ u64(left)${op}u64(right.val),i8(e.type_to_size(expecting))}}else if expecting==ast.int_literal_type_idx{${unsafe_start}return i64(i64(left)${op}i64(right.val))$unsafe_end}')
				if op !in ['<<', '>>'] {
					b.write_string('else if expecting in ast.float_type_idxs{ return Float{ f64(left)${op}f64(right.val), i8(e.type_to_size(expecting))}}else if expecting==ast.float_literal_type_idx{ return f64(f64(left)${op}f64(right.val))}')
				}
				b.write_string(uk_expect_footer)
			}
			for lt2 in literal_types {
				if op in ['<<', '>>'] && lt2 == 'f64' {
					continue
				}
				unsafe_start, unsafe_end := if op in ['<<', '>>'] { 'unsafe{', '}' } else { '', '' }
				b.write_string('$lt2{ if expecting in ast.signed_integer_type_idxs{ return Int{ i64(left)${op}i64(right),i8(e.type_to_size(expecting))}}else if expecting in ast.unsigned_integer_type_idxs{ return Uint{ u64(left)${op}u64(right),i8(e.type_to_size(expecting))}}else if expecting==ast.int_literal_type_idx{${unsafe_start}return i64(i64(left)${op}i64(right))$unsafe_end}')
				if op !in ['<<', '>>'] {
					b.write_string('else if expecting in ast.float_type_idxs{ return Float{ f64(left)${op}f64(right), i8(e.type_to_size(expecting))}}else if expecting==ast.float_literal_type_idx{ return f64(f64(left)${op}f64(right))}')
				}
				b.write_string(uk_expect_footer)
			}
			b.write_string("else { e.error('invalid operands to $op: ")
			b.write_string(if lt == 'i64' { 'int' } else { 'float' })
			b.write_string(" literal and \$right.type_name()')}}}")
		}
		b.write_string("else { e.error('invalid operands to $op: \$left.type_name() and \$right.type_name()')}}}")
	}

	b.write_string(footer)

	path := @FILE.all_before(@FILE.all_after_last('/')) + '../infix.v'
	os.write_file(path, b.str()) or { panic(err) }
	res := os.execute(os.quoted_path(@VEXE) + ' fmt -w ' + os.quoted_path(path))
	if res.exit_code != 0 {
		eprintln('v fmt failed!')
		panic(res.output)
	}
}
