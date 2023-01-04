fn test_match_expr_with_assign_sumtype() {
	parse_args(['1', '+', '-', '*', '/', ' ']) or { println(err) }
}

enum Operator {
	add
	subtract
	multiply
	divide
}

type Value = Operator | int

struct Expression {
	left  ?&Expression = none
	val   Value
	right ?&Expression = none
}

enum State {
	expecting
	parse_num
	parse_operator
}

fn tokenise(args string) ?[]Value {
	mut rv := []Value{}

	mut state := State.expecting
	mut cur_value := Value(0)
	for i in args.runes() {
		match state {
			.expecting {
				match i {
					`0`...`9` {
						state = .parse_num
						cur_value = int(i.str().parse_uint(10, 8)?)
					}
					`+`, `-`, `*`, `/` {
						state = .parse_operator
						cur_value = match i {
							`+` {
								Operator.add
							}
							`-` {
								Operator.subtract
							}
							`*` {
								Operator.multiply
							}
							`/` {
								Operator.divide
							}
							else {
								Value(0)
							}
						}
					}
					` ` {
						state = .expecting
					}
					else {
						return error('invalid token ${i}')
					}
				}
			}
			.parse_num {
				match i {
					`0`...`9` {
						cur_value = 10 + int(i.str().parse_uint(10, 8)?)
					}
					`+`, `-`, `*`, `/` {
						state = .parse_operator
						rv << cur_value
						cur_value = match i {
							`+` { Operator.add }
							`-` { Operator.subtract }
							`*` { Operator.multiply }
							`/` { Operator.divide }
							else { Value(0) }
						}
					}
					` ` {
						state = .expecting
						rv << cur_value
					}
					else {
						return error('invalid token ${i}')
					}
				}
			}
			.parse_operator {
				match i {
					`0`...`9` {
						state = .parse_num
						rv << cur_value
						cur_value = int(i.str().parse_uint(10, 8)?)
					}
					` ` {
						state = .expecting
						rv << cur_value
					}
					else {
						return error('invalid token ${i}')
					}
				}
			}
		}
	}

	return rv
}

fn parse_args(argv []string) ?Expression {
	rv := Expression{
		val: 1
	}
	tokens := tokenise(argv.join(' '))?

	println(tokens)
	assert '${tokens}' == '[Value(1), Value(add), Value(subtract), Value(multiply), Value(divide)]'

	return rv
}
