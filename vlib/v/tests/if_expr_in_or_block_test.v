fn foo(a int) !int {
	if a < 0 {
		return error('foo')
	}
	return a
}

fn bar(a int) !int {
	return foo(a) or {
		if a < 0 {
			-1
		} else {
			return error('bar')
		}
	}
}

fn baz(a int) !int {
	return foo(a) or {
		if a < 0 {
			-1
		} else {
			-2
		}
	}
}

fn qux(a int) ?int {
	if a < 0 {
		return none
	}
	return a
}

fn quux(a int) ?int {
	return qux(a) or {
		if a < 0 {
			-1
		} else {
			return none
		}
	}
}

fn test_if_expr_in_result_or_block_with_return() {
	assert bar(0)! == 0
	assert bar(-1)! == -1
	assert bar(1)! == 1
}

fn test_if_expr_in_result_or_block_simple() {
	assert baz(0)! == 0
	assert baz(-1)! == -1
	assert baz(1)! == 1
}

fn test_if_expr_in_option_or_block_with_return() {
	assert quux(0)? == 0
	assert quux(-1)? == -1
	assert quux(1)? == 1
}

fn test_nested_if_in_or_block() {
	get_val := fn (x int) !int {
		if x < 0 {
			return error('negative')
		}
		return x * 2
	}
	result := get_val(-5) or {
		if true {
			if false {
				100
			} else {
				-10
			}
		} else {
			200
		}
	}
	assert result == -10
}

fn test_if_expr_with_multiple_branches_in_or_block() {
	compute := fn (x int) !int {
		if x == 0 {
			return error('zero')
		}
		return x
	}
	val := compute(0) or {
		match err.msg() {
			'zero' {
				1
			}
			'negative' {
				2
			}
			else {
				3
			}
		}
	}
	assert val == 1
}

struct IfExprEmptyReply {
	value int
}

struct IfExprRedisError {
	msg_ string
}

fn (e IfExprRedisError) msg() string {
	return e.msg_
}

fn (e IfExprRedisError) code() int {
	return 0
}

const if_expr_nil_reply = IfExprRedisError{'nil'}

type IfExprReply = IfExprEmptyReply | IfExprRedisError

fn if_expr_read_reply(msg string) !IfExprReply {
	return IError(IfExprRedisError{msg})
}

fn if_expr_decode_reply(msg string) IfExprReply {
	return if_expr_read_reply(msg) or {
		if err is IfExprRedisError {
			if err == if_expr_nil_reply {
				IfExprEmptyReply{0}
			} else {
				err
			}
		} else {
			panic(err)
		}
	}
}

fn test_if_expr_in_or_block_with_smartcasted_interface_sumtype_value() {
	reply1 := if_expr_decode_reply('nil')
	assert reply1 is IfExprEmptyReply
	if reply1 is IfExprEmptyReply {
		assert reply1.value == 0
	}

	reply2 := if_expr_decode_reply('x')
	assert reply2 is IfExprRedisError
	if reply2 is IfExprRedisError {
		assert reply2.msg_ == 'x'
	}
}
