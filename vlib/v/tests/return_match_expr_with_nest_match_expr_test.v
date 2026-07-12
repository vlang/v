struct Foo {
}

struct Bar {
	b bool
}

type FooBar = Foo | Bar

fn get_foobar(f FooBar) FooBar {
	return match f {
		Foo {
			Foo{}
		}
		Bar {
			b := match f.b {
				true { false }
				false { true }
			}

			Bar{
				b: b
			}
		}
	}
}

fn test_return_match_expr_with_nest_match_expr() {
	assert get_foobar(Bar{}) == FooBar(Bar{
		b: true
	})
}

type Issue27632Operand = i8 | u8

struct Issue27632Instruction {
	src Issue27632Operand
	dst Issue27632Operand
}

fn issue_27632_oops(itn Issue27632Instruction) !bool {
	return match itn.dst {
		u8 {
			match itn.src {
				u8 {
					res := if itn.src != 0 { true } else { false }
					return res
				}
				else {
					panic('failed to cast src')
				}
			}
		}
		else {
			panic('failed to cast dst')
		}
	}
}

fn test_issue_27632_return_match_result_nested_if_expr() {
	itn := Issue27632Instruction{
		dst: u8(1)
		src: u8(42)
	}
	assert issue_27632_oops(itn)! == true
}

enum MatchResultArgKind {
	a
	b
}

struct MatchResultArgWrapped {
	value MatchResultArgKind
}

fn match_result_arg_wrap(value MatchResultArgKind) MatchResultArgWrapped {
	return MatchResultArgWrapped{
		value: value
	}
}

fn match_result_arg_nested_if(x int, cond bool) !MatchResultArgWrapped {
	return match x {
		0 { match_result_arg_wrap(if cond { .a } else { .b }) }
		else { error('x') }
	}
}

fn test_match_result_context_is_cleared_for_call_args() {
	assert match_result_arg_nested_if(0, false)! == MatchResultArgWrapped{
		value: .b
	}
}
