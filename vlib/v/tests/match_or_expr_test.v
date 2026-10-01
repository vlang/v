struct Foo {
	option ?int = unsafe { none }
}

fn test_main() {
	test := true
	foo := Foo{}
	result := foo.option or {
		match test {
			true { 1 }
			else { 2 }
		}
	}

	assert result == 1
}

struct Issue17850Error {
	Error
}

struct Issue17850Data {
pub:
	name string
}

fn (_ &Issue17850Error) msg() string {
	return 'issue 17850'
}

fn issue17850_do_thing(name string, fail bool) !Issue17850Data {
	if fail {
		return Issue17850Error{}
	}
	return Issue17850Data{
		name: name
	}
}

fn test_result_or_block_match_error_type() {
	data := issue17850_do_thing('my_db', true) or {
		match err {
			Issue17850Error {
				Issue17850Data{
					name: 'my_db'
				}
			}
			else {
				panic(err)
			}
		}
	}
	assert data == Issue17850Data{
		name: 'my_db'
	}
}

struct ExhaustiveMatchReturnError {
	Error
}

fn exhaustive_match_return_source() !int {
	return ExhaustiveMatchReturnError{}
}

fn result_or_block_exhaustive_match_returns() !bool {
	value := exhaustive_match_return_source() or {
		match err {
			ExhaustiveMatchReturnError {
				return true
			}
			else {
				return err
			}
		}
	}
	return value > 0
}

fn test_result_or_block_exhaustive_match_returns() {
	assert result_or_block_exhaustive_match_returns()!
}
