struct UserData {
	test string
}

fn test_fn_multiple_returns() {
	name, age, groups, data := fn_mr_get_user()
	assert name == 'joe'
	assert age == 34
	assert groups[0] == 'admins'
	assert groups[1] == 'users'
	assert data.test == 'Test Data'
	println('name: ${name} | age: ${age} | groups: ' + groups.join(',') + ' | data: ${data.test}')
}

fn fn_mr_get_user() (string, int, []string, UserData) {
	groups := ['admins', 'users']
	data := UserData{
		test: 'Test Data'
	}
	return 'joe', 34, groups, data
}

fn split_to_two(s string) !(string, string) {
	mut tokens := s.split_nth(' ', 2)
	if s == '' {
		return error('error')
	}
	if tokens.len != 2 {
		return error('error')
	}
	return tokens[0], tokens[1]
}

fn returnable_fail() string {
	_, _ := split_to_two('bad') or { return 'ok' }
	return 'nok'
}

fn test_multiple_ret() {
	// returnable test
	assert returnable_fail() == 'ok'

	// good case
	res1_1, res1_2 := split_to_two('fish house') or {
		assert false
		return
	}
	assert res1_1 == 'fish'
	assert res1_2 == 'house'

	// none case
	wrapper1 := fn () (string, string) {
		res2_1, res2_2 := split_to_two('') or {
			assert err.msg() == 'error'
			return 'replaced', 'val'
		}
		return res2_1, res2_2
	}
	res2_1, res2_2 := wrapper1()
	assert res2_1 == 'replaced'
	assert res2_2 == 'val'

	// error case
	wrapper2 := fn () (string, string) {
		res3_1, res3_2 := split_to_two('fishhouse') or {
			assert err.msg() == 'error'
			return 'replaced', 'val'
		}
		return res3_1, res3_2
	}
	res3_1, res3_2 := wrapper2()
	assert res3_1 == 'replaced'
	assert res3_2 == 'val'
}

fn multi_values() (string, string) {
	return if 1 > 0 { 'abc', 'def' } else { 'jkl', 'mno' }
}

fn test_multi_values() {
	x, y := multi_values()
	assert x == 'abc'
	assert y == 'def'
}

fn match_expr(x bool) (int, int) {
	return match x {
		true { 1, 1 }
		else { 0, 0 }
	}
}

fn if_expr(x bool) (int, int) {
	return if x { 3, 3 } else { 2, 2 }
}

fn test_multi_return_if_match_expr() {
	a, b := match_expr(true)
	c, d := match_expr(false)
	x, y := if_expr(true)
	z, w := if_expr(false)
	assert a == 1
	assert b == 1
	assert c == 0
	assert d == 0
	assert x == 3
	assert y == 3
	assert z == 2
	assert w == 2
}
