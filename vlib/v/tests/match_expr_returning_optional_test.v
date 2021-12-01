type Any = int | string

fn ok(s string) Any {
	return match s {
		'foo' {
			Any(1)
		}
		else {
			Any('asdf')
		}
	}
}

fn fails(s string) ?Any {
	return match s {
		'foo' {
			Any(1)
		}
		else {
			Any('asdf')
		}
	}
}

fn test_match_expr_returning_optional() {
	ret1 := ok('foo')
	println(ret1)
	assert ret1 == Any(1)

	ret2 := fails('foo') or {
		assert false
		return
	}
	println(ret2)
	assert ret2 == Any(1)
}

fn func() ?string {
	code := 0
	return match code {
		0 { 'zero' }
		else { error('as we are returning an optional') }
	}
}

fn test_match_expr_returning_optional_with_error() {
	ret := func() or { 'error' }
	println(ret)
	assert ret == 'zero'
}
