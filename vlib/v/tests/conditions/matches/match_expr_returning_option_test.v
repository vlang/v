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

fn test_match_expr_returning_option() {
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
		else { error('as we are returning an option') }
	}
}

fn test_match_expr_returning_option_with_error() {
	ret := func() or { 'error' }
	println(ret)
	assert ret == 'zero'
}

fn match_expr_or_block_return_none(i int) ?int {
	return match i {
		0 { 5 }
		1 { none }
		else { match_expr_or_block_subfunc(i) or { none } }
	}
}

fn match_expr_or_block_subfunc(i int) !int {
	if i == 2 {
		return 2
	}
	return error('subfunc error')
}

fn test_match_expr_or_block_return_none() {
	assert match_expr_or_block_return_none(0) or { panic(err) } == 5
	assert match_expr_or_block_return_none(2) or { panic(err) } == 2
	assert match_expr_or_block_return_none(1) == none
	assert match_expr_or_block_return_none(3) == none
}
