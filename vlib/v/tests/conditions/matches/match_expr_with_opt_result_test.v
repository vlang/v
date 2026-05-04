struct Foo {
pub:
	a int
}

fn foo() !Foo {
	return Foo{1}
}

fn bar() ?Foo {
	return Foo{2}
}

fn read_bool(ok bool) !bool {
	if ok {
		return true
	}
	return error('read_bool failed')
}

fn test_main() {
	match foo()!.a {
		1 {
			assert true
		}
		else {
			assert false
		}
	}

	match bar()?.a {
		2 {
			assert true
		}
		else {
			assert false
		}
	}
}

fn test_match_expr_with_trailing_or_block() {
	mut seen_err := ''
	got_false := match read_bool(false) {
		true { 'true' }
		false { 'false' }
	} or {
		seen_err = err.msg()
		false
	}

	assert seen_err == 'read_bool failed'
	assert got_false == 'false'

	got_true := match read_bool(true) {
		true { 'true' }
		false { 'false' }
	} or {
		assert false
		false
	}

	assert got_true == 'true'
}
