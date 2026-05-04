fn foo() ! {
	a := match true {
		true {
			2
		}
		2 == 2 {
			return error('hello')
		}
		else {
			0
		}
	}

	println(a)
	assert a == 2
}

fn foo_first_branch_returns(b bool) ! {
	a := match b {
		true {
			return error('hello')
		}
		else {
			2
		}
	}

	println(a)
	assert a == 2
}

fn test_match_expr_with_branch_returning() {
	foo()!
	foo_first_branch_returns(false)!
}
