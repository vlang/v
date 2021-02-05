struct St {
mut:
	i int
}

fn test_lock_expr() {
	shared xx := St{ i: 173 }
	shared y := St{ i: -57 }
	mut m := 0
	m = lock y { y.i }
	n := rlock xx { xx.i }
	assert m == -57
	assert n == 173
}
