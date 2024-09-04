fn test_anon_fn_direct_call_with_option() {
	z := true
	a := fn [z] () ?int {
		match z {
			true { return 1 }
			else { return none }
		}
	}()
	b := a or {
		println('failed')
		return
	}

	println('b: ${b}')
	println(a)
	assert b == 1
}
