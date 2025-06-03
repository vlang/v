fn g[T](x T, is_pointer bool, is_voidptr bool) {
	println('type: ${typeof[T]().name}, isreftype(T): ${isreftype(T)}')
	$if T is $pointer {
		println('T is \$pointer')
		assert is_pointer
	} $else {
		println('T is NOT a \$pointer')
		assert !is_pointer
	}
	$if T is $voidptr {
		println('T is \$voidptr')
		assert is_voidptr
	} $else {
		println('T is NOT a \$voidptr')
		assert !is_voidptr
	}
	println('--------------------------')
}

struct Abc {}

fn test_is_pointer_and_is_voidptr() {
	unsafe {
		g(voidptr(123), true, true)
		g(&char(456), true, false)
		//
		g(int(1000), false, false)
		g(&int(1001), true, false)
		g(&&int(1002), true, false)
		g(&&&int(1003), true, false)
		//
		g(Abc{}, false, false)
		g(&Abc(10001), true, false)
		g(&&Abc(10002), true, false)
		g(&&&Abc(10003), true, false)
	}
}
