struct MyStruct {
	a u16
	b u8
	c u32
	d u64
}

fn test_comptime_if_sizeof() {
	f[u16]()
	g[MyStruct]()

	x := MyStruct{}
	// TODO: support struct.fieldname
	//$if sizeof(x.a) == 2 {
	//	assert true
	//} $else {
	//	assert false
	//}
	//
	//$if sizeof(x.d) != 2 {
	//	assert false
	//} $else {
	//	assert true
	//}
}

fn f[T]() {
	$if sizeof(T) == 2 {
		assert true
	} $else {
		assert false
	}
	$if sizeof(T) != 2 {
		assert false
	} $else {
		assert true
	}
	$if sizeof(T) < 1 {
		assert false
	} $else {
		assert true
	}
	$if sizeof(T) < 3 {
		assert true
	} $else {
		assert false
	}
	$if sizeof(T) > 1 {
		assert true
	} $else {
		assert false
	}
	$if sizeof(T) > 2 {
		assert false
	} $else {
		assert true
	}
	$if sizeof(T) <= 2 {
		assert true
	} $else {
		assert false
	}
	$if sizeof(T) <= 1 {
		assert false
	} $else {
		assert true
	}
	$if sizeof(T) >= 2 {
		assert true
	} $else {
		assert false
	}
	$if sizeof(T) >= 3 {
		assert false
	} $else {
		assert true
	}
}

fn g[T]() {
	$if sizeof(T) == 16 {
		assert true
	} $else {
		assert false
	}
	$if sizeof(T) != 16 {
		assert false
	} $else {
		assert true
	}
	$if sizeof(T) < 17 {
		assert true
	} $else {
		assert false
	}
	$if sizeof(T) < 15 {
		assert false
	} $else {
		assert true
	}
	$if sizeof(T) > 15 {
		assert true
	} $else {
		assert false
	}
	$if sizeof(T) > 16 {
		assert false
	} $else {
		assert true
	}
	$if sizeof(T) <= 16 {
		assert true
	} $else {
		assert false
	}
	$if sizeof(T) <= 15 {
		assert false
	} $else {
		assert true
	}
	$if sizeof(T) >= 16 {
		assert true
	} $else {
		assert false
	}
	$if sizeof(T) >= 17 {
		assert false
	} $else {
		assert true
	}
}
