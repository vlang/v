fn test_generic_fn_multi_return() {
	mut a1 := GenRef<u32, u32>{32, 99}
	b1, c1 := a1.generic_reference() or {
		assert false
		return
	}
	println(b1)
	assert b1 == 32
	println(c1)
	assert *c1 == 99

	mut a2 := GenRef<u64, u64>{322, 999}
	b2, c2 := a2.generic_reference() or {
		assert false
		return
	}
	println(b2)
	assert b2 == 322
	println(c2)
	assert *c2 == 999

	mut a3 := GenRef<i32, u64>{22, 77}
	b3, c3 := a3.generic_reference() or {
		assert false
		return
	}
	println(b3)
	assert b3 == 22
	println(c3)
	assert *c3 == 77

	mut a4 := GenRef<f64, u64>{2.2, 777}
	b4, c4 := a4.generic_reference() or {
		assert false
		return
	}
	println(b4)
	assert b4 == 2.2
	println(c4)
	assert *c4 == 777
}

struct GenRef<K, V> {
	key K
	val V
}

fn (mut self GenRef<K, V>) generic_reference() ?(K, &V) {
	if false {
		return none
	}
	return self.key, unsafe { &self.val }
}
