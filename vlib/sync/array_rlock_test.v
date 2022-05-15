fn test_shared_modification() {
	shared foo := &[2, 0, 5]
	lock foo {
		unsafe {
			foo[1] = 3
			foo[0] *= 7
			foo[1]--
			foo[2] -= 2
		}
	}
	rlock foo {
		unsafe {
			assert foo[0] == 14
			assert foo[1] == 2
			assert foo[2] == 3
		}
	}
}

[direct_array_access]
fn test_shared_direct_modification() {
	shared foo := &[2, 0, 5]
	lock foo {
		unsafe {
			foo[1] = 3
			foo[0] *= 7
			foo[1]--
			foo[2] -= 2
		}
	}
	rlock foo {
		unsafe {
			assert foo[0] == 14
			assert foo[1] == 2
			assert foo[2] == 3
		}
	}
}
