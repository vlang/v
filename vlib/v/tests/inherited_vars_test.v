struct Shared {
mut:
	a int
}

fn test_atomic() {
	atomic a := 0
	a++

	fn [atomic a] () {
		a++
		dump(a)
	}()
	dump(a)
	assert a == 1
}

fn test_shared() {
	shared b := Shared{
		a: 0
	}

	fn [shared b] () {
		lock b {
			b.a++
		}
	}()
	rlock b {
		dump(b.a)
		assert b.a == 1
	}
}
