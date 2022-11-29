fn main() {
	mut a := true
	b := a && match true {
		true {
			a = false
			true
		}
		false {
			false
		}
	}
	println(a)
	assert a == false
	println(b)
	assert b == true
}
