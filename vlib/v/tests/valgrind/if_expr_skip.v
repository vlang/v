fn main() {
	mut a := false
	b := a && if true {
		a = true
		true
	} else {
		false
	}
	println(b)
	assert b == false
	println(a)
	assert a == false
}
