fn main() {
	mut a := true
	b := a && if true {
		a = false
		true
	} else {
		false
	}
	println(b)
	assert b == true
}
