struct MyStruct {
	s string
}

fn new_st() MyStruct {
	return MyStruct{}
}

fn get_st() MyStruct {
	r := new_st()
	return MyStruct{
		...r
		s: '6'
	}
}

fn main() {
	s := get_st()
	println(s)
	$if prod {
		println('prod mode is on')
		assert true
	} $else {
		println('prod mode is off')
		assert false
	}
}
