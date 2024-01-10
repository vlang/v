struct User {
}

fn test_empty_struct_chan_init() {
	user_ch := chan User{cap: 10}
	println(user_ch)
	assert true
}
