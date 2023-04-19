fn test_nested_multiline_comments_1() {
	/*//println(os.args)
	*/
	assert true
}

fn test_nested_multiline_comments_2() {
	tt()
	assert true
}

/*
fn tt() {
}
//*/

fn tt() {
	println('hello, world')
}
