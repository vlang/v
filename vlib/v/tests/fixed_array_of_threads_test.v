module main

fn test_fixed_array_of_threads() {
	mut avar := [8]thread string{}
	avar[0] = go printme()
	ret := avar[0].wait()
	assert ret == 'me'
}

fn printme() string {
	println('me')
	return 'me'
}
