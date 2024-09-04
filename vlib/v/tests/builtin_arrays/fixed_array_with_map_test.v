module main

struct File {
	root string
	path string
}

struct Test {
mut:
	mod_files map[string][5]File
}

fn test_main() {
	mut test := Test{}
	for i in 0 .. 4 {
		test.mod_files['main'][i] = File{}
	}
	test.mod_files['main'][3] = File{
		root: 'foo'
		path: 'bar'
	}
	assert test.mod_files['main'][3] == File{
		root: 'foo'
		path: 'bar'
	}
}
