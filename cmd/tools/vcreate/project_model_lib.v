module main

fn (mut c Create) set_lib_project_files() {
	c.files << ProjectFiles{
		path: '${c.name}/src/${c.name}.v'
		content: 'module ${c.name}

// square calculates the second power of `x`
pub fn square(x int) int {
	return x * x
}
'
	}
	c.files << ProjectFiles{
		path: '${c.name}/tests/square_test.v'
		content: 'import ${c.name}

fn test_square() {
	assert ${c.name}.square(2) == 4
}
'
	}
}
