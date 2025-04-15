module main

import os

fn (mut c Create) set_lib_project_files() {
	base := if c.new_dir { c.name } else { '' }
	c.files << ProjectFiles{
		path:    os.join_path(base, c.name + '.v')
		content: 'module ${c.name}

// square calculates the second power of `x`
pub fn square(x int) int {
	return x * x
}
'
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'tests', 'square_test.v')
		content: 'import ${c.name}

fn test_square() {
	assert ${c.name}.square(2) == 4
}
'
	}
}
