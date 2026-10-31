module main

import os

fn (mut c Create) set_lib_project_files() {
	base := if c.new_dir { c.name } else { '' }
	// A package name may contain hyphens, but the V module identifier cannot.
	module_name := c.name.replace('-', '_')
	// Imports resolve directories; preserve the manifest when its name differs.
	module_dir := if c.new_dir || module_name == os.file_name(os.getwd()) {
		base
	} else {
		os.join_path(base, module_name)
	}
	c.files << ProjectFiles{
		path:    os.join_path(module_dir, module_name + '.v')
		content: 'module ${module_name}

// square calculates the second power of `x`
pub fn square(x int) int {
	return x * x
}
'
	}
	c.files << ProjectFiles{
		path:    os.join_path(base, 'tests', 'square_test.v')
		content: 'import ${module_name}

fn test_square() {
	assert ${module_name}.square(2) == 4
}
'
	}
}
