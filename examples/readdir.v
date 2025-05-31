import os

// Example usage of os.ls (in the form of top-level statements)
println('readdir example using os.ls')

entries := os.ls(os.home_dir()) or { [] }

for entry in entries {
	if os.is_dir(os.join_path(os.home_dir(), entry)) {
		println('dir: ${entry}')
	} else {
		println('file: ${entry}')
	}
}
