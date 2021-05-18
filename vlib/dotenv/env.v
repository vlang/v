module dotenv

import os { dir, is_file, join_path, read_file, setenv }

// Load all the variables in the list of
// variables passed in as the parameter
// Environment variables are overwritten
fn load(variables []EnvironmentVariable) {
	for index := 0; index < variables.len; index++ {
		current := variables[index]
		setenv(current.variable_name, current.variable_value, true)
	}
}

// The public loadenv function that the user
// have access to
pub fn loadenv() {
	filename := join_path(dir(@FILE), '.env')

	// Checks if the file exists, else throw
	// an error
	if is_file(filename) {
		// Read the file and create variables
		file_content := read_file(filename) or { panic(err) }
		data := create_env_variables(tokenise(file_content)) or { panic(err) }
		load(data)
	}
}
