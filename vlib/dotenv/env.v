module dotenv

import os { is_file, read_file, setenv, join_path, dir }

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
pub fn loadenv() ?int {
	filename := join_path(dir(@FILE), '.env')
	// Checks if the file exists, else throw
	// an error
	if is_file(filename) {
		// Read the file and create variables
		file_content := read_file(filename) or {
			panic('Error reading $filename')
			return 0
		}
		data := create_env_variables(tokenise(file_content)) or {
			panic(err)
			return 0
		}
		load(data)
		return 0
	} else {
		return error('$filename is not a file')
	}
}