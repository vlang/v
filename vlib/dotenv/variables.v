module dotenv

struct EnvironmentVariable {
	// The name of the envrionment variable
	variable_name string
	// The variable value
	variable_value string
}

// Validate the variable strings returned from the
// tokeniser
fn create_env_variables(data []string) ?[]EnvironmentVariable {
	mut environment_variables := []EnvironmentVariable{}
	for index := 0; index < data.len; index++ {
		current := data[index]

		// Make sure that the line contains only
		// one assignment character
		assignment_counts := current.count('=')
		if (assignment_counts > 1) || (assignment_counts < 1) {
			return error('Expected 1 assignment but got $assignment_counts')
		}
		statement := current.split('=')
		if statement.len != 2 {
			return error('Unexpected statement length, Expected 2 got $statement.len')
		}

		// Create a new variable object and append
		// it into the list of variables
		environment_variables << EnvironmentVariable{
			variable_name: statement[0]
			variable_value: statement[1]
		}
	}
	return environment_variables
}
