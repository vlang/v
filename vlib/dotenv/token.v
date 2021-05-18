module dotenv

struct Position {
mut:
	// The current position index of the tokeniser
	// in the data array
	position int
}

// Increment the position by 1
fn (mut position Position) increment() int {
	position.position += 1
	return position.position
}

// Decremented the position by 1
fn (mut position Position) decrement() int {
	position.position += -1
	return position.position
}

// Get the current character based on the
// position and the data array
fn (position Position) current_character(data []string) string {
	if position.position == data.len {
		return '\n'
	} else {
		return data[position.position]
	}
}

// Tokenise the string into list of variable
// strings for further functions
fn tokenise(content string) []string {
	mut position := Position{}

	// The content of the file splitted into
	// lines based on newline characters('\n')
	contents := content.split_into_lines()

	mut variables := []string{}

	mut character := position.current_character(contents)
	for index := 0; index < contents.len; index++ {
		// Ignore comments present in the .env file
		// Comments in .env files start with a hash(#)
		if !character.starts_with('#') {
			variables << character
		}
		position.position = index + 1
		character = position.current_character(contents)
	}
	return variables
}