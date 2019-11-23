module main

fn check_string_eq () {
	println ("checking string_eq")
	assert "monkey" != "rat"
	some_animal := "a bird"
	assert some_animal == "a bird"
	println ("string_eq passed")
}

fn main () {
	check_string_eq ()
	exit(0)
}

