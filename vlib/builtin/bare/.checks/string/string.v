module main

__global buffer [128]byte

fn check_string_eq () {
	println ("checking string_eq")
	assert "monkey" != "rat"
	some_animal := "a bird"
	assert some_animal == "a bird"
	println ("string_eq passed")
}

fn main () {
	s := i64_tos(buffer, 70, 140, 10)
	s1 := i64_tos(buffer, 70, -140, 10)
	println(s)
	println(s1)

	check_string_eq ()
	sys_exit(0)
}

