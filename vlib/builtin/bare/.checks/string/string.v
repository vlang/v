module main

__global buffer [128]byte

fn check_string_eq () {
	println ("checking string_eq")
	assert "monkey" != "rat"
	some_animal := "a bird"
	assert some_animal == "a bird"
	println ("string_eq passed")
}

fn check_i64_tos() {
	s0 := i64_tos(buffer, 70, 140, 10)
	assert s0 == "140"

	s1 := i64_tos(buffer, 70, -160, 10)
	assert s1 == "-160"

	s2 := i64_tos(buffer, 70, 65537, 16)
	assert s2 == "10001"

	s3 := i64_tos(buffer, 70, -160000, 10)
	assert s3 == "-160000"
}

fn main () {
	check_string_eq ()
	check_i64_tos()
	sys_exit(0)
}

