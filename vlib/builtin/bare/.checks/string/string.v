module main
import forkedtest

fn check_string_eq () {
	assert "monkey" != "rat"
	some_animal := "a bird"
	assert some_animal == "a bird"
}

fn check_i64_tos() {
	buffer, e := mm_alloc(128)
	assert e == .enoerror
	assert !isnil(buffer)

	s0 := i64_tos(buffer, 70, 140, 10)
	assert s0 == "140"

	s1 := i64_tos(buffer, 70, -160, 10)
	assert s1 == "-160"

	s2 := i64_tos(buffer, 70, 65537, 16)
	assert s2 == "10001"

	s3 := i64_tos(buffer, 70, -160000, 10)
	assert s3 == "-160000"

	assert mm_free(buffer) == .enoerror
}

fn main () {
	mut fails := 0
	fails += forkedtest.normal_run(check_string_eq, "check_string_eq")
	fails += forkedtest.normal_run(check_i64_tos, "check_i64_tos")
	assert fails == 0
	sys_exit(0)
}

