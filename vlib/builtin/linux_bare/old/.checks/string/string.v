module main
import forkedtest

fn check_string_eq () {
	assert "monkey" != "rat"
	some_animal := "a bird"
	assert some_animal == "a bird"
}

fn check_i64_tos() {
	buffer0 := []byte{len:(128)}
	buffer := byteptr(buffer0.data)

	s0 := i64_tos(buffer, 70, 140, 10)
	assert s0 == "140"

	s1 := i64_tos(buffer, 70, -160, 10)
	assert s1 == "-160"

	s2 := i64_tos(buffer, 70, 65537, 16)
	assert s2 == "10001"

	s3 := i64_tos(buffer, 70, -160000, 10)
	assert s3 == "-160000"
}

fn check_i64_str() {
	assert "141" == i64_str(141, 10)
	assert "-161" == i64_str(-161, 10)
	assert "10002" == i64_str(65538, 16)
	assert "-160001" == i64_str(-160001, 10)
}

fn check_str_clone() {
	a := i64_str(1234,10)
	b := a.clone()
	assert a == b
	c := i64_str(-6789,10).clone()
	assert c == "-6789"
}

fn check_string_add_works(){
  abc := 'abc'
  combined := 'a' + 'b' + 'c'
  assert abc.len == combined.len
  assert abc[0] == combined[0]
  assert abc[1] == combined[1]
  assert abc[2] == combined[2]
  assert abc[0] == `a`
  assert abc == combined
}  

fn main () {
	mut fails := 0
	fails += forkedtest.normal_run(check_string_eq, "check_string_eq")
	fails += forkedtest.normal_run(check_i64_tos, "check_i64_tos")
	fails += forkedtest.normal_run(check_i64_str, "check_i64_str")
	fails += forkedtest.normal_run(check_str_clone, "check_str_clone")
	fails += forkedtest.normal_run(check_string_add_works,    "check_string_add_works")
	assert fails == 0
	sys_exit(0)
}

