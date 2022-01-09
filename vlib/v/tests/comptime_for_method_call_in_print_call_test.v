module main

struct S1 {}

fn test_comptime_for_method_call_str() {
	s1 := S1{}
	mut rets := []string{}

	$for method in S1.methods {
		println(s1.$method('ll'))
		rets << s1.$method('ll').str()
	}
	assert rets.len == 4
	assert rets[0] == '7'
	assert rets[1] == '7.7'
	assert rets[2] == 'foo'
	assert rets[3] == 'true'
}

fn (t S1) m1(s string) int {
	return 7
}

fn (t S1) m2(s string) f64 {
	return 7.7
}

fn (t S1) m3(s string) string {
	return 'foo'
}

fn (t S1) m4(s string) bool {
	return true
}
