import rand

fn string_test() {
	res := rand.string(4)
	assert res.len == 4
	println(res)
}
