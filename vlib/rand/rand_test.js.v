// vtest build: present_node?
import rand

fn test_string() {
	res := rand.string(4)
	assert res.len == 4
	println(res)
}
