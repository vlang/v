// vtest vflags: -W -enable-globals
// Like V1, `n := count++` is a plain post-increment and is not reported, even
// with -W; only `f(x++)` and `a[x--]` are.
__global postfix_value_counter = u64(5)

struct PostfixValueStat {
mut:
	ino u64
}

fn test_post_increment_can_be_assigned() {
	x := postfix_value_counter++
	mut stat := PostfixValueStat{}
	stat.ino = postfix_value_counter++
	assert x == 5
	assert stat.ino == 6
	assert postfix_value_counter == 7
	mut i := 10
	y := i--
	assert y == 10
	assert i == 9
}
