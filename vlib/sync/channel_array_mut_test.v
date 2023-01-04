const (
	num_iterations = 10000
)

struct St {
mut:
	dummy  i64
	dummy2 u32
	dummy3 i64
	n      int
	dummy4 int
}

// this function gets an array of channels for `St` references
fn do_rec_calc_send(chs []chan mut St) {
	for {
		mut s := <-chs[0] or { break }
		s.n++
		chs[1] <- s
	}
}

fn test_channel_array_mut() {
	mut chs := [chan mut St{cap: 1}, chan mut St{}]
	spawn do_rec_calc_send(chs)
	mut t := &St{
		n: 100
	}
	for _ in 0 .. num_iterations {
		chs[0] <- t
		t = <-chs[1]
	}
	chs[0].close()
	assert t.n == 100 + num_iterations
}
