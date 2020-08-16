import sync

const (
	num_iterations = 10000
)

struct St {
mut:
	n int
}

// this function gets an array of channels for `St` references
fn do_rec_calc_send(chs0, chs1 chan mut St, sem sync.Semaphore) {
	mut s := &St(0)
	for _ in 0 .. num_iterations {
		(&sync.Channel(chs0)).pop(&s)
		s.n++
		(&sync.Channel(chs1)).push(&s)
	}
	sem.post()
}

fn test_channel_array_mut() {
	mut chs0 := chan mut St{cap: 1}
	mut chs1 := chan mut St{}
	sem := sync.new_semaphore()
	go do_rec_calc_send(chs0, chs1, sem)
	mut t := &St{
		n: 100
	}
	for _ in 0 .. num_iterations {
		(&sync.Channel(chs0)).push(&t)
		(&sync.Channel(chs1)).pop(&t)
	}
	// (&sync.Channel(chs0)).close()
	sem.wait()
	assert t.n == 100 + num_iterations
}
