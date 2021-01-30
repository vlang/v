import sync

const (
	num_iterations = 10000
)

fn get_val_from_chan(ch chan i64) ?i64 {
	r := <-ch ?
	return r
}

// this function gets an array of channels for `i64`
fn do_rec_calc_send(chs []chan i64, mut sem sync.Semaphore) {
	mut msg := ''
	for {
		mut s := get_val_from_chan(chs[0]) or {
			msg = err.str()
			break
		}
		s++
		chs[1] <- s
	}
	assert msg == 'channel closed'
	sem.post()
}

fn test_channel_array_mut() {
	mut chs := [chan i64{}, chan i64{cap: 10}]
	mut sem := sync.new_semaphore()
	go do_rec_calc_send(chs, mut sem)
	mut t := i64(100)
	for _ in 0 .. num_iterations {
		chs[0] <- t
		t = <-chs[1]
	}
	chs[0].close()
	sem.wait()
	assert t == 100 + num_iterations
}
