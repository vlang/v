import sync

fn do_rec(mut ch sync.Channel, mut resch sync.Channel) {
	mut sum := i64(0)
	for {
		mut a := 0
		if !ch.pop(&a) {
			break
		}
		sum += a
	}
	println(sum)
	resch.push(&sum)
}

fn do_send(mut ch sync.Channel) {
	for i in 0 .. 8000 {
		ch.push(&i)
	}
	ch.close()
}

fn test_channel_close_buffered_multi() {
	mut ch := sync.new_channel<int>(0)
	mut resch := sync.new_channel<i64>(100)
	go do_rec(mut ch, mut resch)
	go do_rec(mut ch, mut resch)
	go do_rec(mut ch, mut resch)
	go do_rec(mut ch, mut resch)
	go do_send(mut ch)
	mut sum := i64(0)
	for _ in 0 .. 4 {
		mut r := i64(0)
		resch.pop(&r)
		sum += r
	}
	assert sum == i64(8000) * (8000 - 1) / 2
}

fn test_channel_close_unbuffered_multi() {
	mut ch := sync.new_channel<int>(0)
	mut resch := sync.new_channel<i64>(100)
	go do_rec(mut ch, mut resch)
	go do_rec(mut ch, mut resch)
	go do_rec(mut ch, mut resch)
	go do_rec(mut ch, mut resch)
	go do_send(mut ch)
	mut sum := i64(0)
	for _ in 0 .. 4 {
		mut r := i64(0)
		resch.pop(&r)
		sum += r
	}
	assert sum == i64(8000) * (8000 - 1) / 2
}

fn test_channel_close_buffered() {
	mut ch := sync.new_channel<int>(0)
	mut resch := sync.new_channel<i64>(100)
	go do_rec(mut ch, mut resch)
	go do_send(mut ch)
	mut sum := i64(0)
	mut r := i64(0)
	resch.pop(&r)
	sum += r
	assert sum == i64(8000) * (8000 - 1) / 2
}

fn test_channel_close_unbuffered() {
	mut ch := sync.new_channel<int>(0)
	mut resch := sync.new_channel<i64>(100)
	go do_rec(mut ch, mut resch)
	go do_send(mut ch)
	mut sum := i64(0)
	mut r := i64(0)
	resch.pop(&r)
	sum += r
	assert sum == i64(8000) * (8000 - 1) / 2
}
