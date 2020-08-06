import sync

fn do_rec(mut ch sync.Channel, mut resch sync.Channel) {
	mut sum := i64(0)
	for _ in 0 .. 2000 {
		mut a := 0
		ch.pop(&a)
		sum += a
	}
	println(sum)
	resch.push(&sum)
}

fn do_send(mut ch sync.Channel) {
	for i in 0 .. 2000 {
		ch.push(&i)
	}
}

fn test_channel_multi_buffered() {
	mut ch := sync.new_channel<int>(100)
	mut resch := sync.new_channel<i64>(0)
	go do_rec(mut ch, mut resch)
	go do_rec(mut ch, mut resch)
	go do_rec(mut ch, mut resch)
	go do_rec(mut ch, mut resch)
	go do_send(mut ch)
	go do_send(mut ch)
	go do_send(mut ch)
	go do_send(mut ch)
	mut sum := i64(0)
	for _ in 0 .. 4 {
		mut r := i64(0)
		resch.pop(&r)
		sum += r
	}
	assert sum == i64(4) * 2000 * (2000 - 1) / 2
}
