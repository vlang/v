fn test_channel_try_buffered() {
	ch := chan int{cap: 5}
	for z in 2..13 {
		if ch.try_push(z) == .not_ready {
			assert z == 7
			break
		}
	}
	mut obj := int(0)
	for ch.try_pop(mut obj) == .success {
		println(obj)
	}
	assert obj == 6
	ch <- 17
	obj = <-ch
	assert obj == 17
}
