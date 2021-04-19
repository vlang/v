fn test_channel_try_unbuffered() {
	ch := chan int{}
	for z in 5..8 {
		if ch.try_push(z) == .not_ready {
			assert z == 5
			break
		}
		panic('push on non-ready channel not detected')
	}
	mut obj := -17
	for ch.try_pop(mut obj) == .success {}
	assert obj == -17
}
