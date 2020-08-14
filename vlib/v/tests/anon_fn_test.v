import sync

fn test_go_anon_fn() {
	mut wg := sync.new_waitgroup()
	go fn (mut wg sync.WaitGroup) {
		wg.done()
	}(mut wg)
	wg.wait()
}
