fn ret_ten() int {
	return 10
}

fn test_thread_str() {
	th := spawn ret_ten()
	assert th.str() == 'thread(int)'
}
