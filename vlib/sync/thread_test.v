import sync

fn simple_thread() u64 {
	tid := sync.thread_id()
	eprintln('simple_thread thread_id: ${tid.hex()}')
	return tid
}

fn test_sync_thread_id() {
	mtid := sync.thread_id()
	eprintln('main thread_id: ${sync.thread_id().hex()}')
	x := spawn simple_thread()
	y := spawn simple_thread()
	xtid := x.wait()
	ytid := y.wait()
	eprintln('main thread_id: ${sync.thread_id().hex()}')
	dump(xtid.hex())
	dump(ytid.hex())
	assert mtid != xtid
	assert mtid != ytid
	assert xtid != ytid
}
