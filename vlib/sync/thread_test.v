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

fn test_sync_thread_local_storage() {
	// test int
	mut tls_int := sync.new_tls(100)!
	dump(tls_int)

	mut x := tls_int.get()!
	assert x == 100
	tls_int.set(200)!
	x = tls_int.get()!
	assert x == 200
	tls_int.destroy()!
	if _ := tls_int.get() {
		assert false
	} else {
		assert err.msg() == 'get: TLS storage is already destroyed'
	}
	tls_int.set(500) or { assert err.msg() == 'set: TLS storage is already destroyed' }

	// test pointer
	mut a := 100
	mut b := 200
	mut tls_ptr := sync.new_tls(&a)!
	dump(tls_ptr)

	mut p := tls_ptr.get()!
	assert p == &a
	tls_ptr.set(&b)!
	p = tls_ptr.get()!
	assert p == &b
	tls_ptr.destroy()!
	if _ := tls_ptr.get() {
		assert false
	} else {
		assert err.msg() == 'get: TLS storage is already destroyed'
	}
	tls_ptr.set(&a) or { assert err.msg() == 'set: TLS storage is already destroyed' }
}
