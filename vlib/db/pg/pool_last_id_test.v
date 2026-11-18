// vtest build: !windows && !musl?
module pg

fn stash_and_read_last_id(mut p Pool, id int) int {
	if id != 0 {
		p.stash_last_id(id)
	}
	return p.take_last_id()
}

// The pooled `DB.last_id()` cache is keyed by `sync.thread_id()`, which can be
// reused once a thread is joined, so reading an id must not leave it behind for
// a later thread (or a later operation on the same thread) to pick up.
fn test_pool_last_id_is_not_left_for_a_reused_thread_id() {
	mut p := new_pool('', PoolConfig{})
	worker_a := spawn stash_and_read_last_id(mut p, 4242)
	assert worker_a.wait() == 4242
	worker_b := spawn stash_and_read_last_id(mut p, 0)
	assert worker_b.wait() == 0
	assert p.last_ids.len == 0

	p.stash_last_id(7)
	assert p.take_last_id() == 7
	assert p.take_last_id() == 0
}
