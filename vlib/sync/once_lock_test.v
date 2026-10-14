import sync
import sync.stdatomic { new_atomic }

// Concurrent get_or_init must run the initializer exactly once and every
// caller must observe the same value.
fn test_once_lock_get_or_init_runs_once() {
	mut cell := sync.new_once_lock[int]()
	mut calls := new_atomic(0)
	mut bad := new_atomic(0)
	done_ch := chan bool{cap: 8}
	defer {
		done_ch.close()
	}
	for _ in 0 .. 8 {
		spawn fn [mut cell, mut calls, mut bad, done_ch] () {
			for _ in 0 .. 10 {
				v := cell.get_or_init(fn [mut calls] () int {
					calls.add(1)
					return 42
				})
				if v != 42 {
					bad.add(1)
				}
			}
			done_ch <- true
		}()
	}
	for _ in 0 .. 8 {
		_ := <-done_ch
	}
	assert calls.load() == 1, 'initializer must run exactly once'
	assert bad.load() == 0, 'all callers must observe the same value'
	assert cell.get() or { -1 } == 42, 'stored value must be visible afterwards'
}

// set stores the first value and rejects later ones.
fn test_once_lock_set_semantics() {
	mut cell := sync.new_once_lock[int]()
	assert cell.set(1) == true, 'first set must succeed'
	assert cell.set(2) == false, 'second set must fail'
	assert cell.get() or { -1 } == 1, 'first value must be kept'
}

// get before any initialisation is none.
fn test_once_lock_get_none_before_init() {
	mut cell := sync.new_once_lock[string]()
	v := cell.get() or { 'empty' }
	assert v == 'empty', 'get before init must be none'
}

// LazyLock computes once on first access, under concurrency.
fn test_lazy_lock_initializes_once() {
	mut calls := new_atomic(0)
	mut lazy := sync.new_lazy_lock(fn [mut calls] () int {
		calls.add(1)
		return 99
	})
	mut bad := new_atomic(0)
	done_ch := chan bool{cap: 8}
	defer {
		done_ch.close()
	}
	for _ in 0 .. 8 {
		spawn fn [mut lazy, mut bad, done_ch] () {
			if lazy.get() != 99 {
				bad.add(1)
			}
			done_ch <- true
		}()
	}
	for _ in 0 .. 8 {
		_ := <-done_ch
	}
	assert calls.load() == 1, 'lazy initializer must run exactly once'
	assert bad.load() == 0, 'all callers must observe the same value'
}

fn test_once_lock_preserves_set_value_without_running_initializer() {
	mut cell := sync.new_once_lock[int]()
	mut calls := new_atomic(0)
	assert cell.set(7)
	assert cell.get_or_init(fn [mut calls] () int {
		calls.add(1)
		return 42
	}) == 7
	assert calls.load() == 0
}
