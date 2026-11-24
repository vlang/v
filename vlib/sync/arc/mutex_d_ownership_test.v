// vtest ownership
module arc

struct MutexLog implements Drop {
mut:
	entries  []string
	released chan int
}

fn (mut log MutexLog) drop() {
	log.released <- log.entries.len
	drop_owned(log.entries)
}

fn add_mutex_log_entry(log MutexLog) MutexUpdate[MutexLog, int] {
	mut next := log
	next.entries << 'entry'.to_owned()
	count := next.entries.len
	return MutexUpdate[MutexLog, int]{ value: next, result: count }
}

fn fill_mutex_log(log Mutex[MutexLog]) int {
	for _ in 0 .. 10 {
		log.update(add_mutex_log_entry)
	}
	return log.update(fn (value MutexLog) MutexUpdate[MutexLog, int] {
		count := value.entries.len
		return MutexUpdate[MutexLog, int]{ value: value, result: count }
	})
}

fn test_mutex_workers_own_and_release_the_protected_value() {
	released := chan int{cap: 32}
	mut log := new_mutex(MutexLog{ released: released })
	first := spawn fill_mutex_log(log.clone())
	second := spawn fill_mutex_log(log.clone())
	log.drop()
	first_count := first.wait()
	second_count := second.wait()
	assert first_count >= 10 && second_count >= 10
	assert first_count == 20 || second_count == 20
	assert <-released == 20
	mut extra := 0
	assert released.try_pop(mut extra) == .not_ready
}

fn rotate_mutex_option(value ?string) MutexUpdate[?string, int] {
	if text := value {
		return MutexUpdate[?string, int]{ value: none, result: text.len }
	}
	return MutexUpdate[?string, int]{ value: 'ready'.to_owned() }
}

fn test_mutex_can_store_an_optional_value() {
	mutex := new_mutex(?string(none))
	assert mutex.update(rotate_mutex_option) == 0
	assert mutex.update(rotate_mutex_option) == 5
}

fn append_mutex_log_label(log MutexLog, label string) MutexUpdate[MutexLog, int] {
	mut next := log
	next.entries << label
	return MutexUpdate[MutexLog, int]{ value: next }
}

fn swap_mutex_logs(first MutexLog, second MutexLog) MutexPairUpdate[MutexLog, bool] {
	return MutexPairUpdate[MutexLog, bool]{
		first:  second
		second: first
		result: true
	}
}

fn swap_mutex_logs_repeatedly(first Mutex[MutexLog], second Mutex[MutexLog]) {
	for _ in 0 .. 200 {
		assert first.update_pair(&second, swap_mutex_logs)
	}
}

fn test_mutex_pair_updates_acquire_locks_in_the_same_order() {
	released := chan int{cap: 4}
	mut first := new_mutex(MutexLog{ released: released })
	mut second := new_mutex(MutexLog{ released: released })
	first.update_with('one'.to_owned(), append_mutex_log_label)
	second.update_with('two'.to_owned(), append_mutex_log_label)
	second.update_with('three'.to_owned(), append_mutex_log_label)

	left := spawn swap_mutex_logs_repeatedly(first.clone(), second.clone())
	right := spawn swap_mutex_logs_repeatedly(second.clone(), first.clone())
	left.wait()
	right.wait()

	first.drop()
	second.drop()
	assert <-released == 1
	assert <-released == 2
}
