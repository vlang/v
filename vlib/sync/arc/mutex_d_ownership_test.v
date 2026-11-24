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
