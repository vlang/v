module main

enum ReadyState {
	pending
	ready
}

struct Entry {
	number int
}

struct OptionalFields {
	callback ?fn (ReadyState)
mut:
	entry ?Entry
	flag  bool
}

fn read_entry(entry &Entry) int {
	return entry.number
}

fn read_optional_entry(fields OptionalFields) !int {
	return read_entry(&(fields.entry or { return error('missing entry') }))
}

fn read_checked_map(entries map[string]int, key string) !int {
	return entries[key]!
}

fn read_checked_array(entries []int, index int) !int {
	return entries[index]!
}

fn test_parallel_assignment_and_address_of_optional_fields() {
	mut fields := OptionalFields{}
	fields.entry, fields.flag = Entry{42}, true
	assert fields.flag
	assert read_optional_entry(fields)! == 42
	mut flag := true
	flag, fields.entry = false, Entry{17}
	assert !flag
	assert read_optional_entry(fields)! == 17
}

fn test_optional_callback_keeps_its_parameter_type() {
	fields := OptionalFields{
		callback: fn (state ReadyState) {
			assert state == .ready
		}
	}
	if callback := fields.callback {
		callback(.ready)
	} else {
		assert false
	}
}

fn test_checked_map_index_propagates_a_result() {
	entries := {
		'known': 42
	}
	assert read_checked_map(entries, 'known')! == 42
	if _ := read_checked_map(entries, 'missing') {
		assert false
	} else {
		assert err.msg() == 'map key does not exist'
	}
}

fn test_checked_array_index_propagates_a_result() {
	assert read_checked_array([42], 0)! == 42
	if _ := read_checked_array([42], 1) {
		assert false
	} else {
		assert err.msg() == ''
	}
}
