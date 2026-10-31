module heapstore

pub struct Record {
pub mut:
	notes    map[string]i64
	name     string
	elements []Record
	n        int
}

pub struct Env {
pub mut:
	bindings map[string]Record
}

// declared_record stores a local whose address is kept.
pub fn declared_record(mut env Env, mut keep []&Record) {
	mut r := Record{
		n: 1
	}
	keep << &r
	r.n = 2
	env.bindings['declared'] = r
}

// record_read_from_map stores a kept local that was read from the map.
pub fn record_read_from_map(mut env Env, mut keep []&Record) bool {
	mut r := env.bindings['declared'] or { return false }
	keep << &r
	r.n = 3
	env.bindings['read'] = r
	return true
}

// record_in_local_map stores a kept local in a map that is returned.
pub fn record_in_local_map(mut keep []&Record) map[string]Record {
	mut m := map[string]Record{}
	mut r := Record{
		n: 4
	}
	keep << &r
	r.n = 5
	m['local'] = r
	return m
}

// append_to_record updates a record through a call on one of its fields.
pub fn append_to_record(key string, added Record, mut env Env) bool {
	mut r := env.bindings[key] or { return false }
	r.elements = r.elements.clone()
	r.elements << added
	env.bindings[key] = r
	return true
}
