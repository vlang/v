module transform

import v.flat

// GenericFnSpecKey keeps source type arguments numeric while declaration symbols stay text.
// The hash only chooses a bucket; full declaration and ordered argument equality decides identity.
struct GenericFnSpecKey {
	decl_key string
	args     []u32
	hash     u64
}

fn (key &GenericFnSpecKey) matches(other GenericFnSpecKey) bool {
	return key.decl_key == other.decl_key && key.args == other.args
}

fn (key &GenericFnSpecKey) clone_owned() GenericFnSpecKey {
	return GenericFnSpecKey{
		decl_key: key.decl_key.clone()
		args:     key.args.clone()
		hash:     key.hash
	}
}

// legacy_text renders only at the lexical export-order and worker byte-hash boundaries.
fn (key &GenericFnSpecKey) legacy_text() string {
	args := flat.TypeTextList{ ids: key.args }.names()
	return '${key.decl_key}[${args.join(', ')}]'
}

struct GenericFnSpecSortKey {
	key         GenericFnSpecKey
	text        string
	write_order u64
}

fn compare_generic_fn_spec_sort_keys(a &GenericFnSpecSortKey, b &GenericFnSpecSortKey) int {
	if a.text < b.text { return -1 }
	if a.text > b.text { return 1 }
	if a.write_order < b.write_order { return -1 }
	return int(a.write_order > b.write_order)
}

struct GenericFnSpecEntry[T] {
	key         GenericFnSpecKey
	value       T
	write_order u64
}

// GenericFnSpecMap stores worker-owned keys and resolves hash collisions with exact numeric IDs.
struct GenericFnSpecMap[T] {
mut:
	buckets          map[u64][]GenericFnSpecEntry[T]
	len              int
	next_write_order u64
}

fn (m &GenericFnSpecMap[T]) get(key GenericFnSpecKey) ?T {
	for entry in m.buckets[key.hash] {
		if entry.key.matches(key) { return entry.value }
	}
	return none
}

fn (m &GenericFnSpecMap[T]) contains(key GenericFnSpecKey) bool {
	for entry in m.buckets[key.hash] {
		if entry.key.matches(key) { return true }
	}
	return false
}

fn (mut m GenericFnSpecMap[T]) set(key GenericFnSpecKey, value T) {
	m.next_write_order++
	for i, entry in m.buckets[key.hash] {
		if entry.key.matches(key) {
			// Replace the key too: a parent-arena re-record must discard the worker copy.
			m.buckets[key.hash][i] = GenericFnSpecEntry[T]{ key: key.clone_owned(), value: value, write_order: m.next_write_order }
			return
		}
	}
	m.buckets[key.hash] << GenericFnSpecEntry[T]{ key: key.clone_owned(), value: value, write_order: m.next_write_order }
	m.len++
}

fn (mut m GenericFnSpecMap[T]) delete(key GenericFnSpecKey) {
	for i, entry in m.buckets[key.hash] {
		if entry.key.matches(key) {
			m.buckets[key.hash].delete(i)
			if m.buckets[key.hash].len == 0 { m.buckets.delete(key.hash) }
			m.len--
			return
		}
	}
}

fn (mut m GenericFnSpecMap[T]) clear() {
	m.buckets.clear()
	m.len = 0
	m.next_write_order = 0
}

fn (m &GenericFnSpecMap[T]) write_order(key GenericFnSpecKey) u64 {
	for entry in m.buckets[key.hash] {
		if entry.key.matches(key) { return entry.write_order }
	}
	return 0
}

fn (m &GenericFnSpecMap[T]) keys() []GenericFnSpecKey {
	mut keys := []GenericFnSpecKey{cap: m.len}
	for _, bucket in m.buckets {
		for entry in bucket { keys << entry.key }
	}
	return keys
}
