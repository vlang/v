module parser

import hash { sum64_string }

struct IndexedKey {
	start int
	len   int
mut:
	next  int
	count int = 1
}

struct KeyIndex {
mut:
	heads map[u64]int
	keys  []IndexedKey
	parts []string
}

fn (key IndexedKey) matches(parts []string, candidate DottedKey) bool {
	if key.len != candidate.len {
		return false
	}
	for i, part in candidate {
		if parts[key.start + i] != part {
			return false
		}
	}
	return true
}

fn (key DottedKey) hash() u64 {
	mut code := u64(0)
	for part in key {
		code = sum64_string(part, code)
	}
	return code
}

fn (index &KeyIndex) has(key DottedKey) bool {
	mut position := index.heads[key.hash()]
	for position > 0 {
		entry := index.keys[position - 1]
		if entry.matches(index.parts, key) {
			return entry.count > 0
		}
		position = entry.next
	}
	return false
}

fn (mut index KeyIndex) add(key DottedKey) {
	code := key.hash()
	mut position := index.heads[code]
	for position > 0 {
		entry := index.keys[position - 1]
		if entry.matches(index.parts, key) {
			index.keys[position - 1].count++
			return
		}
		position = entry.next
	}
	entry := IndexedKey{
		start: index.parts.len
		len:   key.len
		next:  index.heads[code]
	}
	unsafe {
		index.keys.flags |= .noslices
		index.parts.flags |= .noslices
	}
	index.parts << key
	index.keys << entry
	index.heads[code] = index.keys.len
}

fn (mut index KeyIndex) remove(key DottedKey) bool {
	code := key.hash()
	mut position := index.heads[code]
	mut previous := 0
	for position > 0 {
		entry := index.keys[position - 1]
		if !entry.matches(index.parts, key) {
			previous = position
			position = entry.next
			continue
		}
		if entry.count > 1 {
			index.keys[position - 1].count--
			return true
		}
		if entry.count == 0 {
			return false
		}
		if previous == 0 {
			index.heads[code] = entry.next
		} else {
			index.keys[previous - 1].next = entry.next
		}
		return true
	}
	return false
}

fn (mut index KeyIndex) reset(prefix DottedKey) {
	for i, entry in index.keys {
		if entry.count == 0 || entry.len < prefix.len {
			continue
		}
		parent := IndexedKey{
			start: entry.start
			len:   prefix.len
		}
		if parent.matches(index.parts, prefix) {
			index.keys[i].count = 0
		}
	}
}

fn (index &KeyIndex) has_parent(key DottedKey, root DottedKey) bool {
	for length in 2 .. key.len {
		parent := DottedKey(key[..length])
		if parent != root && index.has(parent) {
			return true
		}
	}
	return false
}

fn (mut index KeyIndex) free() {
	index.parts.clear()
	unsafe {
		index.heads.free()
		index.keys.free()
		index.parts.free()
	}
}
