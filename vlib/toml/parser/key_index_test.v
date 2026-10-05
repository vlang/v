module parser

fn test_key_index_preserves_segments_and_duplicate_removal() {
	mut index := KeyIndex{}
	quoted := DottedKey(['a.b', 'c'])
	dotted := DottedKey(['a', 'b.c'])
	index.add(quoted)
	index.add(quoted)
	assert index.has(quoted)
	assert !index.has(dotted)
	assert index.remove(quoted)
	assert index.has(quoted)
	assert index.remove(quoted)
	assert !index.has(quoted)
	assert !index.remove(quoted)
}

fn test_key_index_owns_its_path_segments() {
	mut index := KeyIndex{}
	mut key := DottedKey(['table', 'field'])
	index.add(key)
	key[1] = 'changed'
	assert index.has(DottedKey(['table', 'field']))
	assert !index.has(key)
}

fn test_key_index_checks_hash_collisions_structurally() {
	key := DottedKey(['table', 'value'])
	other := DottedKey(['other', 'value'])
	mut index := KeyIndex{}
	index.add(other)
	index.heads[key.hash()] = index.heads[other.hash()]
	assert !index.has(key)
	assert !index.remove(key)
	index.add(key)
	assert index.has(key)
	assert index.remove(key)
	assert index.heads[key.hash()] == index.heads[other.hash()]
}

fn test_key_index_parent_excludes_root_and_single_segment() {
	mut index := KeyIndex{}
	root := DottedKey(['table', 'root'])
	key := DottedKey(['table', 'root', 'child', 'value'])
	index.add(DottedKey(['table']))
	index.add(root)
	assert !index.has_parent(key, root)
	index.add(DottedKey(['table', 'root', 'child']))
	assert index.has_parent(key, root)
	assert !index.has_parent(root, root)
}

fn test_repeated_paths_share_storage_and_keep_multiplicity() {
	mut index := KeyIndex{}
	key := DottedKey(['entry', 'name'])
	for _ in 0 .. 10000 {
		index.add(key)
	}
	assert index.keys.len == 1
	assert index.parts.len == key.len
	for _ in 0 .. 10000 {
		assert index.has(key)
		assert index.remove(key)
	}
	assert !index.has(key)
	assert !index.remove(key)
}

fn test_index_release_preserves_borrowed_text() {
	key := DottedKey(['table'.clone(), 'field'.clone()])
	mut index := KeyIndex{}
	index.add(key)
	index.free()
	assert key == DottedKey(['table', 'field'])
}
