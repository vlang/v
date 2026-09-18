type CallbackWideKey = u64

type CallbackTextKey = string

struct CallbackKeyMaps {
mut:
	words map[int]int
	wide  map[CallbackWideKey]int
	text  map[CallbackTextKey]int
}

fn test_int_map_keys_keep_high_bits_when_hashing_comparing_and_cloning() {
	if sizeof(int) != 8 {
		return
	}
	// All keys have the same low 32 bits. Four-byte equality conflates them,
	// and four-byte cloning loses the rest of each key even without a collision.
	keys := [int(7), int((u64(1) << 32) + 7), int((u64(3) << 40) + 7), -int((u64(1) << 40) - 7)]
	mut local := map[int]int{}
	mut fields := CallbackKeyMaps{}
	for i, key in keys {
		local[key] = i + 1
		fields.words[key] = i + 1
	}
	assert local.len == keys.len
	assert fields.words.len == keys.len
	for i, key in keys {
		assert key in local.keys()
		assert key in fields.words.keys()
		assert local[key] == i + 1
		assert fields.words[key] == i + 1
	}
	local[keys[1]] = 99
	assert local[keys[0]] == 1
	assert local[keys[1]] == 99
	local.delete(keys[2])
	assert keys[2] !in local
	assert keys[0] in local
	assert local.len == keys.len - 1
}

fn test_inferred_map_literal_keeps_distinct_high_bit_int_keys() {
	if sizeof(int) != 8 {
		return
	}
	low := int(9)
	high := int((u64(1) << 40) + 9)
	values := {
		low:  11
		high: 22
	}
	assert values.len == 2
	assert values[low] == 11
	assert values[high] == 22
	assert low in values.keys()
	assert high in values.keys()
}

fn test_map_alias_keys_match_their_underlying_storage() {
	// u64 must stay eight bytes on 32-bit targets too.
	keys := [CallbackWideKey(7), CallbackWideKey((u64(1) << 40) + 7)]
	mut local := map[CallbackWideKey]int{}
	mut fields := CallbackKeyMaps{}
	for i, key in keys {
		local[key] = i + 1
		fields.wide[key] = i + 1
	}
	assert local.len == 2
	assert fields.wide.len == 2
	for i, key in keys {
		assert key in local.keys()
		assert key in fields.wide.keys()
		assert local[key] == i + 1
		assert fields.wide[key] == i + 1
	}
}

fn test_string_alias_map_keys_use_content_not_pointer_bytes() {
	mut local := map[CallbackTextKey]int{}
	mut fields := CallbackKeyMaps{}
	for i, text in ['alpha', 'café', 'same prefix one', 'same prefix two'] {
		local[CallbackTextKey(text.clone())] = i + 1
		fields.text[CallbackTextKey(text.clone())] = i + 1
		// Fresh allocations with equal contents must find the inserted keys.
		assert local[CallbackTextKey(text.clone())] == i + 1
		assert fields.text[CallbackTextKey(text.clone())] == i + 1
	}
	assert local.len == 4
	assert fields.text.len == 4
	fields.text[CallbackTextKey('alpha'.clone())] = 99
	assert fields.text.len == 4
	assert fields.text[CallbackTextKey('alpha'.clone())] == 99
	fields.text.delete(CallbackTextKey('café'.clone()))
	assert CallbackTextKey('café'.clone()) !in fields.text
	assert fields.text.len == 3
}
