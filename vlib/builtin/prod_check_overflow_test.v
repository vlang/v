// vtest vflags: -prod -check-overflow
// The map hash and the word-wise byte search wrap on purpose. Under `-prod`
// (and a C compiler other than tcc and msvc) they run in V, so `-check-overflow`
// must leave them alone.

fn test_a_map_with_string_keys_of_every_length_works() {
	mut m := map[string]int{}
	for n in 0 .. 40 {
		m['k'.repeat(n)] = n
	}
	for n in 0 .. 40 {
		assert m['k'.repeat(n)] == n
	}
	assert m.len == 40
}

fn test_the_byte_searches_find_and_miss_in_short_strings() {
	text := 'abcdefghijklmnopqrstuvwxyz0123456'
	for size in 8 .. 33 {
		part := text[..size]
		assert part.index_u8(`z`) == if size > 25 { 25 } else { -1 }
		assert part.index_u8(part[size - 1]) == size - 1
		assert part.last_index_u8(`a`) == 0
		assert part.contains_u8(`!`) == false
		assert part.contains_u8(part[size / 2])
		assert part.contains('gh')
		assert part.index('zz') or { -1 } == -1
	}
}
