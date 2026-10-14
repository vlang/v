module datatypes

import hash

fn bloom_errors_hash(s string) u32 {
	return u32(hash.sum64_string(s, 0x12345678))
}

fn bloom_errors_other_hash(s string) u32 {
	return u32(hash.sum64_string(s, 0xdeadbeef))
}

fn bloom_errors_int_hash(v int) u32 {
	return u32(v) * 3 + 1
}

fn bit_count(table []u8) int {
	mut bits := 0
	for byte in table {
		mut mask := u8(1)
		for _ in 0 .. 8 {
			if byte & mask != 0 {
				bits++
			}
			mask <<= 1
		}
	}
	return bits
}

fn test_a_non_positive_table_size_is_rejected() ! {
	for size in [0, -8] {
		if _ := new_bloom_filter[string](bloom_errors_hash, size, 4) {
			return error('new_bloom_filter accepted table_size ${size}')
		} else {
			assert err.msg() == 'table_size should great that 0', 'table_size ${size}'
		}
	}
}

fn test_the_hash_count_must_stay_inside_the_supported_range() ! {
	for count in [0, 17] {
		if _ := new_bloom_filter[string](bloom_errors_hash, 64, count) {
			return error('new_bloom_filter accepted num_functions ${count}')
		} else {
			assert err.msg() == 'num_functions should between 1~16', 'num_functions ${count}'
		}
	}
	// the bounds themselves are accepted
	assert new_bloom_filter[string](bloom_errors_hash, 64, 1) != none
	assert new_bloom_filter[string](bloom_errors_hash, 64, 16) != none
}

fn test_union_requires_both_filters_to_be_configured_alike() ! {
	mut base := new_bloom_filter_fast[string](bloom_errors_hash)
	base.add('kept')
	altered_hash := new_bloom_filter_fast[string](bloom_errors_other_hash)
	other_size := new_bloom_filter[string](bloom_errors_hash, 1024, 4) or { panic(err) }
	other_count := new_bloom_filter[string](bloom_errors_hash, 16384, 8) or { panic(err) }
	for other in [altered_hash, other_size, other_count] {
		if _ := base.@union(other) {
			return error('@union accepted a differently configured filter')
		} else {
			assert err.msg() == 'Both filters must be created with the same values.'
		}
	}
	same := new_bloom_filter_fast[string](bloom_errors_hash)
	same.add('extra')
	merged := base.@union(same) or { panic(err) }
	assert merged.exists('kept')
	assert merged.exists('extra')
}

fn test_intersection_requires_both_filters_to_be_configured_alike() ! {
	mut base := new_bloom_filter_fast[string](bloom_errors_hash)
	base.add('kept')
	altered_hash := new_bloom_filter_fast[string](bloom_errors_other_hash)
	other_size := new_bloom_filter[string](bloom_errors_hash, 1024, 4) or { panic(err) }
	other_count := new_bloom_filter[string](bloom_errors_hash, 16384, 8) or { panic(err) }
	for other in [altered_hash, other_size, other_count] {
		if _ := base.intersection(other) {
			return error('intersection accepted a differently configured filter')
		} else {
			assert err.msg() == 'Both filters must be created with the same values.'
		}
	}
	same := new_bloom_filter_fast[string](bloom_errors_hash)
	same.add('kept')
	same.add('extra')
	shared := base.intersection(same) or { panic(err) }
	assert shared.exists('kept')
	assert !shared.exists('extra')
}

// The one thing a bloom filter must never do: claim an element it holds is
// absent. The documented trade-off only permits the other direction.
fn test_every_element_added_is_still_reported_present() {
	mut b := new_bloom_filter_fast[string](bloom_errors_hash)
	for i in 0 .. 5000 {
		b.add('item-${i}')
	}
	for i in 0 .. 5000 {
		assert b.exists('item-${i}'), 'item-${i} went missing'
	}
}

fn test_a_fresh_filter_reports_nothing_as_present() {
	b := new_bloom_filter_fast[string](bloom_errors_hash)
	for i in 0 .. 5000 {
		assert !b.exists('item-${i}'), 'an unpopulated filter reported item-${i}'
	}
}

fn test_one_hash_function_sets_exactly_one_bit() {
	mut b := new_bloom_filter[int](bloom_errors_int_hash, 16384, 1) or { panic(err) }
	assert bit_count(b.table) == 0
	b.add(7)
	assert bit_count(b.table) == 1
	assert b.exists(7)
	b.add(7)
	assert bit_count(b.table) == 1
}

fn test_the_table_is_one_bit_per_requested_position() {
	mut b := new_bloom_filter[int](bloom_errors_int_hash, 13, 4) or { panic(err) }
	assert b.table.len == (13 + 7) / 8
	for i in 0 .. 50 {
		b.add(i)
	}
	assert b.table[1] & 0xe0 == 0, 'bits above position 13 were set'
	for i in 0 .. 50 {
		assert b.exists(i), 'added ${i} went missing'
	}
}
