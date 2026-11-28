import datatypes

struct FixedArrayKeyMapDefault {
mut:
	counts map[[2]string]int
}

fn test_omitted_fixed_array_key_map_field() {
	mut box := FixedArrayKeyMapDefault{}
	key := ['first', 'second']!
	box.counts[key] = 7
	assert box.counts[['first', 'second']!] == 7
}

fn test_generic_set_fixed_array_key_default() {
	mut set := datatypes.Set[[2]string]{}
	key := ['first', 'second']!
	set.add(key)
	assert set.exists(['first', 'second']!)
	assert set.size() == 1
}
