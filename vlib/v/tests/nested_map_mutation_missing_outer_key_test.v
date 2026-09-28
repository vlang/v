// Mutating through a missing outer key of a map of maps must insert the
// inner map, instead of mutating a temporary default value (issue #28951).
struct NestedMapHolder {
mut:
	m map[string]map[string][]int
}

fn (mut h NestedMapHolder) add(k1 string, k2 string, v int) {
	h.m[k1][k2] << v
}

fn nested_map_append(mut m map[string]map[string][]int, k1 string, k2 string, v int) {
	m[k1][k2] << v
}

struct NestedMapPoint {
mut:
	x int
	y int
}

struct NestedMapKeyCounter {
mut:
	n int
}

type NestedMapVariant = int | []NestedMapVariant
type NestedAppendWords = []string

struct NestedAppendBorrowedHolder {
mut:
	words []string
}

struct NestedAppendFixedBorrowedHolder {
mut:
	words [2]string
}

fn (mut c NestedMapKeyCounter) key(k string) string {
	c.n++
	return k
}

fn test_append_through_missing_outer_key() {
	mut m := map[string]map[string][]int{}
	m['x']['k'] << 3
	assert m.str() == "{'x': {'k': [3]}}"
	m['x']['k'] << 4
	m['x']['j'] << [1, 2]
	assert m.str() == "{'x': {'k': [3, 4], 'j': [1, 2]}}"
}

fn test_three_levels_through_missing_keys() {
	mut appended := map[string]map[string]map[string][]int{}
	appended['a']['b']['c'] << 1
	appended['a']['b']['c'] << 2
	assert appended.str() == "{'a': {'b': {'c': [1, 2]}}}"
	mut assigned := map[string]map[string]map[string]int{}
	assigned['a']['b']['c'] = 7
	assigned['a']['b']['c'] += 2
	assigned['a']['b']['d']++
	assert assigned.str() == "{'a': {'b': {'c': 9, 'd': 1}}}"
}

fn test_compound_assign_through_missing_outer_key() {
	mut ints := map[string]map[string]int{}
	ints['x']['k'] += 1
	ints['x']['k'] += 2
	assert ints.str() == "{'x': {'k': 3}}"
	mut strs := map[string]map[string]string{}
	strs['x']['k'] += 'ab'
	strs['x']['k'] += 'cd'
	assert strs.str() == "{'x': {'k': 'abcd'}}"
}

fn test_fixed_array_assign_through_missing_outer_key() {
	mut fixed := map[string]map[string][3]int{}
	fixed['a']['b'][1] = 9
	assert fixed.str() == "{'a': {'b': [0, 9, 0]}}"
	mut deep := map[string]map[string]map[string][2]int{}
	deep['a']['b']['c'][0] = 5
	assert deep.str() == "{'a': {'b': {'c': [5, 0]}}}"
}

fn test_fixed_array_assign_clones_owned_inner_key() {
	key := ['b'.clone(), 'c'.clone()]!
	mut nested := map[string]map[[2]string][1]int{}
	nested['a'][key][0] = 1
	assert nested['a'][key] == [1]!
	nested['a'][key][0] = 2
	assert nested['a'][key] == [2]!
	assert key == ['b', 'c']!
}

fn nested_map_set_x(mut m map[string]map[string]NestedMapPoint, k1 string, k2 string, x int) {
	m[k1][k2].x = x
}

fn test_field_assign_through_existing_outer_key() {
	mut points := map[string]map[string]NestedMapPoint{}
	points['a']['b'] = NestedMapPoint{}
	points['a']['b'].x = 3
	points['a']['c'].y = 4
	nested_map_set_x(mut points, 'a', 'd', 5)
	assert points['a']['b'].x == 3
	assert points['a']['c'].y == 4
	assert points['a']['d'].x == 5
	assert points.len == 1
	assert points['a'].len == 3
}

fn test_append_through_struct_field_and_mut_param() {
	mut h := NestedMapHolder{}
	h.m['x']['k'] << 1
	h.add('x', 'k', 2)
	h.add('y', 'z', 3)
	assert h.m.str() == "{'x': {'k': [1, 2]}, 'y': {'z': [3]}}"
	mut m := map[string]map[string][]int{}
	nested_map_append(mut m, 'p', 'q', 9)
	nested_map_append(mut m, 'p', 'q', 10)
	assert m.str() == "{'p': {'q': [9, 10]}}"
}

fn test_append_through_non_string_keys() {
	mut m := map[int]map[u8][]string{}
	m[5][1] << 'hi'
	m[5][2] << 'yo'
	assert m.str() == "{5: {1: ['hi'], 2: ['yo']}}"
}

fn test_reads_do_not_insert_keys() {
	m := map[string]map[string][]int{}
	v := m['x']['k']
	assert v.len == 0
	assert m['y']['z'].len == 0
	assert m.len == 0
	mut deep := map[string]map[string]map[string][]int{}
	assert deep['a']['b']['c'].len == 0
	assert deep.len == 0
	deep['a']['b']['c'] << 1
	assert deep['a']['x']['y'].len == 0
	assert deep['a'].len == 1
}

fn test_mutation_keys_are_evaluated_once() {
	mut c := NestedMapKeyCounter{}
	mut appended := map[string]map[string]map[string][]int{}
	appended[c.key('a')][c.key('b')][c.key('c')] << 1
	assert c.n == 3
	assert appended.str() == "{'a': {'b': {'c': [1]}}}"
	c.n = 0
	mut assigned := map[string]map[string]int{}
	assigned[c.key('a')][c.key('b')] += 1
	assert c.n == 2
	assert assigned.str() == "{'a': {'b': 1}}"
}

fn test_mutation_through_parenthesized_outer_index() {
	mut appended := map[string]map[string][]int{}
	(appended['x'])['k'] << 3
	assert appended.str() == "{'x': {'k': [3]}}"
	mut counts := map[string]map[string]int{}
	(counts['x'])['k'] += 1
	(counts['x'])['k'] += 2
	assert counts.str() == "{'x': {'k': 3}}"
}

fn grow_nested_map_outer(mut m map[string]map[string][]int) string {
	for i in 0 .. 2000 {
		m['grow${i}'] = map[string][]int{}
	}
	return 'k'
}

fn test_mutation_when_a_later_operand_grows_the_outer_map() {
	mut m := map[string]map[string][]int{}
	m['a'] = map[string][]int{}
	m['a'][grow_nested_map_outer(mut m)] << 1
	m['b'][grow_nested_map_outer(mut m)] << 2
	assert m['a'].str() == "{'k': [1]}"
	assert m['b'].str() == "{'k': [2]}"
	assert m.len == 2002
}

fn test_field_assign_into_an_existing_empty_inner_map() {
	mut points := map[string]map[string]NestedMapPoint{}
	points['a'] = map[string]NestedMapPoint{}
	points['a']['b'].x = 5
	assert points['a']['b'].x == 5
	assert points['a'].len == 1
}

fn replace_point_outer_rhs(mut m map[string]map[string]NestedMapPoint) int {
	m['a'] = map[string]NestedMapPoint{
		'fresh': NestedMapPoint{
			x: 7
		}
	}
	return 9
}

fn grow_point_outer_rhs(mut m map[string]map[string]NestedMapPoint) int {
	for i in 0 .. 2000 {
		m['grow${i}'] = map[string]NestedMapPoint{}
	}
	return 3
}

fn replace_deep_point_outer_rhs(mut m map[string]map[string]map[string]NestedMapPoint) int {
	m['a'] = map[string]map[string]NestedMapPoint{}
	m['a']['b']['fresh'] = NestedMapPoint{
		x: 7
	}
	return 8
}

fn test_field_assign_after_rhs_changes_outer_map() {
	mut replaced := map[string]map[string]NestedMapPoint{}
	replaced['a'] = map[string]NestedMapPoint{}
	replaced['a']['b'].x = replace_point_outer_rhs(mut replaced)
	assert replaced['a']['fresh'].x == 7
	assert replaced['a']['b'].x == 9
	assert replaced['a'].len == 2
	mut grown := map[string]map[string]NestedMapPoint{}
	grown['a'] = map[string]NestedMapPoint{}
	grown['a']['b'].x = grow_point_outer_rhs(mut grown)
	assert grown['a']['b'].x == 3
	assert grown.len == 2001
	mut missing := map[string]map[string]NestedMapPoint{}
	missing['a']['b'].x = 4
	assert missing.len == 0
	mut deep_missing := map[string]map[string]map[string]NestedMapPoint{}
	deep_missing['a']['b']['c'].x = 4
	assert deep_missing.len == 0
	mut deep_replaced := map[string]map[string]map[string]NestedMapPoint{}
	deep_replaced['a']['b']['old'].x = 1
	deep_replaced['a']['b']['c'].x = replace_deep_point_outer_rhs(mut deep_replaced)
	assert deep_replaced['a']['b']['fresh'].x == 7
	assert deep_replaced['a']['b']['c'].x == 8
	assert deep_replaced['a']['b'].len == 2
}

fn insert_owned_inner_key_rhs(mut m map[string]map[[2]string][]int, key [2]string) int {
	m['a'][key] = [4]
	return 5
}

fn replace_owned_inner_map_rhs(mut m map[string]map[[2]string][]int) int {
	m['a'] = map[[2]string][]int{}
	return 6
}

fn test_append_refreshes_owned_inner_key_existence_after_rhs() {
	key := ['b', 'c']!
	mut inserted := map[string]map[[2]string][]int{}
	inserted['a'] = map[[2]string][]int{}
	inserted['a'][key] << insert_owned_inner_key_rhs(mut inserted, key)
	assert inserted['a'][key] == [4, 5]
	mut replaced := map[string]map[[2]string][]int{}
	replaced['a'][key] << 1
	replaced['a'][key] << replace_owned_inner_map_rhs(mut replaced)
	assert replaced['a'][key] == [6]
}

fn insert_owned_int_inner_key_rhs(mut m map[string]map[[2]string]int, key [2]string) int {
	m['a'][key] = 4
	return 5
}

fn replace_owned_int_inner_map_rhs(mut m map[string]map[[2]string]int) int {
	m['a'] = map[[2]string]int{}
	return 6
}

fn test_compound_refreshes_owned_inner_key_existence_after_rhs() {
	key := ['b', 'c']!
	mut inserted := map[string]map[[2]string]int{}
	inserted['a'] = map[[2]string]int{}
	inserted['a'][key] += insert_owned_int_inner_key_rhs(mut inserted, key)
	assert inserted['a'][key] == 9
	mut replaced := map[string]map[[2]string]int{}
	replaced['a'][key] = 1
	replaced['a'][key] += replace_owned_int_inner_map_rhs(mut replaced)
	assert replaced['a'][key] == 6
}

fn test_postfix_clones_borrowed_owned_inner_key() {
	key := ['b', 'c']!
	mut nested := map[string]map[[2]string]int{}
	nested['a'][key]++
	assert nested['a'][key] == 1
	nested['a'][key]--
	assert nested['a'][key] == 0
	mut direct := map[[2]string]int{}
	direct[key]++
	assert direct[key] == 1
}

fn fail_selector_key() !string {
	return error('key failed')
}

fn fail_selector_rhs() !int {
	return error('rhs failed')
}

fn assign_after_failing_selector_key(mut m map[[2]string]map[string]map[string]NestedMapPoint, key [2]string) ! {
	m[key][fail_selector_key()!]['c'].x = 1
}

fn assign_after_failing_selector_rhs(mut m map[[2]string]map[string]map[string]NestedMapPoint, key [2]string) ! {
	m[key]['b']['c'].x = fail_selector_rhs()!
}

fn assign_after_failing_owned_final_key(mut m map[string]map[[2]string]NestedMapPoint, key [2]string) ! {
	m['a'][key].x = fail_selector_rhs()!
}

fn test_selector_ancestor_keys_are_cleaned_on_early_return() {
	key := ['a', 'b']!
	mut m := map[[2]string]map[string]map[string]NestedMapPoint{}
	mut failures := 0
	assign_after_failing_selector_key(mut m, key) or {
		assert err.msg() == 'key failed'
		failures++
	}
	assign_after_failing_selector_rhs(mut m, key) or {
		assert err.msg() == 'rhs failed'
		failures++
	}
	assert failures == 2
	assert m.len == 0
	mut final_key := map[string]map[[2]string]NestedMapPoint{}
	assign_after_failing_owned_final_key(mut final_key, key) or {
		assert err.msg() == 'rhs failed'
		failures++
	}
	assert failures == 3
	assert final_key.len == 0
}

fn replace_nested_outer(mut m map[string]map[string][]int) string {
	m['a'] = map[string][]int{
		'fresh': [7]
	}
	return 'k'
}

fn delete_nested_outer(mut m map[string]map[string][]int) string {
	m.delete('a')
	return 'k'
}

fn clear_nested_outer(mut m map[string]map[string][]int) string {
	m.clear()
	return 'k'
}

fn replace_nested_outer_rhs(mut m map[string]map[string][]int) int {
	m['a'] = map[string][]int{
		'fresh': [7]
	}
	return 3
}

fn test_mutation_after_later_operand_changes_outer_entry() {
	mut replaced := map[string]map[string][]int{}
	replaced['a']['old'] << 1
	replaced['a'][replace_nested_outer(mut replaced)] << 2
	assert replaced['a'].str() == "{'fresh': [7], 'k': [2]}"
	mut deleted := map[string]map[string][]int{}
	deleted['a']['old'] << 1
	deleted['a'][delete_nested_outer(mut deleted)] << 2
	assert deleted['a'].str() == "{'k': [2]}"
	mut cleared := map[string]map[string][]int{}
	cleared['a']['old'] << 1
	cleared['a'][clear_nested_outer(mut cleared)] << 2
	assert cleared['a'].str() == "{'k': [2]}"
	assert cleared.len == 1
}

fn test_mutation_after_rhs_replaces_outer_entry() {
	mut m := map[string]map[string][]int{}
	m['a']['k'] << 1
	m['a']['k'] << replace_nested_outer_rhs(mut m)
	assert m['a'].str() == "{'fresh': [7], 'k': [3]}"
}

fn replace_nested_int_outer(mut m map[string]map[string]int) string {
	m['a'] = map[string]int{
		'fresh': 7
	}
	return 'k'
}

fn replace_nested_int_outer_rhs(mut m map[string]map[string]int) int {
	m['a'] = map[string]int{
		'fresh': 7
	}
	return 3
}

fn test_nested_assign_after_later_operands_replace_outer_entry() {
	mut assigned := map[string]map[string]int{}
	assigned['a']['old'] = 1
	assigned['a'][replace_nested_int_outer(mut assigned)] = 2
	assert assigned['a'].str() == "{'fresh': 7, 'k': 2}"
	mut added := map[string]map[string]int{}
	added['a']['old'] = 1
	added['a'][replace_nested_int_outer(mut added)] += 2
	assert added['a'].str() == "{'fresh': 7, 'k': 2}"
	mut rhs := map[string]map[string]int{}
	rhs['a']['k'] = 1
	rhs['a']['k'] += replace_nested_int_outer_rhs(mut rhs)
	assert rhs['a'].str() == "{'fresh': 7, 'k': 3}"
}

fn delete_nested_owned_outer_key(mut m map[[2]string]map[string][]int, key [2]string) string {
	m.delete(key)
	return 'k'
}

fn test_later_operand_deletes_owned_outer_key() {
	mut m := map[[2]string]map[string][]int{}
	key := ['a', 'b']!
	m[key]['old'] << 1
	m[key][delete_nested_owned_outer_key(mut m, key)] << 2
	assert m[key]['k'] == [2]
	assert m.len == 1
}

fn replace_three_level_outer(mut m map[string]map[string]map[string][]int) string {
	m['a'] = map[string]map[string][]int{}
	m['a']['b']['fresh'] << 7
	return 'k'
}

fn test_three_level_mutation_after_outer_entry_is_replaced() {
	mut m := map[string]map[string]map[string][]int{}
	m['a']['b']['old'] << 1
	m['a']['b'][replace_three_level_outer(mut m)] << 2
	assert m['a']['b'].str() == "{'fresh': [7], 'k': [2]}"
}

fn replace_fixed_outer(mut m map[string]map[string][2]int) int {
	m['a'] = map[string][2]int{
		'fresh': [7, 0]!
	}
	return 1
}

fn replace_fixed_outer_rhs(mut m map[string]map[string][2]int) int {
	m['a'] = map[string][2]int{
		'fresh': [7, 0]!
	}
	return 9
}

fn test_fixed_array_assignment_after_outer_entry_replacement() {
	mut indexed := map[string]map[string][2]int{}
	indexed['a']['b'][0] = 1
	indexed['a']['b'][replace_fixed_outer(mut indexed)] = 9
	assert indexed['a'].str() == "{'fresh': [7, 0], 'b': [0, 9]}"
	mut rhs := map[string]map[string][2]int{}
	rhs['a']['b'][0] = 1
	rhs['a']['b'][1] = replace_fixed_outer_rhs(mut rhs)
	assert rhs['a'].str() == "{'fresh': [7, 0], 'b': [0, 9]}"
}

fn test_nested_map_append_array_sum_variant() {
	mut m := map[string]map[string][]NestedMapVariant{}
	m['a']['b'] << [NestedMapVariant(1), NestedMapVariant(2)]
	assert m['a']['b'].len == 1
	inner := m['a']['b'][0] as []NestedMapVariant
	assert inner.len == 2
}

fn make_nested_append_words() []string {
	return ['alpha'.clone(), 'beta'.clone()]
}

fn make_nested_append_word() string {
	return 'gamma'.clone()
}

fn make_nested_append_row() []int {
	return [3, 4]
}

fn make_nested_append_alias_words() NestedAppendWords {
	return ['epsilon'.clone(), 'zeta'.clone()]
}

fn make_nested_compound_word() string {
	return 'suffix'.clone()
}

fn test_nested_append_releases_staged_owned_rhs() {
	mut words := map[string]map[string][]string{}
	words['a']['b'] << make_nested_append_words()
	words['a']['b'] << make_nested_append_word()
	words['a']['b'] << ['delta'.clone()]
	assert words['a']['b'] == ['alpha', 'beta', 'gamma', 'delta']
	words['a']['b'] << make_nested_append_alias_words()
	assert words['a']['b'] == ['alpha', 'beta', 'gamma', 'delta', 'epsilon', 'zeta']
	mut rows := map[string]map[string][][]int{}
	rows['a']['b'] << make_nested_append_row()
	rows['a']['b'] << [5, 6]
	assert rows['a']['b'] == [[3, 4], [5, 6]]
}

fn test_nested_compound_releases_staged_owned_string_rhs() {
	mut words := map[string]map[string]string{}
	words['a']['b'] += make_nested_compound_word()
	words['a']['b'] += make_nested_compound_word()
	assert words['a']['b'] == 'suffixsuffix'
}

fn nested_append_borrowed_words(mut holder NestedAppendBorrowedHolder) map[string]map[string][]string {
	mut nested := map[string]map[string][]string{}
	nested['a']['b'] << holder.words
	return nested
}

fn test_nested_append_clones_borrowed_projection() {
	mut holder := NestedAppendBorrowedHolder{
		words: ['borrowed-a'.clone(), 'borrowed-b'.clone()]
	}
	nested := nested_append_borrowed_words(mut holder)
	holder.words[0] = 'source-changed'.clone()
	assert holder.words == ['source-changed', 'borrowed-b']
	assert nested['a']['b'] == ['borrowed-a', 'borrowed-b']
}

fn nested_append_borrowed_fixed_words(mut holder NestedAppendFixedBorrowedHolder) map[string]map[string][][2]string {
	mut nested := map[string]map[string][][2]string{}
	nested['a']['b'] << holder.words
	return nested
}

fn test_nested_append_clones_borrowed_fixed_array_projection() {
	mut holder := NestedAppendFixedBorrowedHolder{
		words: ['fixed-a'.clone(), 'fixed-b'.clone()]!
	}
	nested := nested_append_borrowed_fixed_words(mut holder)
	holder.words[0] = 'source-changed'.clone()
	assert holder.words == ['source-changed', 'fixed-b']!
	assert nested['a']['b'] == [['fixed-a', 'fixed-b']!]
}
