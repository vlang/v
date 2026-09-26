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
