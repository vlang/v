// Arrays whose element types match the expected array, up to aliases, keep working
// now that element types have to match exactly (`[]u8` is not a `[]u64`).

type Ids = []u64

type Id = u64

type Index = int

interface Named {
	name() string
}

struct Item {
	n int
}

fn (i Item) name() string {
	return 'item${i.n}'
}

struct Cache {
mut:
	ids    []u64
	alias  Ids
	nested [][]u64
	fixed  [3]u64
	maybe  ?[]u64
	by_key map[string][]u64
	empty  []u64 = []u64{}
}

fn total(ids []u64) u64 {
	mut sum := u64(0)
	for id in ids {
		sum += id
	}
	return sum
}

fn first_fixed(ids [3]u64) u64 {
	return ids[0]
}

fn push_id(mut ids []u64) {
	ids << u64(0xffff_ffff_0000_0001)
}

fn make_ids() []u64 {
	return []
}

fn make_alias() Ids {
	return []u64{len: 2, init: 7}
}

fn index_sum(indexes []int) int {
	mut sum := 0
	for i in indexes {
		sum += i
	}
	return sum
}

fn names(items []Named) []string {
	return items.map(it.name())
}

fn nested_names(rows [][]Named) []string {
	mut out := []string{}
	for row in rows {
		out << names(row)
	}
	return out
}

fn test_aliases_and_identical_element_types_are_accepted() {
	mut cache := Cache{
		ids:   []u64{}
		alias: []u64{len: 1, init: 3}
	}
	cache.ids = []u64{len: 2, init: 5}
	cache.alias = cache.ids
	cache.ids = make_alias()
	cache.nested = [][]u64{}
	cache.nested << cache.ids
	cache.by_key['a'] = cache.ids
	cache.maybe = cache.ids
	ids := Ids([u64(1), 2])
	assert total(ids) == 3
	assert total(cache.alias) == 10
	assert total(cache.ids) == 14
	assert cache.nested == [[u64(7), 7]]
	assert cache.by_key['a'] == [u64(7), 7]
	assert cache.maybe or { []u64{} } == [u64(7), 7]
	alias_elems := []Id{len: 2, init: 4}
	assert total(alias_elems) == 8
	indexes := []Index{len: 3, init: 2}
	assert index_sum(indexes) == 6
	assert cache.empty.len == 0
}

fn test_untyped_literals_take_the_expected_element_type() {
	mut cache := Cache{}
	cache.ids = []
	assert cache.ids.len == 0
	assert make_ids().len == 0
	cache.ids = [1, 2, 0xffff_ffff_ffff]
	assert cache.ids == [u64(1), 2, 0xffff_ffff_ffff]
	assert total([1, 2, 3]) == 6
	cache.fixed = [1, 2, 3]!
	assert first_fixed(cache.fixed) == 1
	assert first_fixed([4, 5, 6]!) == 4
	cache.fixed = [3]u64[7, 8, 9]
	assert cache.fixed[2] == 9
	cache.nested = [[1, 2], [3]]
	assert cache.nested[1] == [u64(3)]
	// Elements of an untyped literal are converted one by one.
	small := u8(200)
	assert total([small, 2]) == 202
	push_id(mut cache.ids)
	assert cache.ids.last() == u64(0xffff_ffff_0000_0001)
}

fn test_concrete_arrays_are_still_boxed_into_interface_arrays() {
	items := [Item{1}, Item{2}]
	assert names(items) == ['item1', 'item2']
	rows := [[Item{3}], [Item{4}]]
	assert nested_names(rows) == ['item3', 'item4']
}
