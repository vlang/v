import maps

fn test_counter_basic() {
	mut c := maps.new_counter[string]()
	c.inc('apple')
	c.add_many(['apple', 'pear', 'apple'])
	assert c.get('apple') == 3
	assert c.get('pear') == 1
	assert c.get('plum') == 0, 'missing keys count 0'
	assert c.total() == 4
	c.dec('apple')
	assert c.get('apple') == 2
}

fn test_counter_most_common() {
	mut c := maps.new_counter[string]()
	c.add_many(['a', 'b', 'a', 'c', 'a', 'b'])
	top := c.most_common(2)
	assert top.len == 2
	assert top[0].key == 'a' && top[0].count == 3
	assert top[1].key == 'b' && top[1].count == 2
	all := c.most_common(99)
	assert all.len == 3, 'more than size returns everything'
	assert c.most_common(0).len == 0, 'zero returns nothing'
}

fn test_default_map() {
	mut m := maps.new_default_map[string, []int](fn () []int {
		return []int{}
	})
	mut arr := m.get('k')
	arr << 1
	arr << 2
	m.set('k', arr)
	assert m.get('k') == [1, 2]
	assert m.len() == 1, 'factory must fire once per missing key'
	assert m.get_opt('missing') == none
	m.set('s', [9])
	assert m.get('s') == [9]
}

struct DefaultFactoryCalls {
mut:
	count int
}

fn test_default_map_factory_is_lazy_and_called_once_per_key() {
	mut calls := &DefaultFactoryCalls{}
	mut m := maps.new_default_map[string, int](fn [mut calls] () int {
		calls.count++
		return calls.count
	})
	assert m.get_opt('a') == none
	assert calls.count == 0
	assert m.len() == 0
	assert m.get('a') == 1
	assert m.get('a') == 1
	assert calls.count == 1
	m.set('b', 99)
	assert m.get('b') == 99
	assert calls.count == 1
	assert m.get('c') == 2
	assert calls.count == 2
}
