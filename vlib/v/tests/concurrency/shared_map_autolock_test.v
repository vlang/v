// vtest vflags: -enable-globals
// Single operations on a `shared` map lock the map by themselves, so it can be used as a
// thread-safe map without `lock`/`rlock` blocks.
import sync

const iterations = 20000

type Counts = map[string]int

struct Point {
	x int
	y int
}

struct Cache {
mut:
	entries shared map[string]int
}

struct Service {
	Cache
mut:
	name string
}

struct App {
mut:
	service Service
}

__global hits shared map[string]int

fn test_single_operations() {
	shared m := map[string]int{}
	m['a'] = 1
	m['b'] = 2
	m['b'] += 5
	m['b']++
	assert m['a'] == 1
	assert m['b'] == 8
	assert 'a' in m
	assert 'z' !in m
	assert m.len == 2
	assert m['z'] or { -1 } == -1
	x := m['a'] or { 0 }
	assert x == 1
	mut keys := m.keys()
	keys.sort()
	assert keys == ['a', 'b']
	assert m.values().len == 2
	copied := m.clone()
	assert copied['b'] == 8
	m['c'] = m['a'] + m['b']
	assert m['c'] == 9
	assert '${m['c']} ${m.len}' == '9 3'
	m.delete('a')
	assert 'a' !in m
	m.clear()
	assert m.len == 0
	// explicit locks keep working next to the automatic ones
	lock m {
		m['d'] = 4
	}
	rlock m {
		assert m['d'] == 4
	}
}

fn add(shared m map[string]int, key string, mut wg sync.WaitGroup) {
	for i in 0 .. iterations {
		m[key]++
		m['total'] += 1
		m['tmp${i % 3}'] = i
		if 'tmp1' in m {
			m.delete('tmp1')
		}
		_ := m.len
		_ := m['total'] or { 0 }
	}
	wg.done()
}

fn test_concurrent_updates() {
	shared m := map[string]int{}
	mut wg := sync.new_waitgroup()
	keys := ['a', 'b', 'a', 'c']
	wg.add(keys.len)
	for key in keys {
		spawn add(shared m, key, mut wg)
	}
	wg.wait()
	assert m['a'] == 2 * iterations
	assert m['b'] == iterations
	assert m['c'] == iterations
	assert m['total'] == 4 * iterations
}

fn count(mut c Cache, mut wg sync.WaitGroup) {
	for _ in 0 .. iterations {
		c.entries['x'] += 2
		c.entries['y']++
	}
	wg.done()
}

fn test_concurrent_field_updates() {
	mut c := &Cache{}
	mut wg := sync.new_waitgroup()
	wg.add(3)
	for _ in 0 .. 3 {
		spawn count(mut c, mut wg)
	}
	wg.wait()
	assert c.entries['x'] == 6 * iterations
	assert c.entries['y'] == 3 * iterations
}

fn test_fields_globals_and_aliases() {
	mut app := App{}
	app.service.entries['a'] = 1
	app.service.Cache.entries['a']++
	assert app.service.entries['a'] == 2
	assert app.service.entries.len == 1
	hits['q'] = 1
	hits['q']++
	assert hits['q'] == 2
	shared counts := Counts{}
	counts['z'] = 26
	assert counts['z'] == 26
}

fn test_nested_maps_and_values() {
	shared nested := map[string]map[string]int{}
	nested['o'] = {
		'i': 1
	}
	nested['o']['j'] = 2
	nested['o']['j'] += 3
	assert nested['o']['j'] == 5
	assert nested['o'].len == 2
	assert 'i' in nested['o']
	shared lists := map[string][]int{}
	lists['l'] = []int{}
	lists['l'] << 1
	lists['l'] << 2
	assert lists['l'] == [1, 2]
	mut sum := 0
	for v in lists['l'] {
		sum += v
	}
	assert sum == 3
	shared points := map[string]Point{}
	points['p'] = Point{3, 4}
	assert points['p'].x + points['p'].y == 7
	shared names := map[string]string{}
	names['n'] = 'hello'
	names['n'] += ' world'
	assert names['n'].len == 11
	assert names['n'].to_upper() == 'HELLO WORLD'
}

fn first_found(shared m map[string]int, keys []string) int {
	for k in keys {
		v := m[k] or { continue }
		return v
	}
	return m['missing'] or { return -1 }
}

fn test_conditions_loops_and_or_blocks() {
	shared m := map[string]int{}
	m['a'] = 3
	if 'a' in m && m.len == 1 {
		m['seen'] = 1
	} else if m.len > 100 {
		assert false
	}
	x := if m['a'] > 2 { m['seen'] } else { 0 }
	assert x == 1
	y := match m['a'] {
		3 { m['a'] * 2 }
		else { 0 }
	}
	assert y == 6
	for i := 0; i < m['a']; m['steps']++ {
		i++
	}
	assert m['steps'] == 3
	assert first_found(shared m, ['nope', 'a']) == 3
	assert first_found(shared m, ['nope']) == -1
	z := m['nope'] or { m['a'] + 1 }
	assert z == 4
}

fn test_range_bounds_and_select_cases() {
	shared m := map[string]int{}
	m['start'] = 1
	m['limit'] = 4
	mut sum := 0
	for i in m['start'] .. m['limit'] {
		sum += i
	}
	for i in 0 .. m.len {
		sum += i
	}
	assert sum == 7
	shared chans := map[string]chan int{}
	chans['c'] = chan int{cap: 1}
	ch := chan int{cap: 1}
	select {
		ch <- m['limit'] {
		}
	}
	select {
		chans['c'] <- m['start'] {
		}
	}
	mut got := 0
	select {
		x := <-chans['c'] {
			got += x
		}
	}
	select {
		y := <-ch {
			got += y
		}
		m['limit'] * 1000000 {
			assert false
		}
	}
	assert got == 5
	if select {
		ch <- m['start'] {
		}
	} {
		assert <-ch == 1
	}
}

fn get[T](shared m map[string]T, key string) T {
	return m[key]
}

fn set[T](shared m map[string]T, key string, value T) {
	m[key] = value
}

fn test_generic_functions() {
	shared ints := map[string]int{}
	set(shared ints, 'a', 1)
	assert get(shared ints, 'a') == 1
	shared strs := map[string]string{}
	set(shared strs, 'a', 'b')
	assert get(shared strs, 'a') == 'b'
}

fn test_closures() {
	shared m := map[string]int{}
	m['a'] = 1
	read := fn [shared m] (key string) int {
		return m[key]
	}
	assert read('a') == 1
	t := spawn fn [shared m] () {
		m['thread'] = 2
	}()
	t.wait()
	assert m['thread'] == 2
}

fn test_lock_expression_after_hoisted_operand() {
	shared nested := map[string]map[string]int{}
	lock nested {
		nested['o'] = {
			'i': 1
		}
	}
	assert 'i' in rlock nested {
		nested['o']
	}
}
