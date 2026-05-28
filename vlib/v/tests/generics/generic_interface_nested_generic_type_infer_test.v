import sync
import time

interface CacheStore[K, V] {
mut:
	get(key K) (V, bool)
	set(key K, val V)
}

struct CacheItem[K, V] {
	key   K
	value V
}

fn new_cache_item[K, V](key K, val V) &CacheItem[K, V] {
	return &CacheItem[K, V]{
		key:   key
		value: val
	}
}

struct Cache[K, V] {
mut:
	cache CacheStore[K, &CacheItem[K, V]]
	mu    sync.Mutex
}

type CacheOption[K, V] = fn (o &CacheOptions[K, V])

struct CacheOptions[K, V] {
	cache CacheStore[K, &CacheItem[K, V]]
}

fn new_options[K, V]() &CacheOptions[K, V] {
	return &CacheOptions[K, V]{
		cache: new_simple_cache[K, &CacheItem[K, V]]()
	}
}

fn new_cache[K, V](opts ...CacheOption[K, V]) &Cache[K, V] {
	mut o := new_options[K, V]()
	for f in opts {
		f[K, V](o)
	}
	return &Cache[K, V]{
		cache: o.cache
	}
}

fn (mut c Cache[K, V]) get(key K) (V, bool) {
	c.mu.@lock()
	defer {
		c.mu.unlock()
	}
	item, ok := c.cache.get(key)
	if !ok {
		return CacheItem[K, V]{}.value, false
	}
	return item.value, true
}

fn (mut c Cache[K, V]) set(key K, val V) {
	c.mu.@lock()
	defer {
		c.mu.unlock()
	}
	item := new_cache_item(key, val)
	c.cache.set(key, item)
}

struct SimpleCache[K, V] {
mut:
	items map[K]&CacheEntry[V]
}

struct CacheEntry[V] {
	val        V
	created_at time.Time
}

fn new_simple_cache[K, V]() &SimpleCache[K, V] {
	return &SimpleCache[K, V]{
		items: map[K]&CacheEntry[V]{}
	}
}

fn (mut s SimpleCache[K, V]) set(k K, v V) {
	s.items[k] = &CacheEntry[V]{
		val:        v
		created_at: time.now()
	}
}

fn (s SimpleCache[K, V]) get(k K) (V, bool) {
	got := s.items[k] or { return CacheEntry[V]{}.val, false }
	return got.val, true
}

fn test_generic_interface_nested_generic_type_infer() {
	mut cache := new_cache[string, int]()
	cache.set('a', 1)
	value, ok := cache.get('a')
	assert ok
	assert value == 1
}

// Regression test for #18471.
interface Issue18471Returner[T] {
	get() T
}

struct Issue18471Holder[T] {
	value Issue18471Returner[T]
}

fn (h Issue18471Holder[T]) get() T {
	return h.value.get()
}

struct Issue18471Number[T] {
	value T
}

fn (n Issue18471Number[T]) get() T {
	return n.value
}

struct Issue18471Box[T] {
	value T
}

interface Issue18471BoxedValue[T] {
	get() Issue18471Box[T]
}

fn (b Issue18471Box[T]) get() T {
	return b.value
}

struct Issue18471BoxNumber[T] {
	value T
}

fn (n Issue18471BoxNumber[T]) get() Issue18471Box[T] {
	return Issue18471Box[T]{
		value: n.value
	}
}

struct Issue18471NestedBox[T] {
	value Issue18471BoxedValue[T]
}

fn issue18471_nested_box[T](value Issue18471BoxedValue[T]) Issue18471BoxedValue[Issue18471BoxedValue[T]] {
	return Issue18471NestedBox[T]{
		value: value
	}
}

fn (n Issue18471NestedBox[T]) get() Issue18471Box[Issue18471BoxedValue[T]] {
	return Issue18471Box[Issue18471BoxedValue[T]]{
		value: n.value
	}
}

struct Issue18471PickSecond[T, Y] {
	first  Issue18471BoxedValue[T]
	second Issue18471BoxedValue[Y]
}

fn issue18471_pick_second[T, Y](first Issue18471BoxedValue[T], second Issue18471BoxedValue[Y]) Issue18471BoxedValue[Y] {
	return Issue18471PickSecond[T, Y]{
		first:  first
		second: second
	}
}

fn (p Issue18471PickSecond[T, Y]) get() Issue18471Box[Y] {
	return p.second.get()
}

fn test_issue_18471_generic_interface_impl_resolves_concrete_return_type() {
	value := Issue18471Holder[int]{
		value: Issue18471Number[int]{
			value: 7
		}
	}
	assert value.get() == 7
}

fn test_issue_18471_generic_interface_impl_handles_nested_generic_return_types() {
	nested := issue18471_nested_box[int](Issue18471BoxNumber[int]{
		value: 9
	})
	inner := nested.get().value
	assert inner.get().value == 9
}

fn test_issue_18471_generic_interface_impl_handles_multiple_receiver_generics() {
	value := issue18471_pick_second[int, int](Issue18471BoxNumber[int]{
		value: 1
	}, Issue18471BoxNumber[int]{
		value: 2
	})
	assert value.get().value == 2
}
