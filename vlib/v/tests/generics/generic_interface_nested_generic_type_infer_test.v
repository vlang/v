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
