@[has_globals]
module main

struct GlobalInner {
mut:
	arr []int
	m   map[int]string
}

struct GlobalRegistry {
mut:
	m     map[string]int
	arr   []string
	ch    chan int
	inner GlobalInner
	n     int = 7
}

struct GlobalItem {
mut:
	x int
	a i64
	b i64
	c i64
}

struct GlobalSharedFields {
mut:
	sm    shared map[string]int
	sa    shared []int
	inner shared GlobalInner
	items []shared GlobalItem
}

__global (
	plain_registry         GlobalRegistry
	shared_registry        shared GlobalRegistry
	shared_fields_registry GlobalSharedFields
)

fn test_global_struct_without_initializer_gets_usable_container_fields() {
	plain_registry.m['a'] = 1
	plain_registry.arr << 'x'
	plain_registry.inner.arr << 2
	plain_registry.inner.m[3] = 'y'
	assert plain_registry.m['a'] == 1
	assert plain_registry.arr == ['x']
	assert plain_registry.inner.arr == [2]
	assert plain_registry.inner.m[3] == 'y'
	assert plain_registry.ch.cap == 0
	assert plain_registry.n == 7
}

fn test_shared_global_struct_without_initializer_gets_usable_container_fields() {
	lock shared_registry {
		shared_registry.m['a'] = 1
		shared_registry.arr << 'x'
		shared_registry.inner.m[3] = 'y'
	}
	rlock shared_registry {
		assert shared_registry.m.len == 1
		assert shared_registry.m['a'] == 1
		assert shared_registry.arr == ['x']
		assert shared_registry.inner.m[3] == 'y'
		assert shared_registry.n == 7
	}
}

fn test_global_struct_without_initializer_gets_usable_shared_fields() {
	lock shared_fields_registry.sm {
		shared_fields_registry.sm['a'] = 1
	}
	lock shared_fields_registry.sa {
		shared_fields_registry.sa << 2
	}
	lock shared_fields_registry.inner {
		shared_fields_registry.inner.arr << 5
		shared_fields_registry.inner.m[6] = 'z'
	}
	shared first := GlobalItem{
		x: 3
	}
	shared second := GlobalItem{
		x: 4
	}
	shared_fields_registry.items << first
	shared_fields_registry.items << second
	rlock shared_fields_registry.sm {
		assert shared_fields_registry.sm['a'] == 1
	}
	rlock shared_fields_registry.sa {
		assert shared_fields_registry.sa == [2]
	}
	rlock shared_fields_registry.inner {
		assert shared_fields_registry.inner.arr == [5]
		assert shared_fields_registry.inner.m[6] == 'z'
	}
	assert shared_fields_registry.items.len == 2
	assert shared_fields_registry.items.element_size == sizeof(voidptr)
	rlock shared_fields_registry.items[1] {
		assert shared_fields_registry.items[1].x == 4
	}
}
