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

__global (
	plain_registry  GlobalRegistry
	shared_registry shared GlobalRegistry
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
