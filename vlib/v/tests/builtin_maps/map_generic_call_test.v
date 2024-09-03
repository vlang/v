struct FooParams[K, V] {
	k  K
	v  V
	k2 K
}

struct Foo[K, V] {
	k K
mut:
	m map[K]V
}

fn new_foo[K, V](params FooParams[K, V]) Foo[K, V] {
	return Foo[K, V]{
		k: params.k2
		m: {
			params.k: params.v
		}
	}
}

fn (mut p Foo[K, V]) f() {
	mut y := p.m.move()
	y.clear()
	y.reserve(6)
	y.delete(p.k)
	assert y.keys().len == 0
	assert y.values().len == 0
	assert y.clone().len == 0
	unsafe { y.free() }
}

fn test_main() {
	mut foo := new_foo(FooParams{ k: 'abc', v: 42, k2: 'def' })
	foo.f()
}
