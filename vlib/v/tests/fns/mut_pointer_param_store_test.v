@[heap]
struct Node {
mut:
	value int
}

@[heap]
struct Store[K, V] {
mut:
	values map[K]V
}

fn (mut s Store[K, V]) set(key K, value V) {
	s.values[key] = value
}

struct Holder {
mut:
	store &Store[string, &Node]
	field &Node = unsafe { nil }
}

fn identity(value &Node) &Node {
	return value
}

fn (mut h Holder) store_mut_pointer(mut value &Node) {
	h.store.set('pointer', value)
	h.field = value
	assert voidptr(identity(value)) == voidptr(value)
}

fn (mut h Holder) store_mut_value(mut value Node) {
	h.store.set('value', value)
}

fn (mut h Holder) store_pointer(value &Node) {
	h.store.set('immutable', value)
}

fn test_mut_pointer_parameter_values_store_the_object_pointer() {
	mut holder := Holder{ store: &Store[string, &Node]{} }
	mut node := &Node{ value: 7 }
	holder.store_mut_pointer(mut node)
	holder.store_mut_value(mut node)
	holder.store_pointer(node)
	assert voidptr(holder.field) == voidptr(node)
	for _, stored in holder.store.values {
		assert voidptr(stored) == voidptr(node)
		assert stored.value == 7
	}
}
