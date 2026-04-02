module main

struct Item {}

struct Queue[T] {
	item T
}

fn Queue.new[T](item T) Queue[T] {
	return Queue[T]{
		item: item
	}
}

fn test_generic_static_ref_call() {
	item := &Item{}
	queue := Queue.new[&Item](item)
	assert queue.item == item
}
