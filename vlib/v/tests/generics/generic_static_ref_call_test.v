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

fn inc(x int) int {
	return x + 1
}

fn test_generic_static_fn_type_call() {
	queue := Queue.new[fn (int) int](inc)
	assert queue.item(1) == 2
}
