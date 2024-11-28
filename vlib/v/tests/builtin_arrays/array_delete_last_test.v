struct ArrayList[T] {
mut:
	size  usize
	items []&T
}

fn new_array_list[T]() ArrayList[T] {
	return ArrayList[T]{}
}

fn (li ArrayList[T]) is_empty() bool {
	return li.size == 0
}

fn (mut li ArrayList[T]) add(item &T) {
	li.items << item
	li.size += 1
}

fn (mut li ArrayList[T]) pop() ?&T {
	if li.is_empty() {
		return none
	} else {
		item := li.items.last()
		li.items.delete_last()
		li.size -= 1
		return item
	}
}

struct Student {
	name string
mut:
	age u8
}

fn (mut s Student) grow() {
	s.age += 1
}

fn test_main() {
	mut list := new_array_list[Student]()
	assert list.is_empty() && list.size == 0

	mut stu := Student{'Tom', 16}
	list.add(&stu)
	assert !list.is_empty() && list.size == 1
	stu.grow()
	assert stu.age == 17

	tom := list.pop()?
	assert list.is_empty() && list.size == 0
	assert tom == stu
	assert tom.age == 17
}
