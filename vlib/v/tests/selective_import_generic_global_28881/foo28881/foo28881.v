module foo28881

pub struct Foo[T] {
pub mut:
	items []T
}

pub struct Iterator[T] {
mut:
	items []T
	idx   int
}

// iter returns an iterator over the items of `self`.
pub fn (self &Foo[T]) iter() Iterator[T] {
	return Iterator[T]{
		items: self.items
	}
}

// next returns the next item, or none when the iterator is exhausted.
pub fn (mut self Iterator[T]) next() ?T {
	if self.idx >= self.items.len {
		return none
	}
	item := self.items[self.idx]
	self.idx++
	return item
}

pub struct Plain {
pub mut:
	value int
}

// doubled returns twice the plain value.
pub fn (p Plain) doubled() int {
	return p.value * 2
}

pub struct Pair[K, V] {
pub mut:
	key K
	val V
}
