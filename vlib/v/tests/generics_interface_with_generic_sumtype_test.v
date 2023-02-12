struct NonValue {}

pub type OptionSumType[T] = NonValue | T

pub fn (self OptionSumType[T]) is_some[T]() bool {
	return self is T
}

pub fn (self OptionSumType[T]) is_none[T]() bool {
	return !self.is_some()
}

pub fn (self OptionSumType[T]) expect[T](msg string) T {
	if self.is_some() {
		return self as T
	} else {
		panic(msg)
	}
}

pub fn (self OptionSumType[T]) unwrap[T]() T {
	return self.expect('unwrap on a none value')
}

pub fn (self OptionSumType[T]) unwrap_or[T](default T) T {
	if self.is_some() {
		return self as T
	} else {
		return default
	}
}

pub fn (self OptionSumType[T]) unwrap_or_else[T](else_fn fn () T) T {
	if self.is_some() {
		return self as T
	} else {
		return else_fn()
	}
}

pub fn some[T](value T) OptionSumType[T] {
	return OptionSumType[T](value as T)
}

pub fn null[T]() OptionSumType[T] {
	return OptionSumType[T](NonValue{})
}

// iter.v
pub interface Iterator[T] {
mut:
	next() OptionSumType[T]
}

pub fn (mut self Iterator[T]) count[T]() int {
	mut count := 0
	for self.next().is_some() {
		count += 1
	}
	return count
}

struct EmptyIter[T] {
	value NonValue
}

fn (mut self EmptyIter[T]) next[T]() OptionSumType[T] {
	return OptionSumType[T](self.value)
}

// iter_test.v
fn test_generics_interface_with_generic_sumtype() {
	mut iter1 := EmptyIter[u32]{}
	println(iter1)
	assert iter1.next().is_none()

	mut iter2 := Iterator[u32](iter1)
	println(iter2)
	assert iter2.count() == 0
}
