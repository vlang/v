// callback types
type CBnoret[T] = fn (val T)

type CBnoret2[T] = fn (val T, prev T)

type CBvret[T] = fn (val T) T

type CBvret2[T] = fn (val T, prev T) T

type Callback[T] = CBnoret[T] | CBnoret2[T] | CBvret[T] | CBvret2[T]

interface IObv[T] {
	v    T
	prev T
	cb   []Callback[T]
}

struct Obv[T] {
mut:
	v    T
	prev T
	cb   []Callback[T]
}

fn (o Obv[T]) get() T {
	return o.v
}

fn (o Obv[T]) do_callbacks() T {
	return o.v
}

fn (mut o Obv[T]) set(new_value T) T {
	prev := o.v
	if prev != new_value {
		o.v = new_value
		o.prev = prev
	}

	return o.v
}

fn oo2[T](init T) ?Obv[T] {
	return none
}

fn oo[T](init T) Obv[T] {
	return Obv[T]{
		v: init
	}
}

fn test_main() {
	one := oo(1)
	txt := oo('lala')

	println('txt: ${txt.get()}')
	println('one: ${one.get()}')

	assert txt.get() == 'lala'
	assert one.get() == 1
}
