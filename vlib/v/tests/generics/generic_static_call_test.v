module main

type Tag = int
type Octet = string

fn (o Octet) tag() Tag {
	return Tag(1)
}

fn (o Octet) pack() ![]u8 {
	return o.bytes()
}

fn (o Octet) bytes() []u8 {
	s := string(o)
	return s.bytes()
}

// static method on concrete type
fn Octet.unpack(b []u8) Octet {
	return Octet(b.bytestr())
}

// This is generic type
struct Elm[T] {
mut:
	val T
}

fn Elm.new[T](val T) Elm[T] {
	return Elm[T]{
		val: val
	}
}

fn (el Elm[T]) the_t() T {
	return el.val
}

fn Elm.unpack[T](src []u8) !Elm[T] {
	t := T.unpack(src)
	return Elm[T]{t}
}

fn (el Elm[T]) tag() Tag {
	return el.val.tag()
}

fn (el Elm[T]) pack() ![]u8 {
	return el.val.pack()!
}

fn test_main() {
	b := Octet('xx')
	mut el := Elm.new[Octet](b)
	bytes := el.pack()!
	assert bytes == [u8(120), 120]

	// unpack
	ab := Elm.unpack[Octet](bytes)!
	assert el == ab
	assert el.the_t() == Octet('xx')
}
