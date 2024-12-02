module main

type Foo = [4]u8

fn Foo.new(el []u8) !Foo {
	if el.len != 4 {
		return error('el must have 4 members')
	}
	mut bytes := [4]u8{}
	for i := 0; i < 4; i++ {
		bytes[i] = el[i]
	}
	return Foo(bytes)
}

fn Foo.new_no_err(el []u8) Foo {
	mut bytes := [4]u8{}
	for i := 0; i < 4; i++ {
		bytes[i] = el[i]
	}
	return Foo(bytes)
}

fn (f Foo) str() string {
	return '${f[0]}, ${f[1]}, ${f[2]}, ${f[3]}'
}

fn test_main() {
	a := Foo.new_no_err([u8(5), 4, 3, 2])
	println(a)
	assert a == [u8(5), 4, 3, 2]!

	b := Foo.new([u8(1), 2, 3, 4])!
	println(b)
	assert b == [u8(1), 2, 3, 4]!

	c := Foo.new([u8(1), 2, 3, 4]) or { Foo([u8(0), 0, 0, 0]!) }
	println(c)
	assert c == [u8(1), 2, 3, 4]!
}
