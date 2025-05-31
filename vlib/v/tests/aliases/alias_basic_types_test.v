type MyByte = u8

type MyInt = int

type MyString = string

type Foo = string

// bytes
fn test_byte_aliasing() {
	dump(u8(123))
	dump(MyByte(u8(123)))
	dump(u8(MyByte(u8(123))))
	assert true
}

fn test_pbyte_aliasing() {
	unsafe {
		dump(voidptr(&u8(123)))
		dump(voidptr(&MyByte(&u8(123))))
		dump(voidptr(&u8(&MyByte(&u8(123)))))
	}
	assert true
}

// ints
fn test_int_aliasing() {
	dump(int(123))
	dump(int(MyInt(123)))
	dump(MyInt(int(MyInt(123))))
	assert true
}

fn test_pint_aliasing() {
	unsafe {
		dump(voidptr(&int(123456)))
		dump(voidptr(&MyInt(&int(123456))))
		dump(voidptr(&int(&MyInt(&int(123456)))))
	}
	assert true
}

// strings
fn test_string_aliasing() {
	$if msvc {
		eprintln('> TODO: msvc errors out for casting a `string` to a `string`')
	}
	$if !msvc {
		dump(string('abc'))
		dump(string(MyString('abc')))
		dump(MyString(string(MyString('abc'))))
	}
	assert true
}

fn test_pstring_aliasing() {
	s := 'abc'
	unsafe {
		dump(voidptr(&string(&s)))
		dump(voidptr(&string(&MyString(&s))))
		dump(voidptr(&MyString(&string(&MyString(&s)))))
	}
	assert true
}

//

struct MyStruct {
mut:
	a MyInt
	b MyByte
	c MyString
}

fn test_modifying_a_struct_using_an_alias_to_int() {
	mut my_struct := MyStruct{}
	my_struct.a += 5
	my_struct.b += 10
	my_struct.c += 'abc'

	my_struct.a += 3
	my_struct.b += 20
	my_struct.c += 'def'
	println(my_struct)

	assert my_struct.a == 8
	assert my_struct.b == 30
	assert my_struct.c == 'abcdef'
}

fn (f Foo) + (f1 Foo) Foo {
	return '${f} _+_ ${f1}'
}

fn (f Foo) - (f1 Foo) Foo {
	return '${f} _-_ ${f1}'
}

fn (f Foo) * (f1 Foo) Foo {
	return '${f} _*_ ${f1}'
}

fn (f Foo) / (f1 Foo) Foo {
	return '${f} _/_ ${f1}'
}

fn test_op_overrides_for_string_aliases() {
	a := Foo('abc')
	b := Foo('def')
	assert a + b == 'abc _+_ def'
	assert a - b == 'abc _-_ def'
	assert a * b == 'abc _*_ def'
	assert a / b == 'abc _/_ def'
}
