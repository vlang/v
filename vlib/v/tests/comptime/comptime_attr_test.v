@[bar; foo]
fn abc() {
}

@[foo]
struct Foo {
}

@[bar]
fn Foo.bar() {
}

@[baz]
enum EnumFoo {
	a
	b
}

@[iface]
interface IFoo {
}

@[custom]
type CustomType = EnumFoo | Foo

fn test_main() {
	mut c := 0
	$for f in abc.attributes {
		dump(f)
		assert f.name in ['foo', 'bar']
		c++
	}
	assert c == 2

	$for f in Foo.attributes {
		dump(f)
		assert f.name == 'foo'
		c++
	}
	assert c == 3

	a := Foo.bar
	$for f in a.attributes {
		dump(f)
		assert f.name == 'bar'
		c++
	}
	assert c == 4

	b := abc
	$for f in b.attributes {
		dump(f)
		assert f.name in ['foo', 'bar']
		c++
	}
	assert c == 6

	$for f in EnumFoo.attributes {
		dump(f)
		assert f.name == 'baz'
		c++
	}
	assert c == 7

	$for f in IFoo.attributes {
		dump(f)
		assert f.name == 'iface'
		c++
	}
	assert c == 8

	$for f in CustomType.attributes {
		dump(f)
		assert f.name == 'custom'
		c++
	}
	assert c == 9
}
