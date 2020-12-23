import flag

struct Foo {
	x int
	y int = 5
}

fn (f Foo) foo() {}

struct Bar {
	Foo
}

fn test_embed() {
	b := Bar{}
	assert b.x == 0
	b.foo()
}

fn test_embed_direct_access() {
	b := Bar{Foo: Foo{}}
	assert b.Foo.y == 5
}

fn test_default_value() {
	b := Bar{Foo: Foo{}}
	assert b.y == 5
}
/* TODO
fn test_initialize() {
	b := Bar{x: 1, y: 2}
	assert b.x == 1
	assert b.y == 2
}
*/
struct Bar3 {
	Foo
	y string = 'test'
}

fn test_overwrite_field() {
	b := Bar3{}
	assert b.y == 'test'
}

struct TestEmbedFromModule {
	flag.Flag
}

struct BarGeneric<T> {
pub:
	foo T
}
struct BarGenericContainer {
	BarGeneric<int>
}
fn test_generic_embed() {
	b := BarGenericContainer{}
	assert b.BarGeneric.foo == 0
	assert b.foo == 0
}

struct Upper {
mut:
	x int
}

struct UpperHolder {
	Upper
}

fn test_assign() {
	mut h := UpperHolder{}
	h.x = 5
	assert h.x == 5
}
