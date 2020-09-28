import flag

struct Foo {
	x int
	y int = 5
}

struct Bar {
	Foo
}

fn test_embed() {
	b := Bar{}
	assert b.x == 0
}

struct Bar2 {
	Foo
}

fn test_default_value() {
	b := Bar{}
	assert b.y == 5
}

fn test_initialize() {
	b := Bar{x: 1, y: 2}
	assert b.x == 1
	assert b.y == 2
}

struct Bar3 {
	Foo
	y int = 10
}

fn test_overwrite_field() {
	b := Bar3{}
	assert b.y == 10
}

struct TestEmbedFromModule {
	flag.Flag
}
