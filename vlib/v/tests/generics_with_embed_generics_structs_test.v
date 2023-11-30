struct Foo[T] {
	field T
}

struct Bar[T] {
	Foo[T]
}

struct Baz[T] {
	Bar[T]
}

struct OneLevelEmbed[T] {
	Foo[T]
}

fn test_main() {
	m := OneLevelEmbed[int]{}
	assert m.field == 0
}

struct MultiLevelEmbed[T] {
	Baz[T]
}

fn test_multi_level() {
	m := MultiLevelEmbed[int]{}
	assert m.field == 0
}
