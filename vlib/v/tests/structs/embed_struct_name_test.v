struct MyTemplate[T] {
	data T
}

enum MyEnum as u8 {
	client
	server
}

struct Embed {
	a int
}

struct MyStructure {
	Embed
	MyTemplate[MyEnum]
}

fn test_embed_name_with_enum() {
	t := MyStructure{
		a:    10
		data: .server
	}
	dump(t)
	assert t.a == 10
	assert t.data == .server
}
