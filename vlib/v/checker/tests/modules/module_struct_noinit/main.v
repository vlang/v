import mod

fn main() {
	dump(default_value[mod.Foo]())
	println(default_value[mod.Foo]())

	dump(default_value[mod.Foo2[int]]())
	println(default_value[mod.Foo2[int]]())
}

fn default_value[T]() T {
	return T{}
}
