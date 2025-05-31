import mod

fn main() {
	dump(default_value[mod.Foo]())
	println(default_value[mod.Foo]())
}

fn default_value[T]() T {
	return T{}
}
