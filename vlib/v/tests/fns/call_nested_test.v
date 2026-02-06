module main

@[heap]
struct MyStruct {
	a int
}

fn (m &MyStruct) set(s string) !&MyStruct {
	return m
}

fn test_main() {
	mut m := MyStruct{}
	x := m.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
		.set('2')!
	dump(x)
}
