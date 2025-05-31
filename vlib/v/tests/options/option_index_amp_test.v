module main

struct Bar {
	name string
}

struct Foo {
	bars []Bar
}

fn (f Foo) find() ?&Bar {
	return &f.bars[1] or { return none }
}

fn test_main() {
	foo := Foo{
		bars: [Bar{
			name: '123'
		}, Bar{
			name: 'aaa'
		}, Bar{
			name: 's34'
		}]
	}
	bar := foo.find() or { panic('not found') }
	println(bar.name)
	assert bar.name == 'aaa'
}
