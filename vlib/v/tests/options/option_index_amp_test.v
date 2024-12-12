module main

struct Bar {
	name string
}

struct Foo333 {
	bars []Bar
}

fn (f Foo333) find() ?&Bar {
	bar1_2 := &f.bars[1] or { return none }
	bar1_4 := &f.bars[1]
	println('bar1_2 `${ptr_str(bar1_2)}`')
	println('bar1_4 `${ptr_str(bar1_4)}`')
	return &f.bars[1] or { return none }
}

fn test_main() {
	foo := Foo333{
		bars: [Bar{
			name: '123'
		}, Bar{
			name: 'aaa'
		}, Bar{
			name: 's34'
		}]
	}

	println('o bar1 1 `${ptr_str(foo.bars[1])}`')
	println('o bar1 2 `${ptr_str(&foo.bars[1])}`')

	bar := foo.find() or { panic('not found') }
	println('o bar1 3 `${ptr_str(bar)}`')
	println(bar.name)
	assert bar.name == 'aaa'

	bar2 := foo.find() or { panic('not found') }
	println(bar2.name)
	assert bar2.name == 'aaa'
}
