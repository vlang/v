type Foo = string | int | f32

struct Bar {
	log ?Foo
}

fn Bar.init(log ?Foo) {
	mut bar := Bar{
		log: log
	}
	if bar.log != none {
		if bar.log is string {
			assert bar.log == 'foobar'
			return
		}
	}
	assert false
}

fn test_main() {
	Bar.init('foobar')
}
