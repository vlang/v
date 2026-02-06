struct Foo {
	x int
}

struct Bar {
	x int
}

type Foobar = Bar | Foo

struct Foobars {
	m map[string]Foobar
}

fn (f &Foobars) find_foobar(name string) ?Foobar {
	return f.m[name] or { return none }
}

fn (mut f Foobars) is_known(name string) bool {
	return f.find_foobar(name) != none
}

fn test_main() {
	mut foobars := Foobars{
		m: map[string]Foobar{}
	}
	assert foobars.is_known('deadbeef') == false
}
