import v.checker.tests.anon_structs_visibility.amod

fn main() {
	foo := amod.Foo{
		bar: struct {
			baz: 1
		}
	}
	println('foo.bar.baz == $foo.bar.baz')
}
