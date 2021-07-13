struct Foo {
	x &int
}

fn foo(x &int) {
	println(x)
}
fn main() {
	x := 42
	f := Foo{&x}
	foo(f.x) 
}