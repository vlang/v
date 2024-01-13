struct Bar {
	anon struct {
		foofoo Foo = Foo{'foofoo'}
	}
}

struct Foo {
	name string
}

fn test_anon_struct_with_default_expr() {
	bar := Bar{}
	println(bar.anon.foofoo.name)
	assert bar.anon.foofoo.name == 'foofoo'
}

// for issue 20452
// phenomenon:
// cgen generates incorrect code when the default value of an anonymous struct field is the const variable.
const hello = 'hello'

// vfmt off
fn method_passed_an_anon_struct_arg(arg struct {
		name string
		greeting string = hello
	}) string {
	return '${arg.greeting} ${arg.name}!'
}
// vfmt on

fn test_anon_struct_with_const_default_expr() {
	assert method_passed_an_anon_struct_arg(name: 'world') == 'hello world!'
}
