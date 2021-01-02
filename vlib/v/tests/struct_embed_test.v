import flag
import field_publicity

struct Foo {
	x int
	y int = 5
}

fn (f Foo) foo() {}

struct Bar {
	Foo
}

fn test_embed() {
	b := Bar{}
	assert b.x == 0
	b.foo()
}

fn test_embed_direct_access() {
	b := Bar{Foo: Foo{}}
	assert b.Foo.y == 5
}

fn test_default_value() {
	b := Bar{Foo: Foo{}}
	assert b.y == 5
}

fn test_default_value_without_init() {
	b := Bar{}
	assert b.y == 5
}
/* TODO
fn test_initialize() {
	b := Bar{x: 1, y: 2}
	assert b.x == 1
	assert b.y == 2
}
*/
struct Bar3 {
	Foo
	y string = 'test'
}

fn test_overwrite_field() {
	b := Bar3{}
	assert b.y == 'test'
}

struct TestEmbedFromModule {
	flag.Flag
}

struct BarGeneric<T> {
pub:
	foo T
}
struct BarGenericContainer {
	BarGeneric<int>
}
fn test_generic_embed() {
	b := BarGenericContainer{}
	assert b.BarGeneric.foo == 0
	assert b.foo == 0
}

struct Upper {
mut:
	x int
}

struct UpperHolder {
	Upper
}

fn test_assign() {
	mut h := UpperHolder{}
	h.x = 5
	assert h.x == 5
}

fn test_embed_is_public() {
	a := field_publicity.App{}
	assert a.Context.name == ''  
}

struct Eggs {
	name string
}

fn (f &Eggs) test(x int) int {
	return x
}

struct Breakfast {
	Eggs
}

fn (b &Breakfast) name() string {
	return b.name
}

fn test_embed_method_receiver_ptr() {
	b := Breakfast{}
	assert b.test(5) == 5
}

fn test_embed_field_receiver_ptr() {
	b := Breakfast{}
	assert b.name() == ''
}

fn test_embed_mutable() {
	mut a := field_publicity.App{}
	a.Context = field_publicity.Context{}
}

struct Context {
	static_files string
}

fn (c Context) test() bool {
	return true
}

struct App {
	Context
}

fn embed_field_access_generic<T>(mut app T) {
	app.Context = Context{
		static_files: app.static_files
	}
}

fn test_embed_field_access_generic() {
	mut app := App{}
	embed_field_access_generic(mut app)
}

fn embed_method_generic<T>(app T) bool {
	return app.test()
}

fn test_embed_method_generic() {
	mut app := App{}
	assert embed_method_generic(app)
}
