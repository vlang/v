fn test_interface_embedding_call() {
	g1 := G1{}
	do_the_greet(g1)
}

struct G1 {}

fn (g G1) greet() string {
	return 'hello from G1'
}

fn do_the_greet(g ParentGreeter) {
	greet := g.greet()
	println('Someone says: ${greet}')
	assert greet == 'hello from G1'
}

interface ParentGreeter {
	Greeter
}

interface Greeter {
	greet() string
}

// for issue 16496, test the own methods.
interface Foo {
	a_method()
}

fn (f Foo) foo_method() int {
	return 0
}

interface Bar {
	Foo
}

fn (b Bar) bar_method() int {
	// The test calls the method of the embedded interface in the interface method
	return b.foo_method()
}

struct Derived {}

fn (d &Derived) a_method() {
}

fn test_embedding_method_call_cgen() {
	bar := Bar(Derived{})
	assert bar.bar_method() == 0
}

// for issue 20113, test call the method signatures
interface IFoo[T] {
	method(arg T) T
}

interface IBar[T] {
	IFoo
}

interface IBaz[T] {
	IBar
}

struct DerivedStruct[T] {
}

fn (d DerivedStruct[T]) method[T](arg T) T {
	return arg
}

fn main() {
	a := IBaz[int](DerivedStruct[int]{})
	assert a.method(1) == 1
}
