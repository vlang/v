@[heap]
struct Foo {
mut:
	val int
}

@[heap]
struct Bar {
mut:
	val int
}

interface FooBar {
mut:
	val int
}

type FooBarSum = Bar | Foo

type IntHandler = fn (int) int

type HandlerOrString = IntHandler | string

interface Widget {}

interface ResizableWidget {
	Widget
	resize(x int, y int) int
}

struct WidgetImpl {}

fn (_ WidgetImpl) resize(x int, y int) int {
	return x * y
}

fn double(value int) int {
	return value * 2
}

fn test_main() {
	mut fbs := []&FooBar{}
	fbs << &Foo{1}
	a := &(fbs[0] as Foo)
	println(a)
	b := &(fbs.last() as Foo)
	println(b)
	fb := fbs[0]
	field_ptr := &((fb as Foo).val)
	assert *field_ptr == 1
	sum := FooBarSum(Foo{2})
	sum_field_ptr := &((sum as Foo).val)
	assert *sum_field_ptr == 2
	handler_sum := HandlerOrString(IntHandler(double))
	handler_ptr := &(handler_sum as IntHandler)
	handler := *handler_ptr
	assert handler(3) == 6
	widget := Widget(WidgetImpl{})
	if widget is ResizableWidget {
		widget_ptr := &(widget as ResizableWidget)
		assert widget_ptr.resize(2, 3) == 6
	} else {
		assert false
	}
	arr1 := [(fbs.last() as Foo)]
	arr2 := [&(fbs.last() as Foo)]
	arr3 := [&(get_foo_bar() as Foo)]
	println(arr1)
	println(arr2)
	println(arr3)
	println(&(fbs.last() as Foo))
	assert arr2[0] == arr3[0]
}

fn get_foo_bar() FooBar {
	return Foo{1}
}
