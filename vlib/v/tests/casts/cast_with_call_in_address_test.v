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

type StringPairOrInt = [2]string | int

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

fn addressed_handler() &IntHandler {
	handler_sum := HandlerOrString(IntHandler(double))
	return &(handler_sum as IntHandler)
}

fn addressed_widget() &ResizableWidget {
	widget := Widget(WidgetImpl{})
	if widget is ResizableWidget {
		return &(widget as ResizableWidget)
	}
	panic('WidgetImpl should implement ResizableWidget')
}

fn string_pair() StringPairOrInt {
	return ['left', 'right']!
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
	// vfmt off
	nested_handler_ptr := &((handler_sum as IntHandler))
	// vfmt on
	nested_handler := *nested_handler_ptr
	assert nested_handler(4) == 8
	escaped_handler := *addressed_handler()
	assert escaped_handler(5) == 10
	widget := Widget(WidgetImpl{})
	if widget is ResizableWidget {
		widget_ptr := &(widget as ResizableWidget)
		assert widget_ptr.resize(2, 3) == 6
		// vfmt off
		nested_widget_ptr := &((widget as ResizableWidget))
		// vfmt on
		assert nested_widget_ptr.resize(3, 4) == 12
	} else {
		assert false
	}
	escaped_widget := addressed_widget()
	assert escaped_widget.resize(4, 5) == 20
	pair_ptr := &(string_pair() as [2]string)
	assert (*pair_ptr)[0] == 'left'
	assert (*pair_ptr)[1] == 'right'
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
