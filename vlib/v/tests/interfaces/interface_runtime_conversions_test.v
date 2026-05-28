interface Widget {
}

interface ResizableWidget {
	Widget
	resize(x int, y int) int
}

fn draw(w Widget) {
	// w.resize(10, 20) // <- this won't work, since all Widgets may not implement resize()

	// however, we can check if the underlying type of w implements a different interface:
	if w is ResizableWidget {
		assert w is WidgetB
		rw := w as ResizableWidget
		assert rw is WidgetB
		// if so, we can now safely call that extra method
		assert rw.resize(10, 20) == 200
	} else {
		assert w is WidgetA
	}
}

fn draw_ref(w &Widget) {
	if w is ResizableWidget {
		rw := w as ResizableWidget
		assert rw.resize(10, 20) == 200
	} else {
		assert false
	}
}

// implements Widget, but not ResizableWidget
struct WidgetA {
}

// implements both Widget and ResizableWidget
struct WidgetB {
}

fn (w WidgetB) resize(x int, y int) int {
	return x * y
}

fn test_interface_runtime_conversions() {
	draw(WidgetA{})
	draw(WidgetB{})
}

enum CompositeNodeType {
	int
	str
	list
	block
}

interface CompositeValue {
	type_of() CompositeNodeType
}

interface CompositeNode {
	values []CompositeValue
}

type CompositeInt = int

fn (x CompositeInt) type_of() CompositeNodeType {
	return .int
}

type CompositeStr = string

fn (x CompositeStr) type_of() CompositeNodeType {
	return .str
}

struct CompositeList {
	values []CompositeValue
}

fn (x CompositeList) type_of() CompositeNodeType {
	return .list
}

struct CompositeBlock {
	values []CompositeValue
}

fn (x CompositeBlock) type_of() CompositeNodeType {
	return .block
}

fn composite_values_identical(v1 CompositeValue, v2 CompositeValue) bool {
	if v1.type_of() != v2.type_of() {
		return false
	}
	return match v1 {
		CompositeInt { v1 == (v2 as CompositeInt) }
		CompositeStr { v1 == (v2 as CompositeStr) }
		CompositeList, CompositeBlock { composite_nodes_identical(v1, v2 as CompositeNode) }
		else { false }
	}
}

fn composite_nodes_identical(s1 CompositeNode, s2 CompositeNode) bool {
	if s1.values.len != s2.values.len {
		return false
	}
	for i := 0; i < s1.values.len; i++ {
		if !composite_values_identical(s1.values[i], s2.values[i]) {
			return false
		}
	}
	return true
}

fn test_interface_runtime_conversions_from_aggregate_smartcast_to_interface() {
	list1 := CompositeList{
		values: [CompositeValue(CompositeInt(34)), CompositeStr('foo')]
	}
	list2 := CompositeList{
		values: [CompositeValue(CompositeInt(34)), CompositeStr('bar')]
	}
	block1 := CompositeBlock{
		values: [CompositeValue(CompositeInt(34)), CompositeStr('foo')]
	}
	block2 := CompositeBlock{
		values: [CompositeValue(CompositeInt(34)), CompositeStr('foo')]
	}
	assert !composite_values_identical(list1, list2)
	assert composite_values_identical(block1, block2)
}
