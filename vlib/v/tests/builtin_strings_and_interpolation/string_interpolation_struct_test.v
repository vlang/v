// This file tests whether V can generate a convenience default .str() method
// for a custom struct, when the developer has not defined one himself.
// The .str() methods are used for string interpolation and for println() calls.
struct Man {
	name      string
	age       int
	interests []string
}

fn test_default_struct_string_interpolation() {
	superman := Man{'Superman', 30, ['flying', 'fighting evil', 'being nice']}
	s := '${superman}'
	assert s.starts_with('Man{')
	assert s.contains("name: 'Superman'")
	assert s.contains('age: 30')
	assert s.contains('interests: [')
	assert s.contains("'being nice'")
	assert s.ends_with('}')
	// println(s)
}

struct Context {
pub mut:
	vb [8]f64
}

fn test_fixed_array_struct_string_interpolation() {
	mut ctx := Context{}
	x := 2.32
	ctx.vb = [1.1, x, 3.3, 4.4, 5.0, 6.0, 7.0, 8.9]!
	s := '${ctx}'
	assert s.starts_with('Context{')
	assert s.contains('vb: [1.1, 2.32, 3.3, 4.4, 5.0, 6.0, 7.0, 8.9]')
	assert s.ends_with('}')
}

struct Info {
	name string
	dict map[string]int
}

fn test_struct_map_field_string_interpolation() {
	info := Info{
		name: 'test'
		dict: {
			'a': int(1)
			'b': 2
		}
	}
	s := '${info}'
	assert s.starts_with('Info{')
	assert s.contains("name: 'test'")
	assert s.contains("dict: {'a': 1, 'b': 2}")
	assert s.ends_with('}')
}

struct Circular {
mut:
	next &Circular
}

struct DeepCircular {
	value int
mut:
	next &DeepCircular = unsafe { nil }
}

fn test_autostr_address_guard_bounds_cycles_beyond_stack_capacity() {
	mut root := &DeepCircular{}
	mut cursor := root
	for i in 1 .. 80 {
		next := &DeepCircular{
			value: i
		}
		cursor.next = next
		cursor = next
	}
	cursor.next = cursor
	s := '${root}'
	assert s.contains('<circular>')
	assert s.len < 20_000
}

fn render_circular_concurrently() bool {
	for _ in 0 .. 200 {
		mut elem := &Circular{unsafe { nil }}
		elem.next = elem
		s := '${elem}'.replace('\n', '|')
		if s != '&Circular{|    next: &<circular>|}' {
			return false
		}
	}
	return true
}

fn test_autostr_address_guard_state_is_thread_local() {
	mut threads := []thread bool{cap: 8}
	for _ in 0 .. 8 {
		threads << spawn render_circular_concurrently()
	}
	for ok in threads.wait() {
		assert ok
	}
}

struct AcyclicNode {
	value int
	next  &AcyclicNode = unsafe { nil }
}

fn test_acyclic_recursive_pointer_auto_str_keeps_nested_value() {
	child := &AcyclicNode{
		value: 2
	}
	root := AcyclicNode{
		value: 1
		next:  child
	}
	s := '${root}'.replace('\n', '|')
	assert s == 'AcyclicNode{|    value: 1|    next: &AcyclicNode{|        value: 2|        next: &nil|    }|}'
}

struct AddressChild {
	value int
}

struct AddressOuter {
mut:
	child AddressChild
	ref   &AddressChild = unsafe { nil }
}

fn test_autostr_address_guard_distinguishes_first_field_type() {
	mut outer := AddressOuter{
		child: AddressChild{
			value: 7
		}
	}
	outer.ref = &outer.child
	s := '${outer}'.replace('\n', '|')
	assert s == 'AddressOuter{|    child: AddressChild{|        value: 7|    }|    ref: &AddressChild{|        value: 7|    }|}'
}

fn test_stack_circular_elem_auto_str() {
	mut elem := Circular{unsafe { nil }}
	elem.next = &elem
	s := '${elem}'.replace('\n', '|')
	assert s == 'Circular{|    next: &<circular>|}'
}

fn test_heap_circular_elem_auto_str() {
	mut elem := &Circular{unsafe { nil }}
	elem.next = elem
	s := '${elem}'.replace('\n', '|')
	assert s == '&Circular{|    next: &<circular>|}'
}

struct CrossRefWindow {
mut:
	widgets []CrossRefWidget
}

struct CrossRefWidget {
mut:
	parent &CrossRefWindow = unsafe { nil }
}

fn test_cross_reference_field_auto_str() {
	mut window := &CrossRefWindow{}
	mut widget := &CrossRefWidget{}
	widget.parent = window
	window.widgets << widget
	s := '${window}'.replace('\n', '|')
	assert s == '&CrossRefWindow{|    widgets: [CrossRefWidget{|        parent: &<circular>|    }]|}'
}

interface FamilyMember {
	name string
	age  u64
}

struct FamilySelf {
mut:
	brothers []&FamilyMember
	name     string
	age      u64
}

struct FamilyBrother {
mut:
	brothers []&FamilyMember
	name     string
	age      u64
}

fn test_cross_reference_interface_pointer_array_auto_str() {
	mut me := &FamilySelf{
		name: 'Foo'
		age:  33
	}
	mut brother := &FamilyBrother{
		name: 'Bar'
		age:  32
	}
	me.brothers << brother
	brother.brothers << me
	s := '${me}'
	assert s.contains('&FamilySelf{')
	assert s.contains('brothers: [&FamilyMember(FamilyBrother{')
	assert s.contains("name: 'Foo'")
	assert s.contains("name: 'Bar'")
	assert s.contains('brothers: [&<circular>]')
}

struct CircularArray {
mut:
	children []CircularArray
}

fn test_circular_array_field_auto_str_keeps_item_count() {
	mut value := CircularArray{}
	value.children << CircularArray{}
	value.children << CircularArray{}
	s := '${value}'.replace('\n', '|')
	assert s == 'CircularArray{|    children: [<circular>, <circular>]|}'
}

struct ReturnedTree {
mut:
	root ReturnedNode
	refs map[string]&ReturnedNode
}

@[heap]
struct ReturnedNode {
mut:
	children []ReturnedNode
}

fn make_returned_tree() ReturnedTree {
	mut tree := ReturnedTree{}
	tree.root.children << ReturnedNode{}
	tree.refs['root'] = &tree.root
	tree.refs['child'] = &tree.root.children[0]
	return tree
}

fn test_returned_struct_with_internal_pointer_map_field_auto_str() {
	tree := make_returned_tree()
	s := '${tree}'.replace('\n', '|')
	assert s.contains('ReturnedTree{|')
	assert s.contains('root: ReturnedNode{|        children: [<circular>]|    }')
	assert s.contains("refs: {'root': &ReturnedNode{|        children: [<circular>]|    }, 'child': &ReturnedNode{|        children: []|    }}")
}
