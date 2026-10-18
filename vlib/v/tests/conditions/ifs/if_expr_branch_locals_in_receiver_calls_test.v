// A value `if`/`match` whose branch value calls a method on the result of a
// call that uses a name bound by the branch itself (an `if x := opt {` guard
// variable, or a local declared before the value) must see that name, even
// when the expression is typed against an expected type before its branches
// are checked (`field = if ...`, `return match ...`, struct field values).
struct Doc {
	title string
}

struct Node {
mut:
	connected bool
	doc       &Doc = unsafe { nil }
}

@[heap]
struct Element {
	Node
}

fn (n &Node) is_connected() bool {
	return n.connected
}

fn (n &Node) document() &Doc {
	return n.doc
}

fn (d &Doc) name() string {
	return d.title
}

fn (e &Element) as_node() &Node {
	return &e.Node
}

fn find_element(e &Element, found bool) ?&Element {
	if found {
		return e
	}
	return none
}

fn dom_node(e &Element) &Node {
	return &e.Node
}

struct Holder {
mut:
	connected bool
	flags     []bool
}

fn (mut h Holder) update(e &Element, found bool) {
	h.connected = if host := find_element(e, found) {
		host.as_node().is_connected()
	} else {
		false
	}
	h.flags[0] = if host := find_element(e, found) {
		host.as_node().is_connected()
	} else {
		false
	}
}

fn test_if_guard_value_assigned_to_existing_lvalues() {
	e := &Element{
		connected: true
	}
	mut h := Holder{
		flags: [false]
	}
	h.update(e, true)
	assert h.connected
	assert h.flags == [true]
	h.update(e, false)
	assert !h.connected
	assert h.flags == [false]
	mut local := false
	local = if host := find_element(e, true) {
		host.as_node().is_connected()
	} else {
		false
	}
	assert local
}

struct Options {
	doc   ?&Doc
	title string
}

fn options_for(e &Element, found bool) Options {
	return Options{
		doc:   if element := find_element(e, found) {
			dom_node(element).document()
		} else {
			none
		}
		title: if element := find_element(e, found) {
			dom_node(element).document().name()
		} else {
			'none'
		}
	}
}

fn test_if_guard_value_in_struct_field() {
	d := &Doc{'page'}
	e := &Element{
		doc: d
	}
	o := options_for(e, true)
	doc := o.doc or { panic('missing document') }
	assert doc.title == 'page'
	assert o.title == 'page'
	o2 := options_for(e, false)
	assert o2.doc == none
	assert o2.title == 'none'
}

enum Kind {
	element
	text
	missing
}

fn source_text(kind Kind, e &Element) ?string {
	return match kind {
		.element {
			element := find_element(e, true) or { return none }
			element.as_node().document().name()
		}
		.text {
			length := e.as_node().document().title.len
			dom_node(e).document().name().repeat(length)
		}
		.missing {
			element := find_element(e, false) or { return none }
			element.as_node().document().name()
		}
	}
}

fn test_match_branch_locals_in_returned_value() {
	e := &Element{
		doc: &Doc{'ab'}
	}
	assert source_text(.element, e) or { 'none' } == 'ab'
	assert source_text(.text, e) or { 'none' } == 'abab'
	assert source_text(.missing, e) == none
}

fn else_block_locals(e &Element, found bool) ?string {
	return if found {
		'found'
	} else {
		element := find_element(e, true) or { return none }
		element.as_node().document().name()
	}
}

fn test_if_else_block_locals_in_returned_value() {
	e := &Element{
		doc: &Doc{'else'}
	}
	assert else_block_locals(e, true) or { 'none' } == 'found'
	assert else_block_locals(e, false) or { 'none' } == 'else'
}
