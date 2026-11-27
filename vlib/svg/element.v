module svg

// Attr is an XML attribute in source order.
pub struct Attr {
pub:
	name  string
	value string
}

// Element is a node in the SVG document tree. Unknown elements are preserved,
// which makes the model forward-compatible with SVG 2 and vendor extensions.
pub struct Element {
pub mut:
	tag      string
	attrs    []Attr
	style    Style
	children []Element
	text     string
}

// elem creates an element with the given tag name.
pub fn elem(tag string) Element {
	return Element{
		tag:   tag
		attrs: []Attr{cap: 4}
	}
}

// get_attr returns an attribute value or '' when absent.
pub fn (e Element) get_attr(name string) string {
	for a in e.attrs {
		if a.name == name {
			return a.value
		}
	}
	return ''
}

// has_attr reports whether an attribute exists.
pub fn (e Element) has_attr(name string) bool {
	for a in e.attrs {
		if a.name == name {
			return true
		}
	}
	return false
}

// set_attr sets an attribute, preserving order for existing names.
pub fn (mut e Element) set_attr(name string, value string) {
	for i, a in e.attrs {
		if a.name == name {
			e.attrs[i] = Attr{ name: name, value: value }
			return
		}
	}
	e.attrs << Attr{ name: name, value: value }
}

// remove_attr deletes an attribute.
pub fn (mut e Element) remove_attr(name string) {
	mut out := []Attr{cap: e.attrs.len}
	for a in e.attrs {
		if a.name != name {
			out << a
		}
	}
	e.attrs = out
}

// add appends a child element.
pub fn (mut e Element) add(child Element) {
	e.children << child
}

// id returns the element's id attribute.
pub fn (e Element) id() string {
	return e.get_attr('id')
}

// classes returns the element's class tokens.
pub fn (e Element) classes() []string {
	c := e.get_attr('class')
	if c == '' {
		return []
	}
	return c.split(' ')
}

// find scans the subtree for the first element with the given id.
pub fn (e Element) find(id string) ?&Element {
	if id == '' {
		return none
	}
	if e.id() == id {
		return &e
	}
	for i in 0 .. e.children.len {
		if found := e.children[i].find(id) {
			return found
		}
	}
	return none
}

// find_tag scans the subtree for the first element with the given tag.
pub fn (e Element) find_tag(tag string) ?&Element {
	if e.tag == tag {
		return &e
	}
	for i in 0 .. e.children.len {
		if found := e.children[i].find_tag(tag) {
			return found
		}
	}
	return none
}

// each applies a function to every element in the subtree (preorder).
pub fn (e Element) each(f fn (el &Element)) {
	f(&e)
	for i in 0 .. e.children.len {
		e.children[i].each(f)
	}
}

// is_shape reports whether the element draws geometry directly.
pub fn (e Element) is_shape() bool {
	return match e.tag {
		'path', 'rect', 'circle', 'ellipse', 'line', 'polyline', 'polygon' {
			true
		}
		else {
			false
		}
	}
}

// is_container reports whether the element can hold drawable children.
pub fn (e Element) is_container() bool {
	return match e.tag {
		'svg', 'g', 'a', 'switch', 'symbol', 'defs', 'clipPath', 'mask', 'pattern', 'marker' {
			true
		}
		else {
			false
		}
	}
}

// text_content returns the concatenated direct text children.
pub fn (e Element) text_content() string {
	return e.text
}
