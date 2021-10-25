module jsdom

pub struct JS.Element {
	classList         JS.DOMTokenList
	childElementCount JS.Number
	className         JS.String
	clientHeight      JS.Number
	clientWidth       JS.Number
	clientTop         JS.Number
	clientLeft        JS.Number
	id                JS.String
	innerHTML         JS.String
	namespaceURI      JS.String
	outerHTML         JS.String
	scrollHeight      JS.Number
	scrollLeft        JS.Number
	scrollTop         JS.Number
	scrollWidth       JS.Number
}

pub struct Element {
	elem JS.Element [noinit]
}

pub fn (e Element) str() string {
	res := ''
	#res.str = e.elem + ''

	return res
}

pub fn (e Element) class_name() string {
	res := ''
	#res.str = e.elem.className

	return res
}

pub fn (e Element) class_list() DOMTokenList {
	list := DOMTokenList{}
	#list.list = e.elem.classList

	return list
}

// node casts `Element` back to `Node`.
pub fn (elem Element) node() Node {
	node := Node{}
	#node.node = elem.elem

	return node
}

pub fn (elem Element) on_click(cb fn (Element, MouseEvent)) {
	#elem.elem.onclick = function (event) { let e = new jsdom__Element({});
	#let ev = new jsdom__MouseEvent({}); ev.event = event;
	#e.elem = this; return cb(e,ev)
	#}
}
