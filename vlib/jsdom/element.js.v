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
	Node
}

pub fn (e Element) str() string {
	res := ''
	#res.str = e.node + ''

	return res
}

pub fn (elem Element) typ() NodeType {
	return .element
}

pub fn (e Element) class_name() string {
	res := ''
	#res.str = e.node.className

	return res
}

pub fn (e Element) class_list() DOMTokenList {
	list := DOMTokenList{}
	#list.list = e.node.classList

	return list
}

// node casts `Element` back to `Node`.
pub fn (elem Element) node() Node {
	node := Node{}
	#node.node = elem.node

	return node
}

pub fn (elem Element) add_event_listener(event string, cb EventCallback) {
	#elem.node.addEventListener(event.str, function (event) { let e = jsdom__dispatch_event_target(this);
	#let ev = jsdom__dispatch_event(event); ev.event = event;
	#return cb(e,ev)
	#});
}

pub interface IElement {
	INode
}

pub struct HTMLElement {
	Element
}

pub fn (elem HTMLElement) typ() NodeType {
	return .element
}

pub fn (elem HTMLElement) access_key() string {
	res := ''
	#res.str = elem.node.accessKey;

	return res
}

pub fn (mut elem HTMLElement) set_access_key(key string) {
	#elem.val.node.accessKey = key.str;
}

pub fn (elem HTMLElement) add_event_listener(event string, cb EventCallback) {
	#elem.node.addEventListener(event.str, function (event) { let e = jsdom__dispatch_event_target(this);
	#let ev = jsdom__dispatch_event(event); ev.event = event;
	#return cb(e,ev)
	#});
}
