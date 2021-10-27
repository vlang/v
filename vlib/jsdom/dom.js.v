// DOM API for JS backend
module jsdom

[heap]
pub struct JS.Node {
	baseURI         JS.String   [noinit]
	childNodes      JS.NodeList [noinit]
	firstChild      voidptr     [noinit]
	isConnected     JS.bool     [noinit]
	lastChild       voidptr     [noinit]
	nextSibling     voidptr     [noinit]
	nodeName        JS.String   [noinit]
	nodeType        NodeType    [noinit]
	nodeValue       voidptr     [noinit]
	ownerDocument   JS.Document [noinit]
	parentNode      voidptr     [noinit]
	parentElement   JS.Element  [noinit]
	previousSibling voidptr     [noinit]
	textContext     voidptr
}

pub struct JS.NodeList {
pub mut:
	length JS.Number
}

pub enum NodeType {
	element = 1
	attribute = 2
	text = 3
	cdata_section = 4
	entity_reference = 5
	entity = 6
	processing_instruction = 7
	comment = 8
	document = 9
	document_type = 10
	document_fragment = 11
	notation = 12
}

pub struct Node {
	node JS.Node [noinit]
}

pub fn (n Node) typ() NodeType {
	return n.node.nodeType
}

/// IEventTarget interface is implemented by objects that can receive events and may have listeners for them.
// In other words, any target of events implements the three methods associated with this interface.
// TODO: remove_event_listener and dispatch_event
pub interface IEventTarget {
	add_event_listener(event string, cb EventCallback)
}

/// The DOM Node interface is an abstract base class upon which many other DOM API objects are based, thus
// letting those object types to be used similarly and often interchangeably. As an abstract class,
// there is no such thing as a plain Node object. All objects that implement Node functionality are based on one of its subclasses.
// Most notable are Document, Element, and DocumentFragment.
pub interface INode {
	IEventTarget
	typ() NodeType
}

pub fn (elem Node) add_event_listener(event string, cb EventCallback) {
	#elem.node.addEventListener(event.str, function (event) { let e = jsdom__dispatch_event_target(this);
	#let ev = jsdom__dispatch_event(event); ev.event = event;
	#return cb(e,ev)
	#});
}

pub fn (n INode) is_(ty NodeType) bool {
	res := false
	#res.val = n.node.nodeType == ty

	return res
}

pub fn (n INode) document() ?Document {
	res := Document{}
	if n.is_(.document) {
		#res.node = n.node

		return res
	} else {
		return none
	}
}

pub fn (n INode) element() ?Element {
	res := Element{}
	if n.is_(.element) {
		#res.node = n.node

		return res
	} else {
		return none
	}
}

pub fn (n INode) append_child(child INode) {
	#n.node.appendChild(child.node)
}

pub fn (n INode) clone_node(deep ...int) INode {
	cloned := Node{}
	if deep.len == 0 {
		#cloned.node = n.node.cloneNode()
	} else {
		#cloned.node = n.node.cloneNode(deep.arr.get(new int(0)).val)
	}
	return cloned
}

pub fn (n INode) contains(other INode) bool {
	res := false
	#res.val = n.node.contains(other.node)

	return res
}

pub fn (n INode) get_root_node() INode {
	root := Node{}
	#root.node = n.node.getRootNode()

	return root
}

pub fn (n INode) has_child_nodes() bool {
	res := false
	#res.val = n.node.hasChildNodes()

	return res
}

pub fn (n INode) insert_before(new_node INode, reference_node INode) Node {
	inserted := Node{}
	#inserted.node = n.node.insertBefore(new_node.node,reference_node.node)

	return inserted
}

/*
pub fn (x Node) == (y Node) bool {
	res := false
	#res.val = x.node.isEqualNode(y.node)

	return res
}*/

pub fn (n INode) remove_child(child INode) {
	#n.node.removeChild(child.node)
}

pub fn (n INode) replace_child(new_child INode, old_child INode) INode {
	#old_child.node = n.node.replace_child(new_child.node,old_child.node)

	return old_child
}

pub struct JS.EventTarget {}

fn dispatch_event_target(target JS.EventTarget) IEventTarget {
	mut ret := IEventTarget(Element{})
	#if (target instanceof HTMLCanvasElement) { ret = new jsdom__HTMLCanvasElement({}); ret.node = target; }
	#else if (target instanceof HTMLElement) { ret = new jsdom__HTMLElement({}); ret.node = target; }
	#else if (target instanceof Window) { ret = new jsdom__Window({}); ret.node = target;}
	#else if (target instanceof SVGElement) { ret = new jsdom__SVGElement({}); ret.node = target; }
	#else if (target instanceof Element) { ret = new jsdom__Element({}); ret.node = target; }
	#else if (target instanceof Document) { ret = new jsdom__Document({}); ret.node = target; }

	return ret
}

pub type EventCallback = fn (_ IEventTarget, _ IEvent)

pub const (
	document = Document{}
	window   = Window{}
)

fn init() {
	#jsdom__document.node = document;
	#jsdom__window.node = window;
}
