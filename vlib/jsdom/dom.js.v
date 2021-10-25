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

pub fn (n Node) is_(ty NodeType) bool {
	return n.node.nodeType == ty
}

pub fn (n Node) document() ?Document {
	res := Document{}
	if n.is_(.document) {
		#res.doc = n.node

		return res
	} else {
		return none
	}
}

pub fn (n Node) element() ?Element {
	res := Element{}
	if n.is_(.element) {
		#res.elem = n.node

		return res
	} else {
		return none
	}
}

pub fn (n Node) append_child(child Node) {
	#n.node.appendChild(child.node)
}

pub fn (n Node) clone_node(deep ...int) Node {
	cloned := Node{}
	if deep.len == 0 {
		#cloned.node = n.node.cloneNode()
	} else {
		#cloned.node = n.node.cloneNode(deep.arr.get(new int(0)).val)
	}
	return cloned
}

pub fn (n Node) contains(other Node) bool {
	res := false
	#res.val = n.node.contains(other.node)

	return res
}

pub fn (n Node) get_root_node() Node {
	root := Node{}
	#root.node = n.node.getRootNode()

	return root
}

pub fn (n Node) has_child_nodes() bool {
	res := false
	#res.val = n.node.hasChildNodes()

	return res
}

pub fn (n Node) insert_before(new_node Node, reference_node Node) Node {
	inserted := Node{}
	#inserted.node = n.node.insertBefore(new_node.node,reference_node.node)

	return inserted
}

pub fn (x Node) == (y Node) bool {
	res := false
	#res.val = x.node.isEqualNode(y.node)

	return res
}

pub fn (n Node) remove_child(child Node) {
	#n.node.removeChild(child.node)
}

pub fn (n Node) replace_child(new_child Node, old_child Node) Node {
	#old_child.node = n.node.replace_child(new_child.node,old_child.node)

	return old_child
}
