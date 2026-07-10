module document

// Document is a parsed KDL document: a list of top-level nodes.
pub struct Document {
pub mut:
	nodes []Node
}

// Comment holds before/after comments for a node.
pub struct Comment {
pub mut:
	before string
	after  string
}

// Node is a single KDL node with optional type annotation, entries, children, and comment.
pub struct Node {
pub mut:
	type_name string
	name      string
	entries   []Entry
	children  []Node
	comment   ?Comment
}

// Entry is either an Argument or a Property.
pub type Entry = Argument | Property

// Argument is a positional value in a KDL node.
pub struct Argument {
pub:
	type_name string
	value     Value
}

// Property is a key=value pair in a KDL node.
pub struct Property {
pub:
	type_name string
	key       string
	value     Value
}
