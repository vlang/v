// Module kdl reads and writes documents in the KDL 2.0 document language
// (https://kdl.dev), a node-oriented format suited to configuration files.
//
// A document is a list of nodes. Each node has a name, ordered arguments,
// named properties and optional child nodes:
//
// ```kdl
// server "main" port=8080 {
//     tls #true
// }
// ```
//
// `parse` and `parse_file` build a `Document`; `Document.str` writes one back.
// The parser accepts the whole official kdl-org test suite and rejects every
// invalid document of that suite.
module kdl

// Document is the root of a parsed KDL file: an ordered list of nodes.
pub struct Document {
pub mut:
	nodes []Node
}

// Node is a KDL node: an optional type annotation, a name, ordered arguments,
// properties (last assignment wins) and child nodes.
pub struct Node {
pub mut:
	ty         ?string // type annotation, `none` when absent (`("")` gives `''`)
	name       string
	arguments  []Value
	properties map[string]Value
	children   []Node
}

// Value is a KDL value with its optional type annotation. A zero Value is `#null`.
pub struct Value {
pub mut:
	ty   ?string
	data Data = Null{}
}

// Data is the payload of a Value.
// Integers that fit in 64 bits are `i64`; larger ones are kept exactly as BigInt.
pub type Data = BigInt | Null | bool | f64 | i64 | string

// Null is the `#null` value.
pub struct Null {}

// BigInt holds an integer outside the i64 range as decimal digits.
// The parser only produces it for magnitudes that do not fit in i64; `digits`
// holds decimal digits without sign or leading zeros. Values built by hand
// must respect these invariants for `equals` and round trips to behave.
pub struct BigInt {
pub:
	negative bool
	digits   string
}

// ParseError is returned by parse() and parse_file(). Positions are 1-based.
pub struct ParseError {
	Error
pub:
	line    int
	col     int
	offset  int
	message string
}

// msg formats the error as `line:col: message`.
pub fn (e ParseError) msg() string {
	return '${e.line}:${e.col}: ${e.message}'
}

// as_string returns the string payload, or none.
pub fn (v Value) as_string() ?string {
	if v.data is string {
		return v.data
	}
	return none
}

// as_int returns the integer payload when it fits in i64, or none.
pub fn (v Value) as_int() ?i64 {
	if v.data is i64 {
		return v.data
	}
	return none
}

// as_f64 returns the numeric payload as f64 (integers are converted), or none.
pub fn (v Value) as_f64() ?f64 {
	match v.data {
		f64 { return v.data }
		i64 { return f64(v.data) }
		BigInt { return v.data.f64() }
		else { return none }
	}
}

// as_bool returns the boolean payload, or none.
pub fn (v Value) as_bool() ?bool {
	if v.data is bool {
		return v.data
	}
	return none
}

// is_null reports whether the value is `#null`.
pub fn (v Value) is_null() bool {
	return v.data is Null
}

// f64 converts a BigInt to the nearest f64.
pub fn (b BigInt) f64() f64 {
	f := b.digits.f64()
	return if b.negative { -f } else { f }
}

// str returns the decimal representation with its sign.
pub fn (b BigInt) str() string {
	return if b.negative { '-' + b.digits } else { b.digits }
}

// arg returns the i-th argument. When there is no such argument it returns a
// `#null` value, so `node.arg(0).as_int() or { 0 }` reads with a default.
pub fn (n Node) arg(i int) Value {
	if i < 0 || i >= n.arguments.len {
		return Value{}
	}
	return n.arguments[i]
}

// prop returns the property named `name`, or a `#null` value when absent, so
// `node.prop('port').as_int() or { 80 }` reads with a default.
pub fn (n Node) prop(name string) Value {
	return n.properties[name] or { return Value{} }
}

// child returns the first child named `name`, or none.
pub fn (n Node) child(name string) ?Node {
	for c in n.children {
		if c.name == name {
			return c
		}
	}
	return none
}

// children_named returns every direct child named `name`.
pub fn (n Node) children_named(name string) []Node {
	return n.children.filter(it.name == name)
}

// get returns the first top-level node named `name`, or none.
pub fn (d Document) get(name string) ?Node {
	for n in d.nodes {
		if n.name == name {
			return n
		}
	}
	return none
}

// nodes_named returns every top-level node named `name`.
pub fn (d Document) nodes_named(name string) []Node {
	return d.nodes.filter(it.name == name)
}

// equals reports deep structural equality of two documents.
pub fn (a Document) equals(b Document) bool {
	return nodes_equal(a.nodes, b.nodes)
}

// equals reports deep structural equality of two nodes (children included).
pub fn (a Node) equals(b Node) bool {
	if !opt_eq(a.ty, b.ty) || a.name != b.name || a.arguments.len != b.arguments.len
		|| a.properties.len != b.properties.len {
		return false
	}
	for i in 0 .. a.arguments.len {
		if !a.arguments[i].equals(b.arguments[i]) {
			return false
		}
	}
	for k, v in a.properties {
		other := b.properties[k] or { return false }
		if !v.equals(other) {
			return false
		}
	}
	return nodes_equal(a.children, b.children)
}

// equals compares two values; unlike `==`, two NaN floats are considered equal.
pub fn (a Value) equals(b Value) bool {
	if !opt_eq(a.ty, b.ty) {
		return false
	}
	if a.data is f64 && b.data is f64 {
		x := a.data as f64
		y := b.data as f64
		return x == y || (x != x && y != y)
	}
	return a.data == b.data
}

// opt_eq compares two optional type annotations.
fn opt_eq(a ?string, b ?string) bool {
	if x := a {
		if y := b {
			return x == y
		}
		return false
	}
	return b == none
}

fn nodes_equal(a []Node, b []Node) bool {
	if a.len != b.len {
		return false
	}
	for i in 0 .. a.len {
		if !a[i].equals(b[i]) {
			return false
		}
	}
	return true
}
