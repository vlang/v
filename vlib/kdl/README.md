## Description

`kdl` is a parser and writer for [KDL 2.0](https://kdl.dev), a small
node-oriented document language often used for configuration files.
It follows the KDL 2.0.0 specification and passes the complete official
kdl-org test suite: every valid document parses to the expected result and
every invalid one is rejected with a `kdl.ParseError` that carries the line
and column.

A KDL document is a list of nodes. A node has a name, ordered arguments,
named properties and optional children:

```kdl
// comments start with //
server "main" port=8080 debug=#false {
    tls #true
    route "/api" timeout=2.5
    route "/static"
}
```

## Usage

```v
import kdl

const config_text = '
server "main" port=8080 debug=#false {
    tls #true
    route "/api" timeout=2.5
    route "/static"
}
'

fn main() {
	doc := kdl.parse(config_text)!

	server := doc.get('server') or { panic('no server node') }
	name := server.arg(0).as_string() or { 'unnamed' } // 'main'
	port := server.prop('port').as_int() or { 80 } // 8080
	tls_node := server.child('tls') or { kdl.Node{} }
	tls := tls_node.arg(0).as_bool() or { false } // true
	println('${name} listens on ${port} (tls: ${tls})')

	for route in server.children_named('route') {
		path := route.arg(0).as_string() or { continue }
		timeout := route.prop('timeout').as_f64() or { 30.0 }
		println('route ${path} timeout ${timeout}')
	}
}
```

`kdl.parse_file(path)!` reads a file. Syntax errors are `kdl.ParseError`
values whose `msg()` reads `line:col: message`; I/O errors are ordinary
errors.

## Data model

```v ignore
pub struct Document {
pub mut:
	nodes []Node
}

pub struct Node {
pub mut:
	ty         ?string          // type annotation, none when absent
	name       string
	arguments  []Value
	properties map[string]Value // last assignment wins
	children   []Node
}

pub struct Value {
pub mut:
	ty   ?string
	data Data = Null{}
}

pub type Data = string | i64 | f64 | bool | Null | BigInt
```

The mapping from KDL syntax to `Data` is:

| KDL | `Data` |
|---|---|
| `"text"`, `#"raw"#`, `bare-word`, `"""` multi-line `"""` | `string` |
| `42`, `-7`, `0xFF`, `0o17`, `0b101`, `1_000` | `i64` |
| integers outside the `i64` range | `BigInt` (sign flag plus decimal digits) |
| `2.5`, `1e10`, `#inf`, `#-inf`, `#nan` | `f64` |
| `#true`, `#false` | `bool` |
| `#null` | `Null` |

Type annotations such as `(u8)200` or `(date)"2024-01-01"` are kept in the
`ty` field of the value or node and are not interpreted.

`Document.get(name)` and `Node.child(name)` return the first node with that
name, or `none`. `Node.arg(i)` and `Node.prop(name)` return a value, or a
`#null` value when there is no such argument or property. The accessors
`as_string`, `as_int`, `as_f64`, `as_bool` return `none` when the value has
another type, so a configuration reader supplies a default with `or { ... }`
and never panics on a missing or mistyped entry. To handle every case
explicitly, match on `value.data`.

A missing entry and an explicit `#null` look the same through `arg` and
`prop`, and `or { default }` also swallows a value of the wrong type. When a
setting is mandatory, check its presence and convert with an error instead:

```v
import kdl

fn port_of(server kdl.Node) !i64 {
	if 'port' !in server.properties {
		return error('server: missing `port`')
	}
	return server.prop('port').as_int() or { return error('server: `port` must be an integer') }
}

fn main() {
	doc := kdl.parse('server port="8080"')!
	port := port_of(doc.get('server') or { panic('no server node') }) or {
		println(err.msg()) // server: `port` must be an integer
		return
	}
	println(port)
}
```

Matching on `value.data` covers every case:

```v
import kdl

fn describe(v kdl.Value) string {
	return match v.data {
		string { 'string ${v.data}' }
		i64 { 'integer ${v.data}' }
		f64 { 'float ${v.data}' }
		bool { 'boolean ${v.data}' }
		kdl.Null { 'null' }
		kdl.BigInt { 'big integer ${v.data}' }
	}
}

fn main() {
	doc := kdl.parse('node 1 2.5 "three" #true #null 99999999999999999999')!
	for arg in doc.nodes[0].arguments {
		println(describe(arg))
	}
}
```

## Writing documents

`Document.str()` (also used by `println(doc)`) writes canonical KDL 2.0: one
node per line, four-space indentation, properties sorted by name, strings
written bare when they are valid identifiers and quoted otherwise. Documents
can be built by hand:

```v
import kdl

fn main() {
	mut doc := kdl.Document{}
	mut node := kdl.Node{
		name: 'user'
	}
	node.arguments << kdl.Value{
		data: 'alice'
	}
	node.properties['admin'] = kdl.Value{
		data: true
	}
	node.children << kdl.Node{
		name:      'email'
		arguments: [kdl.Value{
			data: 'alice@example.com'
		}]
	}
	doc.nodes << node
	println(doc)
	// user alice admin=#true {
	//     email alice@example.com
	// }
}
```

Anything produced by the parser round-trips: `kdl.parse(doc.str())` gives a
document equal to `doc` (compare with `Document.equals`, which also treats two
`#nan` values as equal).

## Limits

- Floats are stored as `f64`, so a literal outside its range becomes `#inf` or
  `0.0`, and the original notation (`1.0E+10` versus `1e10`) is not preserved.
- Comments are parsed and discarded, as the specification defines them as
  syntax without a value: a document holds nodes and values only, so
  `Document.str()` cannot write them back and always emits its own canonical
  layout. Not keeping comments, whitespace and the original spelling of
  numbers and strings is also what keeps the parser small and fast.
