## Description

`kdl` implements a parser and serializer for [KDL 2.0](https://x.kdl.dev/spec/), a
node-oriented document language. KDL occupies a similar niche to JSON, YAML,
TOML, and XML, but uses a syntax that looks like CLI command invocations:

```kdl
package {
  name my-pkg
  version "2.0.0"
  dependencies {
    lodash "^3.2.1" optional=#true
  }
}
```

## API

### Parsing

```v
import x.kdl

// Basic parsing
doc := x.kdl.parse('name "Alice" age 30 active #true')!
println(doc.nodes[0].name) // "name"

// With options
opts := x.kdl.ParseOpts{
	parse_comments: true
}
doc2 := x.kdl.parse_opts('// greeting\nhello "world"', opts)!

// From file
doc3 := x.kdl.parse_file('config.kdl')!
```

### Serialization

```v
import x.kdl

doc := x.kdl.parse('my-node 1 2 key="val"')!
out := x.kdl.format(doc)!
println(out)
```

### Marshal / Unmarshal

```v
import x.kdl

struct Person {
	name string
	age  int
}

// Encode struct to KDL
person := Person{
	name: 'Alice'
	age:  30
}
println(x.kdl.encode(person))

// Decode KDL into struct
p := x.kdl.decode[Person]('Person name=Alice age=30')!
println(p.name)
```

### Coercion & Property Helpers

```v
import x.kdl

doc := x.kdl.parse('sensor temp=22.5 name="Kitchen" online=true')!
node := doc.nodes[0]

// Property access
temp := x.kdl.property_get(&node, 'temp') or { panic('missing') }
println(x.kdl.as_f64(temp)) // 22.5
println(x.kdl.as_string(temp)) // "22.5"

name := x.kdl.property_get(&node, 'name')!
println(x.kdl.as_string(name)) // "Kitchen"

online := x.kdl.property_get(&node, 'online')!
println(x.kdl.as_bool(online)) // true
```

### Document Model

- `Document` — list of top-level `Node` values
- `Node` — a KDL node with `type_name`, `name`, `entries` (arguments/properties in order),
  `children`, `comment`
- `Entry` — sumtype of `Argument` (positional value with optional type annotation) or `Property`
  (key=value with optional type annotation)
- `Value` — sumtype of `StringVal`, `IntVal`, `FloatVal`, `BoolVal`, `NullVal`
- `SuffixedDecimal` — parsed suffixed number with `number` and `suffix` fields

### Options & Relaxed Mode

```v
import x.kdl

// Relaxed NGINX syntax (allows /, \, (,) in identifiers)
mut relaxed := x.kdl.RelaxedNonCompliant{
	flags: x.kdl.nginx_syntax
}
opts := x.kdl.ParseOpts{
	relaxed: relaxed
}
doc := x.kdl.parse_opts('allow from 192.168.1.1/24', opts)!

// Parse with comment capture
mut opts2 := x.kdl.ParseOpts{
	parse_comments: true
}
doc2 := x.kdl.parse_opts('// section header\nnode "val"', opts2)!
```

## Features

- KDL 2.0 parsing with full tokenizer
  - Identifier strings, quoted strings, raw strings (`#"..."#`, `##"..."##`),
    multiline strings (`"""..."""`)
  - Decimal, hexadecimal (`0x`), octal (`0o`), binary (`0b`) numbers with underscore separators
  - Scientific notation, signed numbers
  - Booleans (`#true`, `#false`), null (`#null`), keyword numbers (`#inf`, `#-inf`, `#nan`)
  - Suffixed decimals (`10ms`, `5KiB`)
  - Type annotations (`(u8)`, `(person)`)
- Children blocks, properties, arguments — any order
- `//`, `/* */` (nestable), `/-` slashdash comments
- Line continuations (`\`)
- BOM handling
- Unicode whitespace and newline support (U+00A0, U+1680, U+2000-U+200A, U+202F,
  U+205F, U+3000, U+0085, U+2028, U+2029, etc.)
- Spec-compliant bare keyword rejection (`true`, `false`, `null`, `inf`, `-inf`, `nan`
  must be `#`-prefixed)
- Configurable relaxed mode (NGINX syntax, YAML/TOML assignments, multiplier suffixes)
- Document ↔ KDL text serialization via `format()`
- File I/O via `parse_file()`
- Marshal/unmarshal with struct tags (`[kdl: 'arg']`, `[kdl: 'children']`,
  `[kdl: 'omitempty']`, etc.)
- Rename strategies for encoding (`snake_case`, `kebab-case`, `camelCase`, `PascalCase`,
  `screaming-snake`)
- Value coercion helpers (`as_string`, `as_int`, `as_i64`, `as_f64`, `as_bool`, `as_u64`,
  `as_numeric`, `is_null`)
- String helpers (`quote_string`, `unquote_string`, `raw_string`, `can_be_bare_identifier`)
- Property helpers (`property_exists`, `property_get`, `property_has`)