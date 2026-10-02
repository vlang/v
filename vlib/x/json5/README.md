# v-json5

JSON5 parsing, decoding and encoding for V.

JSON5 is a superset of JSON: everything that is valid JSON is valid JSON5, and
JSON5 additionally allows comments, unquoted object keys, single-quoted
strings, trailing commas, hexadecimal and leading- or trailing-dot numbers, a
leading `+` on numbers, and `Infinity`/`NaN`.

## Contents
- Install
- Parse
- Decode into V types
- Document access
- Encode
- Encode options
- Custom encoders
- Supported JSON5 syntax
- Errors
- Examples

## Install
```sh
v install x.json5
```

## Parse
`parse` returns the dynamic `Any` tree, and `parse_text` returns a `Doc` that
also supports path lookup.
```v ignore
import x.json5

fn main() {
	value := json5.parse('{ name: "V", tags: [1, 2], }') or { panic(err) }
	println(value.as_map()['name'] or { json5.null }.string())
	println(value.as_map()['tags'] or { json5.null }.array().len)
}
```

## Decode into V types
`decode[T]` converts a JSON5 document into `T`.
```v ignore
import x.json5

struct Config {
	name  string
	port  int = 8080
	ratio f64
}

fn main() {
	cfg := json5.decode[Config]('{ name: "srv", ratio: .5 }') or { panic(err) }
	println(cfg.name)  // srv
	println(cfg.port)  // 8080, the default is kept for a missing key
}
```

Supported targets:

- structs, including embedded structs, which are flattened into the parent;
- enums, by name or by numeric value;
- `map[string]T`, `[]T`, `[N]T`, `?T` fields, and all integer and float widths;
- `Any` itself, for a partially typed document.

A `null` value leaves a field at its default. A missing key also keeps the
default, so a partially filled document decodes without error.

### Field attributes
- `@[skip]` ignores the field in both directions;
- `@[json5: 'name']` renames the field, and works on enum values too.

### Hooks
A type can take over its own decoding. The hooks are tried in order and the
first one defined wins:

- `fn (mut t T) from_json5(value Any)`;
- `fn (mut t T) from_json5_string(raw string) !`;
- `fn (mut t T) from_json5_number(raw string) !`;
- `fn (mut t T) from_json5_boolean(raw bool) !`;
- `fn (mut t T) from_json5_null()`.

The string and number hooks receive the original literal source text, which
means a hexadecimal literal such as `0x10` reaches the hook intact.

## Document access
`Doc` keeps the parsed tree together with a small path query language.
```v ignore
import x.json5

fn main() {
	doc := json5.parse_text('{ servers: [{ host: "a" }, { host: "b" }] }') or {
		panic(err)
	}
	host := doc.get('servers[1].host') or { panic(err) }
	println(host.string())  // b

	// reflect fills what it can and leaves the rest at the defaults.
	println(doc.reflect[Config]().name)
	// decode reports a type error instead.
	println(doc.decode[Config]() or { panic(err) }.name)
}
```

## Encode
`encode[T]` writes compact output that is also valid JSON. `encode_any` writes
an `Any` tree.
```v ignore
import x.json5

struct Point {
	x int
	y int
}

fn main() {
	println(json5.encode(Point{x: 1, y: 2}))
	println(json5.encode(json5.parse('{ a: 1 }') or { panic(err) }))
}
```

## Encode options
`encode_with_opts[T]` takes an `EncodeOpts` value.
```v ignore
import x.json5

fn main() {
	opts := json5.EncodeOpts{
		indent:          '  '
		single_quotes:   true
		unquoted_keys:   true
		trailing_commas: true
	}
	println(json5.encode_with_opts(json5.parse('{ a: [1, 2] }') or { panic(err) },
		opts))
}
```

- `indent`: the indent unit; the zero value writes compact output;
- `single_quotes`: write strings with `'` instead of `"`;
- `unquoted_keys`: write an object key bare when it is a valid identifier;
- `trailing_commas`: write a comma after the last array element and member.

## Custom encoders
A type can control its own output with `to_json5() string`. The returned text
is written verbatim, which is how a type controls quoting or emits a value
that has no V equivalent.
```v ignore
import x.json5

struct Money {
	cents int
}

fn (m Money) to_json5() string {
	return '\$${m.cents / 100}.${m.cents % 100:02}'
}

fn main() {
	println(json5.encode(Money{cents: 1999}))  // $19.99
}
```

## Supported JSON5 syntax
- `//` line comments and `/* */` block comments;
- unquoted object keys, restricted to identifiers and reserved words;
- single-quoted and double-quoted strings;
- trailing commas in objects and arrays;
- `0x` hexadecimal integers, leading-dot and trailing-dot numbers, a leading
  `+` on numbers, and `Infinity`, `-Infinity` and `NaN`;
- a leading byte order mark;
- escaped line continuations inside strings;
- JSON5 escape sequences in strings, including `\'`, `\"`, `\v`, `\0`, `\xNN`
  and `\uNNNN`.

Hexadecimal numbers and escape sequences accept both uppercase `A-F` and lowercase `a-f`.
Escaped UTF-16 surrogate pairs decode to a single Unicode character in strings and quoted keys.
Unpaired surrogate escapes report a parse error instead of producing invalid UTF-8.
When encoding optional values, present values keep their payload and `none` becomes `null`.

## Errors
Parsing, decoding and encoding report typed errors that carry a source
position:

- `ParseError`, for malformed input;
- `TypeError`, for a value that does not fit the target type, including an
  integer that does not fit its width;
- `NameError`, for a string that matches no enum value;
- `EnumError`, for an out-of-range numeric enum value.

## Examples
See the [examples](examples/) directory for complete runnable examples.
