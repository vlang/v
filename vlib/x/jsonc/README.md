# v-jsonc

JSONC parsing, validation and decoding for V.

JSONC is RFC 8259 plus `//` and `/* */` comments. It is the format that
`tsconfig.json`, VS Code's `settings.json` and many editor and tool
configurations are written in. Plain JSON parsers reject those files because of
the comments, and a JSON5 parser accepts them but also accepts a good deal that
the format does not allow, so a typo in a config file passes unnoticed.

This module reads JSONC strictly: comments are allowed and the rest of the JSON5
syntax is refused, with the position of the problem.

## Contents
- Install
- Parse
- Validate
- Decode into V types
- Positions
- Trailing commas
- Stripping comments
- What is accepted and what is refused
- Errors
- Examples

## Install
```sh
v install x.jsonc
```

## Parse
`parse` returns the dynamic `Any` tree, and `parse_text` returns a `Doc` that
also supports path lookup.

```v
import x.jsonc

fn main() {
	text := '{
	// the port to bind
	"host": "0.0.0.0",
	"port": 8080
}'
	value := jsonc.parse(text) or { panic(err) }
	println(value.as_map()['port'] or { panic('no port') }.int())
}
```

`parse_file` reads the document from disk.

## Validate
`is_valid` answers whether a document is JSONC, and `violation` returns the first
reason it is not.

```v
import x.jsonc

fn main() {
	println(jsonc.is_valid('{"a": 1 /* fine */}'))
	println(jsonc.is_valid('{a: 1}'))
	if v := jsonc.violation('{a: 1}') {
		println(v.msg())
	}
}
```

## Decode into V types
`decode` reads a document into a struct. The rules are the ones
[`x.json5`](https://github.com/vlang/v/tree/master/vlib/x/json5) uses, because
the same decoder does the work: structs with embedded structs flattened into the
parent, enums by name or by value, `map[string]T`, arrays, options, and every
scalar width.

A field is matched by name, or by an `@[json5: 'name']` attribute when the
document spells it differently, which is the usual case for a camelCase config
key. A missing key leaves the field at its default, and an explicit `null` clears
an option field.

```v
import x.jsonc

struct CompilerOptions {
	out_dir string @[json5: 'outDir']
	strict  bool
}

struct TsConfig {
	compiler_opts CompilerOptions @[json5: 'compilerOptions']
	include       []string
}

fn main() {
	cfg := jsonc.decode[TsConfig]('{
	// where the output goes
	"compilerOptions": { "outDir": "dist", "strict": true },
	"include": ["src"]
}') or { panic(err) }
	println(cfg.compiler_opts.out_dir) // dist
	println(cfg.include) // ['src']
}
```

`decode_file` reads the document from disk, and `decode_any` converts a tree
that was parsed already.

The dialect is checked before the decoder runs, so a document with a JSON5-only
construct is refused even when the target struct has a field for it.

## Positions
Every dialect violation carries a line, a column, and the byte range the
offending text occupies, so an editor can underline exactly it.

The byte range is reachable through `violation`, whose payload is a `ParseError`.
It is a separate entry point from `parse_text` on purpose: a function returning
`!T` cannot hand back a concrete error type, so the error from `parse_text`
arrives as an `IError` and only its message can be read.

```v
import x.jsonc

fn main() {
	if v := jsonc.violation('{\n  "a": 1,\n  b: 2\n}') {
		println('line ${v.pos.line}, column ${v.pos.col}')
		println('bytes ${v.pos.offset}..${v.pos.end_offset}')
	}
}
```

## Trailing commas
A comma before a closing `}` or `]` is refused by default, which is what the
JSONC parser that VS Code uses does. Configuration files that allow one are read
with the option set.

```v
import x.jsonc

fn main() {
	opts := jsonc.ParseOpts{
		allow_trailing_comma: true
	}
	doc := jsonc.parse_text_opts('{"a": [1, 2,],}', opts) or { panic(err) }
	println(doc.str()) // {"a":[1,2]}
}
```

The option relaxes that one rule and nothing else.

## Stripping comments
`strip_comments` replaces every comment with spaces. The replacement is byte for
byte and keeps the line terminators inside a comment, so every remaining
character keeps the offset it had in the input and the result still parses.

```v
import x.jsonc

fn main() {
	text := '{\n// a note\n"a": 1 /* another */\n}'
	println(jsonc.strip_comments(text) == text) // true, only the bytes changed
	println(jsonc.parse(text) or { panic(err) }.str())
}
```

Use it when the comments have to go before the text reaches something that only
speaks strict JSON. Note that `vlib/json2` rejects comments outright, so a
JSONC document has to be stripped before it is handed to it.

## What is accepted and what is refused
Accepted, because they are JSONC:

- `//` line comments and `/* */` block comments;
- a leading byte order mark;
- every escape in RFC 8259, including `\uXXXX`;
- a trailing comma, when `allow_trailing_comma` is set.

Refused, because they are JSON5 extensions rather than JSONC:

| Construct | Example |
| --- | --- |
| single-quoted string | `{'a': 1}` |
| unquoted key | `{a: 1}` |
| non-string key | `{1: 1}`, `{true: 1}` |
| trailing comma, by default | `[1, 2,]` |
| hexadecimal integer | `0x10` |
| leading dot | `.5` |
| trailing dot | `5.` |
| leading `+` | `+1` |
| `Infinity` and `NaN` | `Infinity` |
| `\'`, `\"`, `\v`, `\0`, `\xNN` | `"\v"` |
| ES6 `\u{...}` escape | `"\u{1F600}"` |
| escaped line break | `"a\<newline>b"` |
| unescaped control character | a raw newline inside a string |

## Errors
Two kinds of problem are reported, and they are told apart by their prefix.

A `ParseError`, prefixed `jsonc:`, is well formed JSON5 that is not valid JSONC.
It is the `ParseError` above, with a position.

Anything else is malformed input, and the error comes from the JSON5 parser
unchanged, prefixed `json5:`. That happens before the dialect is considered, so
a syntax error is never reported as a dialect problem.

Decoding errors are the JSON5 decoder's own, also prefixed `json5:`: they are
about the shape of a value rather than the dialect, and the decoder is shared
with `x.json5` rather than duplicated here.

## Examples
See the [examples](examples/) directory for a complete runnable program.

```sh
./v run vlib/x/jsonc/examples/tour.v
```