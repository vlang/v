## Description
`toml` is a fully fledged [TOML v1.0.0](https://toml.io/en/v1.0.0) compatible parser written in pure V.
The module is tested against the [official compliance tests](https://github.com/toml-lang/compliance).

## Usage

Parsing files or `string`s containing TOML is easy.

Simply import the `toml` module and do:
```v ignore
doc1 := toml.parse_text(<string content>) or { panic(err) }
doc2 := toml.parse_file(<file path>) or { panic(err) }
```

## Example

Here we parse the official [TOML example](https://github.com/toml-lang/toml/blob/3b11f6921da7b6f5db37af039aa021fee450c091/README.md#Example)
and print out some values.

```v
import toml

const toml_text = '# This is a TOML document.

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00-08:00 # First class dates

[database]
server = "192.168.1.1"
ports = [ 8000, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # Indentation (tabs and/or spaces) is allowed but not required
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"

[clients]
data = [ ["gamma", "delta"], [1, 2] ]

# Line breaks are OK when inside arrays
hosts = [
  "alpha",
  "omega"
]'

fn main() {
	doc := toml.parse_text(toml_text) or { panic(err) }
	title := doc.value('title').string()
	println('title: "$title"')
	ip := doc.value('servers.alpha.ip').string()
	println('Server IP: "$ip"')
}
```

## Value retrieval

The `toml` module supports easy retrieval of values from
TOML documents by using a small and simple query syntax
as argument to the `value()` function.

Keys in map entries are denoted by `.` and array entries
uses `[<int>]`. Quoted keys are also supported by using
the delimiters `"` or `'`.

`doc.value('table.array[0].a."b.c"')`

To query for a value that might not be in the document you
can use the `.default_to(...)` function to provide a
default value.

For cases where a default value might not be appropiate or
to check if a value exists you can use `doc.value_opt('query')?`
instead.

```v
import toml

const toml_text = '
val = true

[table]
array = [
	{ a = "A" },
	{ b = "B" }
]
'

doc := toml.parse_text(toml_text) or { panic(err) }

assert doc.value('val').bool() == true
assert doc.value('table.array[0].a').string() == 'A'

// Provides a default value
assert doc.value('non.existing').default_to(false).bool() == false

// Check if value exist
// doc.value_opt('should.exist') or { ... }
// or
if value := doc.value_opt('table.array[1].b') {
	assert value.string() == 'B'
}

// You can pass parts of the TOML document around
// and still use .value()/.value_opt() to get the values
arr := doc.value('table.array')
assert arr.value('[1].b').string() == 'B'
```

### Conversion

Any TOML value can be converted to a V equivalent type.

TOML values are represented as the `toml.Any` sum-type that
can be converted to a V type.

|        TOML value          | V conversion (via `toml.Any`) |
| -------------------------- | ----------------------------- |
| val = "Basic string"       |   .string()                   |
| val = 'Literal string'     |   .string()                   |
| val = true                 |     .bool()                   |
| val = 1979-05-27T07:32:00Z | .datetime() (toml.DateTime)   |
| val = 1979-05-27           |     .date() (toml.Date)       |
| val = 07:32:59             |     .time() (toml.Time)       |
| val = 3.14                 |      .f32() / .f64()          |
| val = 100                  |      .int() / .i64() / .u64() |

Read more about values in the [TOML specification](https://toml.io/en/v1.0.0#spec).

## TOML to JSON

The `toml.to` module supports easy serialization of any TOML to JSON.

```v
import toml
import toml.to

const toml_text = '
val = true
[table]
array = [
	{ a = "A" },
	{ b = "B" }
]
'

doc := toml.parse_text(toml_text) or { panic(err) }
assert to.json(doc) == '{ "val": true, "table": { "array": [ { "a": "A" }, { "b": "B" } ] } }'
```
