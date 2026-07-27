## Description

The `json` module is deprecated and will be removed soon. Use the pure V `json2` module instead.

The `json` module provides encoding/decoding of V data structures to/from JSON.
For more details, see also the
[JSON section of the V documentation](https://github.com/vlang/v/blob/master/doc/docs.md#json)

Large `i64` and `u64` values are encoded as exact decimal JSON numbers, and
`json.decode` preserves those integer values when reading decimal integer input.

Struct fields of type `time.Time` can be decoded from either a JSON number or
from a JSON string in ISO 8601, RFC 3339, or Unix-timestamp form.

## Examples

Here is an example of encoding and decoding a V struct with the recommended `json2` module.
You can specify different JSON names for fields and skip fields when needed.

```v
import json2

enum JobTitle {
	manager
	executive
	worker
}

struct Employee {
mut:
	name   string
	family string @[json: '-'] // this field will be skipped
	age    int
	salary f32
	title  JobTitle @[json: 'ETitle'] // the key for this field will be 'ETitle', not 'title'
	// the JSON property is omitted while the field keeps its zero/default value
	notes string @[omitempty]
}

fn main() {
	x := Employee{'Peter', 'Begins', 28, 95000.5, .worker, ''}
	println(x)
	s := json2.encode(x, escape_unicode: true)
	println('JSON encoding of employee x: ${s}')
	assert s == '{"name":"Peter","age":28,"salary":95000.5,"ETitle":"worker"}'
	mut y := json2.decode[Employee](s)!
	assert y != x
	assert y.family == ''
	y.family = 'Begins'
	assert y == x
	println(y)
	ss := json2.encode(y, escape_unicode: true)
	println('JSON encoding of employee y: ${ss}')
	assert ss == s
}
```

### JSON field attributes

Both JSON modules support attributes that control how struct fields map to JSON:

- `@[json: 'name']` uses a different JSON key for the field.
- `@[json: '-']` skips the field.
- `@[omitempty]` omits the field while encoding if it still has its zero/default value.
- `@[raw]` decodes the field as raw JSON text instead of decoding it into another V value.
  This is useful when you want to keep a nested JSON fragment in a `string` or `?string`
  field.

```v
import json2

struct Message {
	payload string  @[raw]
	note    ?string @[omitempty]
}

fn main() {
	msg := json2.decode[Message]('{"payload":{"kind":"ping"},"note":""}')!
	assert msg.payload == '{"kind":"ping"}'

	out := json2.encode(Message{
		payload: '{"kind":"ping"}'
	},
		escape_unicode: true
	)
	assert out == '{"payload":"{\\"kind\\":\\"ping\\"}"}'
}
```

In the example above, `payload` keeps the original JSON fragment during decoding,
while `note` is omitted during encoding when it is `none`.
