## Description

The `json` module provides encoding/decoding of V data structures to/from JSON.
For more details, see also the
[JSON section of the V documentation](https://github.com/vlang/v/blob/master/doc/docs.md#json)

Large `i64` and `u64` values are encoded as exact decimal JSON numbers, and
`json.decode` preserves those integer values when reading decimal integer input.

## Examples

Here is an example of encoding and decoding a V struct with several fields.
Note that you can specify different names in the json encoding for the fields,
and that you can skip fields too, if needed.

```v
import json

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
	s := json.encode(x)
	println('JSON encoding of employee x: ${s}')
	assert s == '{"name":"Peter","age":28,"salary":95000.5,"ETitle":"worker"}'
	mut y := json.decode(Employee, s)!
	assert y != x
	assert y.family == ''
	y.family = 'Begins'
	assert y == x
	println(y)
	ss := json.encode(y)
	println('JSON encoding of employee y: ${ss}')
	assert ss == s
}
```

### JSON field attributes

The `json` module supports a few field attributes for controlling how struct fields map to JSON:

- `@[json: 'name']` uses a different JSON key for the field.
- `@[json: '-']` skips the field.
- `@[omitempty]` omits the field while encoding if it still has its zero/default value.
- `@[raw]` decodes the field as raw JSON text instead of decoding it into another V value.
  This is useful when you want to keep a nested JSON fragment in a `string` or `?string`
  field.

```v
import json

struct Message {
	payload string  @[raw]
	note    ?string @[omitempty]
}

fn main() {
	msg := json.decode(Message, '{"payload":{"kind":"ping"},"note":""}')!
	assert msg.payload == '{"kind":"ping"}'

	out := json.encode(Message{
		payload: '{"kind":"ping"}'
	})
	assert out == '{"payload":"{\\"kind\\":\\"ping\\"}"}'
}
```

In the example above, `payload` keeps the original JSON fragment during decoding,
while `note` is omitted during encoding when it is `none`.
