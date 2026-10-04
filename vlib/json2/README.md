> `json2` replaces the removed cJSON based `json` module. `v fmt -w file.v`
> rewrites the usual `json.decode(T, s)`, `json.encode(x)` and
> `json.encode_pretty(x)` calls to `json2`, and leaves code it cannot rewrite
> safely unchanged. By hand:
>
> - `json.decode(T, s)` becomes `json2.decode[T](s)`
> - `json.encode(x)` becomes `json2.encode(x, escape_unicode: true, time_as_unix: true)`
> - `json.encode_pretty(x)` becomes `json2.encode(x, prettify: true, legacy_layout: true,
>   escape_unicode: true, time_as_unix: true)`
>
> These options keep the output of the old module: non-ASCII characters escaped as
> `\uXXXX`, `time.Time` values as Unix timestamps, and its tab based pretty layout.
> One difference remains: a `@[raw]` field, or a string that receives a JSON object
> or array, holds the JSON text exactly as written, while the old module returned it
> without whitespace.
>
> `json.decode(?T, s)` has no direct counterpart, since V does not accept `?T` as a
> type argument, and vfmt leaves such files unchanged: decode `T` with
> `json2.decode[T](s)`, and handle a `null` input yourself.
>
> `json.encode(x)` wrote a sum type value narrowed by `x is Cat` or `match x` as the
> whole sum type, with its `_type` field, while `json2.encode(x)` writes the narrowed
> variant. vfmt leaves such files unchanged as well: cast the value back to its sum
> type, as in `json2.encode(Animal(x), escape_unicode: true, time_as_unix: true)`.
>
> A type with its own `to_json()` method (or the deprecated `json_str()`), such as
> `big.Integer`, is written by `json2.encode` through that method, while the old module
> ignored it and wrote the type's fields. vfmt migrates these calls too, since it does
> not resolve types, so check the output of such types after migrating. Decoding is not
> affected: the `from_json_*` methods only handle a JSON string, number, boolean or
> `null`, so objects written by the old module still decode field by field.
>
> `json2.decode` also accepts an enum value given as the number of one of its members,
> for an enum without `@[json_as_number]` too, as `json2.encode(x, enum_as_int: true)`
> writes it; the old module only accepted the member's name there. Other numbers are
> still rejected.

`json2` is an experimental JSON parser written from scratch on V.

## Usage

#### encode[T]

```v
import json2
import time

struct Person {
mut:
	name     string
	age      ?int = 20
	birthday time.Time
	deathday ?time.Time
}

fn main() {
	mut person := Person{
		name:     'Bob'
		birthday: time.now()
	}
	person_json := json2.encode[Person](person)
	// person_json == {"name": "Bob", "age": 20, "birthday": "2022-03-11T13:54:25.000Z"}
}
```

Enums encode as strings by default. Use `@[json_as_number]` on an enum to emit
its integer value instead.

Use `@[omitempty]` to omit empty struct fields. For boolean fields, including optional
booleans, `false` is empty and `true` is encoded. `@[omitempty]` only affects encoding:
`decode` still assigns an explicit empty value such as `0` or `""` from the input.

#### decode[T]

JSON object keys are decoded to the target map key type, including signed and unsigned
integer keys. Nested maps and maps stored in struct fields follow the same conversion.
Enum map keys, including enum type aliases, use member names as written by `encode`.
Member `@[json: ...]` attributes do not rename map keys. Unknown member names return
a decoding error.
Flag-enum keys, including aliases, also accept their encoded form, such as
`Permission{.read | .write}` or `Permission{}` for zero, so maps with flag keys
round-trip through JSON.

The target type keeps its declaring module. A program's own sum type named `Any`
is distinct from `json2.Any`, including through nested dynamic arrays, fixed arrays,
and maps such as `json2.decode[[][2]Any](text)`.

```v
import json2
import time

struct Person {
mut:
	name     string
	age      ?int = 20
	birthday time.Time
	deathday ?time.Time
}

fn main() {
	resp := '{"name": "Bob", "age": 20, "birthday": "${time.now()}"}'
	person := json2.decode[Person](resp)!
	// struct Person {
	//    mut:
	//        name "Bob"
	//        age  20
	//        birthday "2022-03-11 13:54:25"
	//       deathday "2022-03-11 13:54:25"
	// }
}
```

decode[T] is smart and can auto-convert the types of struct fields - this means
examples below will have the same result

Embedded struct fields are decoded from the surrounding object, including reference fields.

```v ignore
json2.decode[Person]('{"name": "Bob", "age": 20, "birthday": "2022-03-11T13:54:25.000Z"}')!
json2.decode[Person]('{"name": "Bob", "age": 20, "birthday": "2022-03-11 13:54:25.000"}')!
json2.decode[Person]('{"name": "Bob", "age": "20", "birthday": 1647006865}')!
json2.decode[Person]('{"name": "Bob", "age": "20", "birthday": "1647006865"}}')!
```

#### raw decode

```v
import json2
import net.http

fn main() {
	resp := http.get('https://reqres.in/api/products/1')!

	// This returns an Any type
	raw_product := json2.decode[json2.Any](resp.body)!
}
```

#### iterative token scanning

`json2` now exposes low-level scanners that let you process JSON token by
token instead of materializing the whole tree first.

Use `new_scanner()` for in-memory strings:

```v
import json2

fn main() {
	mut scanner := json2.new_scanner('{"items":[1,2,3]}')
	for {
		token := scanner.next()!
		if token.is_eof() {
			break
		}
		println('${token.kind}: ${token.literal()}')
	}
}
```

Use `new_reader_scanner()` to stream tokens from a file or any `io.Reader`:

```v
import os
import json2

fn main() {
	mut file := os.open('huge.json')!
	defer {
		file.close()
	}

	mut scanner := json2.new_reader_scanner(reader: file)
	defer {
		scanner.free()
	}

	for {
		token := scanner.next()!
		if token.is_eof() {
			break
		}
		if token.kind == .str && token.literal() == 'id' {
			println('found an id key')
		}
	}
}
```

#### Casting `Any` type / Navigating

```v
import json2
import net.http

fn main() {
	resp := http.get('https://reqres.in/api/products/1')!

	raw_product := json2.decode[json2.Any](resp.body)!

	product := raw_product.as_map()
	data := product['data'] as map[string]json2.Any

	id := data['id'].int() // 1
	name := data['name'].str() // cerulean
	year := data['year'].int() // 2000
}
```

#### Constructing an `Any` type

```v
import json2

fn main() {
	mut me := map[string]json2.Any{}
	me['name'] = 'Bob'
	me['age'] = 18

	mut arr := []json2.Any{}
	arr << 'rock'
	arr << 'papers'
	arr << json2.null
	arr << 12

	me['interests'] = arr

	mut pets := map[string]json2.Any{}
	pets['Sam'] = 'Maltese Shitzu'
	me['pets'] = pets

	// Stringify to JSON
	println(me.str())
	//{
	//   "name":"Bob",
	//   "age":18,
	//   "interests":["rock","papers","scissors",null,12],
	//   "pets":{"Sam":"Maltese"}
	//}
}
```

### Null Values

`json2` has a separate `Null` type for differentiating an undefined value and a null value.
To verify that the field you're accessing is a `Null`, use `[typ] is json2.Null`.

```v ignore
fn (mut p Person) from_json(f json2.Any) {
    obj := f.as_map()
    if obj['age'] is json2.Null {
        // use a default value
        p.age = 10
    }
}
```

## Casting a value to an incompatible type

`json2` provides methods for turning `Any` types into usable types.
The following list shows the possible outputs when casting a value to an incompatible type.

1. Casting non-array values with `as_array()` will return an array with the value as the content.
2. Casting non-map values as map (`as_map()`) will return a map with the value as the content.
3. Casting non-string values to string (`str()`) will return the
   JSON string representation of the value.
4. Casting non-numeric values to int/float (`int()`/`i64()`/`f32()`/`f64()`) will return zero.

## Encoding using string builder instead of []u8

To be more performant, `json2`, in PR 20052, decided to use buffers directly instead of Writers.
If you want to use Writers you can follow the steps below:

```v ignore
mut sb := strings.new_builder(64)
mut buffer := []u8{}

json2.encode_value(<some value to be encoded here>, mut buffer)!

sb.write(buffer)!

unsafe { buffer.free() }
```
