> The name `json2` was chosen to avoid any unwanted potential conflicts with the
> existing codegen tailored for the main `json` module which is powered by CJSON.

`x.json2` is an experimental JSON parser written from scratch on V.

## Usage
```v oksyntax
import x.json2
import net.http

fn main() {
	// Decoding
	resp := http.get('https://example.com')!

	// raw decode
	raw_person := json2.raw_decode(resp.body)!

	// Casting `Any` type / Navigating
	person := raw_person.as_map()
	name := person['name'].str() // Bob
	age := person['age'].int() // 19
	pi := person['pi'].f64() // 3.14....

	// Constructing an `Any` type
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

	// Encode a struct/type to JSON
	encoded_json := json2.encode[Person](person2)
}
```
## Using `decode[T]` and `encode[T]`
> Codegen for this feature is still WIP.
> You need to manually define the methods before using the module to structs.


```v
struct Person {
mut:
    name string
    age  int = 20
    pets []string
}

fn main() {
    resp := '{"name": "Bob", "age": 28, "pets": ["Floof"]}'
    person := json2.decode[Person](resp)!
    println(person) // Person{name: 'Bob', age: 28, pets: ['Floof']}
    person_json := json2.encode[Person](person)
    println(person_json) // {"name": "Bob", "age": 28, "pets": ["Floof"]}
}
```

### Null Values
`x.json2` has a separate `null` type for differentiating an undefined value and a null value.
To verify that the field you're accessing is a `null`, use `[typ] is json2.Null`.

```v ignore
fn (mut p Person) from_json(f json2.Any) {
    obj := f.as_map()
    if obj['age'] is json2.Null {
        // use a default value
        p.age = 10
    }
}
```

### Undefined Values
Getting undefined values has the same behavior as regular V types.
If you're casting a base field into `map[string]json2.Any` and fetch an undefined entry/value,
it simply returns empty. As for the `[]json2.Any`, it returns an index error.

## Casting a value to an incompatible type
`x.json2` provides methods for turning `Any` types into usable types.
The following list shows the possible outputs when casting a value to an incompatible type.

1. Casting non-array values as array (`arr()`) will return an array with the value as the content.
2. Casting non-map values as map (`as_map()`) will return a map with the value as the content.
3. Casting non-string values to string (`str()`) will return the
JSON string representation of the value.
4. Casting non-numeric values to int/float (`int()`/`i64()`/`f32()`/`f64()`) will return zero.
