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
	encoded_json := json2.encode<Person>(person2)
}
```
## Using `decode<T>` and `encode<T>`
> Codegen for this feature is still WIP.
> You need to manually define the methods before using the module to structs.

In order to use the `decode<T>` and `encode<T>` function, you need to explicitly define
two methods: `from_json` and `to_json`. `from_json` accepts a `json2.Any` argument
and inside of it you need to map the fields you're going to put into the type.
As for `to_json` method, you just need to map the values into `json2.Any`
and turn it into a string.

```v ignore
struct Person {
mut:
    name string
    age  int = 20
    pets []string
}

fn (mut p Person) from_json(f json2.Any) {
    obj := f.as_map()
    for k, v in obj {
        match k {
            'name' { p.name = v.str() }
            'age' { p.age = v.int() }
            'pets' { p.pets = v.arr().map(it.str()) }
            else {}
        }
    }
}

fn (p Person) to_json() string {
    mut obj := map[string]json2.Any
    obj['name'] = p.name
    obj['age'] = p.age
    obj['pets'] = p.pets
    return obj.str()
}

fn main() {
    resp := os.read_file('./person.json')!
    person := json2.decode<Person>(resp)!
    println(person) // Person{name: 'Bob', age: 28, pets: ['Floof']}
    person_json := json2.encode<Person>(person)
    println(person_json) // {"name": "Bob", "age": 28, "pets": ["Floof"]}
}
```

## Using struct tags
`x.json2` can access and use the struct field tags similar to the
`json` module by using the comp-time `$for` for structs.

```v ignore
fn (mut p Person) from_json(f json2.Any) {
    mp := an.as_map()
	mut js_field_name := ''
    $for field in Person.fields {
        js_field_name = field.name

        for attr in field.attrs {
			if attr.starts_with('json:') {
				js_field_name = attr.all_after('json:').trim_left(' ')
				break
			}
		}

        match field.name {
            'name' { p.name = mp[js_field_name].str() }
			'age' { u.age = mp[js_field_name].int() }
			'pets' { u.pets = mp[js_field_name].arr().map(it.str()) }
			else {}
		}
    }
}
```

### Null Values
`x.json2` has a separate `null` type for differentiating an undefined value and a null value.
To verify that the field you're accessing is a `null`, use `<typ> is json2.Null`.

```v ignore
fn (mut p Person) from_json(f json2.Any) {
    obj := f.as_map()
    if obj['age'] is json2.Null {
        // use a default value
        p.age = 10
    }
}
```

### Custom field names
Aside from using struct tags, you can also just simply cast the base field into a map (`as_map()`)
and access the field you wish to put into the struct/type.

```v ignore
fn (mut p Person) from_json(f json2.Any) {
    obj := f.as_map()
    p.name = obj['nickname'].str()
}
```

```v oksyntax
import x.json2

fn (p Person) to_json() string {
	mut obj := map[string]json2.Any{}
	obj['nickname'] = p.name
	return obj.str()
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
