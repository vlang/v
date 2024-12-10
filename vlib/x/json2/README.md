> The name `json2` was chosen to avoid any unwanted potential conflicts with the
> existing codegen tailored for the main `json` module which is powered by CJSON.

`x.json2` is an experimental JSON parser written from scratch on V.

## Usage

#### encode[T]

```v
import x.json2
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

#### decode[T]

```v
import x.json2
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
	/*
	struct Person {
      mut:
          name "Bob"
          age  20
          birthday "2022-03-11 13:54:25"
      }
	*/
}
```

decode[T] is smart and can auto-convert the types of struct fields - this means
examples below will have the same result

```v ignore
json2.decode[Person]('{"name": "Bob", "age": 20, "birthday": "2022-03-11T13:54:25.000Z"}')!
json2.decode[Person]('{"name": "Bob", "age": 20, "birthday": "2022-03-11 13:54:25.000"}')!
json2.decode[Person]('{"name": "Bob", "age": "20", "birthday": 1647006865}')!
json2.decode[Person]('{"name": "Bob", "age": "20", "birthday": "1647006865"}}')!
```

#### raw decode

```v
import x.json2
import net.http

fn main() {
	resp := http.get('https://reqres.in/api/products/1')!

	// This returns an Any type
	raw_product := json2.decode[json2.Any](resp.body)!
}
```

#### Casting `Any` type / Navigating

```v
import x.json2
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
import x.json2

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

`x.json2` has a separate `Null` type for differentiating an undefined value and a null value.
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

`x.json2` provides methods for turning `Any` types into usable types.
The following list shows the possible outputs when casting a value to an incompatible type.

1. Casting non-array values as array (`arr()`) will return an array with the value as the content.
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
