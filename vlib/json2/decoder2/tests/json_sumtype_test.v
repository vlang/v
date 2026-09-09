import json2.decoder2 as json
import json2
import json2.decoder2.tests.models

type Prices = Price | []Price

pub struct ShopResponseData {
	attributes Attributes
}

struct Attributes {
	price ?Prices
}

struct Price {
	net f64
}

pub type Animal = Cat | Dog

pub struct Cat {
	cat_name string
}

pub struct Dog {
	dog_name string
}

type Sum = int | string | bool | []string

fn test_simple_sum_type() {
	assert json.decode[Sum]('1')! == Sum(1)

	assert json.decode[Sum]('"hello"')! == Sum('hello')

	assert json.decode[Sum]('true')! == Sum(true)
	assert json.decode[Sum]('false')! == Sum(false)

	assert json.decode[Sum]('["1", "2", "3"]')! == Sum(['1', '2', '3'])
}

fn test_any_sum_type() {
	assert json.decode[json2.Any]('null')! == json2.Any(json2.null)

	assert json.decode[json2.Any]('1')! == json2.Any(f64(1))
	assert json.decode[json2.Any]('123321')! == json2.Any(f64(123321))

	assert json.decode[json2.Any]('"hello"')! == json2.Any('hello')

	assert json.decode[json2.Any]('true')! == json2.Any(true)
	assert json.decode[json2.Any]('false')! == json2.Any(false)

	assert json.decode[json2.Any]('1.1')! == json2.Any(f64(1.1))

	// Uncomment this when #22693 is fixed
	// assert json.decode[[]json2.Any]('["1", "2", "3"]')! == [json2.Any('1'), json2.Any('2'), json2.Any('3')]
	// assert json.decode[json2.Any]('["1", "2", "3"]')! == json2.Any([json2.Any('1'), json2.Any('2'),
	// 	json2.Any('3')])

	// assert json.decode[[]json2.Any]('[true, false, true]')! == [json2.Any(true), json2.Any(false),
	// 	json2.Any(true)]
	// assert json.decode[json2.Any]('[true, false, true]')! == json2.Any([json2.Any(true), json2.Any(false),
	// 	json2.Any(true)])

	assert json.decode[json2.Any]('{"hello": "world"}')! == json2.Any({
		'hello': json2.Any('world')
	})

	assert json.decode[map[string]json2.Any]('{"hello": "world"}')! == {
		'hello': json2.Any('world')
	}
	assert json.decode[map[string]json2.Any]('{"value": null}')! == {
		'value': json2.Any(json2.null)
	}

	// assert json.decode[json2.Any]('{"hello1": {"hello2": "world"}}')! == json2.Any({
	// 	'hello1': json2.Any({
	// 		'hello2': json2.Any('world')
	// 	})
	// })
}

fn test_sum_type_struct() {
	assert json.decode[Animal]('{"cat_name": "Tom"}')! == Animal(Cat{'Tom'})
	assert json.decode[Animal]('{"dog_name": "Rex"}')! == Animal(Cat{''})
	assert json.decode[Animal]('{"dog_name": "Rex", "_type": "Dog"}')! == Animal(Dog{'Rex'})
}

fn test_unknown_sum_type_struct_tag_returns_error() {
	mut failed := false
	json.decode[Animal]('{"_type":"Bird","dog_name":"Rex"}') or {
		failed = true
		assert err.msg().contains('Bird')
	}
	assert failed
}

fn test_imported_sum_type_struct_uses_short_variant_name() {
	dog := models.Animal(models.Dog{
		dog_name: 'Rex'
	})
	encoded := json2.encode(dog)
	assert encoded == '{"dog_name":"Rex","_type":"Dog"}'
	assert json.decode[models.Animal](encoded)! == dog
}
