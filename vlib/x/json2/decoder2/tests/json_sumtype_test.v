import x.json2.decoder2 as json
import x.json2

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

type StructSumTypes = Stru | Stru2

type Mixed = Cat | int | map[string]int

type Maybes = ?int | ?string

type MultiArray = []int
	| []bool
	| [][]string
	| []map[string]map[string][]int
	| map[string]json2.Any
	| string

type StructLists = Cat | []Cat | map[string]Dog

type SumAlias = Sum

pub struct Stru {
	val  int
	val2 string
	val3 Stru2
	val4 int
	val5 int
}

pub struct Stru2 {
	a     int
	steak string
}

type NewAny = int | string | bool | []NewAny | map[string]NewAny | ?int

fn test_simple_sum_type() {
	assert json.decode[Sum]('1')! == Sum(1)

	assert json.decode[Sum]('"hello"')! == Sum('hello')

	assert json.decode[Sum]('true')! == Sum(true)
	assert json.decode[Sum]('false')! == Sum(false)

	assert json.decode[Sum]('["1", "2", "3"]')! == Sum(['1', '2', '3'])
}

fn test_any_sum_type() {
	assert json.decode[json2.Any]('1')! == json2.Any(f64(1))
	assert json.decode[json2.Any]('123321')! == json2.Any(f64(123321))

	assert json.decode[json2.Any]('"hello"')! == json2.Any('hello')

	assert json.decode[json2.Any]('true')! == json2.Any(true)
	assert json.decode[json2.Any]('false')! == json2.Any(false)

	assert json.decode[json2.Any]('1.1')! == json2.Any(f64(1.1))

	assert json.decode[[]json2.Any]('["1", "2", "3"]')! == [json2.Any('1'), json2.Any('2'), json2.Any('3')]
	assert json.decode[json2.Any]('["1", "2", "3"]')! == json2.Any([json2.Any('1'), json2.Any('2'),
		json2.Any('3')])

	assert json.decode[[]json2.Any]('[true, false, true]')! == [json2.Any(true), json2.Any(false),
		json2.Any(true)]
	assert json.decode[json2.Any]('[true, false, true]')! == json2.Any([json2.Any(true), json2.Any(false),
		json2.Any(true)])

	assert json.decode[json2.Any]('{"hello": "world"}')! == json2.Any({
		'hello': json2.Any('world')
	})

	assert json.decode[map[string]json2.Any]('{"hello": "world"}')! == {
		'hello': json2.Any('world')
	}

	assert json.decode[json2.Any]('{"hello1": {"hello2": "world"}}')! == json2.Any({
		'hello1': json2.Any({
			'hello2': json2.Any('world')
		})
	})

	assert json.decode[NewAny]('{"name": null, "value": "hi"}')! == NewAny({
		'name':  NewAny(?int(none))
		'value': NewAny('hi')
	})

	assert json.decode[json2.Any]('[]')! == json2.Any([]json2.Any{})
	assert json.decode[json2.Any]('{}')! == json2.Any(map[string]json2.Any{})
}

fn test_sum_type_struct() {
	if x := json.decode[Animal]('{"cat_name": "Tom"}') {
		assert false
	}
	if x := json.decode[Animal]('{"dog_name": "Rex"}') {
		assert false
	}
	assert json.decode[Animal]('{"dog_name": "Rex", "_type": "Dog"}')! == Animal(Dog{'Rex'})

	// struct sumtype in random order
	assert json.decode[StructSumTypes]('{"_type": "Stru", "val": 1, "val2": "lala", "val3": {"a": 2, "steak": "leleu"}, "val4": 2147483000, "val5": 2147483000}')! == StructSumTypes(Stru{1, 'lala', Stru2{2, 'leleu'}, 2147483000, 2147483000})
}

fn test_sum_type_mixed() {
	assert json.decode[Mixed]('{"key":0}')! == Mixed({
		'key': 0
	})
	assert json.decode[Mixed]('10')! == Mixed(10)
}

// to be implemented
fn test_sum_type_options_fail() {
	assert json.decode[Maybes]('null')! == Maybes(?int(none))
	if x := json.decode[Maybes]('99') {
		assert false
	}
	if x := json.decode[Maybes]('hi') {
		assert false
	}
	if x := json.decode[Maybes]('true') {
		assert false
	}
}

// to be implemented
fn test_sum_type_alias_fail() {
	if x := json.decode[SumAlias]('99') {
		assert false
	}
	if x := json.decode[SumAlias]('true') {
		assert false
	}
	if x := json.decode[SumAlias]('["hi", "bye"]') {
		assert false
	}
	if x := json.decode[SumAlias]('[0, 1]') {
		assert false
	}
}
