// vtest vflags: -w
import json2

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

type Animal = Cat | Dog

struct Cat {
	cat_name string
}

struct Dog {
	dog_name string
}

fn test_main() {
	data := '{"attributes": {"price": [{"net": 1, "_type": "Price"}, {"net": 2, "_type": "Price"}]}}'
	entity := json2.decode[ShopResponseData](data) or { panic(err) }
	assert entity == ShopResponseData{
		attributes: Attributes{
			price: Prices([Price{
				net: 1
			}, Price{
				net: 2
			}])
		}
	}

	data2 := '{"attributes": {"price": {"net": 1, "_type": "Price"}}}'
	entity2 := json2.decode[ShopResponseData](data2) or { panic(err) }
	assert entity2 == ShopResponseData{
		attributes: Attributes{
			price: Prices(Price{
				net: 1
			})
		}
	}

	data3 := json2.encode(ShopResponseData{
		attributes: Attributes{
			price: Prices([Price{
				net: 1.2
			}])
		}
	},
		escape_unicode: true
	)
	assert data3 == '{"attributes":{"price":[{"net":1.2,"_type":"Price"}]}}'

	entity3 := json2.decode[ShopResponseData](data3) or { panic(err) }
	assert entity3 == ShopResponseData{
		attributes: Attributes{
			price: Prices([Price{
				net: 1.2
			}])
		}
	}
}

fn test_sum_types() {
	data1 := json2.encode(Animal(Dog{
		dog_name: 'Caramelo'
	}),
		escape_unicode: true
	)
	assert data1 == '{"dog_name":"Caramelo","_type":"Dog"}'

	s := '{"_type":"Cat","cat_name":"Whiskers"}'
	animal := json2.decode[Animal](s) or {
		println(err)
		assert false
		return
	}

	assert animal is Cat
	if animal is Cat {
		assert animal.cat_name == 'Whiskers'
	} else {
		assert false, 'Wrong sumtype decode. In this case animal is a Cat'
	}

	s2 := '[{"_type":"Cat","cat_name":"Whiskers"}, {"_type":"Dog","dog_name":"Goofie"}]'

	animals := json2.decode[[]Animal](s2) or {
		println(err)
		assert false
		return
	}

	assert animals.len == 2
	assert animals[0] is Cat
	assert animals[1] is Dog
	cat := animals[0] as Cat
	dog := animals[1] as Dog
	assert cat.cat_name == 'Whiskers'
	assert dog.dog_name == 'Goofie'

	// The asserts above narrow `animals[0]` to `Cat`. As the json2 README describes, vfmt
	// leaves such an encode to the author, who casts it back to the sum type.
	j := json2.encode(Animal(animals[0]), escape_unicode: true)
	assert j == '{"cat_name":"Whiskers","_type":"Cat"}'
}

type Value = string | i32

struct Node {
	value Value
}

fn test_sum_types_with_i32() {
	data1 := json2.encode([Node{i32(128)}, Node{'mystring'}], escape_unicode: true)
	assert data1 == '[{"value":128},{"value":"mystring"}]'

	node := json2.decode[[]Node](data1) or {
		println(err)
		assert false
		return
	}
	assert node.len == 2
	assert node[0].value == Value(i32(128))
	assert node[1].value == Value('mystring')
}
