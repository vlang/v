import json

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
	entity := json.decode(ShopResponseData, data) or { panic(err) }
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
	entity2 := json.decode(ShopResponseData, data2) or { panic(err) }
	assert entity2 == ShopResponseData{
		attributes: Attributes{
			price: Prices(Price{
				net: 1
			})
		}
	}

	data3 := json.encode(ShopResponseData{
		attributes: Attributes{
			price: Prices([Price{
				net: 1.2
			}])
		}
	})
	assert data3 == '{"attributes":{"price":[{"net":1.2,"_type":"Price"}]}}'

	entity3 := json.decode(ShopResponseData, data3) or { panic(err) }
	assert entity3 == ShopResponseData{
		attributes: Attributes{
			price: Prices([Price{
				net: 1.2
			}])
		}
	}
}

fn test_sum_types() {
	data1 := json.encode(Animal(Dog{
		dog_name: 'Caramelo'
	}))
	assert data1 == '{"dog_name":"Caramelo","_type":"Dog"}'

	s := '{"_type":"Cat","cat_name":"Whiskers"}'
	animal := json.decode(Animal, s) or {
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

	animals := json.decode([]Animal, s2) or {
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

	j := json.encode(animals[0])
	assert j == '{"cat_name":"Whiskers","_type":"Cat"}'
}
