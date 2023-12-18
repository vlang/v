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
