import json2 as json

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
	data3 := json.encode(ShopResponseData{
		attributes: Attributes{
			price: Prices([Price{
				net: 1.2
			}])
		}
	})
	assert data3 == '{"attributes":{"price":[{"net":1.2,"_type":"Price"}]}}'

	entity3 := json.decode[ShopResponseData](data3) or { panic(err) }
	assert entity3 == ShopResponseData{
		attributes: Attributes{
			price: Prices([Price{
				net: 1.2
			}])
		}
	}
}
