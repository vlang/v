import x.json2 as json

pub struct Stru {
	val  int
	val2 string
	val3 Stru2
}

pub struct Stru2 {
	a               int
	brazilian_steak string
}

struct StructType[T] {
mut:
	val T
}

struct StructTypeOption[T] {
mut:
	val ?T
}

struct StructTypePointer[T] {
mut:
	val &T
}

fn test_array_of_strings() {
	// Structs
	assert json.decode[StructType[string]]('{"val": "2"}')! == StructType{
		val: '2'
	}
	assert json.decode[StructType[int]]('{"val": 2}')! == StructType{
		val: 2
	}

	// maps
	assert json.decode[map[string]string]('{"val": "2"}')! == {
		'val': '2'
	}
	// assert json.decode[map[string]int]('{"val": 2}')! == {"val": 2}

	// // nested map
	// assert json.decode[map[string]map[string]string]('{"val": {"val2": "2"}}')! == {"val": {"val2": "2"}}

	// nested struct
	assert json.decode[Stru]('{"val": 1, "val2": "lala", "val3": {"a": 2, "brazilian_steak": "leleu"}}')! == Stru{
		val:  1
		val2: 'lala'
		val3: Stru2{
			a:               2
			brazilian_steak: 'leleu'
		}
	}

	// pretty print
	assert json.decode[Stru]('{
		"val": 1,
		"val2": "lala",
		"val3": {
			"a": 2,
			"brazilian_steak": "leleu"
		}
	}')! == Stru{
		val:  1
		val2: 'lala'
		val3: Stru2{
			a:               2
			brazilian_steak: 'leleu'
		}
	}
}
