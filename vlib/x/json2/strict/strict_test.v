import x.json2.strict

struct StructType[T] {
mut:
	val T
}

struct StructTypeAndOptionType[T] {
mut:
	val        T
	option_val ?T
}

fn general_test() {
	json_data := r'
	    {
	        "val": 0,
	        "val1": {"val": 63},
	        "key3": ["batata"]
	    }
	'

	key_structs := strict.get_keys_from_json(tokenize(json_data))
	for key_struct in key_structs {
		println('Key: ${key_struct.key}, Value Type: ${key_struct.value_type}')
	}

	assert strict.strict_check[StructTypeAndOptionType[string]]('{"val": "","val": ""}') == StructCheckResult{
		duplicates: ['val']
		superfluous: []
	}

	assert strict.strict_check[StructTypeAndOptionType[string]]('{"val": "","val2": ""}') == StructCheckResult{
		duplicates: []
		superfluous: ['val2']
	}

	// assert strict.strict_check[StructTypeAndOptionType[string]]('{"val": "","val2": "","val3": "","val3": ""}') == StructCheckResult{
	// 	duplicates: ['val3']
	// 	superfluous: ['val2', 'val3']
	// }

	// assert strict.strict_check[StructType[StructTypeAndOptionType[string]]]('{"val": {"val": "","val2": ""}}') == StructCheckResult{
	// 	duplicates: []
	// 	superfluous: ['val.val2']
	// }

	// assert strict.strict_check[StructType[[]StructTypeAndOptionType[string]]]('{"val": [{"val": "","val2": ""}],[{"val": "","gdgd": "sss"}]}') == StructCheckResult{
	// 	duplicates: []
	// 	superfluous: ['val[0].val2', 'val[1].gdgd']
	// }
}
