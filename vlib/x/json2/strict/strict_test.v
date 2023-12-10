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

fn test_get_keys_from_json() {
	json_data := r'
	    {
	        "val": 0,
	        "val1": {"val": 63}
	    }
	'

	key_structs := strict.get_keys_from_json(strict.tokenize(json_data))

	assert key_structs == [
		strict.KeyStruct{
			key: 'val'
			value_type: .literal
			token_pos: 1
		},
		strict.KeyStruct{
			key: 'val1'
			value_type: .map
			token_pos: 5
		},
	]
}

fn test_strict_check() {
	assert strict.strict_check[StructTypeAndOptionType[string]]('{"val": "","val": ""}') == strict.StructCheckResult{
		duplicates: ['val']
		superfluous: []
	}

	assert strict.strict_check[StructTypeAndOptionType[string]]('{"val": "","val2": ""}') == strict.StructCheckResult{
		duplicates: []
		superfluous: ['val2']
	}

	// assert strict.strict_check[StructTypeAndOptionType[string]]('{"val": "","val2": "","val3": "","val3": ""}') == strict.StructCheckResult{
	// 	duplicates: ['val3']
	// 	superfluous: ['val2', 'val3']
	// }

	// assert strict.strict_check[StructType[StructTypeAndOptionType[string]]]('{"val": {"val": "","val2": ""}}') == strict.StructCheckResult{
	// 	duplicates: []
	// 	superfluous: ['val.val2']
	// }

	// assert strict.strict_check[StructType[[]StructTypeAndOptionType[string]]]('{"val": [{"val": "","val2": ""}],[{"val": "","gdgd": "sss"}]}') == strict.StructCheckResult{
	// 	duplicates: []
	// 	superfluous: ['val[0].val2', 'val[1].gdgd']
	// }
}
