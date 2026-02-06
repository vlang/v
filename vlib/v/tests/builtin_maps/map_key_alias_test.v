type String = string
type Rune = rune

fn test_map_key_alias() {
	mut str_data := map[string]map[String]string{}
	str_data['str'][String('String')] = 'test'
	assert '${str_data}' == "{'str': {'String': 'test'}}"

	mut i32_data := map[i32]string{}
	i32_data[23] = 'num'
	assert '${i32_data}' == "{23: 'num'}"

	mut rune_data := map[Rune]string{}
	rune_data[`A`] = 'rune'
	assert '${rune_data}' == "{`A`: 'rune'}"
}
