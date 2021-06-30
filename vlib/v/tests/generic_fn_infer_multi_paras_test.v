pub struct Page {
pub mut:
	lang    string
	page    string
	var_one string
	var_two string
}

fn get_keys_and_values<T>(mut keys []string, mut values []string, mut data T) ([]string, []string, T) {
	$for field in T.fields {
		$if field.typ is string {
			keys << field.name
			values << data.$(field.name)
		} $else {
			keys, values, _ = get_keys_and_values(mut keys, mut values, mut data)
		}
	}
	return keys, values, data
}

fn awesome<T>(mut data T) {
	mut keys := []string{}
	mut values := []string{}
	get_keys_and_values(mut keys, mut values, mut data)
	println(keys)
	assert keys == ['lang', 'page', 'var_one', 'var_two']
	println(values)
	assert values == ['', '', 'variable one', 'variable two']
}

fn test_generic_fn_infer_multi_paras() {
	mut page := Page{
		var_one: 'variable one'
		var_two: 'variable two'
	}
	awesome(mut page)
}
