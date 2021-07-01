pub struct Page {
pub mut:
	lang    string
	page    string
	var_one string
	var_two string
}

fn get_keys_and_values<T>(mut keys []string, mut values []string, mut data T) {
	$for field in T.fields {
		$if field.typ is string {
			keys << field.name
			values << data.$(field.name)
		}
	}
}

fn awesome<T>(mut data T) {
	mut keys := []string{}
	mut values := []string{}
	get_keys_and_values(mut keys, mut values, mut data)
	println(keys)
	assert keys == ['lang', 'page', 'var_one', 'var_two']
	println(values)
	assert values == ['vlang', 'one', 'variable one', 'variable two']
}

fn test_generic_fn_infer_multi_paras() {
	mut page := Page{
		lang: 'vlang'
		page: 'one'
		var_one: 'variable one'
		var_two: 'variable two'
	}
	awesome(mut page)
}
