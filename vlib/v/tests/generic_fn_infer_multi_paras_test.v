pub struct Page {
pub mut:
	lang    string
	page    string
}

fn get_keys_and_values<T>(mut keys []string, mut values []string, mut data T) ([]string, []string, T) {
	keys << 'lang'
	values << 'vlang'
	keys << 'page'
	values << 'one'
	return keys, values, data
}

fn awesome<T>(mut data T) {
	mut keys := []string{}
	mut values := []string{}
	get_keys_and_values(mut keys, mut values, mut data)
	println(keys)
	assert keys == ['lang', 'page']
	println(values)
	assert values == ['vlang', 'one']
}

fn test_generic_fn_infer_multi_paras() {
	mut page := Page{
		lang: 'vlang'
		page: 'one'
	}
	awesome(mut page)
}
