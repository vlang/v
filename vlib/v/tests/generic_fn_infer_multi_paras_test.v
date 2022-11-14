pub struct Two_data {
pub mut:
	title   string
	content string
}

pub struct Page {
pub mut:
	lang      string
	page      string
	var_one   string
	var_two   string
	var_three Two_data
}

fn get_keys_and_values<T>(mut keys []string, mut values []string, mut data T) ([]string, []string, T) {
	$for field in T.fields {
		$if field.typ is string {
			keys << field.name
			values << data.$(field.name)
		}
	}
	return keys, values, data
}

fn awesome<T>(mut data T) {
	mut keys := []string{}
	mut values := []string{}
	keys, values, data = get_keys_and_values(mut keys, mut values, mut data)
	println(keys)
	assert keys == ['lang', 'page', 'var_one', 'var_two']
	println(values)
	assert values == ['vlang', 'one', 'variable one', 'variable two']
	println(data)
	assert '${data}'.contains("title: 'what a title'")
}

fn test_generic_fn_infer_multi_paras() {
	mut page := Page{
		lang: 'vlang'
		page: 'one'
		var_one: 'variable one'
		var_two: 'variable two'
		var_three: Two_data{
			title: 'what a title'
			content: 'what a content'
		}
	}
	awesome(mut page)
}
