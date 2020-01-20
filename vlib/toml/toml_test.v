// test pattern created by BurntSushi and Iarna.
import (
	toml
	yaml
	json
	http
)

fn test_BurntSushi_arrayempty_toml(){
	pub arr_empty_data := http.get('https://github.com/BurntSushi/toml-test/blob/master/tests/valid/array-empty.toml') or{
		println('failed to fetch data from /valid/array-empty.toml')
	}
	assert toml_parse(arr_empty_data)?
}

fn test_BurntSushi_arraynospaces_toml(){
	pub arr_nospace_data := http.get('https://github.com/BurntSushi/toml-test/blob/master/tests/valid/array-nospaces.toml')or{
		println('failed to fetch data from /valid/array-nospace.toml')
	}
	assert toml_parse(arr_nospace_data)?
}

fn test_BurntSushi_array_string_quote_comma_toml(){
	pub arr_str_quote_comma_data := http.get('https://github.com/BurntSushi/toml-test/blob/master/tests/valid/array-string-quote-comma.toml')or{
		println('failed to fetch data from /valid/array-string-quote-comma.toml')
	}
	assert toml_parse(arr_str_quote_comma_data)?
}

fn test_BurntSushi


fn test_BurntSushi_arrayempty_json(){
	json_test_data := http.get('https://github.com/BurntSushi/toml-test/blob/master/tests/valid/array-empty.json')or{
		println('failed to fetch data from /valid/array-empty.json')
	}
	assert json_parse(json_test_data) == arr_empty_data
}

fn test_BurntSushi_arraynospaces_json(){
	json_test_data := http.get('https://github.com/BurntSushi/toml-test/blob/master/tests/valid/array-nospaces.json')or{
		println('failed to fetch data from /valid/array-nospace.json')
	}
	json = json_parse(test_data)
	assert toml_parse() == arr_nospace_data
}