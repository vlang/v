import maps

interface MappedValue {
	text() string
}

struct MapItem {
	value string
}

fn (item MapItem) text() string {
	return item.value
}

fn map_item_text(key string, value MappedValue) (string, string) {
	return key, value.text()
}

fn keep_map_item(key string, value MappedValue) (string, MappedValue) {
	return key, value
}

fn consume_string_map(input map[string]string) string {
	return input['key']
}

fn consume_interface_map(input map[string]MappedValue) string {
	return input['key'].text()
}

fn test_generic_interface_map_callback_inference() {
	mut input := map[string]MappedValue{}
	input['key'] = MapItem{ value: 'value' }
	assert consume_string_map(maps.to_map(input, map_item_text)) == 'value'
	result := maps.to_map(input, map_item_text)
	assert typeof(result).name == 'map[string]string'
	assert result['key'] == 'value'
	assert consume_interface_map(maps.to_map(input, keep_map_item)) == 'value'
}
