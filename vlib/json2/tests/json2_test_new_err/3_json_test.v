import json2 as json

struct Info {
	id    int
	items []string
	maps  map[string]string
}

fn test_decode_null_object() ! {
	info := json.decode[Info]('{"id": 22, "items": null, "maps": null}')!
	assert info.id == 22
	assert '${info.items}' == '[]'
	assert '${info.maps}' == '{}'
}

fn test_decode_missing_maps_field() ! {
	info := json.decode[Info]('{"id": 22, "items": null}')!
	assert info.id == 22
	assert '${info.items}' == '[]'
	assert '${info.maps}' == '{}'
}
