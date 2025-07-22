import x.json2.decoder2
import json

struct Data {
	name  ?string
	value string
}

fn test_decode_option_field() {
	assert decoder2.decode[Data]('{"name": "test", "value": "hi"}')! == Data{
		name:  'test'
		value: 'hi'
	}
	assert decoder2.decode[Data]('{"name": null, "value": "hi"}')! == Data{
		name:  none
		value: 'hi'
	}
	assert decoder2.decode[Data]('{"value": "hi"}')! == Data{
		name:  none
		value: 'hi'
	}
	assert decoder2.decode[Data]('{"name": null, "value": null}') or { return } == Data{}
}
