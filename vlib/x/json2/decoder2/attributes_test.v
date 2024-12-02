import x.json2.decoder2 as json

struct StruWithJsonAttribute {
	a     int
	name2 string @[json: 'name']
	b     int
}

struct StruWithSkipAttribute {
	a    int
	name string @[skip]
	b    int
}

struct StruWithJsonSkipAttribute {
	a    int
	name string @[json: '-']
	b    int
}

fn test_attributes() {
	assert json.decode[StruWithJsonAttribute]('{"name": "hola", "a": 2, "b": 3}')! == StruWithJsonAttribute{
		a:     2
		name2: 'hola'
		b:     3
	}, '`json` attribute not working'

	assert json.decode[StruWithSkipAttribute]('{"name": "hola", "a": 2, "b": 3}')! == StruWithSkipAttribute{
		a:    2
		name: ''
		b:    3
	}, '`skip` attribute not working'

	assert json.decode[StruWithJsonSkipAttribute]('{"name": "hola", "a": 2, "b": 3}')! == StruWithJsonSkipAttribute{
		a:    2
		name: ''
		b:    3
	}, " `json: '-']` skip attribute not working"
}
