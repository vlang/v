import x.json2.decoder2 as json

struct StruWithJsonAttribute {
	a     int
	name2 string @[json: 'name']
	b     int
}

struct StruWithSkipAttribute {
	a    int
	name ?string @[skip]
	b    int
}

struct StruWithJsonSkipAttribute {
	a    int
	name ?string @[json: '-']
	b    int
}

struct StruWithOmitemptyAttribute {
	a    int
	name ?string @[omitempty]
	b    int
}

struct StruWithRawAttribute {
	a      int
	name   string @[raw]
	object string @[raw]
	b      int
}

struct StruWithRequiredAttribute {
	a                 int
	name              string  @[required]
	skip_and_required ?string @[required; skip]
	b                 int
}

fn test_skip_and_rename_attributes() {
	assert json.decode[StruWithJsonAttribute]('{"name": "hola1", "a": 2, "b": 3}')! == StruWithJsonAttribute{
		a:     2
		name2: 'hola1'
		b:     3
	}, '`json` attribute not working'

	assert json.decode[StruWithSkipAttribute]('{"name": "hola2", "a": 2, "b": 3}')! == StruWithSkipAttribute{
		a:    2
		name: none
		b:    3
	}, '`skip` attribute not working'

	assert json.decode[StruWithJsonSkipAttribute]('{"name": "hola3", "a": 2, "b": 3}')! == StruWithJsonSkipAttribute{
		a:    2
		name: none
		b:    3
	}, " `json: '-'` skip attribute not working"

	assert json.decode[StruWithOmitemptyAttribute]('{"name": "", "a": 2, "b": 3}')! == StruWithOmitemptyAttribute{
		a:    2
		name: none
		b:    3
	}, '`omitempty` attribute not working'

	assert json.decode[StruWithOmitemptyAttribute]('{"name": "hola", "a": 2, "b": 3}')! == StruWithOmitemptyAttribute{
		a:    2
		name: 'hola'
		b:    3
	}, '`omitempty` attribute not working'
}

fn test_raw_attribute() {
	assert json.decode[StruWithRawAttribute]('{"name": "hola", "a": 2, "object": {"c": 4, "d": 5}, "b": 3}')! == StruWithRawAttribute{
		a:      2
		name:   '"hola"'
		object: '{"c": 4, "d": 5}'
		b:      3
	}, '`raw` attribute not working'
}

fn test_required_attribute() {
	assert json.decode[StruWithRequiredAttribute]('{"name": "hola", "a": 2, "skip_and_required": "hola", "b": 3}')! == StruWithRequiredAttribute{
		a:                 2
		name:              'hola'
		skip_and_required: none
		b:                 3
	}, '`required` attribute not working'

	mut has_error := false

	json.decode[StruWithRequiredAttribute]('{"name": "hola", "a": 2, "b": 3}') or {
		has_error = true
		assert err.msg() == 'missing required field `skip_and_required`'
	}

	assert has_error, '`required` attribute not working. It should have failed'
	has_error = false
}
