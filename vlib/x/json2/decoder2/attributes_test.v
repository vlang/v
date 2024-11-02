import x.json2.decoder2 as json

struct StruWithJsonAttribute {
	a     int
	name2 string @[json: 'name']
	b     int
}

struct StruWithSkipAttribute {
	a     int
	name2 string @[skip]
	b     int
}

struct StruWithCustomUserAttribute {
	a    int    @[some_custom_attribute_created_by_user]
	name string @[some_custom_attribute_created_by_user]
	b    int    @[some_custom_attribute_created_by_user]
}

fn test_default_attributes() {
	assert json.decode[StruWithJsonAttribute]('{"name": "hola", "a": 2, "b": 3}')! == StruWithJsonAttribute{
		a:     2
		name2: 'hola'
		b:     3
	}

	assert json.decode[StruWithSkipAttribute]('{"name2": "hola", "a": 2, "b": 3}')! == StruWithSkipAttribute{
		a:     2
		name2: ''
		b:     3
	}
}

fn do_something_attribute_handler(arg string, mut field json.MutableFieldData, value_info json.ValueInfo) json.AttributeBehaviorOnComptimeForFieldsLoop {
	println('this do something, like change the field name, print this message, etc')

	// don't decode if the field is int
	if field.typ == typeof[int]().idx {
		return .continue_
	}

	return .none_
}

fn test_custom_user_attribute() {
	mut result := StruWithCustomUserAttribute{}

	mut decoder := json.new_decoder[StruWithCustomUserAttribute]('{"name": "hola", "a": 2, "b": 3}',
		{
		'some_custom_attribute_created_by_user': do_something_attribute_handler
	})!

	decoder.decode_value(mut &result)!

	assert result == StruWithCustomUserAttribute{
		a:    0
		name: 'hola'
		b:    0
	}
}
