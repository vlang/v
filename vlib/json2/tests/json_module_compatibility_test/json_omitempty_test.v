// vtest vflags: -w
import json2

pub struct MyStruct {
pub mut:
	code    int
	message string
	data    string  @[omitempty]
	data2   ?string @[omitempty]
}

fn test_simple() {
	obj := MyStruct{
		code:    1
		message: 'yes'
		data2:   'a'
	}
	assert dump(json2.encode(obj, escape_unicode: true)) == '{"code":1,"message":"yes","data2":"a"}'
}

fn test_none() {
	obj := MyStruct{
		code: 1
	}
	assert dump(json2.encode(obj, escape_unicode: true)) == '{"code":1,"message":""}'
}
