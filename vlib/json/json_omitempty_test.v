import json

pub struct MyStruct {
pub mut:
	code    int
	message string
	data    string [omitempty]
	// data2 ?string [omitempty]
}

fn test_simple() {
	obj := MyStruct{
		code: 1
		message: 'yes'
		// data2: "a"
	}
	dump(json.encode(obj))
}
