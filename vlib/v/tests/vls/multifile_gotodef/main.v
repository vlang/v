module main

fn main() {
	obj := MyStruct{
		value: 42
		name:  'test'
	}
	val := obj.get_value()
	println(val)

	field_val := obj.value
	println(field_val)

	e := MyEnum.first
	println(e)

	match e {
		.second {
			println('second')
		}
		else {}
	}
}
