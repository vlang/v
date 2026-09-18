module staticfnref

pub struct MyStruct {
pub:
	value int
}

pub fn MyStruct.new(value int) MyStruct {
	return MyStruct{
		value: value
	}
}
