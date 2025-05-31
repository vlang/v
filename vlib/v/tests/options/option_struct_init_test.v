pub struct MyStruct {
pub mut:
	valueb ?int
}

pub struct MyStruct2 {
pub mut:
	valuea int
	valueb ?MyStruct
}

fn test_main() {
	a := MyStruct2{
		valuea: 1
	}
	assert a.str() == 'MyStruct2{
    valuea: 1
    valueb: Option(none)
}'
}
