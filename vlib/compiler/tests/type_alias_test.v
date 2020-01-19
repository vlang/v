
type Myint int
type Myf32 f32
type Myf64 f64

fn test_type_alias() {
	i := Myint(10) 
	assert i + 100 == 110

	f := Myf32(1.0)
	assert f + 3.14 == 4.14



}
