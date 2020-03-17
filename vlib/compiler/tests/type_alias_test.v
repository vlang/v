
type Myint int
type Myf32 f32
type Myf64 f64

fn test_type_alias() {
	i := Myint(10) 
	assert i + 100 == 110

	f1 := Myf32(1.0)
	assert f1 + f32(3.14) == f32(4.14)
	
	f2 := Myf64(10.4)
	assert f2 + 0.5 == 10.9  



}
