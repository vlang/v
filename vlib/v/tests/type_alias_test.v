type Myint int
type Myf32 f32
type Myf64 f64

fn test_type_alias() {
	i := Myint(10)
	assert i + 100 == 110
	f := Myf64(10.4)
	assert f + 0.5 == 10.9
}
