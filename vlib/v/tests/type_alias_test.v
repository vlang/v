type Myint = int
type Myf32 = f32
type Myf64 = f64

fn test_type_alias() {
	i := Myint(10)
	assert i + 100 == 110
	f := Myf32(7.4)
	assert f + f32(0.6) == f32(8.0)
	g := Myf64(10.4)
	assert g + 0.5 == 10.9
}

type Myint_2 = int
type Myf32_2 = f32
type Myf64_2 = f64

fn test_type_alias_v2() {
	i := Myint_2(10)
	assert i + 100 == 110
	f := Myf32_2(7.4)
	assert f + f32(0.6) == f32(8.0)
	g := Myf64_2(10.4)
	assert g + 0.5 == 10.9
}

struct Mystruct {
mut:
	i int
}

type Mystruct_2 = Mystruct

fn test_type_alias_struct() {
	mut s := Mystruct_2{}
	s.i = 10
	assert s.i + 100 == 110
}
