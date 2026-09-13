#include "@VMODROOT/vlib/v/gen/c/testdata/multiple_c_cources/file3.c"

@[typedef]
struct C.CStruct {
mut:
	c char
	i int
	f f32
}

type C.PCStruct = &C.CStruct

struct WrapperStruct {
mut:
	arr_fixed_c_type [2]C.PCStruct
}

fn test_main() {
	s1 := &C.CStruct{
		c: char(`A`)
		f: 3.14
		i: 42
	}
	s2 := &C.CStruct{
		c: char(`B`)
		f: 1.4142
		i: 2
	}
	mut w := WrapperStruct{}
	w.arr_fixed_c_type[0] = s1
	w.arr_fixed_c_type[1] = s2
	dump(w)
	assert w.arr_fixed_c_type[0].c == char(`A`)
	assert w.arr_fixed_c_type[1].c == char(`B`)
	assert w.arr_fixed_c_type[0].f == 3.14
	assert w.arr_fixed_c_type[1].f == 1.4142
	assert w.arr_fixed_c_type[0].i == 42
	assert w.arr_fixed_c_type[1].i == 2
}
