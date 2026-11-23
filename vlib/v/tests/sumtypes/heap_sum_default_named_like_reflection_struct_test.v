import v.reflection

// v.reflection declares a struct `SumType`; the program's own `SumType` is a sum
// type, and `T{}` with `T = &SumType` is a heap copy of its default value.
struct Struct {}

type SumType = Struct | int

fn (d Struct) make[T]() T {
	return T{}
}

fn test_heap_sum_default_is_the_program_sum_type() {
	assert reflection.get_funcs().len > 0
	value := Struct{}.make[&SumType]()
	assert value is Struct
}
