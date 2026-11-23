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

struct HeapSumArrayItem {
	value int = 42
}

type HeapFixedSum = [2]HeapSumArrayItem | int

fn test_heap_sum_fixed_array_variant_uses_brace_initializer() {
	value := Struct{}.make[&HeapFixedSum]()
	assert value is [2]HeapSumArrayItem
	items := value as [2]HeapSumArrayItem
	assert items[0].value == 42
	assert items[1].value == 42
}
