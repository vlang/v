struct Value {
	x int
}

struct BValue {
	v ?Value
}

struct Word {
	BValue
}

struct Long {
	BValue
}

struct Variadic {
	BValue
}

type Param = Word | Long | Variadic

fn test_sumtype_with_embedded_struct_of_option_field() {
	a := [Param(Word{}), Long{}, Variadic{}]
	dump(a)
	f := a[0]
	v := f.v
	dump(v)
	assert v == none
}

struct DeepOptionalField {
	value ?int
}

struct MiddleOptionalField {
	DeepOptionalField
}

struct NestedOptionalVariant {
	MiddleOptionalField
}

struct OtherOptionalVariant {}

type NestedOptionalSum = NestedOptionalVariant | OtherOptionalVariant

fn test_smartcasted_nested_embedded_optional_field() {
	mut concrete := NestedOptionalVariant{}
	concrete.value = 42
	value := NestedOptionalSum(concrete)
	if value is NestedOptionalVariant {
		if value.value != none {
			assert value.value? == 42
		}
	}
}
