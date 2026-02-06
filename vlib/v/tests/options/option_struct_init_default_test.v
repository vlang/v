struct Struct {
	f1 string
	f2 int
	f3 bool
	f4 f64
}

struct StructWithFieldsOfOptional {
	f1 ?string
	f2 ?int
	f3 ?bool
	f4 ?f64
}

struct StructWithFieldsOfOptionalArray {
	f1 []?string
	f2 []?int
	f3 []?bool
	f4 []?f64
}

fn test_struct_with_fields_of_optional() {
	v1 := StructWithFieldsOfOptional{
		f1: ?string('a')
		f2: ?int(1)
		f3: ?bool(true)
		f4: ?f64(1.1)
	}
	v2 := Struct{
		f1: v1.f1 or { '' }
		f2: v1.f2 or { 0 }
		f3: v1.f3 or { false }
		f4: v1.f4 or { 0.0 }
	}
	assert v2.f1 == 'a'
	assert v2.f2 == 1
	assert v2.f3 == true
	assert v2.f4 == 1.1
}

fn test_struct_with_fields_of_optional_array() {
	v1 := StructWithFieldsOfOptionalArray{
		f1: [?string('a'), 'b']
		f2: [?int(1), 2]
		f3: [?bool(true), false]
		f4: [?f64(1.1), 2.2]
	}
	v2 := Struct{
		f1: v1.f1[0] or { '' }
		f2: v1.f2[0] or { 0 }
		f3: v1.f3[0] or { false }
		f4: v1.f4[0] or { 0.0 }
	}
	assert v2.f1 == 'a'
	assert v2.f2 == 1
	assert v2.f3 == true
	assert v2.f4 == 1.1
}
