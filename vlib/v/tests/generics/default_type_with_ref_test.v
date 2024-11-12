struct Struct {}

type StructAlias = Struct

type SumType = Struct | int

type AliasInt = int

fn (d Struct) a[T]() T {
	return T{}
}

fn test_main() {
	s := Struct{}
	assert s.a[&int]() == 0
	assert s.a[&int]() == unsafe { nil }
	assert s.a[&Struct]() == Struct{}
	assert s.a[&SumType]() is Struct
	assert s.a[&StructAlias]() == StructAlias{}
	assert s.a[&AliasInt]() == AliasInt(0)
}
