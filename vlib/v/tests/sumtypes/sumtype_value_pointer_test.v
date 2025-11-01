struct Struct {}

type SumType = Struct | int

fn (d Struct) a[T]() T {
	return T{}
}

fn test_main() {
	s := Struct{}
	x := s.a[&SumType]()
	assert x is Struct
	println('first assert passed')
	println(x)
	assert x is Struct
	println('second assert passed')
}
