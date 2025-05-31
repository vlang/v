interface Interface {
	str() string
}

struct Result[T] {
	data []T
}

struct Foobar {
}

fn (f Foobar) str() string {
	return 'foobar'
}

fn test_main() {
	assert Result[Interface]{}.str() == 'Result[Interface]{
    data: []
}'
	assert Result[Foobar]{
		data: [Foobar{}]
	}.str() == 'Result[Foobar]{
    data: [foobar]
}'
}
