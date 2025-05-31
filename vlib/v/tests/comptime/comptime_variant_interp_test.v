module main

struct Foo {
	a int
}

type SumType = Foo | int | string

fn (v SumType) cast[T](val T) string {
	mut res := ''
	$for variant_value in v.variants {
		if v is variant_value {
			println('v: ${v}')
			res = 'v: ${v}'
			println(res)
		}
	}
	return res
}

fn test_main() {
	assert SumType(1).cast[int](1) == 'v: 1'
}
