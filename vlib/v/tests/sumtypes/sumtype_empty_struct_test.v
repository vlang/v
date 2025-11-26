struct Empty {
}

type Sum = int | Empty

fn do_generic[T](val T) T {
	$for v in Sum.variants {
		if val is v {
			return T(v)
		}
	}
	return T{}
}

fn test_empty_cast() {
	assert do_generic(Sum(0)) == Sum(0)
	assert do_generic(Sum(Empty{})) == Sum(Empty{})
}
