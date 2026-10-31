struct EqualityBox[T] {
mut:
	items []T
}

fn (box &EqualityBox[T]) has(value T) bool {
	for item in box.items {
		if item == value {
			return true
		}
	}
	return false
}

struct EqualityLeaf {}

struct EqualityTwig {
	n i64
}

type EqualityTree = EqualityLeaf | EqualityTwig

struct EqualityParam {
	name         string
	default_expr &EqualityBox[EqualityTree] = unsafe { nil }
}

fn test_generic_sumtype_equality_with_an_uncalled_struct_instantiation() {
	params := EqualityBox[EqualityParam]{}
	_ = params
	mut trees := EqualityBox[EqualityTree]{}
	trees.items << EqualityTree(EqualityLeaf{})
	assert trees.has(EqualityTree(EqualityLeaf{}))
	assert !trees.has(EqualityTree(EqualityTwig{ n: 1 }))
}

fn test_generic_equality_keeps_both_instantiations_independent() {
	mut trees := EqualityBox[EqualityTree]{}
	trees.items << EqualityTree(EqualityTwig{ n: 2 })
	mut params := EqualityBox[EqualityParam]{}
	param := EqualityParam{ name: 'x' }
	params.items << param
	assert params.has(param)
	assert !params.has(EqualityParam{ name: 'y' })
	assert trees.has(EqualityTree(EqualityTwig{ n: 2 }))
	assert !trees.has(EqualityTree(EqualityLeaf{}))
}
