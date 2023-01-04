interface IA {
	a int
}

struct AA {
	a int
}

struct AB {
	a int
}

fn test_interface_array_index() {
	mut ia_ary := []IA{}
	aa, ab := AA{12}, AB{13}
	ia_ary << aa
	ia_ary << ab

	abi := IA(AB{13})
	aci := IA(AB{14})
	assert ia_ary.index(abi) == 1
	assert ia_ary.index(aci) == -1
}

fn test_interface_array_contains() {
	mut ia_ary := []IA{}
	aa, ab := AA{12}, AB{13}
	ia_ary << aa
	ia_ary << ab

	abi := IA(AB{13})
	aci := IA(AB{14})

	assert abi in ia_ary
	assert aci !in ia_ary
}
