type Master = Sub1 | Sub2

struct Sub1 {
mut:
	val  int
	name string
}

struct Sub2 {
	name string
	val  int
}

struct Sub3 {
	name string
	val  int
}

type Master2 = Master | Sub3

fn test_common_sumtype_field_access() {
	mut out := []Master{}
	out << Sub1{
		val: 1
		name: 'one'
	}
	out << Sub2{
		val: 2
		name: 'two'
	}
	out << Sub2{
		val: 3
		name: 'three'
	}
	assert out[0].val == 1
	assert out[0].name == 'one'

	assert out[1].val == 2
	assert out[1].name == 'two'

	assert out[2].val == 3
	assert out[2].name == 'three'

	mut out0 := Master2(out[0]) // common fields on a doubly-wrapped sumtype
	assert out0.val == 1
	assert out0.name == 'one'
}

