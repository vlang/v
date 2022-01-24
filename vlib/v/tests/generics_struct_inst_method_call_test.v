fn test_generics_struct_inst_method_call() {
	v1 := V2d<f32>{100}
	r1 := v1.in_bounds(tp_lft, bt_rigt)
	println(r1)
	assert r1
}

const (
	tp_lft  = V2d<f32>{20}
	bt_rigt = V2d<f32>{650}
)

struct V2d<T> {
mut:
	x T
}

pub fn (v1 V2d<T>) unpack() T {
	return v1.x
}

pub fn (v1 V2d<T>) less_than(v2 V2d<T>) V2d<bool> {
	return V2d<bool>{v1.x < v2.x}
}

pub fn (v1 V2d<T>) in_bounds(top_left V2d<T>, bottom_right V2d<T>) bool {
	v1.less_than(bottom_right).unpack()
	return true
}
