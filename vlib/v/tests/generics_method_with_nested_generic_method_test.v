struct Dummy {}

fn (dummy &Dummy) res<T>() !T {
	$if T is int {
		return 1
	} $else $if T is f32 {
		return dummy.res<int>()! + 1
	} $else {
		return error('exhausted')
	}
}

fn test_generics_method_with_nested_generic_method() ! {
	d := Dummy{}

	println(d.res<int>()!)
	ret1 := d.res<int>()!
	assert ret1 == 1

	println(d.res<f32>()!)
	ret2 := d.res<f32>()!
	assert ret2 == 2.0
}
