import rand

pub fn sample<T>(arr []T, k int) ?[]T {
	mut result := arr.clone()

	rand.seed([u32(1), 2]) // set seed to produce same results in order
	rand.shuffle<T>(mut result) ?

	return result[0..k]
}

fn test_generics_with_nested_external_generics_fn() ? {
	mut arr := [11, 32, 24, 45, 57, 32, 37, 52, 37, 24]
	println(arr)

	ret := sample<int>(arr, 5) ?
	println(ret)

	assert ret == [32, 45, 57, 11, 37]
}
