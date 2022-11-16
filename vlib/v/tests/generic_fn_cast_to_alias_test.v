module main

type VkPresentModeKHR = u32

fn create_c_array<T>(len u32) &T {
	return unsafe { &T(malloc(int(sizeof(T) * len))) }
}

fn test_generic_fn_cast_to_alias() {
	arr_vk := create_c_array<VkPresentModeKHR>(5)

	println(typeof(arr_vk).name)
	assert typeof(arr_vk).name == '&VkPresentModeKHR'
	unsafe { free(arr_vk) }
}
