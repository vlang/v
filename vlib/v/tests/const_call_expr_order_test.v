import os

const shdc_exe_name = 'sokol-shdc.exe'
const tool_name = os.file_name(os.executable())
const cache_dir = os.join_path(os.cache_dir(), 'v', tool_name)
const shdc = shdc_exe()

fn test_const_call_expr_order() {
	dump(cache_dir)
	dump(shdc)
	assert shdc.contains(cache_dir)
	assert shdc.contains(tool_name)
	assert shdc.ends_with(shdc_exe_name)
}

fn shdc_exe() string {
	return os.join_path(cache_dir, shdc_exe_name)
}
