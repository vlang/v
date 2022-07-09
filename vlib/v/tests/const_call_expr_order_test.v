import os

const (
	shdc_exe_name = 'sokol-shdc.exe'
	tool_name     = os.file_name(os.executable())
	cache_dir     = os.join_path(os.cache_dir(), 'v', tool_name)
	shdc          = shdc_exe()
)

fn test_const_call_expr_order() {
	dump(cache_dir)
	dump(shdc)
	assert true
}

fn shdc_exe() string {
	return os.join_path(cache_dir, shdc_exe_name)
}
