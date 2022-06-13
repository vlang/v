import os

const (
	shdc_exe_name = 'sokol-shdc.exe'
	tool_name     = os.file_name(os.executable())
	cache_dir     = os.join_path(os.cache_dir(), 'v', tool_name)
	shdc          = shdc_exe()
)

fn shdc_exe() string {
	return os.join_path(cache_dir, shdc_exe_name)
}

fn test_const_order() {
	// Ensures that cache_dir is initialised *first*, and *then* shdc
	// even though `shdc` is initialised with the "simpler" call expression `shdc_exe()`,
	// that seemingly does not use any other consts:
	assert cache_dir.len > 1
	assert shdc != '/sokol-shdc.exe'
}
