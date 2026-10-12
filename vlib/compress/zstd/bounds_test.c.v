module zstd

fn test_compression_parameter_bounds_use_native_integer_width() {
	level := C.ZSTD_cParam_getBounds(.compression_level)
	assert level.error == 0
	assert int(level.lowerBound) == min_c_level()
	assert int(level.upperBound) == max_c_level()
	assert level.lowerBound < 0
	assert level.upperBound > 0
}

fn test_decompression_parameter_bounds_and_invalid_parameters() {
	window := C.ZSTD_dParam_getBounds(.window_log_max)
	assert window.error == 0
	assert window.lowerBound == 10
	assert window.upperBound >= window.lowerBound
	invalid_parameter := -1
	invalid_c := C.ZSTD_cParam_getBounds(unsafe { CParameter(invalid_parameter) })
	assert is_error(invalid_c.error)
	invalid_d := C.ZSTD_dParam_getBounds(unsafe { DParameter(invalid_parameter) })
	assert is_error(invalid_d.error)
}
