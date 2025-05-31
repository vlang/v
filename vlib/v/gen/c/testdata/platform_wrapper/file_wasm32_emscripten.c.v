module platform_wrapper

pub fn abc() int {
	return 987654321
}

pub fn fn_defined_in_wasm32_emscripten() int {
	println('hi from ${@FN}, defined in file_wasm32_emscripten.c.v')
	return 12345
}
