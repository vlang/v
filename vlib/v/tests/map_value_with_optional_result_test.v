import os

struct AdbDevice {
	opts map[string]?string
}

fn test_map_value_with_optional_result() {
	// avoid warnings
	_ := os.max_path_len
	assert true
}
