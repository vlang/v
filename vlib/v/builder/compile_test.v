module builder

fn test_file_has_module_declaration_after_attribute_raw_string_backslash() {
	assert test_file_has_module_declaration("@[deprecated: r'use replacement\\']\nmodule sample\n")
}
