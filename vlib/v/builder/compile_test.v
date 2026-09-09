module builder

fn test_file_has_module_declaration_after_attribute_raw_string_backslash() {
	assert test_file_has_module_declaration("@[deprecated: r'use replacement\\']\nmodule sample\n")
}

fn test_file_has_module_declaration_after_attribute_block_comment_bracket() {
	assert test_file_has_module_declaration('@[has_globals /* ] */]\nmodule sample\n')
}
