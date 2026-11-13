module naming

fn test_cpp_keywords_use_a_disjoint_internal_namespace() {
	for name, _ in cpp_reserved_words {
		assert is_reserved_word(name)
		assert c_name(name) == '${internal_symbol_c_prefix}cpp_${name}'
		assert c_name(name) != c_name('v_${name}')
		assert c_name(name) != c_name(c_name(name))
		assert c_name('${name}_value') == '${name}_value'
		assert c_name('C.${name}') == name
	}
}
