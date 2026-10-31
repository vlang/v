import v.tests.enums.enumaliasstr

struct QualifiedAliasHolder {
	value enumaliasstr.AliasDeep
}

fn test_qualified_alias_struct_str() {
	value := enumaliasstr.AliasDeep(enumaliasstr.Kind.one)
	local := QualifiedAliasHolder{value}
	assert local.value.str() == 'custom imported alias'
	assert '${local.value}' == 'custom imported alias'
	assert local.str() == 'QualifiedAliasHolder{\n    value: custom imported alias\n}'
	assert '${local}' == 'QualifiedAliasHolder{\n    value: custom imported alias\n}'
}

fn test_imported_alias_struct_str() {
	value := enumaliasstr.AliasDeep(enumaliasstr.Kind.one)
	imported := enumaliasstr.Holder{value}
	assert imported.value.str() == 'custom imported alias'
	assert '${imported.value}' == 'custom imported alias'
	assert imported.str() == 'enumaliasstr.Holder{\n    value: custom imported alias\n}'
	assert '${imported}' == 'enumaliasstr.Holder{\n    value: custom imported alias\n}'
}
