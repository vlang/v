type AliasString = string

type AliasValue = AliasString | int | string

struct AliasHolder {
	value AliasValue
}

fn test_alias_variant_in_struct_init() {
	holder := AliasHolder{
		value: AliasString('value')
	}
	assert holder.value is AliasString
	assert holder.value as AliasString == 'value'
}
