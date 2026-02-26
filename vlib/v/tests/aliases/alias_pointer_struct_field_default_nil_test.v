type NilAliasCol = int | string

struct NilAliasMyType {
	cols []NilAliasCol
}

type NilAliasPtr = &NilAliasMyType

struct NilAliasStruct {
mut:
	alias_ptr     NilAliasPtr  = unsafe { nil }
	alias_ptr_ptr &NilAliasPtr = unsafe { nil }
}

fn test_struct_field_default_nil_for_pointer_alias() {
	value := NilAliasStruct{}
	assert isnil(value.alias_ptr)
	assert isnil(value.alias_ptr_ptr)
}

struct NilAliasInnerType {}

type NilAliasInner = NilAliasInnerType

struct NilAliasRefStruct {
mut:
	alias_ref &NilAliasInner = unsafe { nil }
}

fn test_struct_field_default_nil_for_reference_to_alias() {
	value := NilAliasRefStruct{}
	assert isnil(value.alias_ref)
}
