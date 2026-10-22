enum AliasFieldKind {
	one
	two
	three
}

type AliasFieldKindAlias = AliasFieldKind
type AliasFieldKindChain = AliasFieldKindAlias

struct AliasFieldHolder {
	kind  AliasFieldKindAlias
	chain AliasFieldKindChain
}

fn test_enum_alias_struct_field_str() {
	holder := AliasFieldHolder{AliasFieldKind.three, AliasFieldKind.two}
	assert holder.kind.str() == 'three'
	assert holder.chain.str() == 'two'
	assert '${holder.kind}' == 'three'
	assert AliasFieldHolder{AliasFieldKind.one, AliasFieldKind.two}.kind.str() == 'one'
}

enum CustomAliasFieldKind {
	one
}

fn (kind CustomAliasFieldKind) str() string {
	return 'custom kind'
}

type CustomAliasField = CustomAliasFieldKind
type CustomAliasFieldChain = CustomAliasField

fn optional_custom_alias_field() ?CustomAliasFieldChain {
	return CustomAliasFieldKind.one
}

fn test_unwrapped_enum_alias_uses_inherited_custom_str() {
	assert (optional_custom_alias_field() or { panic('missing kind') }).str() == 'custom kind'
}

type CustomMethodAlias = CustomAliasFieldKind

fn (kind CustomMethodAlias) str() string {
	return 'custom alias'
}

type CustomMethodAliasChain = CustomMethodAlias

fn optional_custom_method_alias() ?CustomMethodAliasChain {
	return CustomAliasFieldKind.one
}

fn test_unwrapped_enum_alias_uses_intermediate_custom_str() {
	assert (optional_custom_method_alias() or { panic('missing kind') }).str() == 'custom alias'
}
