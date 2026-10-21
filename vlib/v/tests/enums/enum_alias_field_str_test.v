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
