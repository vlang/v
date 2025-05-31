fn test_enum_flag_alias_op() {
	enum_container := EnumContainer{
		et: .a | .b // works
	}

	alias_container := AliasContainer{
		at: .a | .b // fails
	}

	assert enum_container.et == .a | .b
	assert alias_container.at == .a | .b
}

@[flag]
enum EnumType {
	a
	b
	c
}

struct EnumContainer {
	et EnumType
}

type AliasType = EnumType

struct AliasContainer {
	at AliasType
}
