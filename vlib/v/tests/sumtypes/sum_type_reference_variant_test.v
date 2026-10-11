struct Abc {
	val string
}

struct Xyz {
	foo string
}

type AliasType = string
type Alphabet1 = Abc | string | &Xyz
type Alphabet2 = Abc | &Xyz | string
type Alphabet3 = &Xyz | Abc | string
type Alphabet4 = Xyz | Abc | &AliasType

fn test_pointer_variant_order_and_alias_storage() {
	pointer := &Xyz{ foo: 'value' }
	first := Alphabet1(pointer)
	second := Alphabet2(pointer)
	third := Alphabet3(pointer)
	assert first is &Xyz
	assert second is &Xyz
	assert third is &Xyz
	assert (first as &Xyz) == pointer
	assert (second as &Xyz) == pointer
	assert (third as &Xyz) == pointer
	text := AliasType('alias')
	fourth := Alphabet4(&text)
	assert fourth is &AliasType
	assert *(fourth as &AliasType) == text
}
