struct Foo[T] {
	field T
}

struct Bar[T] {
	Foo[T]
}

struct Baz[T] {
	Bar[T]
}

struct OneLevelEmbed[T] {
	Foo[T]
}

fn test_main() {
	m := OneLevelEmbed[int]{}
	assert m.field == 0
}

struct MultiLevelEmbed[T] {
	Baz[T]
}

fn test_multi_level() {
	m := MultiLevelEmbed[int]{}
	assert m.field == 0
}

struct EmbeddedChoiceValue {
	value int
}

struct EmbeddedChoiceOther {
	other int
}

type EmbeddedChoice = EmbeddedChoiceOther | EmbeddedChoiceValue

fn embedded_choice_value(m MultiLevelEmbed[EmbeddedChoice]) int {
	if m.field is EmbeddedChoiceValue {
		return m.field.value
	}
	return 0
}

fn test_multi_level_generic_embed_smartcast() {
	m := MultiLevelEmbed[EmbeddedChoice]{ field: EmbeddedChoiceValue{ value: 42 } }
	assert embedded_choice_value(m) == 42
}
