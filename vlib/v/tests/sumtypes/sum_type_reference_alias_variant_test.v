type Alias1 = &int
type Alias2 = &string
type Qwe = Alias1 | Alias2

fn test_pointer_alias_variants_keep_pointer_identity() {
	integer := 73
	text := 'alias'
	first := Qwe(Alias1(&integer))
	second := Qwe(Alias2(&text))
	assert first is Alias1
	assert second is Alias2
	assert *(first as Alias1) == integer
	assert *(second as Alias2) == text
}
