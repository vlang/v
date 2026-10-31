import v.tests.generics.generics_from_modules.genericmodule

// A literal sets an embedded generic struct of another module by its bare name,
// `Box`, as a field access reads it. The literal lost it without a word.
struct LocalBox {
	genericmodule.Box[int]
	label string
}

fn test_a_literal_sets_an_embedded_generic_struct_of_another_module_by_its_name() {
	l := LocalBox{
		Box:   genericmodule.Box[int]{
			value: 4
		}
		label: 'l'
	}
	assert l.value == 4
	assert l.Box.value == 4
	assert l.label == 'l'
}
