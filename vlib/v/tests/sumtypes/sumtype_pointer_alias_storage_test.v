struct AliasCat {
mut:
	value int
}

struct AliasDog {}

type AliasAnimal = AliasCat | AliasDog
type AnimalReference = &AliasAnimal
type NestedReference = AnimalReference

fn test_reference_alias_preserves_storage_identity() {
	mut animal := AliasAnimal(AliasCat{ value: 7 })
	reference := NestedReference(&animal)
	assert voidptr(reference) == voidptr(&animal)
	assert reference is AliasCat
	if mut animal is AliasCat {
		animal.value = 23
	}
	if reference is AliasCat {
		assert reference.value == 23
	} else {
		assert false
	}
}

@[noinline]
fn escaped_alias_reference() AnimalReference {
	animal := AliasAnimal(AliasCat{ value: 42 })
	return AnimalReference(&animal)
}

fn test_reference_alias_outlives_its_source_frame() {
	reference := escaped_alias_reference()
	if reference is AliasCat {
		assert reference.value == 42
	} else {
		assert false
	}
}

fn projected_alias_copy(reference &AliasAnimal) &AliasAnimal {
	if reference is AliasCat {
		return &AliasAnimal(reference)
	}
	panic('expected AliasCat')
}

fn test_explicit_reference_construction_still_wraps_variant_values() {
	reference := &AliasAnimal(AliasCat{ value: 19 })
	copied := projected_alias_copy(reference)
	assert voidptr(copied) != voidptr(reference)
	if copied is AliasCat {
		assert copied.value == 19
	} else {
		assert false
	}
}

type PlainAliasReference = &AliasCat

@[noinline]
fn escaped_plain_alias_reference() PlainAliasReference {
	mut value := AliasCat{ value: 3 }
	reference := PlainAliasReference(&value)
	value.value = 9
	return reference
}

fn test_plain_reference_alias_keeps_mutations_before_return() {
	reference := escaped_plain_alias_reference()
	assert reference.value == 9
}
