import v.reflection

struct Uc {}

fn (value &Uc) number() int {
	return 42
}

struct ACRB {}

struct IdentityFields {
	first    Uc
	second   ACRB
	optional ?Uc
	pointer  &Uc = unsafe { nil }
	items    []&Uc
}

type IdentitySum = ACRB | Uc

fn test_reflection_preserves_complete_type_identity() {
	first := reflection.type_of(Uc{})
	second := reflection.type_of(ACRB{})
	assert first.idx == typeof[Uc]().idx
	assert second.idx == typeof[ACRB]().idx
	assert first.idx != second.idx
	assert first.sym.name == 'Uc'
	assert first.sym.methods.any(it.name == 'number')
	assert second.sym.name == 'ACRB'
	value := IdentitySum(Uc{})
	assert value.type_idx() == first.idx
}

fn test_reflection_field_flags_do_not_truncate_type_identity() {
	typ := reflection.type_of(IdentityFields{})
	fields := (typ.sym.info as reflection.Struct).fields
	expected := [typeof[Uc]().idx, typeof[ACRB]().idx]
	for index in 0 .. expected.len {
		assert fields[index].typ.idx() == expected[index]
		assert !fields[index].typ.has_flag(.option)
		assert !fields[index].typ.has_flag(.result)
		assert !fields[index].typ.is_ptr()
	}
	assert fields[2].typ.idx() == expected[0]
	assert fields[2].typ.has_flag(.option)
	assert fields[3].typ.idx() == expected[0]
	assert fields[3].typ.is_ptr()
	assert fields[4].typ.idx() == typeof[[]&Uc]().idx
}

struct AQVA {}

struct CFGS {}

struct CompositeFields {
	first  []AQVA
	second []CFGS
}

fn test_reflected_composite_types_resolve_hash_collisions() {
	first := reflection.type_of([]AQVA{})
	second := reflection.type_of([]CFGS{})
	assert first.idx == typeof[[]AQVA]().idx
	assert second.idx == typeof[[]CFGS]().idx
	assert first.idx != second.idx
	typ := reflection.type_of(CompositeFields{})
	fields := (typ.sym.info as reflection.Struct).fields
	assert fields[0].typ.idx() == first.idx
	assert fields[1].typ.idx() == second.idx
}
