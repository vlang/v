import time

struct IdentityEntry {
	value int
}

type IdentityAlias = IdentityEntry

type IdentityValue = IdentityAlias
	| IdentityEntry
	| &IdentityEntry
	| &&IdentityEntry
	| ?int
	| []IdentityEntry
	| int
	| map[string]IdentityEntry
	| time.Time

struct Uc {}

struct ACRB {}

type CollisionValue = ACRB | Uc

fn assert_identity[T](item T) int {
	value := IdentityValue(item)
	expected := typeof[T]().idx
	assert value.type_idx() == expected
	mut found := false
	$for variant in IdentityValue.variants {
		if variant.typ == expected {
			found = true
		}
	}
	assert found
	return expected
}

fn test_sumtype_identity_matches_reflected_variants() {
	entry := IdentityEntry{42}
	mut indexes := []int{}
	indexes << assert_identity(entry)
	indexes << assert_identity(IdentityAlias(entry))
	indexes << assert_identity(&entry)
	ref := &entry
	indexes << assert_identity(&ref)
	indexes << assert_identity(?int(42))
	indexes << assert_identity(42)
	indexes << assert_identity([entry])
	indexes << assert_identity({
		'entry': entry
	})
	indexes << assert_identity(time.unix(0))
	for index, value in indexes {
		assert value !in indexes[..index]
	}
}

fn test_sumtype_identity_resolves_hash_collisions() {
	first := CollisionValue(Uc{})
	second := CollisionValue(ACRB{})
	assert first.type_idx() == typeof[Uc]().idx
	assert second.type_idx() == typeof[ACRB]().idx
	assert first.type_idx() != second.type_idx()
}
