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
	// Reflection gives ?int the same index as int, while the sum tag still
	// distinguishes the option payload from its unwrapped value.
	assert assert_identity(?int(42)) == assert_identity(42)
	optional_positive := IdentityValue(?int(42))
	optional_negative := IdentityValue(?int(42))
	plain_positive := IdentityValue(42)
	plain_negative := IdentityValue(42)
	assert optional_positive is ?int
	assert optional_negative !is int
	assert plain_positive is int
	assert plain_negative !is ?int
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
