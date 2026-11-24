// A sum type value narrowed by `is` or `match` and converted to an interface must
// box its variant: the interface `_typ` is the variant's, and `_object` holds a
// copy of the variant payload. Boxing it as the declared sum type copied
// `sizeof(Sum)` bytes from the smaller variant payload and left `_typ` unset.
interface BoxedValue {}

struct BoxedCat {}

struct BoxedDog {
	name string
	age  int
}

type BoxedPet = BoxedCat | BoxedDog

type BoxedPetAlias = BoxedPet

struct BoxedOwner {
	pet BoxedPet
}

fn boxed_kind(v BoxedValue) string {
	return match v {
		BoxedCat { 'cat' }
		BoxedDog { 'dog ${v.name} ${v.age}' }
		else { 'other' }
	}
}

fn boxed_kinds(values ...BoxedValue) []string {
	mut res := []string{}
	for v in values {
		res << boxed_kind(v)
	}
	return res
}

fn boxed_generic_kinds[T](x T) []string {
	return boxed_kinds(x)
}

fn boxed_ref_kinds(p &BoxedPet) []string {
	if p is BoxedDog {
		return boxed_kinds(p)
	}
	return []
}

fn boxed_mut_kinds(mut p BoxedPet) []string {
	if mut p is BoxedDog {
		return boxed_kinds(p)
	}
	return []
}

fn test_if_smartcast_sum_variable_boxes_variant() {
	cat := BoxedPet(BoxedCat{})
	if cat is BoxedCat {
		assert boxed_kinds(cat) == ['cat']
		assert boxed_kind(cat) == 'cat'
		v := BoxedValue(cat)
		assert v is BoxedCat
	}
	dog := BoxedPet(BoxedDog{'rex', 3})
	if dog is BoxedDog {
		assert boxed_kinds(dog, dog) == ['dog rex 3', 'dog rex 3']
		assert boxed_kind(dog) == 'dog rex 3'
		v := BoxedValue(dog)
		assert v is BoxedDog
		if v is BoxedDog {
			assert v.name == 'rex'
			assert v.age == 3
		}
	}
}

fn test_match_smartcast_sum_variable_boxes_variant() {
	pets := [BoxedPet(BoxedCat{}), BoxedPet(BoxedDog{'max', 5})]
	mut got := []string{}
	for pet in pets {
		match pet {
			BoxedCat { got << boxed_kinds(pet) }
			BoxedDog { got << boxed_kinds(pet) }
		}
	}
	assert got == ['cat', 'dog max 5']
}

fn test_smartcast_sum_selector_and_index_box_variant() {
	owner := BoxedOwner{
		pet: BoxedDog{'fido', 7}
	}
	if owner.pet is BoxedDog {
		assert boxed_kinds(owner.pet) == ['dog fido 7']
		assert boxed_kind(owner.pet) == 'dog fido 7'
	}
	pets := [BoxedPet(BoxedCat{}), BoxedPet(BoxedDog{'max', 5})]
	if pets[1] is BoxedDog {
		assert boxed_kinds(pets[1]) == ['dog max 5']
	}
}

fn test_smartcast_sum_alias_mut_and_reference_box_variant() {
	alias := BoxedPetAlias(BoxedDog{'al', 1})
	if alias is BoxedDog {
		assert boxed_kinds(alias) == ['dog al 1']
	}
	mut pet := BoxedPet(BoxedDog{'mu', 2})
	if mut pet is BoxedDog {
		assert boxed_kinds(pet) == ['dog mu 2']
	}
	assert boxed_ref_kinds(&pet) == ['dog mu 2']
	assert boxed_mut_kinds(mut pet) == ['dog mu 2']
	if pet is BoxedDog {
		assert boxed_generic_kinds(pet) == ['dog mu 2']
	}
}
