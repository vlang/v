interface PostAssertAnimal {
	name() string
}

struct PostAssertCat {
	label string
}

fn (cat PostAssertCat) name() string {
	return cat.label
}

fn test_interface_is_after_assert_smartcast() {
	animal := PostAssertAnimal(PostAssertCat{'Milo'})
	assert animal is PostAssertCat
	if animal is PostAssertCat {
		assert animal.label == 'Milo'
	} else {
		assert false
	}
	assert animal is PostAssertCat
}
