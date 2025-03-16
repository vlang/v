enum Animal {
	cat
	cow
	dog
	eagle
	horse
	rabbit
	snake
}

fn test_main() {
	map_1 := {
		Animal.cat: 'gato'
		.dog:       'perro'
		.rabbit:    'conejo'
	}
	assert map_1.str() == "{cat: 'gato', dog: 'perro', rabbit: 'conejo'}"
	map_2 := {
		'gato':   Animal.cat
		'perro':  .dog
		'conejo': .rabbit
	}
	assert map_2.str() == "{'gato': cat, 'perro': dog, 'conejo': rabbit}"

	array := [Animal.cat, .dog, .rabbit]
	assert array.str() == '[cat, dog, rabbit]'
	// vfmt off
	array_1 := [Animal.cat .dog .rabbit]
	// vfmt on
	assert array_1.str() == '[cat, dog, rabbit]'
}
