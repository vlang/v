// vtest flance: -Wfatal-errors

type AnimalArg = CatArg | DogArg

struct CatArg {
	name string
}

struct DogArg {
	name string
}

fn animal_arg_names(animals []AnimalArg) []string {
	mut names := []string{cap: animals.len}
	for animal in animals {
		names << animal.name
	}
	return names
}

fn test_array_of_sumtype_variant_is_accepted_as_argument() {
	cats := [
		CatArg{
			name: 'Kitty'
		},
		CatArg{
			name: 'Misty'
		},
	]
	assert animal_arg_names(cats) == ['Kitty', 'Misty']
}

interface NamedArg {
	name() string
}

struct BirdArg {
	name_value string
}

fn (b BirdArg) name() string {
	return b.name_value
}

fn named_arg_names(items []NamedArg) []string {
	return items.map(it.name())
}

fn test_array_of_interface_implementor_is_accepted_as_argument() {
	birds := [
		BirdArg{
			name_value: 'Kiwi'
		},
		BirdArg{
			name_value: 'Mango'
		},
	]
	assert named_arg_names(birds) == ['Kiwi', 'Mango']
}
