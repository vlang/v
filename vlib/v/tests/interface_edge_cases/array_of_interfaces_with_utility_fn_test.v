struct Dog {
	breed string
}

fn (d Dog) name() string {
	return 'Dog'
}

// Utility helper function, to force interface _name_table generation
fn get_name(s Speaker) {
}

//
fn test_an_array_of_interfaces_works() {
	dog := Dog{}
	get_name(dog) // NB: this line does nothing, but forces interface _name_table generation
	get_names([dog, dog])
}

fn get_names(speakers []Speaker) {
	for s in speakers {
		println(s.name())
	}
}

interface Speaker {
	name() string
}
