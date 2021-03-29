struct Dog {
	breed string
}

struct Cat {
	breed string
}

interface Animal {
	breed string
}

fn test_auto_str_gen_for_interfaces() {
	x := Animal(Cat{'Siamese'})
	assert '$x' == "
Animal(Cat{
    breed: 'Siamese'
})
".trim_space()
}

struct Holder {
	x Animal
}

struct Holder2 {
	x     map[string]Holder
	breed string
}

fn test_auto_str_gen_for_complex_interface_types() {
	a := Animal(Dog{'hi'})
	h := Holder{a}
	m := map{
		'dsa': h
	}
	h2 := Holder2{m, 'N/A'}
	a2 := Animal(h2)

	assert '$a2' == r"
Animal(Holder2{
    x: {'dsa': Holder{
        x: Animal(Dog{
            breed: 'hi'
        })
    }}
    breed: 'N/A'
})
	".trim_space()
}
