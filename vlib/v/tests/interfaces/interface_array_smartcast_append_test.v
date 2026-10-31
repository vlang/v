interface Animal {
	name() string
}

struct Dog {
	name_ string
}

fn (d Dog) name() string {
	return d.name_
}

struct Cat {}

fn (c Cat) name() string {
	return 'cat'
}

struct Holder {
	animal Animal
}

fn test_append_smartcast_interface_and_explicit_cast() {
	values := [Animal(Dog{'rex'}), Animal(Cat{}), Animal(Dog{'spot'})]
	mut list := []Animal{}
	for animal in values {
		if animal is Dog {
			list << animal
			list << Animal(animal)
			bound := Animal(animal)
			list << bound
		}
	}
	assert list.len == 6
	for i, animal in list {
		assert animal is Dog
		assert animal.name() == if i < 3 { 'rex' } else { 'spot' }
	}
}

fn test_append_smartcast_selector_and_index() {
	holder := Holder{Animal(Dog{'rex'})}
	values := [Animal(Dog{'spot'})]
	lookup := {
		'dog': Animal(Dog{'fido'})
	}
	mut list := []Animal{}
	if holder.animal is Dog {
		list << holder.animal
		list << Animal(holder.animal)
	}
	if values[0] is Dog {
		list << values[0]
		list << Animal(values[0])
	}
	if lookup['dog'] is Dog {
		list << lookup['dog']
		list << Animal(lookup['dog'])
	}
	assert list.len == 6
	assert list[0].name() == 'rex'
	assert list[1].name() == 'rex'
	assert list[2].name() == 'spot'
	assert list[3].name() == 'spot'
	assert list[4].name() == 'fido'
	assert list[5].name() == 'fido'
}
