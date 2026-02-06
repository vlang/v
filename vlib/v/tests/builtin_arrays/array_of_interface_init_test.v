struct Dog {
	breed string
}

struct Cat {
	breed string
}

fn (d Dog) speak() string {
	return 'woof'
}

fn (c Cat) speak() string {
	return 'meow'
}

interface Speaker {
	breed string
	speak() string
}

fn test_array_of_interface_init() {
	dog := Dog{'Leonberger'}
	cat := Cat{'Siamese'}

	mut rets := []string{}

	for item in [Speaker(dog), cat] {
		println(item.speak())
		rets << item.speak()
	}

	assert rets.len == 2
	assert rets[0] == 'woof'
	assert rets[1] == 'meow'
}
