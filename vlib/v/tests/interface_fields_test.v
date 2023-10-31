interface Animal {
mut:
	breed string
}

struct Cat {
	padding int // ensures that the field offsets can be different
mut:
	breed string
}

struct Dog {
	padding  [256]u8
	padding2 int
mut:
	breed string
}

fn use_interface(a Animal) {
	assert a.breed in ['Persian', 'Labrador']
	if a is Cat {
		assert a.breed == 'Persian'
	} else {
		assert a.breed == 'Labrador'
	}
}

fn mutate_interface(mut a Animal) {
	if mut a is Cat {
		a.breed = 'Siamese'
	} else {
		a.breed = 'Golden Retriever'
	}
	if mut a is Cat {
		assert a.breed == 'Siamese'
	} else {
		assert a.breed == 'Golden Retriever'
	}
	a.breed = 'what??'
	assert a.breed == 'what??'
}

fn test_interface_fields() {
	mut c := Cat{
		breed: 'Persian'
	}
	mut d := Dog{
		breed: 'Labrador'
	}
	use_interface(c)
	use_interface(d)
	mutate_interface(mut c)
	mutate_interface(mut d)
	assert c.breed == 'what??'
	assert d.breed == 'what??'
}

struct Nofun {
	foo fn (int) int
}

interface NofunInterface {
	foo fn (int) int
}

fn my_fn(a int) int {
	assert a == 123
	return a * 2
}

fn test_interface_fn_pointer_fields() {
	nf := NofunInterface(Nofun{my_fn})
	assert nf.foo(123) == 246
}

// For issue: 16198 errors when the reference interface type field of the struct is nil(init, assign...)
interface Speaker {
	speak() string
}

struct Wolf {}

fn (w Wolf) speak() string {
	return 'woof'
}

struct Foo {
mut:
	speaker &Speaker = unsafe { nil }
}

fn test_set_nil_to_ref_interface_type_fields() {
	mut foo := Foo{
		speaker: unsafe { nil }
	}
	assert true

	foo.speaker = unsafe { nil }
	assert true

	foo.speaker = &Wolf{}
	assert foo.speaker.speak() == 'woof'
}
