struct Dog {
	breed string
}

struct Cat {
	breed string
}

fn (d Cat) name() string {
	return 'Cat'
}

fn (d Cat) speak() {
	println('meow')
}

fn (d Dog) speak() {
	println('woof')
}

fn (d Dog) name() string {
	return 'Dog'
}

fn test_todo() {}

interface Speaker {
	name ()string
	speak()
	}

/*
interface Speak2er {
	name ()string
	speak()
	}

struct Foo {
	speaker  Speaker
	speakers []Speaker
}

fn perform_speak(s Speaker) {
	if true {
		// QTODO
		return
	}
	s.speak()
	assert true
	name := s.name()
	assert name == 'Dog' || name == 'Cat'
	println(s.name())
}

fn perform_speakers(speakers []Speaker) {}

fn test_perform_speak() {
	if true {
		// QTODO
		return
	}
	dog := Dog{}
	perform_speak(dog)
	cat := Cat{}
	perform_speak(cat)
	// perform_speakers([dog, cat])
	/*
	f := Foo {
		speaker: dog
	}
	*/

}

interface Register {
	register()}

struct RegTest {
	a int
}

fn (f RegTest) register() {}

fn handle_reg(r Register) {}

fn test_register() {
	if true {
		// QTODO
		return
	}
	f := RegTest{}
	f.register()
	handle_reg(f)
}
*/
