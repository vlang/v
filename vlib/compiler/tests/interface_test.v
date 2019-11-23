struct Dog {
	breed string
}

struct Cat {
       breed string
}


fn (d Cat) name() string { return 'Cat' }
fn (d Cat) speak() { println('meow') }

fn (d Dog) speak() { println('woof') }
fn (d Dog) name() string {  return 'Dog'}

interface Speaker {
	name() string
	speak()
}

interface Speak2er {
	name() string
	speak()
}

struct Foo {
	speaker Speaker
	speakers []Speaker
}	

fn perform_speak(s Speaker) {
	s.speak()
	assert true
	name := s.name()
	assert name == 'Dog' || name == 'Cat'
}

fn perform_speakers(speakers []Speaker) {
	
}	

fn test_perform_speak() {
	dog := Dog{}
	perform_speak(dog)
	cat := Cat{}
	perform_speak(cat)
	//perform_speakers([dog, cat])
	/*
	f := Foo {
		speaker: dog
	}	
	*/

}

interface Register {
	register()
}

struct RegTest {a int}

fn (f RegTest) register() {
}

fn handle_reg(r Register) {
}

fn test_register() {
	f := RegTest{}
	f.register()
	handle_reg(f)
}



