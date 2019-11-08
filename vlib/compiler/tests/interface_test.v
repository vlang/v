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
}	

fn perform_speak(s Speaker) bool {
	s.speak()
	return true
}

fn test_perform_speak() {
	d := Dog{}
	assert perform_speak(d)
	cat := Cat{}
	assert perform_speak(cat)
	f := Foo {
		//speaker: d
	}	

}

