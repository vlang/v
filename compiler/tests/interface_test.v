
struct Dog {
}

fn (d Dog) speak() {
	println('dog.speak()')
}

fn (d Dog) name() string {
	return 'old gray'
}

interface Speaker {
	name() string
	speak() 
}

interface Speak2er {
	speak() 
	name() string
}

fn perform_speak(s Speaker) bool {
	s.speak()
	return true
}

fn test_perform_speak() {
	d := Dog{}
	assert perform_speak(d)
}

