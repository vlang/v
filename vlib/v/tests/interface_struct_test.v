interface Speaker {
	say_hello() string
	speak(msg string)
}

struct Boss {
	name string
}

fn (b Boss) say_hello() string {
	return "Hello, My name is $b.name and I\'m the bawz"
}

fn (b Boss) speak(msg string) {
	println(msg)
}

struct Cat {
	name  string
	breed string
}

fn (c Cat) say_hello() string {
	return 'Meow meow $c.name the $c.breed meow'
}

fn (c Cat) speak(msg string) {
	println('Meow $msg')
}

struct Baz {
mut:
	sp Speaker
}

fn test_interface_struct() {
	bz1 := Baz{
		sp: Boss{
			name: 'Richard'
		}
	}
	assert bz1.sp.say_hello() == "Hello, My name is Richard and I\'m the bawz"
	print('Test Boss inside Baz struct: ')
	bz1.sp.speak('Hello world!')
	bz2 := Baz{
		sp: Cat{
			name: 'Grungy'
			breed: 'Persian Cat'
		}
	}
	assert bz2.sp.say_hello() == 'Meow meow Grungy the Persian Cat meow'
	print('Test Cat inside Baz struct: ')
	bz2.sp.speak('Hello world!')
}

fn test_interface_mut_struct() {
	mut mbaz := Baz{
		sp: Boss{
			name: 'Derek'
		}
	}
	assert mbaz.sp.say_hello() == "Hello, My name is Derek and I\'m the bawz"
	mbaz.sp = Cat{
		name: 'Dog'
		breed: 'Not a dog'
	}
	assert mbaz.sp.say_hello() == 'Meow meow Dog the Not a dog meow'
}

fn test_interface_struct_from_array() {
	bazs := [
		Baz{
			sp: Cat{
				name: 'Kitty'
				breed: 'Catty Koo'
			}
		},
		Baz{
			sp: Boss{
				name: 'Bob'
			}
		},
	]
	assert bazs[0].sp.say_hello() == 'Meow meow Kitty the Catty Koo meow'
	assert bazs[1].sp.say_hello() == "Hello, My name is Bob and I\'m the bawz"
}

/*
// TODO: fix this too; currently with V 0.1.30 7426544 produces: `V panic: as cast: cannot cast 200 to 197`
fn test_interface_struct_from_mut_array() {
	mut bazs := [
		Baz{
			sp: Cat{
				name: 'Kitty'
				breed: 'Catty Koo'
			}
		},
		Baz{
			sp: Boss{
				name: 'Bob'
			}
		}
	]

	bazs[0].sp = Boss{
		name: 'Ross'
	}

	bazs[1].sp = Cat{
		name: 'Doggy'
		breed: 'Doggy Doo'
	}

	assert bazs[0].sp.say_hello() == 'Hello, My name is Ross and I\'m the bawz'
	assert bazs[1].sp.say_hello() == 'Meow meow Doggy the Doggy Doo meow'
}
*/
