module main

interface Speaker {
	say() string
}

struct ChatRoom {
mut:
	talkers map[string]Speaker
}

fn new_room() &ChatRoom {
	return &ChatRoom{
		talkers: map[string]Speaker{}
	}
}

fn (mut r ChatRoom) add(name string, s Speaker) {
	r.talkers[name] = s
}

fn test_using_a_map_of_speaker_interfaces() {
	mut room := new_room()
	room.add('my cat', Cat{ name: 'Tigga' })
	room.add('my dog', Dog{ name: 'Pirin' })
	room.add('stray dog', Dog{ name: 'Anoni' })
	room.add('me', Human{ name: 'Bilbo' })
	room.add('she', Human{ name: 'Maria' })
	mut text := ''
	for name, subject in room.talkers {
		line := '${name:12s}: ${subject.say()}'
		println(line)
		text += line
	}
	assert text.contains(' meows ')
	assert text.contains(' barks ')
	assert text.contains(' says ')
}

struct Cat {
	name string
}

fn (c &Cat) say() string {
	return '${c.name} meows "MEOW!"'
}

struct Dog {
	name string
}

fn (d &Dog) say() string {
	return '${d.name} barks "Bau Bau!"'
}

struct Human {
	name string
}

fn (h &Human) say() string {
	return '${h.name} says "Hello"'
}
