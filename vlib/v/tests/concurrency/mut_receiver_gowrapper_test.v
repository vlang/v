module main

@[heap]
interface IGameObject {
mut:
	name     string
	parent   ?&IGameObject
	children []&IGameObject
	advance()
}

@[heap]
struct GameObject implements IGameObject {
mut:
	name     string
	parent   ?&IGameObject
	children []&IGameObject
}

struct Ship implements IGameObject {
	GameObject
	speed f32
}

fn (mut gameobject GameObject) advance() {
	for mut child in gameobject.children {
		go child.advance()
	}
}

fn test_main() {
	mut ship := &Ship{
		name: 'ship'
	}
	ship.advance()
	assert true
}
