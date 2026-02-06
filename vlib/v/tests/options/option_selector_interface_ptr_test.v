module main

interface IGameObject {
mut:
	name     string
	parent   ?&IGameObject
	children []&IGameObject
	add_child(mut o IGameObject)
}

@[heap]
struct GameObject implements IGameObject {
mut:
	name     string
	parent   ?&IGameObject
	children []&IGameObject
}

fn (mut gameobject GameObject) add_child(mut o IGameObject) {
	o.parent = gameobject
	gameobject.children << o
}

fn test_main() {
	mut v1 := &GameObject{
		name: 'v1'
	}
	mut v2 := &GameObject{
		name: 'v2'
	}
	v1.add_child(mut v2)
	if v1.children[0].parent != none {
		assert '${v1.children[0].parent.name}' == 'v1'
	}
}
