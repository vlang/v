module main

@[heap]
interface IGameObject {
mut:
	name       string
	parent     ?&IGameObject
	next       ?&IGameObject
	child      ?&IGameObject
	last_child ?&IGameObject
	add_child(mut o IGameObject)
}

@[heap]
struct GameObject implements IGameObject {
mut:
	name       string
	parent     ?&IGameObject
	next       ?&IGameObject
	child      ?&IGameObject
	last_child ?&IGameObject
}

fn (mut gameobject GameObject) add_child(mut o IGameObject) {
	o.parent = gameobject
	if gameobject.last_child != none {
		gameobject.last_child.next = o
	} else {
		gameobject.child = o
	}
	gameobject.last_child = o
}

fn test_main() {
	mut v1 := &GameObject{
		name: 'v1'
	}
	mut v2 := &GameObject{
		name: 'v2'
	}
	v1.add_child(mut v2)
	assert v1.child? == IGameObject(v2)
	assert v1.last_child? == IGameObject(v2)
}
