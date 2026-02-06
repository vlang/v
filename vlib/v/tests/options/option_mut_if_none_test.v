module main

@[heap]
interface IGameObject {
mut:
	name       string
	parent     ?&IGameObject
	next       ?&IGameObject
	child      ?&IGameObject
	last_child ?&IGameObject
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

fn test_main() {
	mut v1 := &GameObject{
		name: 'v1'
	}
	v1.next = &GameObject{
		name: 'v2'
	}

	mut next := v1.next
	for {
		if mut next != none {
			eprintln(next.name)
			assert next.name == 'v2'
			next = next.next
		} else {
			break
		}
	}
	assert next == none
}
