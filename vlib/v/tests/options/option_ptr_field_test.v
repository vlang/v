module main

@[heap]
interface IGameObject {
mut:
	name   string
	parent ?&IGameObject
}

@[heap]
struct GameObject implements IGameObject {
mut:
	name   string
	parent ?&IGameObject
}

struct Ship implements IGameObject {
	GameObject
	speed f32
}

fn test_main() {
	mut world := &GameObject{
		name: 'world'
	}
	mut ship := &Ship{
		name:   'ship'
		parent: world
	}
	assert '${ship}' == "&Ship{
    GameObject: GameObject{
        name: 'ship'
        parent: &Option(IGameObject(GameObject{
            name: 'world'
            parent: &Option(none)
        }))
    }
    speed: 0.0
}"
}
