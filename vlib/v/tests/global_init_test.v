@[has_globals]
module main

interface IGameObject {
mut:
	name string
}

struct Game {
mut:
	objects        []IGameObject
	delete_objects []IGameObject
}

__global (
	game Game
)

fn test_main() {
	println('game: ${game}')
	assert game.objects.len == 0
	assert game.delete_objects.len == 0
}
