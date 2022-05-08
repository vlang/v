struct Player {
mut:
	x     int
	y     int
	level int
}

struct Enemy {
mut:
	x      int
	y      int
	damage f64
}

type PlayerOrEnemy = Enemy | Player

fn test_match_reference_sumtype_var() {
	mut entity := PlayerOrEnemy(Player{10, 12, 3})

	x_move := 11
	y_move := 22

	mut ref := &entity

	match mut entity {
		Player {
			entity.x += x_move
			entity.y += y_move

			println('Player is moved to $entity.x, $entity.y and its level is $entity.level')
		}
		Enemy {
			entity.x += x_move
			entity.y += y_move

			println('Enemy is moved to $entity.x, $entity.y and its damage is $entity.damage')
		}
	}

	println(typeof(ref).name)
	assert true
}
