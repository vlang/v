struct MapPostfixStats {
mut:
	mana int
	xp   int
}

struct MapPostfixHero {
mut:
	stats MapPostfixStats
	level int
}

struct MapPostfixGame {
mut:
	heroes map[int]MapPostfixHero
}

// A map value has no address to take, but `m[k].field++` updates the entry in
// place, as `m[k].field += 1` does.
fn test_postfix_on_map_value_field() {
	mut heroes := {
		'a': MapPostfixHero{}
	}
	heroes['a'].level++
	heroes['a'].level++
	heroes['a'].stats.mana--
	heroes['a'].stats.xp++
	assert heroes['a'].level == 2
	assert heroes['a'].stats.mana == -1
	assert heroes['a'].stats.xp == 1
	assert heroes.len == 1
}

fn test_postfix_on_map_value_field_of_struct_field() {
	mut game := MapPostfixGame{
		heroes: {
			1: MapPostfixHero{
				level: 7
			}
		}
	}
	game.heroes[1].stats.mana--
	game.heroes[1].stats.xp++
	game.heroes[1].level++
	assert game.heroes[1].stats.mana == -1
	assert game.heroes[1].stats.xp == 1
	assert game.heroes[1].level == 8
	assert game.heroes.len == 1
}

fn map_postfix_bump(mut game MapPostfixGame) {
	game.heroes[1].level++
	game.heroes[1].stats.mana--
}

fn test_postfix_on_map_value_field_of_mut_param() {
	mut game := &MapPostfixGame{
		heroes: {
			1: MapPostfixHero{}
		}
	}
	map_postfix_bump(mut game)
	map_postfix_bump(mut game)
	assert game.heroes[1].level == 2
	assert game.heroes[1].stats.mana == -2
}

fn test_postfix_on_map_value_array_element() {
	mut rows := {
		'a': [1, 2, 3]
	}
	rows['a'][1]++
	rows['a'][2]--
	assert rows['a'] == [1, 3, 2]
}

struct MapPostfixCounter {
mut:
	calls int
}

fn map_postfix_next_key(mut counter MapPostfixCounter) string {
	counter.calls++
	return 'missing'
}

fn test_postfix_inserts_a_missing_map_value_before_updating_fields() {
	mut counter := MapPostfixCounter{}
	mut heroes := map[string]MapPostfixHero{}
	heroes[map_postfix_next_key(mut counter)].level++
	assert counter.calls == 1
	assert heroes.len == 1
	assert heroes['missing'].level == 1
	heroes[map_postfix_next_key(mut counter)].stats.mana--
	assert counter.calls == 2
	assert heroes['missing'].stats.mana == -1
	mut nested := map[string]MapPostfixHero{}
	nested['missing'].stats.xp--
	assert nested.len == 1
	assert nested['missing'].stats.xp == -1
}

fn test_postfix_inserts_missing_map_value_of_struct_and_mut_param() {
	mut game := &MapPostfixGame{}
	map_postfix_bump(mut game)
	assert game.heroes.len == 1
	assert game.heroes[1].level == 1
	assert game.heroes[1].stats.mana == -1
}
