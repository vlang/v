struct Hero {
mut:
	skills  [4]int
	choices []int
}

struct Party {
mut:
	leader Hero
	team   [2]Hero
	names  []string
}

fn upgraded_skill_delta(hero Hero, id int) int {
	mut upgraded := hero
	upgraded.skills[id] = 3
	return upgraded.skills[id] - hero.skills[id]
}

fn trained_party_total(party Party) int {
	mut trained := party
	trained.leader.skills[0] = 5
	trained.team[1].skills[2] = 7
	return trained.leader.skills[0] + trained.team[1].skills[2]
}

fn test_fixed_array_field_of_a_mutable_copy_is_written_without_touching_the_source() {
	hero := Hero{
		skills:  [1, 0, 0, 0]!
		choices: [7]
	}
	assert upgraded_skill_delta(hero, 0) == 2
	assert upgraded_skill_delta(hero, 3) == 3
	assert hero.skills == [1, 0, 0, 0]!
	assert hero.choices == [7]
}

fn test_fixed_arrays_nested_by_value_in_a_mutable_copy_are_written_without_touching_the_source() {
	party := Party{
		leader: Hero{
			skills: [1, 1, 1, 1]!
		}
		names:  ['a']
	}
	assert trained_party_total(party) == 12
	assert party.leader.skills == [1, 1, 1, 1]!
	assert party.team[1].skills == [0, 0, 0, 0]!
}
