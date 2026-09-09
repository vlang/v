interface ILevel {
mut:
	uid    int
	parent int
}

struct Level0 {
mut:
	uid    int
	parent int
	x      f32
}

struct Level1 {
	Level0
mut:
	y f32
}

struct Level2 {
	Level1
mut:
	z f32
}

type AliasLevel2 = Level2

fn mutate_level(mut level ILevel) {
	level.uid = 11
	level.parent = 22
}

fn test_interface_cast_with_deeply_embedded_fields() {
	mut level := Level2{}
	mutate_level(mut level)
	assert level.uid == 11
	assert level.parent == 22
}

fn test_interface_cast_with_deeply_embedded_fields_through_alias() {
	mut level := AliasLevel2{}
	mutate_level(mut level)
	assert level.uid == 11
	assert level.parent == 22
}
