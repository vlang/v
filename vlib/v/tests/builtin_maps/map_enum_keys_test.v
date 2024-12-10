// This tests that V can import and use enums from other modules,
// and that vfmt can handle all edge cases.
import geometry { Shape }

enum Token {
	aa = 2
	bb
	cc
}

fn test_map_with_enum_keys() {
	mut m := map[Token]string{}
	m[.aa] = 'abc'
	m[Token.bb] = 'def'
	assert m[Token.aa] == 'abc'
	assert m[.bb] == 'def'
	s := '${m}'
	assert s == "{aa: 'abc', bb: 'def'}"
	println(m)
}

fn test_map_with_imported_enum_keys() {
	mut fm := map[geometry.Form3D]string{}
	fm[.cube] = 'a cube'
	fm[geometry.Form3D.sphere] = 'a sphere'
	assert fm[.invalid] == ''
	assert geometry.Form3D.cube in fm
	assert fm[.sphere] == 'a sphere'
}

fn test_map_with_selective_imported_enum_keys() {
	mut shapes := map[Shape]string{}
	shapes[.circle] = 'a circle'
	shapes[Shape.rectangle] = 'a rectangle'
	assert shapes[.circle] == 'a circle'
	shapes.delete(Shape.circle)
	assert Shape.circle !in shapes
}
