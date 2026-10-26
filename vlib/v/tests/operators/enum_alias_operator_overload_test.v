// An enum cannot overload operators, but an alias of one can, and its
// operators call the overloads instead of the enum's integer arithmetic.
enum Note {
	c = 1
	d
	e
}

type Pitch = Note

fn (a Pitch) + (b Pitch) Pitch {
	return Pitch(unsafe { Note(int(Note(a)) + int(Note(b)) + 1) })
}

// == groups the notes in pairs: `c` and `d`, then `e`.
fn (a Pitch) == (b Pitch) bool {
	return (int(Note(a)) - 1) / 2 == (int(Note(b)) - 1) / 2
}

// < orders the notes in reverse.
fn (a Pitch) < (b Pitch) bool {
	return int(Note(a)) > int(Note(b))
}

fn test_enum_alias_operator_overloads_are_called() {
	c := Pitch(Note.c)
	d := Pitch(Note.d)
	e := Pitch(Note.e)
	// The enum's integer arithmetic would give `Note.d` (1 + 1).
	assert Note(c + c) == .e
	mut sum := c
	sum += c
	assert Note(sum) == .e
	assert c == d
	assert !(c != d)
	assert c != e
	assert d < c
	assert !(c < d)
}
