interface FieldSound {
mut:
	play(n int) int
}

struct FieldWorld {
mut:
	base  int
	calls int
}

fn (mut w FieldWorld) play(n int) int {
	w.calls++
	return w.base + n
}

interface FieldSession {
mut:
	sw FieldSound
	frame() int
}

struct FieldSessionLocal {
mut:
	sw FieldSound
	n  int
}

fn (mut s FieldSessionLocal) frame() int {
	return s.n
}

fn play_through(mut session FieldSession, n int) int {
	return session.sw.play(n)
}

fn test_method_called_on_a_field_read_through_an_interface() {
	mut first := &FieldWorld{
		base: 10
	}
	mut local := &FieldSessionLocal{
		sw: FieldSound(first)
	}
	mut session := FieldSession(local)
	assert session.sw.play(1) == 11
	// The field of the object changes after the interface value was made.
	mut second := &FieldWorld{
		base: 40
	}
	local.sw = FieldSound(second)
	assert session.sw.play(2) == 42
	assert play_through(mut session, 3) == 43
	assert first.calls == 1
	assert second.calls == 2
}
