struct Model {}

struct Process {
mut:
	on_done ?fn ()
}

fn (m Model) run(mut p Process) bool {
	p.on_done = m.on_done
	return p.do_work()
}

fn (m Model) on_done() {}

fn (p Process) do_work() bool {
	if f := p.on_done {
		f()
		return true
	}
	return false
}

fn test_optional_void_callback_with_same_named_method() {
	m := Model{}
	mut p := Process{}
	assert !p.do_work()
	assert m.run(mut p)
	assert p.do_work()
	p.on_done = none
	assert !p.do_work()
}

struct ValueModel {
	value int
}

struct ValueProcess {
mut:
	on_value ?fn () int
}

fn (m ValueModel) run(mut p ValueProcess) int {
	p.on_value = m.on_value
	return p.do_work()
}

fn (m ValueModel) on_value() int {
	return m.value
}

fn (p ValueProcess) do_work() int {
	if f := p.on_value {
		return f()
	}
	return -1
}

fn test_optional_callback_preserves_receiver_with_same_named_method() {
	first := ValueModel{
		value: 42
	}
	second := ValueModel{
		value: 17
	}
	mut p := ValueProcess{}
	assert p.do_work() == -1
	assert first.run(mut p) == 42
	assert p.do_work() == 42
	assert second.run(mut p) == 17
	assert p.do_work() == 17
	p.on_value = none
	assert p.do_work() == -1
}
