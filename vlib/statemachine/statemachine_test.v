import statemachine

struct FakeReceiver {
mut:
	data []string
}

fn test_statemachine_works_when_single_transition() {
	mut receiver := &FakeReceiver{}
	mut s := statemachine.new()
	s.add_state("A",on_test_entry, on_test_run, on_test_exit)
	s.add_state("B",on_test_entry, on_test_run, on_test_exit)
	s.add_transition("A", "B", condition_a_b)
	s.run(receiver)

	assert receiver.data.len == 3
}

fn test_statemachine_works_when_typical() {
	mut receiver := &FakeReceiver{}
	mut s := statemachine.new()
	s.add_state("A",on_test_entry, on_test_run, on_test_exit)
	s.add_state("B",on_test_entry, on_test_run, on_test_exit)
	s.add_transition("A", "B", condition_a_b)
	s.run(receiver)

	assert receiver.data[0] == "on_test_exit"
	assert receiver.data[1] == "on_test_run"
	assert receiver.data[2] == "on_test_entry"
}

fn test_statemachine_works_when_final_state() {
	mut receiver := &FakeReceiver{}
	mut s := statemachine.new()
	s.add_state("A",on_test_entry, on_test_run, on_test_exit)
	s.add_state("B",on_test_entry, on_test_run, on_test_exit)
	s.add_transition("A", "B", condition_a_b)
	s.run(receiver)
	s.run(receiver)

	assert receiver.data.len == 3
}

fn on_test_entry(mut receiver &FakeReceiver) {
	receiver.data << "on_test_entry"
}
fn on_test_run(mut receiver &FakeReceiver) {
	receiver.data << "on_test_run"
}
fn on_test_exit(mut receiver &FakeReceiver) {
	receiver.data << "on_test_exit"
}

fn condition_a_b(receiver &FakeReceiver) bool {
	return true
}
