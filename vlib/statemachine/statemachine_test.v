import statemachine

struct MyReceiver {
mut:
	data []string
}

fn test_statemachine_works_when_single_transition() {
	mut receiver := &MyReceiver{}
	mut s := statemachine.new()
	s.add_state('A', on_state_entry, on_state_run, on_state_exit)
	s.add_state('B', on_state_entry, on_state_run, on_state_exit)
	s.add_transition('A', 'B', condition_transition)
	s.run(receiver)

	assert receiver.data.len == 3
}

fn test_statemachine_works_when_typical() {
	mut receiver := &MyReceiver{}
	mut s := statemachine.new()
	s.add_state('A', on_state_entry, on_state_run, on_state_exit)
	s.add_state('B', on_state_entry, on_state_run, on_state_exit)
	s.add_transition('A', 'B', condition_transition)
	s.run(receiver)

	assert receiver.data[0] == 'on_state_exit'
	assert receiver.data[1] == 'on_state_entry'
	assert receiver.data[2] == 'on_state_run'
}

fn test_statemachine_works_when_final_state() {
	mut receiver := &MyReceiver{}
	mut s := statemachine.new()
	s.add_state('A', on_state_entry, on_state_run, on_state_exit)
	s.add_state('B', on_state_entry, on_state_run, on_state_exit)
	s.add_transition('A', 'B', condition_transition)
	s.run(receiver)
	s.run(receiver)

	assert receiver.data.len == 3
}

fn on_state_entry(mut receiver MyReceiver) {
	receiver.data << 'on_state_entry'
}

fn on_state_run(mut receiver MyReceiver) {
	receiver.data << 'on_state_run'
}

fn on_state_exit(mut receiver MyReceiver) {
	receiver.data << 'on_state_exit'
}

fn condition_transition(receiver &MyReceiver) bool {
	return true
}
