import datatypes.fsm

struct MyReceiver {
mut:
	data []string
}

fn default_setup() (MyReceiver, fsm.StateMachine) {
	mut receiver := MyReceiver{}
	mut s := fsm.new()
	s.add_state('A', on_state_entry, on_state_run, on_state_exit)
	s.add_state('B', on_state_entry, on_state_run, on_state_exit)
	s.add_transition('A', 'B', condition_transition)
	return receiver, s
}

fn test_statemachine_number_of_callbacks_correct_when_single_transition() {
	mut receiver, mut s := default_setup()

	s.run(receiver)

	assert receiver.data.len == 3
}

fn test_statemachine_sequence_works_when_typical() {
	mut receiver, mut s := default_setup()

	s.run(receiver)

	assert receiver.data[0] == 'on_state_exit'
	assert receiver.data[1] == 'on_state_entry'
	assert receiver.data[2] == 'on_state_run'
}

fn test_statemachine_works_when_final_state() {
	mut receiver, mut s := default_setup()

	s.run(receiver)
	s.run(receiver)

	assert receiver.data.len == 3
}

fn on_state_entry(mut receiver MyReceiver, from string, to string) {
	receiver.data << 'on_state_entry'
}

fn on_state_run(mut receiver MyReceiver, from string, to string) {
	receiver.data << 'on_state_run'
}

fn on_state_exit(mut receiver MyReceiver, from string, to string) {
	receiver.data << 'on_state_exit'
}

fn condition_transition(receiver &MyReceiver, from string, to string) bool {
	return true
}
