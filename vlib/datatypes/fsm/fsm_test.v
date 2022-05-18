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

fn test_statemachine_number_of_callbacks_correct_when_single_transition() ? {
	mut receiver, mut s := default_setup()

	s.run(receiver)?

	assert receiver.data.len == 3
}

fn test_statemachine_sequence_works_when_typical() ? {
	mut receiver, mut s := default_setup()

	s.run(receiver)?

	assert receiver.data[0] == 'on_state_exit: A -> B'
	assert receiver.data[1] == 'on_state_entry: A -> B'
	assert receiver.data[2] == 'on_state_run: A -> B'
}

fn test_statemachine_works_when_final_state() ? {
	mut receiver, mut s := default_setup()

	// current state `A`, with a possible transition to `B`:
	s.run(receiver)? // run should not error here

	// Note: run will now return error, because for state `B`,
	// there are no more transitions:
	s.run(receiver) or { assert true }
	s.run(receiver) or { assert true }

	assert receiver.data.len == 5
	assert receiver.data[2] == 'on_state_run: A -> B'
	assert receiver.data[3] == 'on_state_run: B -> B'
	assert receiver.data[4] == 'on_state_run: B -> B'
}

fn test_simple_loop() ? {
	mut receiver, mut s := default_setup()

	// Add a transition back to `A` too:
	s.add_transition('B', 'A', condition_transition)

	// Run the FSM for a while.
	// It will loop forever between `A` -> `B` -> `A` -> `B` ...
	for _ in 0 .. 100 {
		s.run(receiver) or { assert false }
	}
	assert receiver.data[1] == 'on_state_entry: A -> B'
	assert receiver.data[4] == 'on_state_entry: B -> A'
	assert receiver.data[7] == 'on_state_entry: A -> B'
	assert receiver.data[10] == 'on_state_entry: B -> A'
}

// Helper functions

fn on_state_entry(mut receiver MyReceiver, from string, to string) {
	receiver.data << 'on_state_entry: ' + from + ' -> ' + to
}

fn on_state_run(mut receiver MyReceiver, from string, to string) {
	receiver.data << 'on_state_run: ' + from + ' -> ' + to
}

fn on_state_exit(mut receiver MyReceiver, from string, to string) {
	receiver.data << 'on_state_exit: ' + from + ' -> ' + to
}

fn condition_transition(receiver &MyReceiver, from string, to string) bool {
	// The condition callback is a way to provide input to the FSM
	// It can return true or false, based on external events/state.
	// For these tests however, that is not used, and it simply always returns true.
	return true
}
