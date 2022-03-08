import datatypes.fsm

struct MyReceiver {
mut:
	data []string
}

fn main() {
	mut receiver := &MyReceiver{}
	mut s := fsm.new()
	s.add_state('A', on_state_entry, on_state_run, on_state_exit)
	s.add_state('B', on_state_entry, on_state_run, on_state_exit)
	s.add_transition('A', 'B', condition_transition)
	s.run(receiver)
	s.run(receiver)
	s.run(receiver)
}

fn on_state_entry(mut receiver MyReceiver) {
	println('on_state_entry')
}

fn on_state_run(mut receiver MyReceiver) {
	println('on_state_run')
}

fn on_state_exit(mut receiver MyReceiver) {
	println('on_state_exit')
}

fn condition_transition(receiver &MyReceiver) bool {
	return true
}
