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

fn on_state_entry(mut receiver MyReceiver, from string, to string) {
	println('on_state_entry: ' + from + ' -> ' + to)
}

fn on_state_run(mut receiver MyReceiver, from string, to string) {
	println('on_state_run: ' + from + ' -> ' + to)
}

fn on_state_exit(mut receiver MyReceiver, from string, to string) {
	println('on_state_exit: ' + from + ' -> ' + to)
}

fn condition_transition(receiver &MyReceiver, from string, to string) bool {
	return true
}
