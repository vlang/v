module fsm

pub type EventHandlerFn = fn (receiver voidptr)

pub type ConditionFn = fn (receiver voidptr) bool

struct State {
mut:
	entry_handler EventHandlerFn
	run_handler   EventHandlerFn
	exit_handler  EventHandlerFn
}

struct Transition {
mut:
	to                string
	condition_handler ConditionFn = voidptr(0)
}

pub struct StateMachine {
mut:
	states        map[string]State
	transitions   map[string]Transition
	current_state string
}

pub fn new() StateMachine {
	return StateMachine{}
}

pub fn (mut s StateMachine) add_state(name string, entry EventHandlerFn, run EventHandlerFn, exit EventHandlerFn) {
	s.states[name] = State{
		entry_handler: entry
		run_handler: run
		exit_handler: exit
	}
	if s.states.len == 1 {
		s.current_state = name
	}
}

pub fn (mut s StateMachine) add_transition(from string, to string, condition_handler ConditionFn) {
	s.transitions[from] = Transition{
		to: to
		condition_handler: condition_handler
	}
}

pub fn (mut s StateMachine) run(receiver voidptr) {
	for from_state, transition in s.transitions {
		if from_state == s.current_state {
			if transition.condition_handler(receiver) {
				s.change_state(receiver, s.transitions[from_state].to)
			}
			s.states[s.current_state].run_handler(receiver)
		}
	}
}

pub fn (mut s StateMachine) change_state(receiver voidptr, newstate string) {
	mut current_state := s.current_state
	s.states[current_state].exit_handler(receiver)
	current_state = newstate
	s.states[current_state].entry_handler(receiver)
	s.current_state = current_state
}
