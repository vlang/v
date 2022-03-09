module fsm

pub type EventHandlerFn = fn (receiver voidptr, from string, to string)

pub type ConditionFn = fn (receiver voidptr, from string, to string) bool

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
	transitions   map[string][]Transition
	current_state string
}

pub fn new() StateMachine {
	return StateMachine{}
}

pub fn (mut s StateMachine) set_state(name string) ? {
	if name in s.states {
		s.current_state = name
	}
	return error('unknown state: $name')
}

pub fn (mut s StateMachine) get_state() string {
	return s.current_state
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
	t := Transition{
		to: to
		condition_handler: condition_handler
	}
	if from in s.transitions {
		s.transitions[from] << t
		return
	}
	s.transitions[from] = [t]
}

pub fn (mut s StateMachine) run(receiver voidptr) ? {
	from_state := s.current_state
	mut to_state := s.current_state
	if transitions := s.transitions[s.current_state] {
		for transition in transitions {
			if transition.condition_handler(receiver, from_state, transition.to) {
				s.change_state(receiver, transition.to)
				to_state = transition.to
				break
			}
		}
	} else {
		s.states[s.current_state].run_handler(receiver, from_state, to_state)
		return error('no more transitions')
	}
	s.states[s.current_state].run_handler(receiver, from_state, to_state)
}

fn (mut s StateMachine) change_state(receiver voidptr, newstate string) {
	s.states[s.current_state].exit_handler(receiver, s.current_state, newstate)
	s.states[newstate].entry_handler(receiver, s.current_state, newstate)
	s.current_state = newstate
}
