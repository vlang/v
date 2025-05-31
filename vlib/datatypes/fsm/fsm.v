module fsm

pub type EventHandlerFn = fn (receiver voidptr, from string, to string)

pub type ConditionFn = fn (receiver voidptr, from string, to string) bool

fn dummy_event_handler_fn(receiver voidptr, from string, to string) {
}

fn dummy_condition_fn(receiver voidptr, from string, to string) bool {
	return true
}

struct State {
mut:
	entry_handler EventHandlerFn = dummy_event_handler_fn
	run_handler   EventHandlerFn = dummy_event_handler_fn
	exit_handler  EventHandlerFn = dummy_event_handler_fn
}

struct Transition {
mut:
	to                string
	condition_handler ConditionFn = dummy_condition_fn
}

pub struct StateMachine {
mut:
	states        map[string]State
	transitions   map[string][]Transition
	current_state string
}

// StateMachine static method returns a new StateMachine instance.
pub fn new() StateMachine {
	return StateMachine{}
}

// set_state sets the current state of the state machine to the given state by `name`.
pub fn (mut s StateMachine) set_state(name string) ! {
	if name in s.states {
		s.current_state = name
	} else {
		return error('unknown state: ${name}')
	}
}

// get_state returns the current state of the state machine.
pub fn (mut s StateMachine) get_state() string {
	return s.current_state
}

// add_state adds a new state to the state machine.
// It takes the `name` of the state, and three event handlers: `entry`, `run`, and `exit`.
pub fn (mut s StateMachine) add_state(name string, entry EventHandlerFn, run EventHandlerFn, exit EventHandlerFn) {
	s.states[name] = State{
		entry_handler: entry
		run_handler:   run
		exit_handler:  exit
	}
	if s.states.len == 1 {
		s.current_state = name
	}
}

// add_transition adds a new transition to the state machine.
// It takes the `from` and `to` states, and a condition handler.
pub fn (mut s StateMachine) add_transition(from string, to string, condition_handler ConditionFn) {
	t := Transition{
		to:                to
		condition_handler: condition_handler
	}
	if from in s.transitions {
		s.transitions[from] << t
		return
	}
	s.transitions[from] = [t]
}

// run runs the state machine. It takes a `receiver` argument that is passed to the event handlers.
pub fn (mut s StateMachine) run(receiver voidptr) ! {
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
