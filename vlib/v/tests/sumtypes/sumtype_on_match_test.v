struct MotionEvent {}

struct ButtonEvent {}

struct WheelEvent {}

type Event = ButtonEvent | MotionEvent | WheelEvent

type GameEvent = ButtonEvent | MotionEvent

fn test_main() {
	be := ButtonEvent{}
	assert handle_event(be) == true
	e := Event(be)
	assert handle_event(e) == true
	match e {
		MotionEvent {
			assert handle_game_event(e) == true
		}
		ButtonEvent {
			assert handle_game_event(e) == true
		}
		else {}
	}
}

fn handle_game_event(ge GameEvent) bool {
	is_button_event := ge is ButtonEvent
	return is_button_event
}

fn handle_event(e Event) bool {
	is_button_event := e is ButtonEvent
	return is_button_event
}
