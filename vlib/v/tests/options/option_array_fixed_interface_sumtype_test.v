interface Speaker {}

struct SpeakerA {}

struct SpeakerHolder {
	speakers [2]?Speaker
}

struct EventA {
	a u32
}

struct EventB {
	b u32
}

type Event = EventA | EventB

struct EventHolder {
	events [2]?Event
}

fn test_fixed_array_of_option_interface_and_sumtype() {
	holder := SpeakerHolder{
		speakers: [?Speaker(SpeakerA{}), none]!
	}
	assert holder.speakers[0] != none
	assert holder.speakers[1] == none

	event_holder := EventHolder{
		events: [?Event(EventA{
			a: 1
		}), none]!
	}
	assert event_holder.events[0] != none
	assert event_holder.events[1] == none
}
