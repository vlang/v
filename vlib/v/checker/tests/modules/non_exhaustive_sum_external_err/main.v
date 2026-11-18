module main

import amod

fn test_event(e amod.Event) int {
	match e {
		amod.Connected {
			return 1
		}
		amod.Disconnected {
			return 2
		}
		amod.MessageReceived {
			return 3
		}
	}
	return 0
}

fn main() {
	println(test_event(amod.Connected{}))
}
