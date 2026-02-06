module main

import arrays
import x.json2

enum EventType {
	message
	reaction
	unknown
}

struct ResponseStub {
	etyp EventType @[json: 'type']
}

fn count_messages(events []json2.Any) !int {
	return if events.len > 0 {
		arrays.fold(events, 0, fn (acc int, a json2.Any) int {
			typ := json2.decode[ResponseStub](a.str()) or { ResponseStub{.unknown} }.etyp
			res := match typ {
				.message { 1 }
				.reaction { 2 }
				else { none }
			}
			if r := res {
				return acc + r
			}
			return acc
		})
	} else {
		error('empty')
	}
}

fn test_fix_issue_26498() {
	events := [
		json2.Any('{"type":"message"}'),
		json2.Any('{"type":"reaction"}'),
	]
	assert count_messages(events)! == 3
}
