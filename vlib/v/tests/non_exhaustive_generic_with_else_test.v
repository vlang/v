import non_exhaustive_types

fn generic_status_value[T](s T) int {
	return match s {
		non_exhaustive_types.Status.ready {
			1
		}
		non_exhaustive_types.Status.running {
			2
		}
		non_exhaustive_types.Status.failed {
			3
		}
		else {
			0
		}
	}
}

fn generic_event_value[T](e T) int {
	return match e {
		non_exhaustive_types.Connected {
			1
		}
		non_exhaustive_types.Disconnected {
			2
		}
		non_exhaustive_types.MessageReceived {
			3
		}
		else {
			0
		}
	}
}

fn test_generic_non_exhaustive_with_else() {
	assert generic_status_value(non_exhaustive_types.Status.ready) == 1
	assert generic_status_value(non_exhaustive_types.Status.running) == 2
	assert generic_status_value(non_exhaustive_types.Status.failed) == 3
	assert generic_event_value(non_exhaustive_types.Event(non_exhaustive_types.Connected{})) == 1
	assert generic_event_value(non_exhaustive_types.Event(non_exhaustive_types.Disconnected{})) == 2
	assert generic_event_value(non_exhaustive_types.Event(non_exhaustive_types.MessageReceived{})) == 3
}
