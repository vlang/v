import non_exhaustive_types

fn test_external_non_exhaustive_with_else() {
	s := non_exhaustive_types.Status.ready
	res := match s {
		.ready {
			1
		}
		.running {
			2
		}
		.failed {
			3
		}
		else {
			0
		}
	}
	assert res == 1
	e := non_exhaustive_types.Event(non_exhaustive_types.Connected{})
	eres := match e {
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
	assert eres == 1
}
