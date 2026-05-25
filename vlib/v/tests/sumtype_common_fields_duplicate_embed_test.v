struct CommonTrace {
mut:
	name   string
	marker int
}

struct TraceWithDuplicateMarker {
	CommonTrace
mut:
	marker int
}

struct TraceWithMarker {
	CommonTrace
}

struct TraceWithoutMarker {
mut:
	name string
}

type Trace = TraceWithDuplicateMarker | TraceWithMarker | TraceWithoutMarker

fn test_duplicate_embedded_fields_do_not_create_false_common_sumtype_field() {
	traces := [
		Trace(TraceWithDuplicateMarker{
			name:   'duplicate'
			marker: 1
		}),
		Trace(TraceWithMarker{
			name: 'marker'
		}),
		Trace(TraceWithoutMarker{
			name: 'plain'
		}),
	]

	assert traces[0].name == 'duplicate'
	assert traces[1].name == 'marker'
	assert traces[2].name == 'plain'
}
