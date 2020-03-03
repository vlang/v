module yaml

enum EventKind{
	no_event
	stream_start
	stream_end
	document_start
	document_end
	mapping_start
	mapping_end
	sequence_start
	sequence_end
	scalar
	alias
}

enum ScalarKind{
	any
	plain
	single_quoted
	double_quoted
	literal
	folded
}

enum SequenceStyle{
	ant
	block
	flow
}

enum MappingStyle{
	any
	block
	flow
}

struct Event{
	
}