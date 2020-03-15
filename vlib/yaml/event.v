module yaml


struct VersionDriective{
	major i8
	minor i8
}

struct TagDirective{
	handle []byte
	prefix []byte
}

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

struct YamlPointer{
	index int
	line int
	column int
}

struct Event{
	typ EventKind
	start YamlPointer 
	end YamlPointer
	version 
	tag []byte
	anchor []byte
	implicit bool
	quoted_implicit bool
	
}

fn (e &Event) parser_state_machine{
	match 
}