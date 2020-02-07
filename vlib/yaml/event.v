module yaml

enum Event{
	no_event
	stream_start
	document_start
	document_end
	mapping_end
	sequence_start
	sequence_end
	scalar
	plain_scalar
	single_quoted_scalar
	dobule_quotes_scaler
	literal_scalar
	folded_scaler
	any_scalar
	alias
	stream_end
}

