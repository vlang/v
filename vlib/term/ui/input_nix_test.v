module ui

fn test_escape_sequence_parses_csi_u_plain_key() {
	seq := '\x1b[97;1u'
	event, len := escape_sequence(seq)
	assert len == seq.len
	assert event.typ == .key_down
	assert event.code == .a
	assert event.modifiers.is_empty()
	assert event.utf8 == seq
}

fn test_escape_sequence_parses_csi_u_plain_key_without_modifier_param() {
	seq := '\x1b[27u'
	event, len := escape_sequence(seq)
	assert len == seq.len
	assert event.typ == .key_down
	assert event.code == .escape
	assert event.modifiers.is_empty()
	assert event.utf8 == seq
}

fn test_escape_sequence_parses_csi_u_modified_key() {
	seq := '\x1b[97;5u'
	event, len := escape_sequence(seq)
	assert len == seq.len
	assert event.typ == .key_down
	assert event.code == .a
	assert event.modifiers.has(.ctrl)
	assert !event.modifiers.has(.shift)
	assert !event.modifiers.has(.alt)
	assert event.utf8 == seq
}

fn test_escape_sequence_parses_csi_u_key_release() {
	seq := '\x1b[97;1:3u'
	event, len := escape_sequence(seq)
	assert len == seq.len
	assert event.typ == .key_up
	assert event.code == .a
	assert event.modifiers.is_empty()
	assert event.utf8 == seq
}

fn test_escape_sequence_parses_csi_u_key_repeat_with_text() {
	seq := '\x1b[97;1:2;97u'
	event, len := escape_sequence(seq)
	assert len == seq.len
	assert event.typ == .key_down
	assert event.code == .a
	assert event.ascii == `a`
	assert event.utf8 == 'a'
}

fn test_escape_sequence_parses_csi_u_shifted_text_with_associated_text() {
	seq := '\x1b[97;2;65u'
	event, len := escape_sequence(seq)
	assert len == seq.len
	assert event.typ == .key_down
	assert event.code == .a
	assert event.ascii == `A`
	assert event.modifiers == .shift
	assert event.utf8 == 'A'
}

fn test_escape_sequence_parses_csi_u_enter_key() {
	seq := '\x1b[13;1u'
	event, len := escape_sequence(seq)
	assert len == seq.len
	assert event.typ == .key_down
	assert event.code == .enter
	assert event.utf8 == seq
}

fn test_escape_sequence_parses_modify_other_keys_sequence() {
	seq := '\x1b[27;3;97~'
	event, len := escape_sequence(seq)
	assert len == seq.len
	assert event.typ == .key_down
	assert event.code == .a
	assert event.modifiers == .alt
	assert event.utf8 == seq
}

fn test_escape_sequence_parses_modified_special_key_sequence() {
	seq := '\x1b[3;5~'
	event, len := escape_sequence(seq)
	assert len == seq.len
	assert event.typ == .key_down
	assert event.code == .delete
	assert event.modifiers == .ctrl
	assert event.utf8 == seq
}

fn test_escape_sequence_parses_modified_special_key_release_sequence() {
	seq := '\x1b[1;5:3A'
	event, len := escape_sequence(seq)
	assert len == seq.len
	assert event.typ == .key_up
	assert event.code == .up
	assert event.modifiers == .ctrl
	assert event.utf8 == seq
}
