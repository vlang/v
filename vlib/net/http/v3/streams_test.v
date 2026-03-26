module v3

// Tests for HTTP/3 unidirectional stream management and encoder instruction generation.

fn test_stream_type_constants() {
	assert control_stream_type == u64(0x00)
	assert push_stream_type == u64(0x01)
	assert qpack_encoder_stream_type == u64(0x02)
	assert qpack_decoder_stream_type == u64(0x03)
}

fn test_uni_stream_manager_initial_state() {
	m := UniStreamManager{}
	assert m.control_stream_id == i64(-1)
	assert m.encoder_stream_id == i64(-1)
	assert m.decoder_stream_id == i64(-1)
	assert m.peer_control_stream_id == i64(-1)
	assert m.peer_encoder_stream_id == i64(-1)
	assert m.peer_decoder_stream_id == i64(-1)
}

fn test_identify_peer_control_stream() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(3, control_stream_type) or {
		assert false, 'unexpected error: ${err}'
		return
	}
	assert m.peer_control_stream_id == i64(3)
}

fn test_identify_peer_encoder_stream() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(7, qpack_encoder_stream_type) or {
		assert false, 'unexpected error: ${err}'
		return
	}
	assert m.peer_encoder_stream_id == i64(7)
}

fn test_identify_peer_decoder_stream() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(11, qpack_decoder_stream_type) or {
		assert false, 'unexpected error: ${err}'
		return
	}
	assert m.peer_decoder_stream_id == i64(11)
}

fn test_identify_peer_stream_duplicate_control_error() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(3, control_stream_type) or {
		assert false, 'first call should succeed'
		return
	}
	m.identify_peer_stream(7, control_stream_type) or {
		assert err.msg().contains('duplicate')
		return
	}
	assert false, 'expected duplicate stream error'
}

fn test_identify_peer_stream_duplicate_encoder_error() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(3, qpack_encoder_stream_type) or {
		assert false, 'first call should succeed'
		return
	}
	m.identify_peer_stream(7, qpack_encoder_stream_type) or {
		assert err.msg().contains('duplicate')
		return
	}
	assert false, 'expected duplicate stream error'
}

fn test_identify_peer_stream_duplicate_decoder_error() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(3, qpack_decoder_stream_type) or {
		assert false, 'first call should succeed'
		return
	}
	m.identify_peer_stream(7, qpack_decoder_stream_type) or {
		assert err.msg().contains('duplicate')
		return
	}
	assert false, 'expected duplicate stream error'
}

fn test_identify_peer_stream_unknown_type() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(3, u64(0xFF)) or {
		assert err.msg().contains('unknown')
		return
	}
	assert false, 'expected unknown stream type error'
}

fn test_identify_all_three_peer_streams() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(3, control_stream_type) or {
		assert false, 'control failed: ${err}'
		return
	}
	m.identify_peer_stream(7, qpack_encoder_stream_type) or {
		assert false, 'encoder failed: ${err}'
		return
	}
	m.identify_peer_stream(11, qpack_decoder_stream_type) or {
		assert false, 'decoder failed: ${err}'
		return
	}
	assert m.peer_control_stream_id == i64(3)
	assert m.peer_encoder_stream_id == i64(7)
	assert m.peer_decoder_stream_id == i64(11)
}

fn test_encode_stream_type_control() {
	data := encode_stream_type(control_stream_type) or {
		assert false, 'encode failed: ${err}'
		return
	}
	assert data == [u8(0x00)]
}

fn test_encode_stream_type_encoder() {
	data := encode_stream_type(qpack_encoder_stream_type) or {
		assert false, 'encode failed: ${err}'
		return
	}
	assert data == [u8(0x02)]
}

fn test_encode_stream_type_decoder() {
	data := encode_stream_type(qpack_decoder_stream_type) or {
		assert false, 'encode failed: ${err}'
		return
	}
	assert data == [u8(0x03)]
}

fn test_has_peer_control_stream_false_initially() {
	m := UniStreamManager{}
	assert m.has_peer_control_stream() == false
}

fn test_has_peer_control_stream_true_after_identify() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(3, control_stream_type) or { return }
	assert m.has_peer_control_stream() == true
}

fn test_all_peer_streams_identified_false_partial() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(3, control_stream_type) or { return }
	assert m.all_peer_streams_identified() == false
}

fn test_all_peer_streams_identified_true_when_complete() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(3, control_stream_type) or { return }
	m.identify_peer_stream(7, qpack_encoder_stream_type) or { return }
	m.identify_peer_stream(11, qpack_decoder_stream_type) or { return }
	assert m.all_peer_streams_identified() == true
}

fn test_generate_set_capacity_instruction() {
	data := generate_set_capacity_instruction(4096)
	assert data.len > 0
	decoded, _ := decode_set_dynamic_table_capacity(data) or {
		assert false, 'decode failed: ${err}'
		return
	}
	assert decoded.capacity == 4096
}

fn test_generate_set_capacity_instruction_zero() {
	data := generate_set_capacity_instruction(0)
	assert data.len > 0
	decoded, _ := decode_set_dynamic_table_capacity(data) or {
		assert false, 'decode failed: ${err}'
		return
	}
	assert decoded.capacity == 0
}

fn test_generate_encoder_instructions_static_name() {
	header := HeaderField{
		name:  ':path'
		value: '/test/resource'
	}
	instructions := generate_encoder_instruction(header)
	assert instructions.len > 0
	assert (instructions[0] & 0x80) != 0
}

fn test_generate_encoder_instructions_literal() {
	header := HeaderField{
		name:  'x-custom-header'
		value: 'custom-value'
	}
	instructions := generate_encoder_instruction(header)
	assert instructions.len > 0
	assert (instructions[0] & 0x40) != 0
}

fn test_generate_encoder_instruction_roundtrip_static() {
	header := HeaderField{
		name:  ':authority'
		value: 'example.com'
	}
	data := generate_encoder_instruction(header)
	decoded, _ := decode_insert_with_name_ref(data) or {
		assert false, 'decode failed: ${err}'
		return
	}
	assert decoded.is_static == true
	assert decoded.value == 'example.com'
}

fn test_generate_encoder_instruction_roundtrip_literal() {
	header := HeaderField{
		name:  'x-test-key'
		value: 'test-val'
	}
	data := generate_encoder_instruction(header)
	decoded, _ := decode_insert_without_name_ref(data) or {
		assert false, 'decode failed: ${err}'
		return
	}
	assert decoded.name == 'x-test-key'
	assert decoded.value == 'test-val'
}
