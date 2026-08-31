// vtest build: present_openssl?
module quic

fn test_qpack_max_table_capacity_from_settings_default_zero() {
	assert qpack_max_table_capacity_from_settings([]H3Setting{}) == 0
}

fn test_qpack_max_table_capacity_from_settings_present() {
	settings := [
		H3Setting{
			identifier: qpack_settings_max_table_capacity_id
			value:      4096
		},
	]
	assert qpack_max_table_capacity_from_settings(settings) == 4096
}

fn test_qpack_blocked_streams_from_settings_default_zero() {
	assert qpack_blocked_streams_from_settings([]H3Setting{}) == 0
}

fn test_qpack_blocked_streams_from_settings_present() {
	settings := [
		H3Setting{
			identifier: qpack_settings_blocked_streams_id
			value:      16
		},
	]
	assert qpack_blocked_streams_from_settings(settings) == 16
}

fn test_qpack_settings_ignore_unrelated_identifiers() {
	settings := [H3Setting{ identifier: 0x06, value: 999 }] // MAX_FIELD_SECTION_SIZE, unrelated
	assert qpack_max_table_capacity_from_settings(settings) == 0
	assert qpack_blocked_streams_from_settings(settings) == 0
}
