import payloads { Payload }
import responses

fn test_selective_generic_type_argument_in_imported_method() {
	mut writer := responses.Writer{}
	assert writer.write(Payload[int]{ value: 42 }) == '{"value":42}'
	assert writer.write(Payload[string]{ value: 'ok' }) == '{"value":"ok"}'
}
