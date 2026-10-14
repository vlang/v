import json2

struct SharedDecodeState {
	head    int @[required]
	ignored int @[skip; required]
	f00     int
	f01     int
	f02     int
	f03     int
	f04     int
	f05     int
	f06     int
	f07     int
	f08     int
	f09     int
	f10     int
	f11     int
	f12     int
	f13     int
	f14     int
	f15     int
	f16     int
	f17     int
	f18     int
	f19     int
	f20     int
	f21     int
	f22     int
	f23     int
	f24     int
	f25     int
	f26     int
	f27     int
	f28     int
	f29     int
	f30     int
	f31     int
	f32     int
	f33     int
	f34     int
	f35     int
	f36     int
	f37     int
	f38     int
	f39     int
	f40     int
	f41     int
	f42     int
	f43     int
	f44     int
	f45     int
	f46     int
	f47     int
	f48     int
	f49     int
	f50     int
	f51     int
	f52     int
	f53     int
	f54     int
	f55     int
	f56     int
	f57     int
	f58     int
	f59     int
	f60     int
	f61     int
	f62     int
	f63     int
	tail    int @[json: 'renamed'; required]
}

fn test_shared_struct_decode_state_tracks_fields_beyond_mask_and_skips() {
	text := '{"unknown":{"nested":[1,2]},"head":1,"ignored":false,"renamed":2}'
	value := json2.decode[SharedDecodeState](text)!
	assert value.head == 1
	assert value.ignored == 0
	assert value.tail == 2
	mut buffer := json2.DecodeBuffer{}
	reused := json2.decode_reuse[SharedDecodeState](text, mut buffer)!
	assert reused == value
	for invalid in ['{"head":1,"ignored":0}', '{"head":1,"renamed":2}',
		'{"head":null,"ignored":0,"renamed":2}'] {
		json2.decode[SharedDecodeState](invalid) or {
			assert err.msg().contains('required field')
			continue
		}
		assert false, 'required field should have been rejected'
	}
	json2.decode[SharedDecodeState]('{"head":1,"ignored":0,"renamed":') or {
		return
	}
	assert false, 'truncated input should have been rejected'
}
