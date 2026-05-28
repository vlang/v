module main

import encoding.cbor

fn test_smoke_uint() {
	out := cbor.encode[u64](u64(0), cbor.EncodeOpts{})!
	assert out == [u8(0x00)]
}
