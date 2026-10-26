module json2

import json2.decoder2

type LegacySigned = isize

struct LegacyInteger {
	value isize
}

fn test_legacy_integer_encoding_and_decoding() ! {
	assert encode(isize(-42)) == '-42'
	assert encode(LegacyInteger{-42}) == '{"value":-42}'
	assert decode[isize]('-42')! == -42
	assert decode[LegacySigned]('-42')! == -42
	assert decode[LegacyInteger]('{"value":-42}')!.value == -42
	assert decode[LegacyInteger]('{"value":"-42"}')!.value == -42
	assert decoder2.decode[isize]('-42')! == -42
	assert decoder2.decode[LegacySigned]('-42')! == -42
	assert decoder2.decode[LegacyInteger]('{"value":-42}')!.value == -42
}
