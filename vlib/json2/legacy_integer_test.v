// vtest vflags: -w
// Deliberate deprecated-type coverage must also run in suites using -W.
module json2

type LegacySigned = int

struct LegacyInteger {
	value int
}

fn test_legacy_integer_encoding_and_decoding() ! {
	assert encode(int(-42)) == '-42'
	assert encode(LegacyInteger{-42}) == '{"value":-42}'
	assert decode[int]('-42')! == -42
	assert decode[LegacySigned]('-42')! == -42
	assert decode[LegacyInteger]('{"value":-42}')!.value == -42
	assert decode[LegacyInteger]('{"value":"-42"}')!.value == -42
}
