module asn1

// taken examples from https://letsencrypt.org/id/docs/a-warm-welcome-to-asn1-and-der/#explicit-vs-implicit
fn test_encode_with_or_without_options() ! {
	// Utf8String with tag == 12 (0c)
	obj := Utf8String.new('hi')!

	// without options
	out_1 := encode(obj)!
	normal := [u8(0x0C), 0x02, 0x68, 0x69]
	assert out_1 == normal

	// with implicit definded as [5] IMPLICIT UTF8String was serialized into 85 02 68 69
	impl_expected := [u8(0x85), 0x02, 0x68, 0x69]
	out_2 := encode_with_options(obj, 'context_specific:5;implicit;inner:12')!
	assert out_2 == impl_expected

	// as explicit tagging defined as [5] EXPLICIT UTF8String encoded into A5 04 0C 02 68 69
	expl_expected := [u8(0xA5), 0x04, 0x0C, 0x02, 0x68, 0x69]
	out_3 := encode_with_options(obj, 'context_specific:5;explicit;inner:0x0c')!
	assert out_3 == expl_expected
}
