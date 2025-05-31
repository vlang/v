import x.encoding.asn1

struct AExample {
	a asn1.OctetString
}

fn (a AExample) tag() asn1.Tag {
	return asn1.default_sequence_tag
}

fn (a AExample) payload() ![]u8 {
	mut out := []u8{}
	out << asn1.encode(a.a)!
	return out
}

// BExample is aliased type, without redefined methods of AExample
type BExample = AExample

fn test_main() {
	exa := AExample{
		a: asn1.OctetString.new('hi')!
	}
	exb := BExample(exa)
	assert '${exa.tag()}' == 'universal-true-16'
	assert exa.payload()!.len == 4
	assert asn1.encode(exa)!.len == 6

	assert '${exb.tag()}' == 'universal-true-16'
	assert exb.payload()!.len == 4
	assert asn1.encode(exb)!.len == 6
}
