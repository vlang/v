module asn1

// ASN.1 OPTIONAL Element.
//
// note: At the abstract ASN.1 level the absence of a DEFAULT value in an encoding is the same as its being present.
// Contrast this with OPTIONAL, where a value being present in the encoding is semantically distinct from its being absent.
// In some encoding rules (like BER/PER) it is at the whim of the sender whether a DEFAULT value is encoded or not
// (except for primitive type values in PER which are required by the PER standard to be absent in the encoding),
// while with others (like DER) the DEFAULT value is NEVER encoded. For all encoding rules,
// if the component that has a DEFAULT value is not encoded the receiving application must behave as though the DEFAULT value had been encoded.
pub struct Optional {
	// underlying element marked as an optional
	elem Element
mut:
	// presence of this flag negates optionality of this elemeent.
	// set to true when its should present, if not sure, just set to to false
	present bool
}

// new creates and marked element as an Optional element.
pub fn Optional.new(el Element, with_present bool) !Optional {
	if el is Optional {
		return error('recursive optional is not allowed')
	}
	return Optional{
		elem:    el
		present: with_present
	}
}

// set_present_bit sets this Optional element with flag in present.
pub fn (mut opt Optional) set_present_bit(present bool) {
	opt.present = present
}

// tag returns the tag of Optional element.
pub fn (opt Optional) tag() Tag {
	return opt.elem.tag()
}

// payload the payload of Optional element.
pub fn (opt Optional) payload() ![]u8 {
	return opt.elem.payload()!
}

// encode serializes this Optional element into bytes array.
pub fn (opt Optional) encode() ![]u8 {
	return opt.encode_with_rule(.der)!
}

fn (opt Optional) encode_with_rule(rule EncodingRule) ![]u8 {
	if opt.present {
		// elem := opt.into_element()!
		return encode_with_rule(opt, .der)!
	}
	// not present
	return []u8{}
}

// into_element turns this optional into Element.
pub fn (opt Optional) into_element() !Element {
	return parse_element(opt.tag(), opt.payload()!)!
}

// into_object tries to turns this optional into real underlying object T.
// Its return object T on success or error on fails.
pub fn (opt Optional) into_object[T]() !T {
	$if T !is Element {
		return error('T is not element')
	}
	$if T is Optional {
		return error('T is optional')
	}
	elem := opt.into_element()!
	if elem is Optional {
		return error('elem is also optional')
	}
	return elem.into_object[T]()!
}
