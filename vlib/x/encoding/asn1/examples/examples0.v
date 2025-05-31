module main

import x.encoding.asn1

// Previously, defined type KerberosString = asn1.GeneralString directly
// without reduplicating required Element methods, would produces
// RUNTIME ERROR: invalid memory access.
// **Updates**
// Its has been resolved in [22901](https://github.com/vlang/v/issues/22901)
// Thanks to @felipensp
type KerberosString = asn1.GeneralString

fn KerberosString.new(s string) !KerberosString {
	return KerberosString(asn1.GeneralString.new(s)!)
}

type KerberosStringList = []KerberosString

fn (ksl KerberosStringList) tag() asn1.Tag {
	return asn1.default_sequence_tag
}

fn (ksl KerberosStringList) payload() ![]u8 {
	mut out := []u8{}
	for item in ksl {
		// maybe produces x00000000: at ???: RUNTIME ERROR: invalid memory access
		// `item` cannot be used as interface object outside `unsafe` blocks as it
		// might be stored on stack. Consider declaring `KerberosString` as `@[heap]`
		obj := unsafe { item }
		out << asn1.encode(obj)!
	}
	return out
}

// PrincipalName   ::= SEQUENCE {
//    name-type       [0] Int32,
//    name-string     [1] SEQUENCE OF KerberosString
// }
struct PrincipalName {
	name_type asn1.Integer @[context_specific: 0; explicit; inner: 2] // integer tag = (universal, false, 2)

	// defines name_string  as asn1.SequenceOf[KerberosString] would produces unexpected result
	// when you build payload with `asn1.make_payload`,
	// see issues at https://github.com/vlang/v/issues/22721
	// so we build with KerberosStringList
	name_string KerberosStringList @[context_specific: 1; explicit; inner: 16] // sequence tag = (universal, true, 16)
}

fn (pn PrincipalName) tag() asn1.Tag {
	return asn1.default_sequence_tag
}

fn (pn PrincipalName) payload() ![]u8 {
	kd := asn1.new_key_default()
	payload := asn1.make_payload[PrincipalName](pn, kd)!
	return payload
}

fn PrincipalName.decode(bytes []u8) !PrincipalName {
	// decode should produces Sequence type
	elem := asn1.decode(bytes)!
	assert elem.tag().equal(asn1.default_sequence_tag)

	// cast it into Sequence type and get the fields
	seq := elem.into_object[asn1.Sequence]()!
	fields := seq.fields()

	// every fields of the sequence is raw of wrapped element, so we should unwrap it with
	// the same options used to wrap in encode step, and turn to the real underlying object.
	el_name_type := fields[0].unwrap_with_options('context_specific: 0; explicit; inner: 2')!
	name_type := el_name_type.into_object[asn1.Integer]()!

	// tag 16 is sequence tag, its decoded into Sequence type
	el_name_string := fields[1].unwrap_with_options('context_specific: 1; explicit; inner: 16')!
	el_seq := el_name_string.into_object[asn1.Sequence]()!

	// KerberosString string has a general_string tag, so it would be parsed as asn1.GeneralString
	// so, we transforms it into KerberosString back and build KerberosStringList.
	mut a := []KerberosString{}
	for item in el_seq.fields() {
		gst := item.into_object[asn1.GeneralString]()!
		obj := KerberosString(gst)
		a << obj
	}

	name_string_list := KerberosStringList(a)

	return PrincipalName{
		name_type:   name_type
		name_string: name_string_list
	}
}

fn main() {
	// Basically this is a Kerberos PrincipalName data
	data := [u8(0x30), 0x15, 0xa0, 0x03, 0x02, 0x01, 0x01, 0xa1, 0x0e, 0x30, 0x0c, 0x1b, 0x0a,
		0x62, 0x6f, 0x62, 0x62, 0x61, 0x2d, 0x66, 0x65, 0x74, 0x74]
	els := [KerberosString.new('bobba-fett')!]

	pn := PrincipalName{
		name_type:   asn1.Integer.from_int(1)
		name_string: KerberosStringList(els)
	}

	out1 := asn1.encode(pn)!
	back := PrincipalName.decode(data)!
	dump(back == pn) // should assert to true
	dump(out1.hex() == data.hex()) // should assert to true

	// run this produces:
	// [examples/examples_0.v:128] back == pn: true
	// [examples/examples_0.v:129] out1.hex() == data.hex(): true
}
